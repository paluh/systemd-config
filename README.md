# systemd-config
Simple haskell helper for systemd configs generation. Currently it is quite verbose:

  ```haskell
    module Main where

    import Net.Types (IPv4Range(IPv4Range))
    import Net.IPv4 (fromOctets)
    import System.Systemd.Config.Unit (pattern Value)
    import System.Systemd.Config.Networkd.NetDev (Device(BridgeDevice), netDev, writeNetDev')
    import System.Systemd.Config.Networkd.Network (NetworkConfig(match, network),
                                                   Network(networkAddress, networkBridge, networkGateway),
                                                   Match(matchName), writeNetwork')
    import qualified System.Systemd.Config.Nspawn as Nspawn

    main :: IO ()
    main = do
      let
        bridge = "br01"

      -- writes config to /etc/systemd/network
      -- you can use writeNetDev if you want
      -- to change location
      writeNetDev'
        (netDev bridge (BridgeDevice mempty))

      let
        network = mempty
          { networkAddress = Value (IPv4Range (fromOctets 10 0 0 0) 24)
          , networkBridge = Value bridge
          , networkGateway = Value (fromOctets 10 0 0 1)
          }
        match = mempty { matchName = Value bridge }

      writeNetwork'
        bridge
        mempty { network = network }

      Nspawn.writeNspawn'
        "my-new-machine"
        (mempty
          { Nspawn.network = mempty { Nspawn.networkBridge = Value bridge }
          , Nspawn.exec = mempty { Nspawn.execBoot = Value True }
          }
        )

      return ()
    ```

This will generate:

    * /etc/systemd/network/br01.netdev

        ```ini
        [NetDev]
        Kind=bridge
        Name=br01
        ```

    * /etc/systemd/network/br01.network

        ```ini
        [Match]
        Name=br01

        [Network]
        Address=10.0.0.0/24
        Bridge=br01
        Gateway=10.0.0.1
        ```

    * /var/lib/machines/my-new-machine.nspawn

        ```ini
        [Exec]
        Boot=yes

        [Network]
        Bridge=br01
        ```

## Limitations
I'm implementing only those parts of systemd configuration which I currently need. If you need antyhing more, pull requests are really welcome.

## Design
This library does not prevent you from creating incomplete configuration files as they can be used as partial configs which overrides...

There are `Monoids` instances (based on `Last` - there is handy `PatternSynonyms` defined: `Value` and `Missing`) for config data types, but they should be rethinked in some cases.

module System.Systemd.Config.Nspawn where

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Systemd.Config.Unit (section, showBool, Unit)

-- systemd v231

-- ExecStart=/usr/bin/systemd-nspawn --quiet --keep-unit --boot --link-journal=try-guest  --settings=override -n --machine=%I --network-macvlan="$i"
data Exec = Exec
  { -- Takes a boolean argument, which defaults to off. If enabled, systemd-nspawn will automatically
    -- search for an init executable and invoke it. In this case, the specified parameters using
    -- Parameters= are passed as additional arguments to the init process. This setting corresponds to
    -- the --boot switch on the systemd-nspawn command line. This option may not be combined with
    -- ProcessTwo=yes. This option is the default if the systemd-nspawn@.service template unit file is
    -- used.
    execBoot :: Maybe Bool
  }

execSection :: Exec -> Unit
execSection Exec {..} =
  section "Exec" options
 where
  options = [("Boot", showBool <$> execBoot)]

data Network = Network
  { -- Takes an interface name. This setting implies VirtualEthernet=yes and
    -- Private=yes and has the effect that the host side of the created virtual
    -- Ethernet link is connected to the specified bridge interface. This option
    -- corresponds to the --network-bridge= command line switch. This option is
    -- privileged (see above).
    networkBridge :: Maybe Text
    -- Takes a space-separated list of interfaces to add to the container. This
    -- option corresponds to the --network-interface= command line switch and
    -- implies Private=yes. This option is privileged (see above).
  , networkInterface :: [Text]
    -- Takes a space-separated list of interfaces to add IPVLAN
    -- interfaces to, which are then added to the container. These options
    -- correspond to the --network-ipvlan= command line
    -- switch and imply Private=yes. These option are privileged (see above).
  , networkIPVLAN :: [Text]
    -- Takes a space-separated list of interfaces to add MACLVAN
    -- interfaces to, which are then added to the container. These options
    -- correspond to the --network-macvlan= command line
    -- switch and imply Private=yes. These option are privileged (see above).
  , networkMACVLAN :: [Text]
  }

networkSection :: Network -> Unit
networkSection Network {..} =
  section "Network" options
 where
  options =
    [("Bridge", networkBridge)] <>
    map (\i -> ("Interface", Just i)) networkInterface <>
    map (\i -> ("IPVLAN", Just i)) networkIPVLAN <>
    map (\m -> ("MACVLAN", Just m)) networkMACVLAN

data NspawnConfig = NspawnConfig
  { exec :: Exec
  , network :: Network
  }

toUnit :: NspawnConfig -> Unit
toUnit NspawnConfig {..} = execSection exec <> networkSection network

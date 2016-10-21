module System.Systemd.Config.Networkd.Network where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Net.Mac.Text as Mac
import qualified Net.IPv4.Text as IPv4
import qualified Net.IPv4.Range.Text as IPv4Range
import Net.Types (Mac, IPv4, IPv4Range)
import System.Systemd.Config.Unit (Architecture, showArchitecture, showBool, Unit, section)

-- systemd v231

data Match = Match
  { -- Checks whether the system is running on a specific architecture. See "ConditionArchitecture=" in systemd.unit(5) for details.
    matchArchitecture :: Maybe Architecture
    -- A whitespace-separated list of shell-style globs matchIng the driver currently bound to the
    -- device, matchas exposed by the udev property "DRIVER" of its parent device, matchor if that is not set the
    -- driver as exposed by "ethtool -i" of the device itself.
  , matchDriver :: Maybe Text
    -- Matches against the hostname or machine ID of the host. See "ConditionHost=" in systemd.unit(5) for details.
  , matchHost :: Maybe Text
    -- Checks whether a specific kernel command line option is set (or if prefixed with the exclamation mark unset). See "ConditionKernelCommandLine=" in systemd.unit(5) for details.
  , matchKernelCommandLine :: Maybe Text
    -- The hardware address of the interface (use full colon-delimited hexadecimal, matche.g., match01:23:45:67:89:ab).
  , matchMacAddress :: Maybe Mac
    -- A whitespace-separated list of shell-style globs matchIng the device name, matchAs exposed by the udev property "INTERFACE".
  , matchName :: Maybe Text
    -- A whitespace-separated list of shell-style globs matchIng the persistent path, matchAs exposed by the
    -- udev property "ID_PATH".
  , matchPath :: Maybe Text
    -- A whitespace-separated list of shell-style globs matchIng the device type, matchAs exposed by the udev property "DEVTYPE".
  , matchTypeOfDevice :: Maybe Text
    -- Checks whether the system is executed in a virtualized environment and optionally test whether it
    -- is a specific implementation. See "ConditionVirtualization=" in systemd.unit(5) for details.
  , matchVirtualization :: Maybe Bool
  }

matchSection :: Match -> Unit
matchSection Match {..} =
  section "Match" options
 where
  options =
    [ ("Architecture", showArchitecture <$> matchArchitecture)
    , ("Driver", matchDriver)
    , ("Host", matchHost)
    , ("KernelCommandLine", matchKernelCommandLine)
    , ("MACAddress", Mac.encode <$> matchMacAddress)
    , ("Name", matchName)
    , ("Path", matchPath)
    , ("Type", matchTypeOfDevice)
    , ("Virtualization", showBool <$> matchVirtualization)
    ]

-- Accepts "yes", "no", "ipv4", or "ipv6".
data NetworkDHCP = EnableDHCP | DisableDHCP | EnableIPv4DHCP | EnableIPv6DHCP
showNetworkDHCP :: NetworkDHCP -> Text
showNetworkDHCP EnableDHCP = "yes"
showNetworkDHCP DisableDHCP = "no"
showNetworkDHCP EnableIPv4DHCP = "ipv4"
showNetworkDHCP EnableIPv6DHCP = "ipv6"

-- XXX: handle IPv6 setup...
data Network = Network
  { -- A description of the device. This is only used for presentation purposes.
    networkDescription :: Maybe Text
    -- A static IPv4 or IPv6 address and its prefix length, separated by a "/" character. Specify this key more than once to configure several addresses. The format of the address must be as described in inet_pton(3). This is a
    -- short-hand for an [Address] section only containing an Address key (see below). This option may be specified more than once.
    --
    -- If the specified address is 0.0.0.0 (for IPv4) or [::] (for IPv6), a new address range of the requested size is automatically allocated from a system-wide pool of unused ranges. The allocated range is checked against all
    -- current network interfaces and all known network configuration files to avoid address range conflicts. The default system-wide pool consists of 192.168.0.0/16, 172.16.0.0/12 and 10.0.0.0/8 for IPv4, and fc00::/7 for IPv6.
    -- This functionality is useful to manage a large number of dynamically created network interfaces with the same network configuration and automatic address range assignment.
  , networkAddress :: Maybe IPv4Range
    -- The gateway address, which must be in the format described in inet_pton(3). This is a short-hand for a [Route] section only containing a Gateway key. This option may be specified more than once.
  , networkGateway :: Maybe IPv4
    -- A DNS server address, which must be in the format described in inet_pton(3). This option may be specified more than once. This setting is read by systemd-resolved.service(8).
  , networkDNS :: [IPv4]
    -- Enables DHCPv4 and/or DHCPv6 client support. Accepts "yes", "no", "ipv4", or "ipv6".
    --
    -- Note that DHCPv6 will by default be triggered by Router Advertisement, if that is enabled, regardless of this parameter. By enabling DHCPv6 support explicitly, the DHCPv6 client will be started regardless of the presence
    -- of routers on the link, or what flags the routers pass. See "IPv6AcceptRA=".
    --
    -- Furthermore, note that by default the domain name specified through DHCP is not used for name resolution. See option UseDomains= below.
    --
    -- See the "[DHCP]" section below for further configuration options for the DHCP client support.
  , networkDHCP :: Maybe NetworkDHCP
    -- The name of the bridge to add the link to.
  , networkBridge :: Maybe Text
    -- The name of a MACVLAN to create on the link. This option may be specified more than once.
  , networkMACVLAN :: Maybe Text
  }

networkSection :: Network -> Unit
networkSection Network {..} =
  section "Network" options
 where
  options =
    [ ("Address", IPv4Range.encode <$> networkAddress)
    , ("Bridge", networkBridge)
    , ("Description", networkDescription)
    , ("DHCP", showNetworkDHCP <$> networkDHCP)
    ] <>
    map (\dns -> ("DNS", Just . IPv4.encode $ dns)) networkDNS <>
    [ ("Gateway", IPv4.encode <$> networkGateway)
    , ("MACVLAN", networkMACVLAN)
    ]

data NetworkConfig = NetworkConfig
  { match :: Match
  , network :: Network
  }

toUnit :: NetworkConfig -> Unit
toUnit NetworkConfig {..} = matchSection match <> networkSection network

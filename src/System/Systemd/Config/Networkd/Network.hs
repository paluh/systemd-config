module System.Systemd.Config.Networkd.Network where

import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.IO (writeFile)
import qualified Net.Mac.Text as Mac
import qualified Net.IPv4.Text as IPv4
import qualified Net.IPv4.Range.Text as IPv4Range
import Net.Types (Mac, IPv4, IPv4Range)
import Path (Abs, Dir, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import System.Systemd.Config.Unit (Architecture, showArchitecture, showBool, Unit, section, printUnit)

-- systemd v231

data Match = Match
  { matchArchitecture :: Maybe Architecture
  , matchDriver :: Maybe Text
  , matchHost :: Maybe Text
  , matchKernelCommandLine :: Maybe Text
  , matchMacAddress :: Maybe Mac
  , matchName :: Maybe Text
  , matchPath :: Maybe Text
  , matchTypeOfDevice :: Maybe Text
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
  { networkDescription :: Maybe Text
  , networkAddress :: Maybe IPv4Range
  , networkGateway :: Maybe IPv4
  , networkDNS :: [IPv4]
  , networkDHCP :: Maybe NetworkDHCP
  , networkBridge :: Maybe Text
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

type NetworkDir = Path Abs Dir
type NetworkName = Text

writeNetwork :: NetworkName -> NetworkDir -> NetworkConfig -> IO ()
writeNetwork networkName networkDir net = do
  networkFile <- parseRelFile (unpack networkName <> ".network")
  writeFile (fromAbsFile $ networkDir </> networkFile) config
 where
  config = printUnit . toUnit $ net

writeNetwork' :: NetworkName -> NetworkConfig -> IO ()
writeNetwork' name = writeNetwork name $(mkAbsDir "/etc/systemd/network/")

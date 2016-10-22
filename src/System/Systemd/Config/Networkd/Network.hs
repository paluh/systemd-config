{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Systemd.Config.Networkd.Network where

import Data.Monoid ((<>), Last)
import Data.Text (Text, unpack)
import Data.Text.IO (writeFile)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (GMonoid, gmempty, gmappend)
import qualified Net.Mac.Text as Mac
import qualified Net.IPv4.Text as IPv4
import qualified Net.IPv4.Range as IPv4.Range
import qualified Net.IPv4.Range.Text as IPv4.Range.Text
import Net.Types (Mac, IPv4, IPv4Range)
import Path (Abs, Dir, File, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import System.Systemd.Config.Unit (Architecture, showArchitecture, showBool, Unit, section, printUnit, pattern Value)

data Match = Match
  { matchArchitecture :: Last Architecture
  , matchDriver :: Last Text
  , matchHost :: Last Text
  , matchKernelCommandLine :: Last Text
  , matchMacAddress :: Last Mac
  , matchName :: Last Text
  , matchPath :: Last Text
  , matchTypeOfDevice :: Last Text
  , matchVirtualization :: Last Bool
  }
  deriving (Generic, GMonoid, Show)

instance Monoid Match where
  mempty = gmempty
  mappend = gmappend

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
  { networkDescription :: Last Text
  , networkAddress :: Last IPv4Range
  , networkGateway :: Last IPv4
  , networkDNS :: [IPv4]
  , networkDHCP :: Last NetworkDHCP
  , networkBridge :: Last Text
  , networkMACVLAN :: Last Text
  }
  deriving (Generic, GMonoid)

instance Monoid Network where
  mempty = gmempty
  mappend = gmappend

networkSection :: Network -> Unit
networkSection Network {..} =
  section "Network" options
 where
  options =
    [ ("Address", (IPv4.Range.Text.encode . IPv4.Range.normalize) <$> networkAddress)
    , ("Bridge", networkBridge)
    , ("Description", networkDescription)
    , ("DHCP", showNetworkDHCP <$> networkDHCP)
    ] <>
    map (\dns -> ("DNS", Value . IPv4.encode $ dns)) networkDNS <>
    [ ("Gateway", IPv4.encode <$> networkGateway)
    , ("MACVLAN", networkMACVLAN)
    ]

data NetworkConfig = NetworkConfig
  { match :: Match
  , network :: Network
  }
  deriving (Generic, GMonoid)

instance Monoid NetworkConfig where
  mempty = gmempty
  mappend = gmappend

toUnit :: NetworkConfig -> Unit
toUnit NetworkConfig {..} = matchSection match <> networkSection network

type NetworkDir = Path Abs Dir
type NetworkName = Text

writeNetwork :: NetworkName -> NetworkDir -> NetworkConfig -> IO (Path Abs File)
writeNetwork networkName networkDir net = do
  networkFileName <- parseRelFile (unpack networkName <> ".network")
  let networkFile = networkDir </> networkFileName
  writeFile (fromAbsFile networkFile) config
  return networkFile
 where
  config = printUnit . toUnit $ net

writeNetwork' :: NetworkName -> NetworkConfig -> IO (Path Abs File)
writeNetwork' name = writeNetwork name $(mkAbsDir "/etc/systemd/network/")

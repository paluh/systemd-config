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
import System.Systemd.Config.Unit (showBool, Unit, section, printUnit, pattern Value)

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

data Architecture
  = X86
  | X86_64
  | Ppc
  | Ppc_le
  | Ppc64
  | Ppc64_le
  | Ia64
  | Parisc
  | Parisc64
  | S390
  | S390x
  | Sparc
  | Sparc64
  | Mips
  | Mips_le
  | Mips64
  | Mips64_le
  | Alpha
  | Arm
  | Arm_be
  | Arm64
  | Arm64_be
  | Sh
  | Sh64
  | M86k
  | Tilegx
  | Cris
  deriving (Generic, Show)

showArchitecture :: Architecture -> Text
showArchitecture X86 = "x86"
showArchitecture X86_64 = "x86-64"
showArchitecture Ppc = "ppc"
showArchitecture Ppc_le = "ppc-le"
showArchitecture Ppc64 = "ppc64"
showArchitecture Ppc64_le = "ppc64-le"
showArchitecture Ia64 = "ia64"
showArchitecture Parisc = "parisc"
showArchitecture Parisc64 = "parisc64"
showArchitecture S390 = "s390"
showArchitecture S390x = "s390x"
showArchitecture Sparc = "sparc"
showArchitecture Sparc64 = "sparc64"
showArchitecture Mips = "mips"
showArchitecture Mips_le = "mips-le"
showArchitecture Mips64 = "mips64"
showArchitecture Mips64_le = "mips64-le"
showArchitecture Alpha = "alpha"
showArchitecture Arm = "arm"
showArchitecture Arm_be = "arm-be"
showArchitecture Arm64 = "arm64"
showArchitecture Arm64_be = "arm64-be"
showArchitecture Sh = "sh"
showArchitecture Sh64 = "sh64"
showArchitecture M86k = "m86k"
showArchitecture Tilegx = "tilegx"
showArchitecture Cris = "cris"

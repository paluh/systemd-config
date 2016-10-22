module System.Systemd.Config.Networkd.NetDev where

import Data.Monoid ((<>), Last)
import qualified Data.Semigroup as Semigroup
import Data.Text (Text, pack, unpack)
import Data.Text.IO (writeFile)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (GMonoid, gmempty, gmappend)
import Net.Mac.Text (encode)
import Net.Types (Mac)
import Path (Abs, Dir, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import TextShow (showt)
import System.Systemd.Config.Unit (printUnit, section, showBool, Unit, pattern Missing, pattern Value)

-- systemd v321

data Device
  = BridgeDevice Bridge
  -- Bond
  -- | Dummy
  -- | Gre
  -- | Gretap
  -- | Ip6gre
  -- | Ip6tnln
  -- | Ip6gretap
  -- | Ipipn
  -- | Ipvlann
  -- | Macvlan
  -- | Macvtap
  -- | Sitn
  -- | Tap
  -- | Tun
  -- | Vethn
  -- | Vlan
  -- | Vtin
  -- | Vti6n
  -- | Vxlan
  -- | Vrf
  deriving (Show)

data NetDev = NetDev
  { netDevDescription :: Last Text
    -- XXX: this is incorrect
    -- we should keep track of all sums of
    -- defined devices and put here
    -- last used kind
  , netDevKind :: Device
  , netDevMtuBytes :: Last Int
  , netDevMacAddress :: Last Mac
  , netDevName :: Text
  }

netDev :: Text -> Device -> NetDev
netDev name kind = NetDev Missing kind Missing Missing name

data Bridge = Bridge
  { bridgeAgeingTimeSec :: Last Int
  , bridgeDefaultPVID :: Last Text
  , bridgeForwardDelaySec :: Last Int
  , bridgeHelloTimeSec :: Last Int
  , bridgeMaxAgeSec :: Last Int
  , bridgeMulticastQuerier :: Last Bool
  , bridgeMulticastSnooping :: Last Bool
  , bridgePriority :: Last Int
  , bridgeSTP :: Last Bool
  , bridgeVLANFiltering :: Last Bool
  }
  deriving (Generic, GMonoid, Show)

instance Monoid Bridge where
  mempty = gmempty
  mappend = gmappend

bridgeSection :: Bridge -> Unit
bridgeSection Bridge {..} =
  section "Bridge" config
 where
  config =
    [ ("AgeingTimeSec", showt <$> bridgeAgeingTimeSec)
    , ("DefaultPVID", showt <$> bridgeDefaultPVID)
    , ("ForwardDelaySec", showt <$> bridgeForwardDelaySec)
    , ("HelloTimeSec", showt <$> bridgeHelloTimeSec)
    , ("MaxAgeSec", showt <$> bridgeMaxAgeSec)
    , ("MulticastQuerier", showBool <$> bridgeMulticastQuerier)
    , ("MulticastSnooping", showBool <$> bridgeMulticastSnooping)
    , ("Priority", showt <$> bridgePriority)
    , ("STP", showBool <$> bridgeSTP)
    , ("VLANFiltering", showBool <$> bridgeVLANFiltering)
    ]

toUnit :: NetDev -> Unit
toUnit NetDev {..} =
  section "NetDev" config <> kindSection netDevKind
 where
  config =
    [ ("Description", netDevDescription)
    , ("Kind", Value . pack . encodeKind $ netDevKind)
    , ("MACAddress", encode <$> netDevMacAddress)
    , ("MTUBytes", showt <$> netDevMtuBytes)
    , ("Name", Value netDevName)
    ]
  encodeKind (BridgeDevice _) = "bridge"
  kindSection (BridgeDevice bridge) = bridgeSection bridge

type NetworkDir = Path Abs Dir

writeNetDev :: NetworkDir -> NetDev -> IO ()
writeNetDev networkDir nd = do
  netDevFile <- parseRelFile ((unpack . netDevName $ nd) <> ".netdev")
  writeFile (fromAbsFile $ networkDir </> netDevFile) config
 where
  config = printUnit . toUnit $ nd

writeNetDev' :: NetDev -> IO ()
writeNetDev' = writeNetDev $(mkAbsDir "/etc/systemd/network/")

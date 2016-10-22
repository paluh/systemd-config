module System.Systemd.Config.Networkd.NetDev where

import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (writeFile)
import Net.Mac.Text (encode)
import Net.Types (Mac)
import Path (Abs, Dir, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import TextShow (showt)
import System.Systemd.Config.Unit (printUnit, section, showBool, Unit)

-- systemd v321

data Kind
  = BridgeKind Bridge
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
  { netDevDescription :: Maybe Text
  , netDevKind :: Kind
  , netDevMtuBytes :: Maybe Int
  , netDevMacAddress :: Maybe Mac
  , netDevName :: Text
  }

netDev :: Text -> Kind -> NetDev
netDev name kind = NetDev Nothing kind Nothing Nothing name

data Bridge = Bridge
  { bridgeAgeingTimeSec :: Maybe Int
  , bridgeDefaultPVID :: Maybe Text
  , bridgeForwardDelaySec :: Maybe Int
  , bridgeHelloTimeSec :: Maybe Int
  , bridgeMaxAgeSec :: Maybe Int
  , bridgeMulticastQuerier :: Maybe Bool
  , bridgeMulticastSnooping :: Maybe Bool
  , bridgePriority :: Maybe Int
  , bridgeSTP :: Maybe Bool
  , bridgeVLANFiltering :: Maybe Bool
  }
  deriving (Show)

emptyBridge :: Bridge
emptyBridge =
  Bridge
    Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

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
    , ("Kind", Just . pack . encodeKind $ netDevKind)
    , ("MACAddress", encode <$> netDevMacAddress)
    , ("MTUBytes", showt <$> netDevMtuBytes)
    , ("Name", Just netDevName)
    ]
  encodeKind (BridgeKind _) = "bridge"
  kindSection (BridgeKind bridge) = bridgeSection bridge

type NetworkDir = Path Abs Dir

writeNetDev :: NetworkDir -> NetDev -> IO ()
writeNetDev networkDir nd = do
  netDevFile <- parseRelFile ((unpack . netDevName $ nd) <> ".netdev")
  writeFile (fromAbsFile $ networkDir </> netDevFile) config
 where
  config = printUnit . toUnit $ nd

writeNetDev' :: NetDev -> IO ()
writeNetDev' = writeNetDev $(mkAbsDir "/etc/systemd/network/")

module System.Systemd.Config.Networkd.NetDev where

import Data.Text (toLower, Text, pack)
import Data.List (sort)
import Net.Mac.Text (encode)
import Net.Types (Mac)
import TextShow (showt)
import System.Systemd.Config.Unit (printUnit, Unit(Unit))

-- systemd v321

data Kind
  = Bond
  | Bridge
  | Dummy
  | Gre
  | Gretap
  | Ip6gre
  | Ip6tnln
  | Ip6gretap
  | Ipipn
  | Ipvlann
  | Macvlan
  | Macvtap
  | Sitn
  | Tap
  | Tun
  | Vethn
  | Vlan
  | Vtin
  | Vti6n
  | Vxlan
  | Vrf
  deriving (Show)

data NetDev = NetDev
  { description :: Maybe Text
  , kind :: Maybe Kind
  , mtuBytes :: Maybe Int
  , macAddress :: Maybe Mac
  , name :: Text
  }

netDev :: Text -> NetDev
netDev = NetDev Nothing Nothing Nothing Nothing

toUnit :: NetDev -> Unit
toUnit NetDev {..} =
  Unit [("NetDev", config)]
 where
  config = sort $ ("Name", name) : [(k, v) | (k, Just v) <- vs]
  vs = [ ("Description", description)
       , ("Kind", toLower . pack . show <$> kind)
       , ("MACAddress", encode <$> macAddress)
       , ("MTUBytes", showt <$> mtuBytes)
       ]

toConfig :: NetDev -> Text
toConfig = printUnit . toUnit

module System.Systemd.Config.Networkd.NetDevSpec where

import           Data.Text (unlines)
import           Net.Mac (fromOctets)
import           Prelude hiding (unlines)
import           Test.Hspec (describe, it, shouldBe, Spec)
import           System.Systemd.Config.Networkd.NetDev (toUnit, netDev, Device(BridgeDevice), NetDev(..))
import           System.Systemd.Config.Unit (printUnit, pattern Value)

suite :: Spec
suite =
  describe "System.Systemd.Config.Networkd.NetDev" $ do
    it "generates correct unit for empty config" $
      (printUnit . toUnit $ netDev "br0" (BridgeDevice mempty)) `shouldBe` "[NetDev]\nKind=bridge\nName=br0\n"
    it "generates correct unit for fully customized config" $
      shouldBe
        (printUnit . toUnit $
          NetDev
            { netDevDescription = Value "A free-form description..."
            , netDevKind = BridgeDevice mempty
            , netDevMtuBytes = Value 1500
            , netDevMacAddress = Value (fromOctets 0xf0 0xde 0xf1 0x62 0x90 0x55)
            , netDevName = "br0"
            })
        (unlines
          [ "[NetDev]"
          , "Description=A free-form description..."
          , "Kind=bridge"
          , "MACAddress=f0:de:f1:62:90:55"
          , "MTUBytes=1500"
          , "Name=br0"
          ])


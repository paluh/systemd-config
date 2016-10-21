module System.Systemd.Config.Networkd.NetDevSpec where

import           Data.Text (unlines)
import           Net.Mac (fromOctets)
import           Prelude hiding (unlines)
import           Test.Hspec (describe, it, shouldBe, Spec)
import           System.Systemd.Config.Networkd.NetDev (toConfig, netDev, Kind(Bridge), NetDev(..))

suite :: Spec
suite =
  describe "System.Systemd.Config.Networkd.NetDev" $ do
    it "generates correct unit for empty config" $
      toConfig (netDev "br0") `shouldBe` "[NetDev]\nName=br0\n"
    it "generates correct unit for fully customized config" $
      shouldBe
        (toConfig
          NetDev
            { description = Just "A free-form description..."
            , kind = Just Bridge
            , mtuBytes = Just 1500
            , macAddress = Just (fromOctets 0xf0 0xde 0xf1 0x62 0x90 0x55)
            , name = "br0"
            })
        (unlines
          [ "[NetDev]"
          , "Description=A free-form description..."
          , "Kind=bridge"
          , "MACAddress=f0:de:f1:62:90:55"
          , "MTUBytes=1500"
          , "Name=br0"
          ])


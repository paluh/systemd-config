{-# LANGUAGE OverloadedStrings #-}
import           System.Systemd.Config.Networkd.NetDevSpec as NetDevSpec
import           System.Systemd.Config.Nspawn.CommandSpec as CommandSpec
import           Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  NetDevSpec.suite
  CommandSpec.suite

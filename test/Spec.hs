{-# LANGUAGE OverloadedStrings #-}
import           System.Systemd.Config.Networkd.NetDevSpec as NetDevSpec
import           Test.Hspec (hspec)

main :: IO ()
main = hspec NetDevSpec.suite

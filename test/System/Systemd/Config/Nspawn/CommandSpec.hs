module System.Systemd.Config.Nspawn.CommandSpec where

import           System.Systemd.Config.Nspawn.Command (emptyOptions, Machine(..), nspawnCommand, Options(..), Settings(..), boot, PersistencyMode(..))
import           Test.Hspec (describe, it, shouldBe, Spec)


-- ExecStart=/usr/bin/systemd-nspawn --quiet --keep-unit --boot --link-journal=try-guest --network-veth -U --settings=override --machine=%i
suite :: Spec
suite =
  describe "System.Systemd.Config.Nspan.Command" $ do
    it "generates correct command for simple boot" $
      shouldBe
        (nspawnCommand (boot (MachineName "debian-jessie-01" (Persistent Nothing))))
        "/usr/bin/systemd-nspawn --machine debian-jessie-01 --boot"

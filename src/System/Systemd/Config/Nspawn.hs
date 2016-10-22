module System.Systemd.Config.Nspawn where

import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.IO (writeFile)
import Path (Abs, Dir, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import System.Systemd.Config.Unit (printUnit, section, showBool, Unit)

-- systemd v231

data Exec = Exec
  { execBoot :: Maybe Bool
  }

emptyExec :: Exec
emptyExec = Exec Nothing

execSection :: Exec -> Unit
execSection Exec {..} =
  section "Exec" options
 where
  options = [("Boot", showBool <$> execBoot)]

data Network = Network
  { networkBridge :: Maybe Text
  , networkInterface :: [Text]
  , networkIPVLAN :: [Text]
  , networkMACVLAN :: [Text]
  }

emptyNetwork :: Network
emptyNetwork = Network Nothing [] [] []

networkSection :: Network -> Unit
networkSection Network {..} =
  section "Network" options
 where
  options =
    [("Bridge", networkBridge)] <>
    map (\i -> ("Interface", Just i)) networkInterface <>
    map (\i -> ("IPVLAN", Just i)) networkIPVLAN <>
    map (\m -> ("MACVLAN", Just m)) networkMACVLAN

data Nspawn = Nspawn
  { exec :: Exec
  , network :: Network
  }

toUnit :: Nspawn -> Unit
toUnit Nspawn {..} = execSection exec <> networkSection network

type MachinesDir = Path Abs Dir
type MachineName = Text

writeNspawn :: MachineName -> MachinesDir -> Nspawn -> IO ()
writeNspawn machineName machinesDir nspawn = do
  machineFile <- parseRelFile (unpack machineName <> ".nspawn")
  writeFile (fromAbsFile $ machinesDir </> machineFile) config
 where
  config = printUnit . toUnit $ nspawn

writeNspawn' :: MachineName -> Nspawn -> IO ()
writeNspawn' machineName  = writeNspawn machineName $(mkAbsDir "/var/lib/machines")

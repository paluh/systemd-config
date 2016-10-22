module System.Systemd.Config.Nspawn where

import Data.Monoid ((<>), Last)
import Data.Text (Text, unpack)
import Data.Text.IO (writeFile)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (GMonoid, gmempty, gmappend)
import Path (Abs, Dir, fromAbsFile, mkAbsDir, Path, (</>), parseRelFile)
import Prelude hiding (writeFile)
import System.Systemd.Config.Unit (printUnit, section, showBool, Unit, pattern Value)


data Exec = Exec
  { execBoot :: Last Bool
  }
  deriving (Generic, GMonoid, Show)

instance Monoid Exec where
  mempty = gmempty
  mappend = gmappend

execSection :: Exec -> Unit
execSection Exec {..} =
  section "Exec" options
 where
  options = [("Boot", showBool <$> execBoot)]

data Network = Network
  { networkBridge :: Last Text
  , networkInterface :: [Text]
  , networkIPVLAN :: [Text]
  , networkMACVLAN :: [Text]
  }
  deriving (Generic, GMonoid, Show)

instance Monoid Network where
  mempty = gmempty
  mappend = gmappend

networkSection :: Network -> Unit
networkSection Network {..} =
  section "Network" options
 where
  options =
    [("Bridge", networkBridge)] <>
    map (\i -> ("Interface", Value i)) networkInterface <>
    map (\i -> ("IPVLAN", Value i)) networkIPVLAN <>
    map (\m -> ("MACVLAN", Value m)) networkMACVLAN

data Nspawn = Nspawn
  { exec :: Exec
  , network :: Network
  }
  deriving (Generic, GMonoid, Show)

instance Monoid Nspawn where
  mempty = gmempty
  mappend = gmappend

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

module System.Systemd.Config.Nspawn where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Path (Abs, Dir, File, fromAbsFile, mkAbsDir, Path, Rel, (</>))
import Prelude hiding (writeFile)
import System.Systemd.Config.Unit (printUnit, section, showBool, Unit)

-- systemd v231

data Exec = Exec
  { -- Takes a boolean argument, which defaults to off. If enabled, systemd-nspawn will automatically
    -- search for an init executable and invoke it. In this case, the specified parameters using
    -- Parameters= are passed as additional arguments to the init process. This setting corresponds to
    -- the --boot switch on the systemd-nspawn command line. This option may not be combined with
    -- ProcessTwo=yes. This option is the default if the systemd-nspawn@.service template unit file is
    -- used.
    execBoot :: Maybe Bool
  }

emptyExec :: Exec
emptyExec = Exec Nothing

execSection :: Exec -> Unit
execSection Exec {..} =
  section "Exec" options
 where
  options = [("Boot", showBool <$> execBoot)]

data Network = Network
  { -- Takes an interface name. This setting implies VirtualEthernet=yes and
    -- Private=yes and has the effect that the host side of the created virtual
    -- Ethernet link is connected to the specified bridge interface. This option
    -- corresponds to the --network-bridge= command line switch. This option is
    -- privileged (see above).
    networkBridge :: Maybe Text
    -- Takes a space-separated list of interfaces to add to the container. This
    -- option corresponds to the --network-interface= command line switch and
    -- implies Private=yes. This option is privileged (see above).
  , networkInterface :: [Text]
    -- Takes a space-separated list of interfaces to add IPVLAN
    -- interfaces to, which are then added to the container. These options
    -- correspond to the --network-ipvlan= command line
    -- switch and imply Private=yes. These option are privileged (see above).
  , networkIPVLAN :: [Text]
    -- Takes a space-separated list of interfaces to add MACLVAN
    -- interfaces to, which are then added to the container. These options
    -- correspond to the --network-macvlan= command line
    -- switch and imply Private=yes. These option are privileged (see above).
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
type MachineName = Path Rel File

writeNspawn :: MachineName -> MachinesDir -> Nspawn -> IO ()
writeNspawn machineName machinesDir nspawn =
  writeFile (fromAbsFile $ machinesDir </> machineName) config
 where
  config = printUnit . toUnit $ nspawn

writeNspawn' :: MachineName -> Nspawn -> IO ()
writeNspawn' machineName  = writeNspawn machineName $(mkAbsDir "/var/lib/machines")

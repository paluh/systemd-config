module System.Systemd.Config.Nspawn.Command where

import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.Text (intercalate, Text, pack)
import Path (Abs, Dir, Path, File, fromAbsFile, fromAbsDir)

-- | --as-pid2 or --boot
data Nspawn
  -- run init stub just to execute given command
  = AsPid2 Options [Text]
  -- params to container init process
  | Boot Options [Text]
  -- use command as an init process
  | Init Options [Text]

type Template = Path Abs Dir

-- | --ephemeral or --template
data PersistencyMode
  = Ephemeral
  | Persistent (Maybe Template)

-- choose machine to spawn:
--  * --directory (and optional --machine which defines machine name in this case)
--  * --image (and optional --machine which defines machine name in this case)
--  * --machine (spawn machine which matches given name)
type MachineName = Text
data Machine
  = Directory (Path Abs Dir) PersistencyMode (Maybe MachineName)
  | Image (Path Abs File) (Maybe MachineName)
  | MachineName MachineName PersistencyMode

type HostPort = Int
type ContainerPort = Int
data Protocol = TCP | UDP
data PortForwarding
  = PordForwarding HostPort Protocol ContainerPort

data Options = Options
  { chdir :: Maybe (Path Abs Dir)
  , machine :: Machine
  , networkBridge :: [Text]
  , networkIPVLAN :: [Text]
  , networkInterface :: [Text]
  , networkMACVLAN :: [Text]
  , networkVeth :: Maybe Bool
  , privateNetwork :: Bool
  , register :: Maybe Bool
  , setEnv :: [(Text, Text)]
  , user :: Maybe Text
  }

type Command = Text
type Key = Text
type Value = Text
type Arg = Text

data CommandOption
  = Option Key Value
  | Switch Key

data CommandLine = CommandLine Command [CommandOption] [Arg]

switch :: Key -> Bool -> [CommandOption] -> [CommandOption]
switch key True opts = Switch key : opts
switch _ False opts = opts

option :: Key -> Value -> [CommandOption] -> [CommandOption]
option key value opts = Option key value : opts

opt' :: (Key -> a -> [CommandOption] -> [CommandOption]) -> Key -> Maybe a -> [CommandOption] -> [CommandOption]
opt' _ _ Nothing = id
opt' o key (Just v) = o key v

opt :: Key -> Maybe Value -> [CommandOption] -> [CommandOption]
opt key (Just value) opts = (Option key value) : opts
opt _ Nothing opts = opts

many :: Key -> [Value] -> [CommandOption] -> [CommandOption]
many key vs =
  foldl' step id vs
 where
  step b v = option key v . b

fromAbsDirT :: Path Abs Dir -> Text
fromAbsDirT = pack . fromAbsDir

fromAbsFileT :: Path Abs File -> Text
fromAbsFileT = pack . fromAbsFile

optionsAsCommandLine :: Options -> [CommandOption] -> [CommandOption]
optionsAsCommandLine Options {..} =
    machineOpts machine
  . opt' option "chdir" (fromAbsDirT <$> chdir)
  . many "network-bridge" networkBridge
  . many "network-ipvlan" networkIPVLAN
  . many "network-interface" networkInterface
  . many "network-macvlan" networkMACVLAN
  . opt' switch "network-veth" networkVeth
  . opt' option "register" (yesNo <$> register)
  . many "setenv" (map (\(k, v) -> k <> "=" <> v) setEnv)
  . opt' option "user" user
 where
  machineOpts (Directory d p mm) =
    option "directory" (fromAbsDirT d) . opt' option "machine" mm . persistencyModeOpts p
  machineOpts (Image i mm) =
    option "directory" (fromAbsFileT i) . opt' option "machine" mm
  machineOpts (MachineName m p) =
    option "machine" m . persistencyModeOpts p

  persistencyModeOpts (Persistent mt) = opt "template" (fromAbsDirT <$> mt)
  persistencyModeOpts Ephemeral = switch "ephemeral" True

  yesNo :: Bool -> Text
  yesNo True = "yes"
  yesNo False = "no"

-- asCommand :: Nspawn -> Text
-- asCommand =
--   let CommandLine command options = asCommandLine nspawn
--   in command <> intercalate " " options

asCommandLine :: Nspawn -> CommandLine
asCommandLine (AsPid2 options command) =
  CommandLine "systemd-nspawn" (switch "as-pid2" True . optionsAsCommandLine options $ []) command
asCommandLine (Boot options initArgs) =
  CommandLine "systemd-nspawn" (switch "boot" True . optionsAsCommandLine options $ []) initArgs
asCommandLine (Init options command) =
  CommandLine "systemd-nspawn" (optionsAsCommandLine options $ []) command


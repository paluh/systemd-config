module System.Systemd.Config.Nspawn.Command where

import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (unwords, Text, pack)
import Path (Abs, Dir, Path, File, fromAbsFile, fromAbsDir, mkAbsFile)
import Prelude hiding (unwords)
import TextShow (showt)

type InitOption = Text
type NspawnExecutable = Path Abs File

-- default nspan location
nspawnExecutable :: NspawnExecutable
nspawnExecutable = $(mkAbsFile "/usr/bin/systemd-nspawn")

-- | --as-pid2 or --boot
data Nspawn
  -- run init stub just to execute given command
  = AsPid2 NspawnExecutable Options (NonEmpty InitOption)
  -- params to container init process
  | Boot NspawnExecutable Options [InitOption]
  -- use command as an init process
  | Init NspawnExecutable Options (NonEmpty InitOption)


data Switch = SwitchMissing | SwitchPresent
pattern Value a = Just a
pattern Missing = Nothing

emptyOptions :: Machine -> Options
emptyOptions machine =
  Options
    { chdir = Missing
    , keepUnit = SwitchMissing
    , linkJournal = Missing
    , machine = machine
    , networkBridge = []
    , networkIPVLAN = []
    , networkInterface = []
    , networkMACVLAN = []
    , networkVeth = SwitchMissing
    , quiet = SwitchMissing
    , privateNetwork = SwitchMissing
    , privateUsers = Missing
    , privateUsersChown = SwitchMissing
    , register = Missing
    , setEnv = []
    , settings = Missing
    , user = Missing
    }

asPid2 :: Machine -> NonEmpty Text -> Nspawn
asPid2 machine =
  AsPid2 nspawnExecutable (emptyOptions machine)

boot :: Machine -> Nspawn
boot machine = Boot nspawnExecutable (emptyOptions machine) []

init :: Machine -> NonEmpty Text -> Nspawn
init machine = Init nspawnExecutable (emptyOptions machine)

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

data Settings = Settings Bool | SettingsOverride | SettingsTrusted

data LinkJournal
  = LinkJournalNo | LinkJournalHost | LinkJournalTryHost
  | LinkJournalGuest | LinkJournalTryGuest | LinkJournalAuto

type FirstUidGid = Int
type IdsNumber = Int

data PriveteUsers
  -- private-users=65536,65535
  = PrivateUsersUid FirstUidGid (Maybe IdsNumber)
  -- private-users=false/true
  | PrivateUsers Bool
  -- private-users=pick
  | PrivateUsersPick

data Options = Options
  { chdir :: Maybe (Path Abs Dir)
  , keepUnit :: Switch
  , linkJournal :: Maybe LinkJournal
  , machine :: Machine
  , networkBridge :: [Text]
  , networkIPVLAN :: [Text]
  , networkInterface :: [Text]
  , networkMACVLAN :: [Text]
  , networkVeth :: Switch
  , privateNetwork :: Switch
  , privateUsers :: Maybe PriveteUsers
  , privateUsersChown :: Switch
  , quiet :: Switch
  , register :: Maybe Bool
  , setEnv :: [(Text, Text)]
  , settings :: Maybe Settings
  , user :: Maybe Text
  }

type Command = Text
type Key = Text
type Value = Text

data CommandOption
  = Option Key Value
  | Switch Key

encodeOptions :: [CommandOption] -> [Text]
encodeOptions opts =
  foldl' (\b o -> encodeOption o . b) id opts $ []
 where
  encodeOption :: CommandOption -> [Text] -> [Text]
  encodeOption (Option k v) os = "--" <> k : v : os
  encodeOption (Switch k) os = "--" <> k : os

data CommandLine = CommandLine Command [CommandOption] [InitOption]

switch :: Key -> Switch -> [CommandOption] -> [CommandOption]
switch key SwitchPresent opts = Switch key : opts
switch _ SwitchMissing opts = opts

option :: Key -> Value -> [CommandOption] -> [CommandOption]
option key value opts = Option key value : opts

opt' :: (Key -> a -> [CommandOption] -> [CommandOption]) -> Key -> Maybe a -> [CommandOption] -> [CommandOption]
opt' _ _ Nothing = id
opt' o key (Just v) = o key v

many :: Key -> [Value] -> [CommandOption] -> [CommandOption]
many key =
  foldl' step id
 where
  step b v = option key v . b

fromAbsDirT :: Path Abs Dir -> Text
fromAbsDirT = pack . fromAbsDir

fromAbsFileT :: Path Abs File -> Text
fromAbsFileT = pack . fromAbsFile


optionsAsCommandLine :: Options -> [CommandOption] -> [CommandOption]
optionsAsCommandLine Options {..} =
  ( machineOpts machine
  . opt' option "chdir" (fromAbsDirT <$> chdir)
  . switch "keep-unit" keepUnit
  . opt' option "link-journal" (linkJournalOpt <$> linkJournal)
  . many "network-bridge" networkBridge
  . many "network-ipvlan" networkIPVLAN
  . many "network-interface" networkInterface
  . many "network-macvlan" networkMACVLAN
  . switch "network-veth" networkVeth
  . switch "private-network" privateNetwork
  . opt' option "private-users" (privateUsersOpt <$> privateUsers)
  . switch "private-users-chown" privateUsersChown
  . switch "quiet" quiet
  . opt' option "register" (yesNo <$> register)
  . many "setenv" (map (\(k, v) -> k <> "=" <> v) setEnv)
  . opt' option "settings" (settingsOpt <$> settings)
  . opt' option "user" user
  )
 where
  machineOpts (Directory d p mm) =
    option "directory" (fromAbsDirT d) . opt' option "machine" mm . persistencyModeOpts p
  machineOpts (Image i mm) =
    option "directory" (fromAbsFileT i) . opt' option "machine" mm
  machineOpts (MachineName m p) =
    option "machine" m . persistencyModeOpts p

  persistencyModeOpts (Persistent mt) = opt' option "template" (fromAbsDirT <$> mt)
  persistencyModeOpts Ephemeral = switch "ephemeral" SwitchPresent

  yesNo :: Bool -> Text
  yesNo True = "yes"
  yesNo False = "no"

  settingsOpt :: Settings -> Text
  settingsOpt (Settings True) = "yes"
  settingsOpt (Settings False) = "no"
  settingsOpt SettingsOverride = "override"
  settingsOpt SettingsTrusted = "trusted"
  --
  -- "no", "host", "try-host", "guest", "try-guest", "auto"
  linkJournalOpt LinkJournalNo = "no"
  linkJournalOpt LinkJournalHost = "host"
  linkJournalOpt LinkJournalTryHost = "try-host"
  linkJournalOpt LinkJournalGuest = "guest"
  linkJournalOpt LinkJournalTryGuest = "try-guest"
  linkJournalOpt LinkJournalAuto = "auto"

  privateUsersOpt (PrivateUsersUid f (Just n)) = showt f <> "," <> showt n
  privateUsersOpt (PrivateUsersUid f Nothing) = showt f
  privateUsersOpt (PrivateUsers True) = "true"
  privateUsersOpt (PrivateUsers False) = "false"
  privateUsersOpt PrivateUsersPick = "pick"


toCommandLine :: Nspawn -> CommandLine
toCommandLine (AsPid2 nspawnExec options command) =
  CommandLine (fromAbsFileT nspawnExec) (switch "as-pid2" SwitchPresent . optionsAsCommandLine options $ []) (toList command)
toCommandLine (Boot nspawnExec options initOpts) =
  CommandLine (fromAbsFileT nspawnExec) (switch "boot" SwitchPresent . optionsAsCommandLine options $ []) initOpts
toCommandLine (Init nspawnExec options command) =
  CommandLine (fromAbsFileT nspawnExec) (optionsAsCommandLine options $ []) (toList command)

nspawnCommand :: Nspawn -> Text
nspawnCommand nspawn =
  let CommandLine command options initOpts = toCommandLine nspawn
  in unwords (command : (encodeOptions options <> initOpts))


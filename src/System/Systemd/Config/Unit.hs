{-# LANGUAGE PatternSynonyms #-}
module System.Systemd.Config.Unit where

import Data.Monoid (mempty, Last(Last), Monoid, (<>))
import Data.Text (concat, Text)
import Prelude hiding (concat)
import GHC.Generics (Generic(..))

-- | Loosely inspired by Chris Done ini package.
type Section = Text
type Key = Text
type Value = Text
newtype Unit = Unit [(Section, [(Key, Value)])]
  deriving (Generic)

instance Monoid Unit where
  mempty = Unit mempty
  mappend (Unit u1) (Unit u2) = Unit (u1 <> u2)

-- shorthand to handle optional value and derive
-- Monoid instances (which takes last defined value
-- from sum) for configuration data types
pattern Value opt = Last (Just opt)
pattern Missing = Last Nothing

section :: Text -> [(Text, Last Text)] -> Unit
section header values =
  Unit [(header, values')]
 where
  values' = [(k, v) | (k, Value v) <- values]

printUnit :: Unit -> Text
printUnit (Unit sections) =
  concat (map buildSection sections)
 where
  buildSection (_, []) = ""
  buildSection (name, pairs) = "[" <> name <> "]\n" <> concat (map buildPair pairs)
  buildPair (name,value) = name <> "=" <> value <> "\n"

showBool :: Bool -> Text
showBool True = "yes"
showBool False = "no"

data Architecture
  = X86
  | X86_64
  | Ppc
  | Ppc_le
  | Ppc64
  | Ppc64_le
  | Ia64
  | Parisc
  | Parisc64
  | S390
  | S390x
  | Sparc
  | Sparc64
  | Mips
  | Mips_le
  | Mips64
  | Mips64_le
  | Alpha
  | Arm
  | Arm_be
  | Arm64
  | Arm64_be
  | Sh
  | Sh64
  | M86k
  | Tilegx
  | Cris
  deriving (Generic, Show)

showArchitecture :: Architecture -> Text
showArchitecture X86 = "x86"
showArchitecture X86_64 = "x86-64"
showArchitecture Ppc = "ppc"
showArchitecture Ppc_le = "ppc-le"
showArchitecture Ppc64 = "ppc64"
showArchitecture Ppc64_le = "ppc64-le"
showArchitecture Ia64 = "ia64"
showArchitecture Parisc = "parisc"
showArchitecture Parisc64 = "parisc64"
showArchitecture S390 = "s390"
showArchitecture S390x = "s390x"
showArchitecture Sparc = "sparc"
showArchitecture Sparc64 = "sparc64"
showArchitecture Mips = "mips"
showArchitecture Mips_le = "mips-le"
showArchitecture Mips64 = "mips64"
showArchitecture Mips64_le = "mips64-le"
showArchitecture Alpha = "alpha"
showArchitecture Arm = "arm"
showArchitecture Arm_be = "arm-be"
showArchitecture Arm64 = "arm64"
showArchitecture Arm64_be = "arm64-be"
showArchitecture Sh = "sh"
showArchitecture Sh64 = "sh64"
showArchitecture M86k = "m86k"
showArchitecture Tilegx = "tilegx"
showArchitecture Cris = "cris"

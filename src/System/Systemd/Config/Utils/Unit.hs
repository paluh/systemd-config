module System.Systemd.Config.Utils.Unit where

import Data.Text (concat, Text)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (gmappend)
import Prelude hiding (concat)
import GHC.Generics (Generic(..))

type Section = Text
type Key = Text
type Value = Text
newtype Unit = Unit [(Section, [(Key, Value)])]
  deriving (Generic)

instance Semigroup Unit where
  (<>) = gmappend

-- | Copied from Chris Done ini package
printUnit :: Unit -> Text
printUnit (Unit sections) =
  concat (map buildSection sections)
 where
  buildSection (name, pairs) = "[" <> name <> "]\n" <> concat (map buildPair pairs)
  buildPair (name,value) = name <> "=" <> value <> "\n"

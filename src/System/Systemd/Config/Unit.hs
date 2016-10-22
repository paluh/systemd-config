{-# LANGUAGE PatternSynonyms #-}
module System.Systemd.Config.Unit where

import Data.Monoid (mempty, Last(Last), Monoid, (<>))
import Data.Text (concat, intercalate, Text)
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

-- | Shortcuts to handle optional values
-- | (you can use `Value v` instead of `Last (Just v)`)
-- | We are using `Last` instead of `Maybe`
-- | to easily derive quite sane `Monoid` instances
-- | (which take last defined value
-- | from sum) for configuration data types
pattern Value opt = Last (Just opt)
pattern Missing = Last Nothing

section :: Text -> [(Text, Last Text)] -> Unit
section header values =
  Unit [(header, values')]
 where
  values' = [(k, v) | (k, Value v) <- values]

printUnit :: Unit -> Text
printUnit (Unit sections) =
  intercalate "\n" (map buildSection . filter (not . null . snd) $ sections)
 where
  buildSection (name, pairs) = "[" <> name <> "]\n" <> concat (map buildPair pairs)
  buildPair (name,value) = name <> "=" <> value <> "\n"

showBool :: Bool -> Text
showBool True = "yes"
showBool False = "no"


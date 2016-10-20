-- | Module havely inspired by Chris Done ini package.
module System.Systemd.Config.Utils.Unit where

import Data.Monoid ((<>))
import Data.Text (concat, Text)
import Prelude hiding (concat)

type Section = Text
type Key = Text
type Value = Text
data Unit = Unit [(Section, [(Key, Value)])]

printUnit :: Unit -> Text
printUnit (Unit sections) =
  concat (map buildSection sections)
 where
  buildSection (name, pairs) = "[" <> name <> "]\n" <> concat (map buildPair pairs)
  buildPair (name,value) = name <> "=" <> value <> "\n"

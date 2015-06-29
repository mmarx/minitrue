module Languages where

import Prelude ( Read
               , Show
               , Enum
               , Eq
               , Bounded
               )
import Data.Data
import Database.Persist.TH

data Language = English | German
              deriving (Read, Show, Eq, Enum, Bounded, Data, Typeable)

derivePersistField "Language"

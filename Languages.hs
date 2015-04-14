module Languages where

import Prelude ( Read
               , Read
               , Show
               , Enum
               , Eq
               , Bounded
               )
import Database.Persist.TH

data Language = English | German
              deriving (Read, Show, Eq, Enum, Bounded)

derivePersistField "Language"

module Languages where

import Prelude ( Maybe (..)
               , Read
               , Show
               , Enum
               , Eq
               , Bounded
               , read
               , show
               , (.)
               )
import Database.Persist.TH

data Language = English | German
              deriving (Read, Show, Eq, Enum, Bounded)

derivePersistField "Language"

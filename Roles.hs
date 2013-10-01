module Roles where

import Prelude (Read, Show, Eq)
import Database.Persist.TH

data Role = Consumer | InnerCircle | Admin
          deriving (Read, Show, Eq)
data ListRole = Receiver | Sender
              deriving (Read, Show, Eq)

derivePersistField "Role"
derivePersistField "ListRole"

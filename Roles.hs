module Roles where

import Prelude ( Maybe (..)
               , Read
               , Show
               , Enum
               , Eq
               , read
               , show
               , (.)
               )
import Database.Persist.TH
import Yesod.Core.Dispatch (PathPiece (..))

data Role = Consumer | InnerCircle | Admin
          deriving (Read, Show, Eq, Enum)
data ListRole = Receiver | Sender
              deriving (Read, Show, Eq, Enum)

derivePersistField "Role"
derivePersistField "ListRole"

instance PathPiece Role where
  toPathPiece = toPathPiece . show
  fromPathPiece "Consumer" = Just Consumer
  fromPathPiece "InnerCircle" = Just InnerCircle
  fromPathPiece "Admin" = Just Admin
  fromPathPiece _ = Nothing

instance PathPiece ListRole where
  toPathPiece = toPathPiece . show
  fromPathPiece "Receiver" = Just Receiver
  fromPathPiece "Sender" = Just Sender
  fromPathPiece _ = Nothing

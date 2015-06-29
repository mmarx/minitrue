{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Model where

import Prelude hiding (concat)
import Yesod
import Data.Fixed (showFixed)
import Data.Text (Text, concat, pack)
import Database.Persist.Quasi
import Database.Persist.Sql
import Data.Typeable ()
import Data.Time
import Data.Data
import Data.Aeson.TH

import Roles
import Languages
import SharedTypes

deriving instance Typeable Key
deriving instance Typeable Textarea
deriving instance Typeable BackendKey
deriving instance Data Textarea
deriving instance Data (BackendKey SqlBackend)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

$(deriveJSON defaultOptions ''Day)
$(deriveJSON defaultOptions ''TimeOfDay)

data Message = Message { messageSubject :: Text
                       , messageBody :: Textarea
                       }

anchor :: ToBackendKey SqlBackend ent => Key ent -> Text
anchor = pack . show . fromSqlKey

routeAnchor :: (Yesod master, ToBackendKey SqlBackend ent) => Route master -> Key ent -> HandlerT master IO Text
routeAnchor route key = do
  renderUrl <- getUrlRender
  return $ concat [ renderUrl route
                  , "#"
                  , anchor key
                  ]

redirectAnchor :: (Yesod master, ToBackendKey SqlBackend ent) => Route master -> Key ent -> HandlerT master IO Html
redirectAnchor route key = routeAnchor route key >>= redirect

class Fay to from where
  toFay :: from -> to
  unFay :: to -> from

instance Fay FayMailingListId (Key MailingList) where
  toFay list = FayMailingListId . fromIntegral . fromSqlKey $ list
  unFay (FayMailingListId list) = toSqlKey $ fromIntegral list

instance Fay FayTimeOfDay TimeOfDay where
  toFay tod = FayTimeOfDay h m s
    where h = todHour tod
          m = todMin tod
          s = showFixed True $ todSec tod
  unFay (FayTimeOfDay h m s) = TimeOfDay { todHour = h
                                         , todMin = m
                                         , todSec = read s
                                         }

instance Fay FayEvent Event where
  toFay event = FayEvent name loc day time list
    where name = eventName event
          loc = eventLocation event
          day = eventDate event
          time = toFay $ eventTime event
          list = toFay $ eventList event
  unFay (FayEvent name loc day time list) = Event { eventName = name
                                                  , eventLocation = loc
                                                  , eventDate = day
                                                  , eventTime = unFay time
                                                  , eventList = unFay list
                                                  }

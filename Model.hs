module Model where

import Prelude hiding (concat)
import Yesod
import Data.Text (Text, concat, pack)
import Database.Persist.Quasi
import Database.Persist.Sql
import Data.Typeable (Typeable)
import Data.Time

import Roles
import Languages

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

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

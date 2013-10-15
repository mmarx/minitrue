module Model where

import Prelude hiding (concat)
import Yesod
import Data.Text (Text, concat)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time

import Roles

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data Message = Message { messageSubject :: Text
                       , messageBody :: Textarea
                       }

anchor :: Key ent -> Text
anchor key = case fromPersistValue . unKey $ key of
  Left _ -> ""
  Right k -> k

routeAnchor :: Yesod master => Route master -> Key ent -> HandlerT master IO Text
routeAnchor route key = do
  renderUrl <- getUrlRender
  return $ concat [ renderUrl route
                  , "#"
                  , anchor key
                  ]

redirectAnchor :: Yesod master => Route master -> Key ent -> HandlerT master IO Html
redirectAnchor route key = routeAnchor route key >>= redirect

module Handler.Events where

import Import

getEventsR :: MailingListId -> Handler TypedContent
getEventsR _ = return $ toTypedContent ()

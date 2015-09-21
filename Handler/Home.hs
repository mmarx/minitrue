module Handler.Home where

import Import hiding ((<>))
import qualified Yesod.Table as Table
import Data.Monoid ((<>))

getStatus :: UserId -> Entity MailingList -> YesodDB App (MailingListId, MailingList, Maybe ListRole)
getStatus userId (Entity listId list) = do
  mSubscription <- getBy $ UniqueListUser userId listId
  return (listId, list, (mailingListUserRole . entityVal) <$> mSubscription)

getHomeR :: Handler Html
getHomeR = do
  userId <- requireAuthId
  lists <- runDB $ do
    ls <- selectList [] [Asc MailingListName]
    mapM (getStatus userId) ls
  innerCircle <- do
    isIC <- isInnerCircle
    return $ case isIC of
      Authorized -> True
      _ -> False
  r <- getMessageRender
  r' <- getMessageRender
  let canEdit mRole = innerCircle || (maybe False (==Sender) mRole)
      theSubscriptions = subscriptionsTable canEdit r r' lists
  defaultLayout $(widgetFile "subscriptions")

subscriptionsTable :: (Maybe ListRole -> Bool)
                   -> (AppMessage -> Text)
                   -> (Maybe ListRole -> Text)
                   -> [(MailingListId, MailingList, Maybe ListRole)]
                   -> WidgetT App IO ()
subscriptionsTable canEdit r r' =
  buildBootstrap $ mempty
  <> Table.text (r MsgNameField) name
  <> Table.text (r MsgDescField) desc
  <> Table.text (r MsgSubscriptions) role
  <> Table.widget (r MsgListActions) actions
  where name (_, l, _) = mailingListName l
        desc (_, l, _) = mailingListDescription l
        role (_, _, x) = r' x
        actions (listId, _, mRole) = $(widgetFile "subscriptions-actions")

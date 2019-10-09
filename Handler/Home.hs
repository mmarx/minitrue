module Handler.Home where

import Import hiding ((<>))
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
  let canEdit mRole = innerCircle || (maybe False (==Sender) mRole)
      theSubscriptions = subscriptionsTable canEdit lists
  defaultLayout $(widgetFile "subscriptions")

type Subscription = (MailingListId, MailingList, Maybe ListRole)
type CanEdit = Maybe ListRole -> Bool

subscriptionActions :: CanEdit -> Subscription -> WidgetFor App ()
subscriptionActions canEdit (listId, _, mRole) = $(widgetFile "subscriptions-actions")

subscriptionsColonnade :: CanEdit -> (AppMessage -> Cell App) -> (Maybe ListRole -> Text)
                       -> Colonnade Headed Subscription (Cell App)
subscriptionsColonnade canEdit r r' = headed (r MsgNameField) name
                                   <> headed (r MsgDescField) desc
                                   <> headed (r MsgSubscriptions) role
                                   <> headed (r MsgListActions) (cell . subscriptionActions canEdit)
  where name (_, l, _) = textCell $ mailingListName l
        desc (_, l, _) = textCell $ mailingListDescription l
        role (_, _, x) = textCell $ r' $ x

subscriptionsTable :: CanEdit -> [Subscription] -> WidgetFor App ()
subscriptionsTable canEdit subs = do
  r <- handlerToWidget getMessageRender
  r' <- handlerToWidget getMessageRender
  encodeCellTable [class_ "table table-striped"]
    (subscriptionsColonnade canEdit (textCell . r) r')
    subs

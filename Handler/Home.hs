{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

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
  innerCircle <- isInnerCircle
  let canEdit = maybe (innerCircle == Authorized) (==Sender)
  defaultLayout $ do
    $(widgetFile "subscriptions")

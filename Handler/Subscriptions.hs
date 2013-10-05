module Handler.Subscriptions where

import Import
import Data.Text (pack)
import System.Random (newStdGen)
import Network.Mail.Mime (randomString)

randomKey :: m -> IO Text
randomKey _ = newStdGen >>= return . pack . fst . randomString 10

addSubscription :: UserId -> MailingListId -> YesodDB App ()
addSubscription userId listId = do
  list <- get404 listId
  master <- lift getYesod
  unsubkey <- liftIO $ randomKey master
  _ <- insertBy $ MailingListUser userId listId Receiver unsubkey
  lift $ setMessageI $ MsgSubscribeSuccess $ mailingListName list
  return ()

delSubscription :: UserId -> MailingListId -> YesodDB App ()
delSubscription userId listId = do
  list <- get404 listId
  _ <- deleteBy $ UniqueListUser userId listId
  lift $ setMessageI $ MsgUnsubscribeSuccess $ mailingListName list
  return ()

subscriptionHelper :: (UserId -> MailingListId -> YesodDB App ()) -> MailingListId -> Handler Html
subscriptionHelper action listId = do
  userId <- requireAuthId
  runDB $ do
    _ <- get404 listId
    _ <- action userId listId
    return ()
  redirect HomeR

postSubscribeR :: MailingListId -> Handler Html
postSubscribeR = subscriptionHelper addSubscription

postUnsubscribeR :: MailingListId -> Handler Html
postUnsubscribeR = subscriptionHelper delSubscription

getUnsubscribeDirectlyR :: MailingListId -> Text -> Handler Html
getUnsubscribeDirectlyR listId key = do
  (list, mMlu) <- runDB $ do
    lst <- get404 listId
    mlu <- getBy $ UniqueUnsubscribe key listId
    return (lst, mlu)
  case mMlu of
    Just (Entity _ mlu) -> defaultLayout $ do
      $(widgetFile "unsubscribe")
    Nothing -> do
      setMessageI $ MsgUnsubscribeFail $ mailingListName list
      redirect HomeR

postUnsubscribeDirectlyR :: MailingListId -> Text -> Handler Html
postUnsubscribeDirectlyR listId key = do
  (list, mMlu) <- runDB $ do
    lst <- get404 listId
    mlu <- getBy $ UniqueUnsubscribe key listId
    return (lst, mlu)
  case mMlu of
    Just (Entity _ mlu) -> runDB $ delSubscription (mailingListUserUser mlu) listId
    Nothing -> do
      setMessageI $ MsgUnsubscribeFail $ mailingListName list
  redirect HomeR

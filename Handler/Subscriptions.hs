module Handler.Subscriptions where

import Import

addSubscription :: UserId -> MailingListId -> YesodDB App ()
addSubscription userId listId = do
  _ <- insertBy $ MailingListUser userId listId Receiver
  return ()

delSubscription :: UserId -> MailingListId -> YesodDB App ()
delSubscription userId listId = do
  _ <- deleteBy $ UniqueListUser userId listId
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

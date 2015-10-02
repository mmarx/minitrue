module ListMail where

import Import hiding (toLower)
import Handler.Events (eventsTables)
import Data.Char
import qualified Data.Text as T

canonicalizeListName :: Text -> Text
canonicalizeListName = T.map canonicalize
  where canonicalize c
          | isAsciiUpper c = toLower c
          | isAsciiLower c = c
          | isDigit c = c
          | otherwise = '-'

sendMessageToList :: Message -> MailingListId -> Handler ()
sendMessageToList msg listId = runDB $ do
  addrs <- selectList [MailingListUserList ==. listId] []
  lift $ mapM_ (sendMessageToListUser msg listId) addrs

sendPendingNoticeToList :: QueueId -> MailingListId -> Handler ()
sendPendingNoticeToList queueId listId = do
  r <- getMessageRender
  runDB $ do
    (Queue authorId _ subject body _) <- get404 queueId
    author <- get404 authorId
    addrs <- selectList [ MailingListUserList ==. listId
                        , MailingListUserRole ==. Sender
                        , MailingListUserSupervised ==. Unsupervised
                        ] []
    let msg' = Message
                 { messageSubject = r $ MsgQueuedMailNotification subject $ userEmail author
                 , messageBody = body
                 }
    lift $ mapM_ (sendMessageToListUser msg' listId) addrs

sendMessageToListUser :: Message -> MailingListId -> Entity MailingListUser -> Handler ()
sendMessageToListUser msg listId (Entity _ mLU) = do
  settings <- appSettings <$> getYesod
  renderUrl <- getUrlRender
  mEvents <- eventsTables listId
  (user, list) <- runDB $ do
    usr <- get404 $ mailingListUserUser mLU
    lst <- get404 listId
    return (usr, lst)
  let unsubscribeR key = renderUrl $ UnsubscribeDirectlyR listId key
      sender = mailSenderAddress settings
      subject = T.concat [ "["
                         , mailingListName list
                         , "] "
                         , messageSubject msg
                         ]
      body = textareaToBody . messageBody $ msg
      listid = T.concat [ "<"
                        , canonicalizeListName $ mailingListName list
                        , ".minitrue."
                        , appMailListIdSuffix settings
                        , ">"
                        ]
      headers key = [ ("List-Id", listid)
                    , ("List-Unsubscribe", unsubscribeR key)
                    ]
      ad = Address Nothing
      message' (addr, key) = mailFromToList sender (ad addr) (unsubscribeR key) mEvents subject body
      message ak@(_, key) = sendMail $ addHeaders (headers key) $ message' ak
  message (userEmail user, mailingListUserUnsubkey mLU)

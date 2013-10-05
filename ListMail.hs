module ListMail where

import Import
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
sendMessageToList msg listId = do
  (list, addresses) <- runDB $ do
    list <- get404 listId
    addrs <- selectList [MailingListUserList ==. listId] [] >>= mapM address
    return (list, addrs)
  extra <- getExtra
  renderUrl <- getUrlRender
  let unsubscribeR key = renderUrl $ UnsubscribeDirectlyR listId key
      sender = mailSenderAddress extra
      subject = T.concat [ "["
                         , mailingListName list
                         , "] "
                         , messageSubject msg
                         ]
      body = textareaToBody . messageBody $ msg
      listid = T.concat [ "<"
                        , canonicalizeListName $ mailingListName list
                        , ".minitrue."
                        , extraMailListIdSuffix extra
                        , ">"
                        ]
      headers key = [ ("List-Id", listid)
                    , ("List-Unsubscribe", unsubscribeR key)
                    ]
      ad = Address Nothing
      message' (addr, key) = mailFromToList sender (ad addr) (unsubscribeR key) subject body
      message ak@(_, key) = sendMail $ addHeaders (headers key) $ message' ak

  mapM_ message addresses
  where address (Entity _ mLU) = do
          user <- get404 $ mailingListUserUser mLU
          return (userEmail user, mailingListUserUnsubkey mLU)

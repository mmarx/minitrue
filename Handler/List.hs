module Handler.List where

import Import
import Data.Text (unpack)

getListsR :: Handler Html
getListsR = do
  lists <- runDB $ selectList [] [Asc MailingListName]
  (listForm, listET) <- generateFormPost . listForm $ Nothing
  defaultLayout $ do
    $(widgetFile "lists")

postListsR :: Handler Html
postListsR = do
  ((listResult, listWidget), listET) <- runFormPost . listForm $ Nothing
  case listResult of
    FormSuccess list -> do
      listId <- runDB $ insert list
      setMessageI . MsgCreateListSuccess . unpack . mailingListName $ list
      redirect ListsR
    _ -> do
      setMessageI MsgCreateListFail
      redirect ListsR

getListR :: MailingListId -> Handler Html
getListR _ = error "Net yet implemented getListR"

listForm :: Maybe MailingList -> Form MailingList
listForm mList = renderBootstrap $ MailingList
                 <$> areq textField nameS (mailingListName <$> mList)
                 <*> areq textField descS (mailingListDescription <$> mList)
  where nameS = fieldSettingsLabel MsgNameField
        descS = fieldSettingsLabel MsgDescField

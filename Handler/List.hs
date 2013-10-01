module Handler.List where

import Import
import ListMail

getListsR :: Handler Html
getListsR = do
  lists <- runDB $ selectList [] [Asc MailingListName]
  deleteForm <- generateFormPost $ listDeleteForm
  defaultLayout $ do
    $(widgetFile "lists")

postListsR :: Handler Html
postListsR = do
  ((listResult, _), _) <- runFormPost . listForm $ Nothing
  case listResult of
    FormSuccess list -> do
      _ <- runDB $ insert list
      setMessageI . MsgCreateListSuccess . mailingListName $ list
    _ -> do
      setMessageI MsgCreateListFail
  redirect ListsR

getListR :: MailingListId -> Handler Html
getListR listId = do
  list <- runDB $ get404 listId
  defaultLayout $ do
    $(widgetFile "list")

postListR :: MailingListId -> Handler Html
postListR listId = do
  _ <- runDB $ get404 listId
  ((listResult, _), _) <- runFormPost . listForm $ Nothing
  case listResult of
    FormSuccess list -> do
      runDB $ replace listId list
      setMessageI . MsgEditListSuccess . mailingListName $ list
    _ -> do
      setMessageI MsgEditListFail
  redirect $ ListR listId

postListDeleteR :: MailingListId -> Handler Html
postListDeleteR listId = do
  ((listResult, _), _) <- runFormPost $ listDeleteForm
  case listResult of
    FormSuccess _ -> do
      name <- runDB $ do
        list <- get404 listId
        delete listId
        return $ mailingListName list
      setMessageI . MsgDeleteListSuccess $ name
    _ -> do
      setMessageI MsgDeleteListFail
  redirect ListsR

getSendMessageR :: MailingListId -> Handler Html
getSendMessageR listId = do
  Entity userId user <- requireAuth
  list <- runDB $ get404 listId
  (msgWidget, msgET) <- generateFormPost $ messageForm Nothing
  defaultLayout $ do
    $(widgetFile "send-message")

postSendMessageR :: MailingListId -> Handler Html
postSendMessageR listId = do
  list <- runDB $ get404 listId
  ((msgResult, _), _) <- runFormPost . messageForm $ Nothing

  case msgResult of
    FormSuccess msg -> do
      sendMessageToList msg listId
      setMessageI $ MsgSendMessageSuccess (messageSubject msg) $ mailingListName list
      redirect HomeR
    _ -> do
      setMessageI MsgSendMessageFail
      redirect $ SendMessageR listId

listEntry :: Entity MailingList -> (Widget, Enctype) -> Widget
listEntry (Entity listId list) (deleteWidget, deleteET) = do
  modalId <- newIdent
  labelId <- newIdent
  $(widgetFile "list-entry")

listForm :: Maybe MailingList -> Form MailingList
listForm mList = renderBootstrap $ MailingList
                 <$> areq textField nameS (mailingListName <$> mList)
                 <*> areq textField descS (mailingListDescription <$> mList)
  where nameS = fieldSettingsLabel MsgNameField
        descS = fieldSettingsLabel MsgDescField

listDeleteForm :: Html -> MForm Handler (FormResult Text, Widget)
listDeleteForm extra = do
  (res, view) <- mreq hiddenField "dummy" (Just "dummy")
  let widget = [whamlet|$newline never
                #{extra}
                ^{fvInput view}|]
  return (res, widget)

listEditor :: Maybe (MailingListId, MailingList) -> Widget
listEditor mIL = do
  let editAction = case mIL of
        Nothing -> ListsR
        Just (lId, _) -> ListR lId
  (editWidget, editET) <- handlerToWidget . generateFormPost . listForm $ snd <$> mIL
  $(widgetFile "list-edit")

messageForm :: Maybe Message -> Form Message
messageForm mMsg = renderBootstrap $ Message
                   <$> areq textField subjectS (messageSubject <$> mMsg)
                   <*> areq textareaField bodyS (messageBody <$> mMsg)
  where subjectS = fieldSettingsLabel MsgSubjectField
        bodyS = fieldSettingsLabel MsgBodyField

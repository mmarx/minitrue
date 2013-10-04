module Handler.List where

import Import
import ListMail

getListsR :: Handler Html
getListsR = do
  lists <- runDB $ selectList [] [Asc MailingListName]
           >>= mapM listInfo
  deleteForm <- generateFormPost $ listDeleteForm
  defaultLayout $ do
    $(widgetFile "lists")
  where listInfo lst@(Entity listId _) = do
          subs <- count [MailingListUserList ==. listId]
          auths <- count [ MailingListUserList ==. listId
                        , MailingListUserRole ==. Sender
                        ]
          return (lst, subs, auths)

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
  (list, subscribers) <- runDB $ do
    lst <- get404 listId
    subs <- selectList [MailingListUserList ==. listId] [Asc MailingListUserUser]
            >>= mapM subscriber
    return (lst, subs)
  defaultLayout $ do
    $(widgetFile "list")
  where subscriber :: Entity MailingListUser -> YesodDB App (UserId, Text, ListRole)
        subscriber (Entity _ mlu) = do
          let userId = mailingListUserUser mlu
              role = mailingListUserRole mlu
          email <- get404 userId >>= return . userEmail
          return (userId, email, role)

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
        deleteWhere [MailingListUserList ==. listId]
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
  cancelR <- routeAnchor ListsR listId
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

postPromoteR :: MailingListId -> UserId -> Handler Html
postPromoteR listId userId = do
  runDB $ do
    (Entity mluId _) <- getBy404 $ UniqueListUser userId listId
    update mluId [MailingListUserRole =. Sender]
  redirect $ ListR listId

postDemoteR :: MailingListId -> UserId -> Handler Html
postDemoteR listId userId = do
  runDB $ do
    (Entity mluId _) <- getBy404 $ UniqueListUser userId listId
    update mluId [MailingListUserRole =. Receiver]
  redirect $ ListR listId

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
        bodyS' = fieldSettingsLabel MsgBodyField :: FieldSettings App
        bodyS = bodyS' { fsAttrs = fsAttrs bodyS' ++ [("rows", "15")] }

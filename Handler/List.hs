module Handler.List where

import Import hiding ((<>))
import Handler.Events
import ListMail
import Data.Monoid ((<>))
import Text.Blaze.Html (preEscapedToHtml)

getListsR :: Handler Html
getListsR = do
  lists <- runDB $ selectList [] [Asc MailingListName]
           >>= mapM listInfo
  let theLists = listsTable lists
  defaultLayout $(widgetFile "lists")
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
  let theSubs = subscribersTable listId subscribers
  defaultLayout $(widgetFile "list")
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
    _ -> setMessageI MsgEditListFail
  redirect $ ListR listId

postListDeleteR :: MailingListId -> Handler Html
postListDeleteR listId = do
  ((listResult, _), _) <- runFormPost $ dummyDeleteForm
  case listResult of
    FormSuccess _ -> do
      name <- runDB $ do
        list <- get404 listId
        deleteWhere [ArchiveList ==. listId]
        deleteWhere [MailingListUserList ==. listId]
        delete listId
        return $ mailingListName list
      setMessageI . MsgDeleteListSuccess $ name
    _ -> setMessageI MsgDeleteListFail
  redirect ListsR

getSendMessageR :: MailingListId -> Handler Html
getSendMessageR listId = do
  _ <- requireAuth
  list <- runDB $ get404 listId
  (msgWidget, msgET) <- generateFormPost $ messageForm $ messageTemplate list
  mEvents <- eventsTables listId >>= return . fmap snd
  cancelR <- routeAnchor ListsR listId
  defaultLayout $ do
    $(widgetFile "send-message")

postSendMessageR :: MailingListId -> Handler Html
postSendMessageR listId = do
  authId <- requireAuthId
  time <- liftIO getCurrentTime
  list <- runDB $ get404 listId
  ((msgResult, _), _) <- runFormPost . messageForm $ Nothing

  case msgResult of
    FormSuccess msg -> do
      let subject = messageSubject msg
          body = messageBody msg
      _ <- runDB $ insert $ Archive (Just authId) listId subject body time
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
  canSend <- handlerToWidget $ do
    userId <- requireAuthId
    mr <- runDB $ getBy $ UniqueListUser userId listId
    case mailingListUserRole . entityVal <$> mr of
      Just Sender -> return True
      _ -> return False
  modalId <- newIdent
  labelId <- newIdent
  $(widgetFile "list-entry")

listForm :: Maybe MailingList -> Form MailingList
listForm mList = renderBootstrap3 BootstrapBasicForm $ MailingList
                 <$> areq textField (bfs MsgNameField) (mailingListName <$> mList)
                 <*> areq textField (bfs MsgDescField) (mailingListDescription <$> mList)
                 <*> aopt textareaField (bfs MsgHeadField) (mailingListHeader <$> mList)
                 <*> aopt textareaField (bfs MsgFootField) (mailingListFooter <$> mList)
                 <*> areq langField (bfs MsgLangField) (mailingListLanguage <$> mList)
  where langField = selectField optionsEnum

listsColonnade :: (AppMessage -> Cell App) -> (Widget, Enctype) -> Colonnade Headed (Entity MailingList, Int, Int) (Cell App)
listsColonnade r theDeleteForm = headed (r MsgNameField) (textCell . mailingListName . lst)
                   <> headed (r MsgDescField) (textCell . mailingListDescription . lst)
                   <> headed (r MsgLangField) (stringCell . show . mailingListLanguage . lst)
                   <> headed (r MsgSubscriberCount) (stringCell . show . subs)
                   <> headed (r MsgSenderCount) (stringCell . show . auts)
                   <> headed (r MsgListActions) (cell . actions theDeleteForm)
  where lst (l, _, _) = entityVal l
        subs (_, s, _) = s
        auts (_, _, a) = a
        actions deleteForm (list, _, _) = $(widgetFile "list-actions")

listsTable :: [(Entity MailingList, Int, Int)]
           -> WidgetFor App ()
listsTable lists = do
  r <- handlerToWidget getMessageRender
  deleteForm <- handlerToWidget $ generateFormPost $ dummyDeleteForm
  encodeCellTable [class_ "table table-striped"]
    (listsColonnade (textCell . r) deleteForm)
    lists

subscribersColonnade :: MailingListId -> (AppMessage -> Cell App) -> (ListRole -> Text) -> Colonnade Headed (UserId, Text, ListRole) (Cell App)
subscribersColonnade listId r r' = headed (r MsgEmailAddress) (textCell . mail)
                                   <> headed (r MsgRole) (textCell . r' . role')
                                   <> headed (r MsgSubscriberActions) (cell . actions)
  where mail (_, m, _) = m
        role' (_, _, x) = x
        actions (userId, _, role) = $(widgetFile "subscriber-actions")

subscribersTable :: MailingListId
                 -> [(UserId, Text, ListRole)]
                 -> WidgetFor App ()
subscribersTable listId subs = do
  r <- handlerToWidget getMessageRender
  r' <- handlerToWidget getMessageRender
  encodeCellTable [class_ "table table-striped"]
    (subscribersColonnade listId (textCell . r) r')
    subs

listEditor :: Maybe (Entity MailingList) -> Widget
listEditor mIL = do
  let (editAction, msgSubmitButton) = case mIL of
        Nothing -> (ListsR, MsgNewListButton)
        Just (Entity lId _) -> (ListR lId, MsgEditListButton)
  (editWidget, editET) <- handlerToWidget . generateFormPost . listForm $ entityVal <$> mIL
  $(widgetFile "edit")

messageTemplate :: MailingList -> Maybe Message
messageTemplate list = Just Message { messageSubject = ""
                                    , messageBody = Textarea $ fromMaybe "" template
                                    }
  where template = header <> footer
        header = ((<>"\n\n") . unTextarea) <$> mailingListHeader list
        footer = ("\n\n"<>) . unTextarea <$> mailingListFooter list

messageForm :: Maybe Message -> Form Message
messageForm mMsg = renderBootstrap3 BootstrapBasicForm $ Message
                   <$> areq textField (bfs MsgSubjectField) (messageSubject <$> mMsg)
                   <*> areq textareaField bodyS (messageBody <$> mMsg)
  where bodyS' = bfs MsgBodyField :: FieldSettings App
        bodyS = bodyS' { fsAttrs = fsAttrs bodyS' ++ [("rows", "15")] }

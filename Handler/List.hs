module Handler.List where

import Import hiding ((<>))
import Handler.Events
import ListMail
import qualified Yesod.Table as Table
import Data.Monoid ((<>))

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
  where subscriber :: Entity MailingListUser -> YesodDB App (UserId, Text, ListRole, Supervision)
        subscriber (Entity _ mlu) = do
          let userId = mailingListUserUser mlu
              role = mailingListUserRole mlu
              supervised = mailingListUserSupervised mlu
          email <- get404 userId >>= return . userEmail
          return (userId, email, role, supervised)

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
  void $ runDB $ get404 listId
  ((msgResult, _), _) <- runFormPost . messageForm $ Nothing
  isPostponement <- runInputPost $ iopt textField "postpone"
  supervised <- runDB $ do
    (Entity authId _) <- lift $ requireAuth
    mlu <- getBy404 $ UniqueListUser authId listId
    return $ mailingListUserSupervised . entityVal $ mlu
  case msgResult of
    FormSuccess msg -> do
      sendOrQueueMessage isPostponement supervised listId msg
      redirect HomeR
    _ -> do
      case supervised of
        Supervised -> setMessageI MsgQueueMessageFail
        Unsupervised -> setMessageI MsgSendMessageFail
      redirect $ SendMessageR listId

sendOrQueueMessage :: Maybe Text
                   -> Supervision
                   -> MailingListId
                   -> Message
                   -> Handler ()
sendOrQueueMessage (Just _) _ = queueMessage False
sendOrQueueMessage Nothing Supervised = queueMessage True
sendOrQueueMessage Nothing Unsupervised = sendMessage

prepareMessage :: MailingListId
               -> Message
               -> Handler (AuthId App, UTCTime, MailingList, Text, Textarea)
prepareMessage listId msg = do
  authId <- requireAuthId
  time <- lift getCurrentTime
  list <- runDB $ get404 listId
  let subject = messageSubject msg
      body = messageBody msg
  return (authId, time, list, subject, body)

sendMessage :: MailingListId -> Message -> Handler ()
sendMessage listId msg = do
  (authId, time, list, subject, body) <- prepareMessage listId msg
  void $ runDB $ insert $ Archive authId listId subject body time
  sendMessageToList msg listId
  setMessageI $ MsgSendMessageSuccess subject $ mailingListName list

queueMessage :: Bool -> MailingListId -> Message -> Handler ()
queueMessage supervision listId msg = do
  (authId, time, list, subject, body) <- prepareMessage listId msg
  queueId <- runDB $ insert $ Queue authId listId subject body time
  when supervision $ sendPendingNoticeToList queueId listId
  setMessageI $ MsgQueueMessageSuccess subject $ mailingListName list

updateMailingListUser :: [Update MailingListUser] -> MailingListId -> UserId ->  Handler Html
updateMailingListUser upd listId userId = do
  runDB $ (getBy404 $ UniqueListUser userId listId)
             >>= \(Entity mluId _) -> update mluId upd
  redirect $ ListR listId

postPromoteR :: MailingListId -> UserId -> Handler Html
postPromoteR = updateMailingListUser [MailingListUserRole =. Sender]

postDemoteR :: MailingListId -> UserId -> Handler Html
postDemoteR = updateMailingListUser [MailingListUserRole =. Receiver]

postSuperviseR :: MailingListId -> UserId -> Handler Html
postSuperviseR = updateMailingListUser [MailingListUserSupervised =. Supervised]

postUnsuperviseR :: MailingListId -> UserId -> Handler Html
postUnsuperviseR = updateMailingListUser [MailingListUserSupervised =. Unsupervised]

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

listsTable :: [(Entity MailingList, Int, Int)]
           -> WidgetT App IO ()
listsTable subscribers = do
  r <- handlerToWidget getMessageRender
  deleteForm <- handlerToWidget $ generateFormPost $ dummyDeleteForm
  buildBootstrap (mempty
    <> Table.text (r MsgNameField) (mailingListName . lst)
    <> Table.text (r MsgDescField) (mailingListDescription . lst)
    <> Table.show (r MsgLangField) (mailingListLanguage . lst)
    <> Table.show (r MsgSubscriberCount) subs
    <> Table.show (r MsgSenderCount) auts
    <> Table.widget (r MsgListActions) (actions deleteForm)) subscribers
  where lst (l, _, _) = entityVal l
        subs (_, s, _) = s
        auts (_, _, a) = a
        actions deleteForm (list, _, _) = $(widgetFile "list-actions")

subscribersTable :: MailingListId
                 -> [(UserId, Text, ListRole, Supervision)]
                 -> WidgetT App IO ()
subscribersTable listId subs = do
  r <- handlerToWidget getMessageRender
  r' <- handlerToWidget getMessageRender
  r'' <- handlerToWidget getMessageRender
  buildBootstrap (mempty
    <> Table.text (r MsgEmailAddress) mail
    <> Table.text (r MsgRole) (r' . role')
    <> Table.text (r MsgSupervision) (r'' . super)
    <> Table.widget (r MsgSubscriberActions) actions) subs
  where mail (_, m, _, _) = m
        role' (_, _, x, _) = x
        super (_, _, _, y) = y
        actions (userId, _, role, supervised) = $(widgetFile "subscriber-actions")

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

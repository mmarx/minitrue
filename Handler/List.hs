module Handler.List where

import Import
import Data.Text (unpack)

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
      setMessageI . MsgCreateListSuccess . unpack . mailingListName $ list
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
      setMessageI . MsgEditListSuccess . unpack . mailingListName $ list
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
      setMessageI . MsgDeleteListSuccess . unpack $ name
    _ -> do
      setMessageI MsgDeleteListFail
  redirect ListsR

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

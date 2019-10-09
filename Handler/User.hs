module Handler.User where

import Import hiding ((<>))
import qualified Yesod.Table as Table
import Data.Monoid ((<>))

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserId]
           >>= mapM userInfo
  let theUsers = userTable users
  defaultLayout $(widgetFile "users")
  where userInfo (Entity userId user) = do
          mRole <- getBy $ UniqueUserRole userId
          return (userId, user, (userRoleRole . entityVal) <$> mRole)

getRoleR :: UserId -> Role -> Handler Html
getRoleR userId role = do
  (roleWidget, roleET) <- generateFormPost userDummyForm
  (mail, mOldRole) <- runDB $ do
    user <- get404 userId
    mRole <- getBy $ UniqueUserRole userId
    return (userEmail user, (userRoleRole . entityVal) <$> mRole)
  cancelR <- routeAnchor UsersR userId
  msgOldRole <- getMessageRender >>= \r -> return $ r mOldRole
  msgRole <- getMessageRender >>= \r -> return $ r role
  defaultLayout $(widgetFile "user-role")

postRoleR :: UserId -> Role -> Handler Text
postRoleR userId role = do
  ((roleResult, _), _) <- runFormPost userDummyForm
  mail <- runDB $ get404 userId >>= return . userEmail
  case roleResult of
    FormSuccess _ -> do
      runDB $ do
        mRole <- getBy $ UniqueUserRole userId
        case mRole of
          Nothing -> void $ insert $ UserRole userId role
          Just (Entity roleId _) -> update roleId [UserRoleRole =. role]
      renderMsg <- getMessageRender
      setMessageI $ MsgChangeRoleSuccess mail $ renderMsg role
    _ -> do
      setMessageI $ MsgChangeRoleFail mail
  redirectAnchor UsersR userId

postUserDeleteR :: UserId -> Handler Text
postUserDeleteR userId = do
  authId <- requireAuthId
  if authId == userId
    then redirectAnchor UsersR userId
    else do
      ((deleteResult, _), _) <- runFormPost userDummyForm
      case deleteResult of
        FormSuccess _ -> do
          mail <- runDB $ do
            user <- get404 userId
            deleteWhere [UserRoleUser ==. userId]
            deleteWhere [MailingListUserUser ==. userId]
            delete userId
            return $ userEmail user
          setMessageI $ MsgDeleteUserSuccess mail
        _ -> do
          setMessageI MsgDeleteUserFail
      redirect UsersR

userDeleteEntry :: Entity User -> Widget
userDeleteEntry (Entity userId user) = do
  (deleteWidget, deleteET) <- handlerToWidget $ generateFormPost userDummyForm
  authId <- handlerToWidget $ requireAuthId
  modalId <- newIdent
  labelId <- newIdent
  if authId == userId
   then [whamlet|$newline never
        |]
   else $(widgetFile "user-delete")

userDummyForm :: Html -> MForm Handler (FormResult Text, Widget)
userDummyForm extra = do
  (res, view) <- mreq hiddenField "dummy" (Just "dummy")
  let widget = [whamlet|$newline never
                #{extra}
                ^{fvInput view}|]
  return (res, widget)

userTable :: [(UserId, User, Maybe Role)] -> WidgetFor App ()
userTable users = do
  r <- handlerToWidget getMessageRender
  buildBootstrap (mempty
    <> Table.text (r MsgEmailAddress) mail
    <> Table.text (r MsgVerifiedStatus) (r . verified)
    <> Table.widget (r MsgUserRole) role
    <> Table.widget (r MsgUserActions) del) users
    where mail (_, u, _) = userEmail u
          verified (_, u, _)
                   | userVerified u = MsgVerified
                   | otherwise = MsgUnverified
          role (userId, _, mRole) = do
            authId <- handlerToWidget requireAuthId
            $(widgetFile "user-entry-role")
          del (userId, user, _) = userDeleteEntry (Entity userId user)

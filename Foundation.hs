module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Blaze.Html      (preEscapedToHtml)
import Text.Hamlet          (hamletFile)
import Text.Shakespeare.Text (lt)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Form.Jquery    (YesodJquery (..))
import Languages            (Language (..))
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Yesod.Auth.Message as AuthMessage
import qualified Network.Mail.SMTP as SMTP
import qualified Data.Text as T
import Auth

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings          :: AppSettings
    , appStatic            :: Static -- ^ Settings for static file serving.
    , appConnPool          :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager       :: Manager
    , appLogger            :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        mAuth <- maybeAuth
        mRole <- getUserRole
        modalId <- newIdent
        labelId <- newIdent
        navbar <- widgetToPageContent $(widgetFile "navbar")

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(combineScripts 'StaticR
                [ js_jquery_js
                , js_bootstrap_js
                ])
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized HomeR _ = isLoggedIn
    isAuthorized ListsR _ = isInnerCircle
    isAuthorized (ListR listId) _ = canEditList listId
    isAuthorized (ListDeleteR _) _ = isInnerCircle
    isAuthorized (SendMessageR listId) _ = canSendToList listId
    isAuthorized (SubscribeR listId) _ = canSubscribeToList listId
    isAuthorized (UnsubscribeR listId) _ = canUnsubscribeFromList listId
    isAuthorized (PromoteR listId _) _ = canEditList listId
    isAuthorized (DemoteR listId _) _ = canEditList listId
    isAuthorized (UsersR) _ = isAdmin
    isAuthorized (UserDeleteR userId) _ = canDeleteUser userId
    isAuthorized (RoleR userId role) _ = canSetRole userId role
    isAuthorized (UnsubscribeDirectlyR _ _) _ = return Authorized
    isAuthorized (ListEventsR listId) _ = canEditList listId
    isAuthorized (EventR eventId) _ = canEditEvent eventId
    isAuthorized (EventDeleteR eventId) _ = canEditEvent eventId
    isAuthorized (CategoryR _) _ = isInnerCircle
    isAuthorized (CategoryDeleteR _) _ = isInnerCircle

    isAuthorized AllEventsR False = return Authorized
    isAuthorized AllEventsR True = isInnerCircle
    isAuthorized (CategoryEventsR _) False = return Authorized
    isAuthorized (CategoryEventsR _) True = isInnerCircle
    isAuthorized CategoriesR False = return Authorized
    isAuthorized CategoriesR True = isInnerCircle
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    authenticate creds = runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
       Just (Entity uid _) -> return $ Authenticated uid
       Nothing -> do
        Authenticated <$> insert User
           { userEmail = credsIdent creds
           , userPassword = Nothing
           , userVerkey = Nothing
           , userVerified = False
           }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail]

    authHttpManager = error "No HTTP manager neccessary."

    renderAuthMessage _ ("en":_) = AuthMessage.englishMessage
    renderAuthMessage _ ("de":_) = germanAuthMessage
    renderAuthMessage master (_:langs) = renderAuthMessage master langs
    renderAuthMessage _ _ = AuthMessage.defaultMessage

germanAuthMessage :: AuthMessage.AuthMessage -> Text
germanAuthMessage AuthMessage.LoginTitle = "Anmelden"
germanAuthMessage msg = AuthMessage.germanMessage msg

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  afterPasswordRoute _ = HomeR
  addUnverified email verkey = runDB $ insert $
                               User email Nothing (Just verkey) False

  verifyAccount uId = runDB $ do
    mUser <- get uId
    case mUser of
      Nothing -> return Nothing
      Just _ -> do
        update uId [ UserVerified =. True
                   , UserVerkey =. Nothing
                   ]
        return $ Just uId

  sendVerifyEmail email verkey verurl = do
    renderMsg <- getMessageRender
    master <- getYesod
    let sender = mailSenderAddress . appSettings $ master
        receiver = Address Nothing email
        subject = renderMsg MsgVerifyEmailSubject
        body = renderMsg $ MsgVerifyEmailBody verkey verurl
        body' = MailBody { plainBody = [lt|#{T.replace "\\n" "\n" body}|]
                         , htmlBody = [shamlet|#{preEscapedToHtml $ T.replace "\\n" "<br>" body}|]
                         }
    sendMail $ mailFromTo sender receiver subject body'

  getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
  setVerifyKey uId verkey = runDB $ update uId [UserVerkey =. Just verkey]
  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uId pwd = runDB $ update uId [UserPassword =. Just pwd]

  getEmail = runDB . fmap (fmap userEmail) . get
  getEmailCreds email = runDB $ do
    mUser <- getBy $ UniqueUser email
    case mUser of
      Nothing -> return Nothing
      Just (Entity uId user) -> return $ Just EmailCreds
        { emailCredsId = uId
        , emailCredsAuthId = Just uId
        , emailCredsEmail = userEmail user
        , emailCredsStatus = isJust $ userPassword user
        , emailCredsVerkey = userVerkey user
        }

  checkPasswordSecurity _ pwd = do
    case compareLength pwd (8 :: Int) of
      LT -> return $ Left "Password must be at least 8 characters long."
      _ -> return $ Right ()

  confirmationEmailSentResponse mail = liftM toTypedContent $ defaultLayout $ do
    setMessageI $ AuthMessage.ConfirmationEmailSent mail
    redirect HomeR

instance YesodAuthPersist App

instance YesodJquery App where
  urlJqueryJs _ = Left $ StaticR js_jquery_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_css

sendMail :: Mail -> Handler ()
sendMail mail = do
  master <- getYesod
  let host = mailHost $ appSettings master
  lift $ SMTP.sendMail host mail

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage App Role where
  renderMessage master langs = renderMessage master langs . roleMessage . Just

instance RenderMessage App (Maybe Role) where
  renderMessage master langs = renderMessage master langs . roleMessage

instance RenderMessage App ListRole where
   renderMessage master langs = renderMessage master langs . listRoleMessage . Just

instance RenderMessage App (Maybe ListRole) where
   renderMessage master langs = renderMessage master langs . listRoleMessage

instance RenderMessage App Language where
  renderMessage master langs = renderMessage master langs . languageMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

roleMessage :: Maybe Role -> AppMessage
roleMessage Nothing = MsgRoleNothing
roleMessage (Just Consumer) = MsgRoleConsumer
roleMessage (Just InnerCircle) = MsgRoleInnerCircle
roleMessage (Just Admin) = MsgRoleAdmin

listRoleMessage :: Maybe ListRole -> AppMessage
listRoleMessage Nothing = MsgListRoleNothing
listRoleMessage (Just Receiver) = MsgListRoleReceiver
listRoleMessage (Just Sender) = MsgListRoleSender

languageMessage :: Language -> AppMessage
languageMessage English = MsgLangEnglish
languageMessage German = MsgLangGerman

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> return AuthenticationRequired
    Just _ -> return Authorized

getUserRole :: Handler (Maybe Role)
getUserRole = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> return Nothing
    Just userId -> do
      userRole <- runDB $ getBy $ UniqueUserRole userId
      return $ fmap (userRoleRole . entityVal) userRole

getListUserRole :: MailingListId -> Handler (Maybe ListRole)
getListUserRole listId = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> return Nothing
    Just userId -> do
      listRole <- runDB $ getBy $ UniqueListUser userId listId
      return $ fmap (mailingListUserRole . entityVal) listRole

isAdmin :: Handler AuthResult
isAdmin = do
  role <- getUserRole
  case role of
    Just Admin -> return Authorized
    Nothing -> return AuthenticationRequired
    _ -> return $ Unauthorized "Must be an Admin."

isInnerCircle :: Handler AuthResult
isInnerCircle = do
  role <- getUserRole
  case role of
    Just Admin -> return Authorized
    Just InnerCircle -> return Authorized
    Nothing -> return AuthenticationRequired
    _ -> return $ Unauthorized "Must be a Member of the Inner Circle™."

canEditList :: MailingListId -> Handler AuthResult
canEditList listId = do
  role <- getListUserRole listId
  case role of
    Just Sender -> return Authorized
    _ -> isInnerCircle

canSendToList :: MailingListId -> Handler AuthResult
canSendToList listId = do
  role <- getListUserRole listId
  case role of
    Just Sender -> return Authorized
    _ -> return $ Unauthorized "Must be an Author."

canSubscribeToList :: MailingListId -> Handler AuthResult
canSubscribeToList _ = return Authorized

canUnsubscribeFromList :: MailingListId -> Handler AuthResult
canUnsubscribeFromList _ = return Authorized

canDeleteUser :: UserId -> Handler AuthResult
canDeleteUser userId = do
 mAuthId <- maybeAuthId
 case mAuthId of
   Just uId ->
     if userId == uId
       then return $ Unauthorized "Can't delete yourself."
       else isAdmin
   Nothing -> return AuthenticationRequired

canSetRole :: UserId -> Role -> Handler AuthResult
canSetRole userId _ = do
  mAuthId <- maybeAuthId
  case mAuthId of
    Just uId ->
      if userId == uId
        then return $ Unauthorized "Can't change your own role."
        else isAdmin
    Nothing -> return AuthenticationRequired

canEditEvent :: EventId -> Handler AuthResult
canEditEvent eventId = do
  mEvt <- runDB $ get eventId
  case mEvt of
    Nothing -> return $ Unauthorized "Event doesn't exist."
    Just evt -> canEditList $ eventList evt

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

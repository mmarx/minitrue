{-# LANGUAGE RankNTypes #-}
module Auth ( authEmail
            , YesodAuthEmail (..)
            , EmailCreds (..)
            , saltPass
            , isValidPass
            , loginR
            , registerR
            , forgotPasswordR
            , setpassR
            ) where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Control.Monad (void)
import Data.Text (Text)
import Yesod.Auth
import Yesod.Auth.Email ( YesodAuthEmail (..)
                        , EmailCreds (..)
                        , saltPass
                        , isValidPass
                        , loginR
                        , registerR
                        , forgotPasswordR
                        , setpassR
                        )
import Yesod.Core
import qualified Yesod.Auth.Email as AE
import qualified Yesod.Auth.Message as Msg
import Settings

data AuthMessage = MsgResetButton
                 | MsgLoginButton
                 | MsgRegisterButton
                 | MsgForgotButton
                 | MsgCurrentPassword

instance YesodAuthEmail master => RenderMessage master AuthMessage where
  renderMessage _ ("en":_) = renderEnglish
  renderMessage _ ("de":_) = renderGerman
  renderMessage _ ("de-DE":_) = renderGerman
  renderMessage master (_:langs) = renderMessage master langs
  renderMessage _ _ = renderDefault

renderDefault :: AuthMessage -> Text
renderDefault = renderEnglish

renderEnglish :: AuthMessage -> Text
renderEnglish MsgResetButton = "Reset"
renderEnglish MsgLoginButton = "Login"
renderEnglish MsgRegisterButton = "Register"
renderEnglish MsgForgotButton = "Recover forgotten password"
renderEnglish MsgCurrentPassword = "Current password"

renderGerman :: AuthMessage -> Text
renderGerman MsgResetButton = "Zurücksetzen"
renderGerman MsgLoginButton = "Anmelden"
renderGerman MsgRegisterButton = "Registrieren"
renderGerman MsgForgotButton = "Vergessenes Passwort zurücksetzen"
renderGerman MsgCurrentPassword = "Aktuelles Passwort"

origAuth :: YesodAuthEmail master => AuthPlugin master
origAuth = AE.authEmail

authEmail :: YesodAuthEmail master => AuthPlugin master
authEmail = origAuth { apDispatch = dispatch } { apLogin = login }

login :: YesodAuthEmail master => (Route Auth -> Route master) -> WidgetFor master ()
login toMaster = do
  email <- newIdent
  pwd <- newIdent
  $(widgetFile "login")

dispatch :: YesodAuthEmail master => Text -> [Text] -> AuthHandler master TypedContent
dispatch "GET" ["register"] = getRegisterR >>= sendResponse
dispatch "GET" ["forgot-password"] = getForgotPasswordR >>= sendResponse
dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
dispatch method params = origDispatch method params
  where origDispatch = apDispatch origAuth

getRegisterR :: YesodAuthEmail master => AuthHandler master Html
getRegisterR = do
  email <- newIdent
  toParent <- getRouteToParent
  liftHandler . defaultLayout $ do
    setTitleI Msg.RegisterLong
    $(widgetFile "register")

getForgotPasswordR :: YesodAuthEmail master => AuthHandler master Html
getForgotPasswordR = do
  email <- newIdent
  toParent <- getRouteToParent
  liftHandler . defaultLayout $ do
    setTitleI Msg.PasswordResetTitle
    $(widgetFile "forgot-password")

getPasswordR :: YesodAuthEmail master => AuthHandler master Html
getPasswordR = do
  mAuthId <- maybeAuthId
  case mAuthId of
    Just _ -> return ()
    Nothing -> void $ loginErrorMessageI LoginR Msg.BadSetPass
  pwdCur <- newIdent
  pwdNew <- newIdent
  pwdCon <- newIdent
  toParent <- getRouteToParent
  needOld <- maybe (return True) needOldPassword mAuthId
  liftHandler . defaultLayout $ do
    setTitleI Msg.SetPassTitle
    $(widgetFile "password")

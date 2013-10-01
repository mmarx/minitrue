{-# LANGUAGE NoCPP #-}

module Mail
       ( Address (..)
       , MailBody (..)
       , Mail
       , mailFromTo
       , addHeaders
       , textareaToBody
       ) where

import Prelude
import Data.Text
import Network.Mail.Mime (Mail (mailHeaders))
import Network.Mail.SMTP hiding (sendMail)
import Text.Shakespeare.Text
import Text.Hamlet (Html, shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.ByteString (ByteString)
import Yesod.Form.Fields (Textarea (..))
import qualified Data.Text.Lazy as LT

data MailBody = MailBody { plainBody :: LT.Text
                         , htmlBody :: Html
                         }

wrapPlain :: Maybe Text -> LT.Text -> LT.Text
wrapPlain mName body = [lt|Hi#{name},

#{body}

--The Ministry of Truth.
|]
  where name = maybe "" (cons ' ') mName

wrapHtml :: Maybe Text -> Html -> Html
wrapHtml mName body = [shamlet|$newline always
$doctype 5
<html>
  <body>
    $maybe name <- mName
      <p>Hi #{name},
    $nothing
      <p>Hi,
    <p>#{body}
    <p><i>--The Ministry of Truth.</i>
|]

wrapBody :: Maybe Text -> MailBody -> MailBody
wrapBody mName mail = MailBody { plainBody = (wrapPlain mName) . plainBody $ mail
                               , htmlBody = (wrapHtml mName) . htmlBody $ mail
                               }

textareaToBody :: Textarea -> MailBody
textareaToBody body = MailBody { plainBody = [lt|#{unTextarea body}|]
                               , htmlBody = [shamlet|#{body}|]
                               }

addHeaders :: [(ByteString, Text)] -> Mail -> Mail
addHeaders headers mail = mail { mailHeaders = newHeaders }
  where newHeaders = mailHeaders mail ++ headers

mailFromTo :: Address -> Address -> Text -> MailBody -> Mail
mailFromTo sender receiver subject body = do
  addHeaders headers $ simpleMail sender [receiver] [] [] subject body'
  where wrapped = wrapBody name body
        body' = [ plainTextPart . plainBody $ wrapped
                , htmlPart . renderHtml . htmlBody $ wrapped
                ]
        name = addressName receiver
        headers = [ ("User-Agent", "The Ministry of Truth (minitrue-0.0.1)")
                  ]

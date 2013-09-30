{-# LANGUAGE NoCPP #-}

module Mail
       ( Address (..)
       , Mail
       , mailFromTo
       , addHeaders
       ) where

import Prelude
import Data.Text
import Network.Mail.Mime (Mail (mailHeaders))
import Network.Mail.SMTP hiding (sendMail)
import Text.Shakespeare.Text
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as LT

wrapPlain :: Maybe Text -> Text -> LT.Text
wrapPlain mName body = [lt|Hi#{name},

#{body}

--The Ministry of Truth.
|]
  where name = maybe "" (cons ' ') mName

wrapHtml :: Maybe Text -> Text -> LT.Text
wrapHtml mName body = renderHtml [shamlet|$newline always
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

addHeaders :: [(ByteString, Text)] -> Mail -> Mail
addHeaders headers mail = mail { mailHeaders = newHeaders }
  where newHeaders = mailHeaders mail ++ headers

mailFromTo :: Address -> Address -> Text -> Text -> Mail
mailFromTo sender receiver subject body = do
  addHeaders headers $ simpleMail sender [receiver] [] [] subject body'
  where body' = [ plainTextPart $ wrapPlain name body
               , htmlPart $ wrapHtml name body
               ]
        name = addressName receiver
        headers = [ ("User-Agent", "The Ministry of Truth (minitrue-0.0.1)")
                  ]

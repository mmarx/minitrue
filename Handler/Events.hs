module Handler.Events where

import Import hiding ((<>), length, maximum)
import Languages
import Prelude (maximum)
import Control.Arrow ((<<<))
import Data.Monoid ((<>))
import Data.Text (center, length, justifyLeft)
import Data.Time.Format (TimeLocale (wDays))
import Data.Time.LocalTime (timeOfDayToTime)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (shamletFile)
import Text.Cassius (cassiusFile, renderCss)
import qualified Yesod.Table as Table

getEventR :: EventId -> Handler Html
getEventR eventId = do
  event <- runDB $ get404 eventId
  defaultLayout $(widgetFile "event")

getListEventsR :: MailingListId -> Handler Html
getListEventsR listId = do
  list <- runDB $ get404 listId
  events <- upcomingEvents listId
  let theEvents = eventTable (mailingListLanguage list) events
  defaultLayout $(widgetFile "list-events")

postListEventsR :: MailingListId -> Handler Html
postListEventsR listId = do
  ((eventResult, _), _) <- runFormPost $ eventForm listId Nothing
  case eventResult of
    FormSuccess event -> do
      let evt = event { eventList = listId }
      _ <- runDB $ insert evt
      setMessageI $ MsgCreateEventSuccess $ eventName evt
    _ -> setMessageI MsgCreateEventFail
  redirect $ ListEventsR listId

eventInfo :: Entity Event -> Handler (Language, Entity Event)
eventInfo evt@(Entity _ event) = do
  list <- runDB $ get404 $ eventList event
  return (mailingListLanguage list, evt)

getAllEventsR :: Handler Html
getAllEventsR = do
  events <- (upcomingEvents' []) >>= mapM eventInfo
  let mCategory = Nothing
  defaultLayout $(widgetFile "events")

getCategoryEventsR :: CategoryId -> Handler Html
getCategoryEventsR categoryId = do
  cat <- runDB $ get404 categoryId
  events <- (upcomingEvents' [EventCategory ==. categoryId]) >>= mapM eventInfo
  let mCategory = Just cat
  defaultLayout $(widgetFile "events")

postEventR :: EventId -> Handler Html
postEventR eventId = do
  event <- runDB $ get404 eventId
  ((eventResult, _), _) <- runFormPost $ eventForm (eventList event) (Just event)
  case eventResult of
    FormSuccess evt' -> do
      runDB $ replace eventId evt'
      setMessageI $ MsgEditEventSuccess $ eventName evt'
    _ -> do
      setMessageI $ MsgEditEventFail
  redirect $ ListEventsR $ eventList event

postEventDeleteR :: EventId -> Handler Html
postEventDeleteR eventId = do
  ((eventResult, _), _) <- runFormPost $ dummyDeleteForm
  listId <- runDB $ get404 eventId >>= return . eventList
  case eventResult of
    FormSuccess _ -> do
      name <- runDB $ do
        event <- get404 eventId
        delete eventId
        return $ eventName event
      setMessageI $ MsgDeleteEventSuccess name
    _ -> setMessageI MsgDeleteEventFail
  redirect $ ListEventsR listId

upcomingEvents :: MailingListId -> Handler [Entity Event]
upcomingEvents listId = upcomingEvents' [EventList ==. listId]

upcomingEvents' :: [Filter Event] -> Handler [Entity Event]
upcomingEvents' filt = do
  today <- liftIO $ utctDay <$> getCurrentTime
  runDB $ selectList ([EventDate >=. today] ++ filt)
                     [ Asc EventDate
                     , Asc EventTime
                     ]

eventEditor :: MailingListId -> Maybe (Entity Event) -> Widget
eventEditor listId mIE = do
  let (editAction, msgSubmitButton) = case mIE of
        Nothing -> (ListEventsR listId, MsgNewEventButton)
        Just (Entity eId _) -> (EventR eId, MsgEditEventButton)
  (editWidget, editET) <- handlerToWidget . generateFormPost . eventForm listId $ entityVal <$> mIE
  $(widgetFile "edit")

eventForm :: MailingListId -> Maybe Event -> Form Event
eventForm listId mEvt =
  renderBootstrap3 BootstrapBasicForm $
  Event <$>
  areq textField
       (bfs MsgEventName)
       (eventName <$> mEvt) <*>
  areq textField
       (bfs MsgEventLocation)
       (eventLocation <$> mEvt) <*>
  areq textareaField
       (bfs MsgEventDescription)
       (eventDescription <$> mEvt) <*>
  areq (jqueryDayField def)
       (bfs MsgEventDate)
       (eventDate <$> mEvt) <*>
  areq timeFieldTypeTime
       (bfs MsgEventTime)
       (eventTime <$> mEvt) <*>
  areq hiddenField
       ""
       (Just $
        maybe listId eventList mEvt) <*>
  areq categoryField
       (bfs MsgEventCategory)
       (eventCategory <$> mEvt)
  where categoryField =
          selectField $
          optionsPersistKey []
                            [Asc CategoryName]
                            categoryName

eventTable :: Language -> [Entity Event] -> WidgetFor App ()
eventTable lang events = do
  r <- handlerToWidget getMessageRender
  deleteForm <- handlerToWidget $ generateFormPost $ dummyDeleteForm
  buildBootstrap (mempty
    <> Table.text (r MsgEventDateTime) (formatDateTime lang . entityVal)
    <> Table.widget (r MsgEventCategory) category
    <> Table.text (r MsgEventLocation) (eventLocation . entityVal)
    <> Table.text (r MsgEventName) (eventName . entityVal)
    <> Table.widget (r MsgEventActions) (actions deleteForm)) events
  where actions (deleteWidget, deleteET) (Entity eventId event) = do
          modalId <- handlerToWidget newIdent
          labelId <- handlerToWidget newIdent
          $(widgetFile "event-actions")
        category (Entity _ event) = do
          cat <- handlerToWidget $ runDB $ get404 $ eventCategory event
          [whamlet|#{categoryName cat}|]

eventsPlainTable :: MailingListId -> Handler Text
eventsPlainTable listId = do
  list <- runDB $ get404 listId
  events <- upcomingEvents listId
  if null events
    then return ""
    else return $ renderEvents (mailingListLanguage list) (entityVal <$> events)

eventsHtmlTable :: MailingListId -> Handler Html
eventsHtmlTable listId = do
  list <- runDB $ get404 listId
  events <- upcomingEvents listId
  dummy <- newIdent
  let lang = mailingListLanguage list
      style = $(cassiusFile "templates/events-table.cassius")
  return $ if null events
             then [shamlet||]
             else $(shamletFile "templates/events-table.hamlet")

eventsTables :: MailingListId -> Handler (Maybe (Text, Html))
eventsTables listId = do
  plain <- eventsPlainTable listId
  html <- eventsHtmlTable listId
  return $ case plain of
             "" -> Nothing
             p -> Just (p, html)

eventUTCTime :: Event -> UTCTime
eventUTCTime evt = UTCTime { utctDay = eventDate evt
                           , utctDayTime = timeOfDayToTime $ eventTime evt
                           }

germanTimeLocale :: TimeLocale
germanTimeLocale = defaultTimeLocale { wDays = [ ("Sonntag", "So")
                                               , ("Montag", "Mo")
                                               , ("Dienstag", "Di")
                                               , ("Mittwoch", "Mi")
                                               , ("Donnerstag", "Do")
                                               , ("Freitag", "Fr")
                                               , ("Samstag", "Sa")
                                               ]
                                     }

formatDateTime' :: TimeLocale -> Event -> Text
formatDateTime' loc evt = pack $ formatTime loc "%a. %F %H:%M" $ eventUTCTime evt

formatDateTime :: Language -> Event -> Text
formatDateTime English = formatDateTime' defaultTimeLocale
formatDateTime German = formatDateTime' germanTimeLocale

eventWidths :: [Event] -> (Int, Int)
eventWidths = (maximum *** maximum) <<< unzip <<< map el
  where el = (length . eventLocation) &&& (length . eventName)

renderEvent :: Language -> (Int, Int) -> Event -> Text
renderEvent lang (lW, nW) evt =
  concat $ intersperse " | " [ center 21 ' ' $ formatDateTime lang evt
                             , justifyLeft lW ' ' $ eventLocation evt
                             , justifyLeft nW ' ' $ eventName evt
                             ]

renderEvents :: Language -> [Event] -> Text
renderEvents lang evts = unlines $ renderEvent lang ws <$> evts
  where ws = eventWidths evts

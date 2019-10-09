module Handler.Events where

import Import hiding ((<>), length)
import Languages
import Data.Monoid ((<>))
import Data.Time.Format (TimeLocale (wDays))
import Data.Time.LocalTime (timeOfDayToTime)
import Text.Blaze.Colonnade (encodeHtmlTable)

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

eventActions :: (Widget, Enctype) -> Entity Event -> WidgetFor App ()
eventActions (deleteWidget, deleteET) (Entity eventId event) = do
  modalId <- handlerToWidget newIdent
  labelId <- handlerToWidget newIdent
  $(widgetFile "event-actions")

eventCategoryName :: Entity Event -> WidgetFor App ()
eventCategoryName (Entity _ event) = do
  cat <- handlerToWidget $ runDB $ get404 $ eventCategory event
  [whamlet|#{categoryName cat}|]

eventColonnade :: (AppMessage -> Cell App) -> Language -> (Widget, Enctype) -> Colonnade Headed (Entity Event) (Cell App)
eventColonnade r lang deleteForm = headed (r MsgEventDateTime) (textCell . formatDateTime lang . entityVal)
                                   <> headed (r MsgEventCategory) (cell . eventCategoryName)
                                   <> headed (r MsgEventLocation) (textCell . eventName . entityVal)
                                   <> headed (r MsgEventName) (textCell . eventName . entityVal)
                                   <> headed (r MsgEventActions) (cell . eventActions deleteForm)

eventColonnadeSimple :: Language -> (AppMessage -> Text) -> (Text -> c) -> Colonnade Headed Event c
eventColonnadeSimple lang r u = headed (u $ r MsgEventDateTime) (u . formatDateTime lang)
                            <> headed (u $ r MsgEventLocation) (u . eventLocation)
                            <> headed (u $ r MsgEventName) (u . eventName)

eventTable :: Language -> [Entity Event] -> WidgetFor App ()
eventTable lang events = do
  r <- handlerToWidget getMessageRender
  deleteForm <- handlerToWidget $ generateFormPost $ dummyDeleteForm
  encodeCellTable [class_ "table table-striped"]
    (eventColonnade (textCell . r) lang deleteForm)
    events

eventsPlainTable :: Colonnade Headed Event String -> [Event] -> Text
eventsPlainTable col events = pack $ ascii col events

eventsHtmlTable :: Colonnade Headed Event Html -> [Event] -> Html
eventsHtmlTable col events = encodeHtmlTable (class_ "table table-striped") col
  events

eventsTables :: MailingListId -> Handler (Maybe (Text, Html))
eventsTables listId = do
  list <- runDB $ get404 listId
  events <- map entityVal <$> upcomingEvents listId
  let lang = mailingListLanguage list
  let langCode = case lang of
                   English -> ["en"]
                   German -> ["de"]
  site <- getYesod
  let render = renderMessage site langCode
  let plain = eventsPlainTable (eventColonnadeSimple lang render unpack) events
  let html = eventsHtmlTable (eventColonnadeSimple lang render toHtml) events
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

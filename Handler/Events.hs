module Handler.Events where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery

getEventR :: EventId -> Handler Value
getEventR eventId = (runDB $ get404 eventId) >>= returnJson

getListEventsR :: MailingListId -> Handler Value
getListEventsR listId = do
  today <- lift $ utctDay <$> getCurrentTime
  events <- runDB $ selectList [ EventDate >=. today
                              , EventList ==. listId
                              ]
                     [ Asc EventDate
                     , Asc EventTime
                     ]
  returnJson events

postListEventsR :: MailingListId -> Handler Value
postListEventsR listId = do
  ((eventResult, _), _) <- runFormPost $ eventForm listId Nothing
  case eventResult of
    FormSuccess event -> do
      let evt = event { eventList = listId }
      _ <- runDB $ insert evt
      returnJson evt
    _ -> returnJson ()

postEventR :: EventId -> Handler Value
postEventR eventId = do
  event <- runDB $ get404 eventId
  ((eventResult, _), _) <- runFormPost $ eventForm (eventList event) (Just event)
  case eventResult of
    FormSuccess evt' -> do
      runDB $ replace eventId evt'
      returnJson evt'
    _ -> returnJson ()

postEventDeleteR :: EventId -> Handler Value
postEventDeleteR eventId = do
  runDB $ get404 eventId >> delete eventId
  returnJson ()

eventForm :: MailingListId -> Maybe Event -> Form Event
eventForm listId mEvt = renderBootstrap3 BootstrapBasicForm $ Event
                        <$> areq textField (bfs MsgEventName) (eventName <$> mEvt)
                        <*> areq textField (bfs MsgEventLocation) (eventLocation <$> mEvt)
                        <*> areq (jqueryDayField def) (bfs MsgEventDate) (eventDate <$> mEvt)
                        <*> areq timeFieldTypeTime (bfs MsgEventTime) (eventTime <$> mEvt)
                        <*> areq hiddenField "" (Just $ maybe listId eventList mEvt)

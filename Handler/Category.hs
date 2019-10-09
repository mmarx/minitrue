module Handler.Category where

import Import hiding ((<>))
import Data.Monoid ((<>))

getCategoriesR :: Handler Html
getCategoriesR = do
  (editWidget, editET) <- generateFormPost $ categoryForm Nothing
  categories <- runDB $ selectList [] [Asc CategoryName]
  let theCategories = categoryTable categories
      editAction = CategoriesR
      msgSubmitButton = MsgNewCategoryButton
      edit = $(widgetFile "edit")
  defaultLayout $(widgetFile "categories")

postCategoriesR :: Handler Html
postCategoriesR =
  do ((categoryResult,_),_) <- runFormPost $ categoryForm Nothing
     case categoryResult of
       FormSuccess category -> do
         result <- runDB $ insertUnique category
         case result of
              Nothing -> setMessageI MsgCreateCategoryFail
              _       -> return ()
         setMessageI . MsgCreateCategorySuccess . categoryName $ category
       _ -> setMessageI MsgCreateCategoryFail
     redirect CategoriesR

getCategoryR :: CategoryId -> Handler Html
getCategoryR cId = do
  category <- runDB $ get404 cId
  (editWidget, editET) <- generateFormPost . categoryForm $ Just category
  let editAction = CategoryR cId
      msgSubmitButton = MsgEditCategoryButton
      edit = $(widgetFile "edit")
  defaultLayout $(widgetFile "category")

postCategoryR :: CategoryId -> Handler Html
postCategoryR cId = do
  _ <- runDB $ get404 cId
  ((categoryResult, _), _) <- runFormPost . categoryForm $ Nothing
  case categoryResult of
    (FormSuccess category) -> do
      runDB $ replace cId category
      setMessageI . MsgEditCategorySuccess . categoryName $ category
    _ -> setMessageI MsgEditCategoryFail
  redirect $ CategoryR cId

postCategoryDeleteR :: CategoryId -> Handler Html
postCategoryDeleteR categoryId = do
  runDB $ do
    _ <- get404 categoryId
    deleteWhere [EventCategory ==. categoryId]
    delete categoryId
  redirect CategoriesR

categoryForm :: Maybe Category -> Form Category
categoryForm mCat =
  renderBootstrap3 BootstrapBasicForm $
  Category <$>
  areq textField
       (bfs MsgCategoryName)
       (categoryName <$> mCat) <*>
  areq textField
       (bfs MsgCategoryDescription)
       (categoryDescription <$> mCat)

categoryDeleteForm :: Html -> MForm Handler (FormResult Text, Widget)
categoryDeleteForm extra = do
  (res, view) <- mreq hiddenField "dummy" (Just "dummy")
  let widget = [whamlet|$newline never
                #{extra}
                ^{fvInput view}|]
  return (res, widget)

categoryActions :: Entity Category -> WidgetFor App ()
categoryActions category = do
  modalId <- newIdent
  labelId <- newIdent
  (deleteWidget, deleteET) <- handlerToWidget $ generateFormPost $ categoryDeleteForm
  $(widgetFile "category-actions")

categoryColonnade :: (AppMessage -> Cell App) -> Colonnade Headed (Entity Category) (Cell App)
categoryColonnade r = headed (r MsgNameField) (textCell . categoryName . entityVal)
                      <> headed (r MsgDescField) (textCell . categoryDescription . entityVal)
                      <> headed (r MsgCategoryActions) (cell . categoryActions)

categoryTable :: [Entity Category]
              -> WidgetFor App ()
categoryTable cats = do
  r <- handlerToWidget getMessageRender
  encodeCellTable [class_ "table table-striped"]
    (categoryColonnade $ textCell . r)
    cats

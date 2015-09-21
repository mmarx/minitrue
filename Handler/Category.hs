module Handler.Category where

import Import

getCategoriesR :: Handler Html
getCategoriesR = defaultLayout $ return ()

postCategoriesR :: Handler Value
postCategoriesR = do
   ((categoryResult, _), _) <- runFormPost $ categoryForm Nothing
   case categoryResult of
     FormSuccess category -> (runDB $ insert_ category) >> returnJson category
     _ -> returnJson ()

getCategoryR :: CategoryId -> Handler Html
getCategoryR cId = defaultLayout $ return ()

postCategoryR :: CategoryId -> Handler Html
postCategoryR cId = defaultLayout $ return ()

postCategoryDeleteR :: CategoryId -> Handler Value
postCategoryDeleteR categoryId = do
  runDB $ get404 categoryId >> delete categoryId
  returnJson ()

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

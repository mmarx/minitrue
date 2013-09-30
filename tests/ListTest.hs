{-# LANGUAGE OverloadedStrings #-}
module ListTest
       ( listSpecs
       ) where

import TestImport

listSpecs :: Spec
listSpecs =
  ydescribe "List handler:" $ do

    yit "load the list index" $ do
      get ListsR
      statusIs 200
      htmlAnyContain "h2" "Available lists"
      htmlAnyContain "h2" "New list"

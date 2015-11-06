{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

import Test.Hspec
import Yesod.Core
import Yesod.Raml.Mock
import Yesod.Test

$(parseRamlMockFile "tests/test.raml")

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/api/v1/user/#String HogeR GET
|]

main :: IO ()
main =  hspec $ before (return App) $ do
  it "loads mock handler" $ do
    get $ HogeR "hoge"
    statusIs 200
    bodyContains "123"

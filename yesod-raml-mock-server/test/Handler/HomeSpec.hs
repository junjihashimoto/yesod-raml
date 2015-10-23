module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "loads top page" $ do
        get HomeR
        statusIs 200
        htmlAllContain "h1" "Welcome to Yesod Raml"


    it "test mock output" $ do
        get UserR
        statusIs 200
        bodyContains "taro"

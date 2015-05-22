{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -ddump-splices #-}
import Test.Hspec
import Data.Text (Text, pack, unpack, singleton)
import Yesod.Core
import Language.Haskell.TH.Syntax
import Yesod.Raml.Type
import Yesod.Raml.Routes.Internal
import Yesod.Routes.TH.Types
import qualified Data.Map as M
import Control.Applicative

deriving instance Eq (Dispatch String)
deriving instance Eq (Piece String)
deriving instance Eq (Resource String)
deriving instance Eq (ResourceTree String)
deriving instance Show (ResourceTree String)


routea :: [ResourceTree String]
routea = [parseRamlRoutes|
#%RAML 0.8
title: Hoge API
baseUri: 'https://hoge/api/{version}'
version: v1
protocols: [ HTTPS ]
/user:
  /{String}:
    handler: HogeR
    get:
      description: hoger
    post:
      description: hoger
    /del:
      handler: Hoge2R
      post:
        description: hoger
|]

type Userid = String

routea' :: [ResourceTree String]
routea' = [parseRamlRoutes|
#%RAML 0.8
title: Hoge API
baseUri: 'https://hoge/api/{version}'
version: v1
protocols: [ HTTPS ]
/user:
  /{userid}:
    description: |
      handler:HogeR
    get:
      description: hoger
    /del:
      handler: Hoge2R
      post:
        description: hoger
|]

routeb = [parseRoutes|
/api/v1/user/#String HogeR GET POST
/api/v1/user/#String/del Hoge2R POST
|]

routeb' = [parseRoutes|
/api/v1/user/#Userid HogeR GET
/api/v1/user/#Userid/del Hoge2R POST
|]


main :: IO ()
main = hspec $ do
  describe "handler" $ do
    it "parse handler from description" $ do
      toHandler Nothing
        (RamlResource {
            r_displayName = Nothing
          , r_description = Just "handler: hogehoge\n"
          , r_handler = Nothing
          , r_methods = M.fromList [("GET",RamlMethod M.empty)]
          , r_paths = M.empty
          } ) `shouldBe` Right "hogehoge"
    it "parse handler from description" $ do
      toHandler Nothing
        (RamlResource {
            r_displayName = Nothing
          , r_description = Just "burabura\nhandler: hogehoge\n"
          , r_handler = Nothing
          , r_methods = M.fromList [("GET",RamlMethod M.empty)]
          , r_paths = M.empty
          } ) `shouldBe` Right "hogehoge"
    it "parse handler from handler" $ do
      toHandler Nothing
        (RamlResource {
            r_displayName = Nothing
          , r_description = Just "handler: hogehoge\n"
          , r_handler = Just "hoge"
          , r_methods = M.fromList [("GET",RamlMethod M.empty)]
          , r_paths = M.empty
          } ) `shouldBe` Right "hoge"
  describe "resource tree" $ do
    it "parseRamlRoutes from handler" $ do
      routea `shouldBe` routeb
    it "parseRamlRoutes from description" $ do
      routea' `shouldBe` routeb'
    it "parseRamlRoutesFile" $ do
      $(parseRamlRoutesFile "tests/test.raml") `shouldBe` routeb'

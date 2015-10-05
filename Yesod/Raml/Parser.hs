{-#LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Raml.Parser () where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types(Parser)
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Yesod.Raml.Type


toResponseBody :: HashMap Text Value -> Parser (Map Text RamlResponseBody)
toResponseBody hashmap = do
  list <- forM (HM.toList hashmap) $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlResponseBody
    return (k,val)
  return $ M.fromList list

toResponse :: HashMap Text Value -> Parser (Map Text RamlResponse)
toResponse hashmap = do
  list <- forM (HM.toList hashmap) $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlResponse
    return (k,val)
  return $ M.fromList list


toMethod :: Object -> Parser (Map Text RamlMethod)
toMethod hashmap = do
  let methods = filter (\(k,_) ->  elem k ["get","post","delete","put", "options",
                                            "GET","POST","DELETE","PUT", "OPTIONS"
                                           ]
                       ) (HM.toList hashmap)
  list <- forM methods $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlMethod
    return (k,val)
  return $ M.fromList list


toResource :: HashMap Text Value -> Parser (Map Text RamlResource)
toResource hashmap = do
  let rs = filter (\(k,_) ->  T.isPrefixOf "/" k) (HM.toList hashmap)
  list <- forM rs $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlResource
    return (k,val)
  return $ M.fromList list

instance FromJSON RamlResponseBody where
  parseJSON (Object obj) = RamlResponseBody
                           <$> obj .:? "schema"
                           <*> obj .:? "example"
  parseJSON m = fail $ "Can not parse:" ++ show m

instance FromJSON RamlResponse where
  parseJSON (Object obj) = RamlResponse
                           <$> obj .:? "description"
                           <*> toResponseBody obj
  parseJSON m = fail $ "Can not parse:" ++ show m


instance FromJSON RamlMethod where
  parseJSON (Object obj) = do
    mres <- obj .:? "responses" :: Parser (Maybe Value)
    res <- case mres of
      Nothing -> return $ M.empty
      Just (Object obj') -> toResponse obj'
      Just m -> fail $ "Can not parse:" ++ show m
    RamlMethod
      <$> return res
  parseJSON m = fail $ "Can not parse:" ++ show m

instance FromJSON RamlResource where
  parseJSON (Object obj) = RamlResource
                           <$> obj .:? "displayName"
                           <*> obj .:? "description"
                           <*> obj .:? "handler"
                           <*> toMethod obj
                           <*> toResource obj
  parseJSON m = fail $ "Can not parse:" ++ show m

instance FromJSON RamlDocumentation where
  parseJSON (Object obj) = RamlDocumentation
                           <$> obj .: "title"
                           <*> obj .: "content"
  parseJSON m = fail $ "Can not parse:" ++ show m


instance FromJSON Raml where
  parseJSON (Object obj) = Raml <$> obj .: "title"
                                <*> obj .: "version"
                                <*> obj .: "baseUri"
                                <*> obj .:? "documentation"
                                <*> toResource obj
  parseJSON m = fail $ "Can not parse:" ++ show m


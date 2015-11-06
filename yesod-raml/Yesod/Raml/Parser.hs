{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE FlexibleInstances#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Raml.Parser (
  parseRaml
, parseRamlFile
, applyVersion
, applyTrait
, applyResourceType
, genUriParamDescription
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types(Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Monoid
import Data.Default

import Yesod.Raml.Type

import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

--import Yesod.Routes.TH.Types
import Language.Haskell.TH.Lift

(.::?) ::  Object ->  Text ->  Parser (Maybe Text)
(.::?) obj key = do
  mobj <- obj .:? key
  case mobj of
    Nothing ->  return Nothing
    Just (String str) ->  return $ Just str
    Just (Number str) ->  return $ Just $ T.pack $ show str
    Just (Bool str) ->  if str then return ( Just "true" ) else return (Just "false")
    _ ->  fail $ ".::? : Can not parse :" ++ show mobj

(.::) ::  Object ->  Text ->  Parser Text
(.::) obj key = do
  mobj <- obj .:? key
  case mobj of
    Just (String str) ->  return $ str
    Just (Number str) ->  return $ T.pack $ show str
    _ ->  fail $ ".::? : Can not parse :" ++ show mobj


toMap :: FromJSON a => Object -> Text -> Parser (Map Text a)
toMap obj key = do
  mobj <- obj .:? key
  case mobj of
    Nothing -> return M.empty
    Just (Object hashmap) -> do
      list <- forM (HM.toList hashmap) $ \(k,v) -> do
        val <- parseJSON v
        return (k,val)
      return $ M.fromList list
    Just Null ->  return M.empty
    Just _ -> fail $ "Can not parse Map:" ++ show mobj

toArray :: FromJSON a => Object -> Text -> Parser [a]
toArray obj key = do
  mobj <- obj .:? key
  case mobj of
    Nothing -> return []
    Just (Array ary) -> do
      list <- forM (V.toList ary) $ \v -> do
        val <- parseJSON v
        return val
      return list
    Just Null ->  return []
    Just _ -> fail $ "Can not parse Array:" ++ show mobj

toMethod :: Object -> Parser (Map Text RamlMethod)
toMethod hashmap = do
  let methods = filter (\(k,_) ->  elem k ["get",     "GET",
                                            "post",    "POST",
                                            "head",    "HEAD",
                                            "delete",  "DELETE",
                                            "trace",   "TRACE",
                                            "connect", "CONNECT",
                                            "put",     "PUT",
                                            "options", "OPTIONS",
                                            "patch",   "PATCH"
                                           ]
                       ) (HM.toList hashmap)
  list <- forM methods $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlMethod
    return (k,val)
  return $ M.fromList list


toResource :: Object -> Parser (Map Text RamlResource)
toResource hashmap = do
  let rs = filter (\(k,_) ->  T.isPrefixOf "/" k) (HM.toList hashmap)
  list <- forM rs $ \(k,v) -> do
    val <- parseJSON v :: Parser RamlResource
    return (k,val)
  return $ M.fromList list

instance FromJSON RamlResponseBody where
  parseJSON (Object obj) = RamlResponseBody
                           <$> obj .:? "schema"
                           <*> obj .::? "example"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlResponseBody:" ++ show m

instance FromJSON RamlResponse where
  parseJSON (Object obj) = RamlResponse
                           <$> obj .:? "description"
                           <*> toMap obj "headers"
                           <*> toMap obj "body"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlResponse:" ++ show m

instance FromJSON RamlNamedParameters where
  parseJSON (Object obj) = RamlNamedParameters
                           <$> obj .:? "displayName"
                           <*> obj .:? "description"
                           <*> obj .:? "type"
                           <*> toArray obj "enum"
                           <*> obj .:? "pattern"
                           <*> obj .:? "minLength"
                           <*> obj .:? "maxLength"
                           <*> obj .:? "minimum"
                           <*> obj .:? "maximum"
                           <*> obj .::? "example"
                           <*> obj .:? "repeat"
                           <*> obj .:? "required"
                           <*> obj .::? "default"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlNamedParameters:" ++ show m

instance FromJSON RamlRequestBody where
  parseJSON (Object obj) = RamlRequestBody
                           <$> toMap obj "formParameters"
                           <*> obj .:? "schema"
                           <*> obj .::? "example"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlRequestBody:" ++ show m

instance FromJSON RamlMethod where
  parseJSON (Object obj) = do
    RamlMethod
      <$> toMap obj "responses"
      <*> obj .:? "description"
      <*> toMap obj "headers"
      <*> toArray obj "securedBy"
      <*> toArray obj "protocols"
      <*> toMap obj "queryParameters"
      <*> toMap obj "body"
      <*> toArray obj "is"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlMethod:" ++ show m

instance FromJSON RamlTrait where
  parseJSON (Object obj) = RamlTrait
                           <$> obj .:? "usage"
                           <*> toMap obj "responses"
                           <*> obj .:? "description"
                           <*> toMap obj "headers"
                           <*> toArray obj "securedBy"
                           <*> toArray obj "protocols"
                           <*> toMap obj "queryParameters"
                           <*> toMap obj "body"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlTrait:" ++ show m

instance FromJSON RamlResource where
  parseJSON (Object obj) = RamlResource
                           <$> obj .:? "displayName"
                           <*> obj .:? "description"
                           <*> obj .:? "handler"
                           <*> toMethod obj
                           <*> toResource obj
                           <*> toMap obj "uriParameters"
                           <*> toMap obj "baseUriParameters"
                           <*> obj .:? "type"
                           <*> toArray obj "is"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlResource:" ++ show m


instance FromJSON RamlResourceType where
  parseJSON (Object obj) = RamlResourceType
                           <$> obj .:? "usage"
                           <*> obj .:? "displayName"
                           <*> obj .:? "description"
                           <*> toMethod obj
                           <*> toResource obj
                           <*> toMap obj "uriParameters"
                           <*> toMap obj "baseUriParameters"
  parseJSON m = fail $ "Can not parse RamlResourceType:" ++ show m

instance FromJSON RamlDocumentation where
  parseJSON (Object obj) = RamlDocumentation
                           <$> obj .: "title"
                           <*> obj .: "content"
  parseJSON m = fail $ "Can not parse RamlDocumentation:" ++ show m


instance FromJSON RamlSecuritySchemes where
  parseJSON (Object obj) = RamlSecuritySchemes
                           <$> obj .: "description"
                           <*> obj .: "type"
                           <*> obj .: "describedBy"
                           <*> obj .: "settings"
  parseJSON Null = return def
  parseJSON m = fail $ "Can not parse RamlSecuritySchemes:" ++ show m


instance FromJSON Raml where
  parseJSON (Object obj) = Raml <$> obj .: "title"
                                <*> obj .:: "version"
                                <*> obj .: "baseUri"
                                <*> toMap obj "baseUriParameters"
                                <*> toArray obj "Protocol"
                                <*> obj .:? "mediaType"
                                <*> toArray obj "schemas"
                                <*> toMap obj "uriParameters"
                                <*> toArray obj "documentation"
                                <*> toResource obj
                                <*> toArray obj "securitySchemes"
                                <*> toArray obj "resourceTypes"
                                <*> toArray obj "traits"
  parseJSON m = fail $ "Can not parse Raml:" ++ show m

parseRaml :: QuasiQuoter
parseRaml = QuasiQuoter
    { quoteExp = lift . toRamlFromString
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined            
    }
  where
    toRamlFromString :: String ->  Raml
    toRamlFromString ramlStr =
      let eRaml = Y.decodeEither (B.pack ramlStr) :: Either String Raml
          raml = case eRaml of
            Right v -> v
            Left e -> error $ "Invalid raml :" ++ e
      in raml

parseRamlFile :: FilePath -> Q Exp
parseRamlFile file = do
  qAddDependentFile file
  s <- qRunIO $ toRamlFromFile file
  lift s
  where
    toRamlFromFile :: String -> IO Raml
    toRamlFromFile file' = do
      eRaml <- YI.decodeFileEither file'
      let raml = case eRaml of
            Right v -> v
            Left e -> error $ "Invalid raml :" ++ show e
      return raml

instance Lift Text where
  lift txt = [| T.pack $(lift $ T.unpack txt) |]

$(deriveLift ''Map)
$(deriveLift ''RamlNamedParameters)
$(deriveLift ''RamlRequestBody)
$(deriveLift ''RamlResource)
$(deriveLift ''RamlResourceType)
$(deriveLift ''RamlTrait)
$(deriveLift ''RamlSecuritySchemes)
$(deriveLift ''RamlResponse)
$(deriveLift ''RamlResponseBody)
$(deriveLift ''RamlDocumentation)
$(deriveLift ''RamlMethod)
$(deriveLift ''Raml)

applyVersion ::  Raml -> Raml
applyVersion raml = raml { baseUri = T.replace "{version}" (version raml) (baseUri raml) }


applyTrait :: Raml -> Raml
applyTrait raml = raml { paths = applyTraitForPath (paths raml) }
  where
    traits' :: Map TraitKey RamlTrait
    traits' = foldr (<>) mempty (traits raml)
    fromTraitKeys :: [TraitKey] -> RamlTrait
    fromTraitKeys keys = foldr (<>) mempty (map (traits' M.!) keys)
    applyTraitForPath paths' = M.map applyTraitForResource paths'
    applyTraitForResource res =
      res {
        r_paths = applyTraitForPath (r_paths res)
      , r_methods = applyTraitForMethod (r_methods res)
      }
      where
        trait = fromTraitKeys (r_is res)
        applyTraitForMethod methods' = M.map (appendTrait' traits' trait) methods'
    
    appendTrait :: RamlTrait -> RamlMethod -> RamlMethod
    appendTrait a b = 
      b {
        m_responses = t_responses a <> m_responses b
      , m_description = t_description a <> m_description b
      , m_headers = t_headers a <> m_headers b
      , m_securedBy = t_securedBy a <> m_securedBy b
      , m_protocols = t_protocols a <> m_protocols b
      , m_queryParameters = t_queryParameters a <>  m_queryParameters b
      , m_body = t_body a <> m_body b
      }
    
    appendTrait' :: Map TraitKey RamlTrait -> RamlTrait -> RamlMethod -> RamlMethod
    appendTrait' m a b = appendTrait (a <> trait) b 
      where
        trait = foldr (<>) mempty (map (m M.!) (m_is b))
  

applyResourceType :: Raml -> Raml
applyResourceType raml = raml { paths = applyResourceTypeForPath (paths raml) }
  where
    types' :: Map ResourceTypeKey RamlResourceType
    types' = foldr (<>) mempty (resourceTypes raml)
    fromResourceTypeKey ::  ResourceTypeKey -> RamlResourceType
    fromResourceTypeKey key = types' M.! key
    applyResourceTypeForPath paths' = M.map applyResourceTypeForResource paths'
    applyResourceTypeForResource res =
      let res'' = case r_type res of
            Just typ ->  appendResourceType (fromResourceTypeKey typ) res
            Nothing ->  res
      in res'' {
        r_paths = applyResourceTypeForPath (r_paths res)
      }
    
    appendResourceType :: RamlResourceType -> RamlResource -> RamlResource
    appendResourceType a b = 
      b {
        r_methods = rt_methods a <> r_methods b
      , r_paths = rt_paths a <> r_paths b
      , r_uriParameters = rt_uriParameters a <> r_uriParameters b
      , r_baseUriParameters = rt_baseUriParameters a <> r_baseUriParameters b
      }

  
genUriParamDescription :: Raml -> Raml
genUriParamDescription raml = raml { paths = applyUri "" (paths raml) }
  where
    applyUri uri map' = M.fromList $ map (applyUri' uri) $ M.toList map'
    applyUri' uri (path,res) = (path,
                                res{
                                  r_uriParameters = M.fromList (path2uriParameters (uri<>path)) <> r_uriParameters res
                                , r_paths = applyUri (uri<>path) (r_paths res)
                                })
    
    routeToParams :: T.Text -> [T.Text]
    routeToParams str | T.dropWhile (/= '{') str /= "" &&
                        T.dropWhile (/= '}') str /= ""    = [T.takeWhile (/= '}') (T.tail (T.dropWhile (/= '{') str))] ++
                                                          routeToParams (T.tail (T.dropWhile (/= '}') str))
                      | otherwise = []
    
    
    path2uriParameters uri =
      flip map (routeToParams uri) $
        \param ->
           (param,
            RamlNamedParameters {
          h_displayName = Nothing
          , h_description = Nothing
          , h_type = Just "string"
          , h_enum = []
          , h_pattern = Nothing
          , h_minLength = Nothing
          , h_maxLength = Nothing
          , h_minimum = Nothing
          , h_maximum = Nothing
          , h_example = Nothing
          , h_repeat = Nothing
          , h_required = Just True
          , h_default = Nothing
          })



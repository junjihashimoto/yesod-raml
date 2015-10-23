{-#LANGUAGE OverloadedStrings#-}

module Yesod.Raml.Routes.Internal where

import Control.Applicative
import Control.Monad
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Yesod.Raml.Type
import Yesod.Raml.Parser()

import Text.Regex.Posix hiding (empty)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Yesod.Routes.TH.Types
import Network.URI hiding (path)

routesFromRaml ::  Raml -> Either String [RouteEx]
routesFromRaml raml = do
  let buri =  T.replace "{version}" (version raml) (baseUri raml)
  uripaths <- case parsePath buri of
    (Just uri) -> return $ fmap ("/" <> ) $ T.split (== '/') $ if T.isPrefixOf "/" uri then T.tail uri else uri
    Nothing -> Left $ "can not parse: " ++  (T.unpack buri)
  v <- forM (M.toList (paths raml)) $ \(k,v) -> do
    routesFromRamlResource v $ uripaths ++ splitPath k
  return $ concat v
  where
    parsePath uri = fmap T.pack $ fmap uriPath $ parseURI (T.unpack uri)

splitPath :: T.Text -> [T.Text]
splitPath path = 
  case T.split (== '/') path of
    (_:xs) -> map (T.append "/") xs
    _  -> [path]


methodExFromRamlMethod ::  (Method,RamlMethod) ->  MethodEx
methodExFromRamlMethod (method,val) =
  let example = do
        response <-  M.lookup "200" (m_responses val)
        (contentType,body) <- listToMaybe (M.toList (res_body response))
        ex <- res_example body
        return (contentType,ex)
  in MethodEx (T.unpack (T.toUpper (method))) example

routesFromRamlResource :: RamlResource -> [Path] -> Either String [RouteEx]
routesFromRamlResource raml paths' = do
  rrlist <- forM (M.toList (r_paths raml)) $ \(k,v) -> do
    routesFromRamlResource v (paths' ++ splitPath k)
  let rlist = concat rrlist
  case toHandler Nothing raml of
    Right handle -> do
      let methods =  flip map  (M.toList (r_methods raml)) methodExFromRamlMethod
          route = RouteEx {
              re_pieces = toPieces paths'
            , re_handler = T.unpack handle
            , re_methods = methods
            }
      return $ route : rlist
    Left err -> do
      case rrlist of
        [] -> Left err
        _ -> return rlist
      
toHandler :: Maybe HandlerHint -> RamlResource -> Either String Handler
toHandler mhint ramlMethod =
  (fromHandlerTag ramlMethod) <|>
  (fromDescription (fromMaybe "handler: *(.*)" mhint) ramlMethod)
  where
    fromHandlerTag :: RamlResource -> Either String Handler
    fromHandlerTag ramlMethod' = do
      handler <- case r_handler ramlMethod' of
        Nothing -> Left "handler of method is empty"
        (Just desc') -> return desc'
      return handler
    fromRegex :: T.Text -> T.Text -> Maybe T.Text
    fromRegex pattern str =
      let v = (T.unpack str) =~ (T.unpack pattern) :: (String,String,String,[String])
      in case v of
        (_,_,_,[]) -> Nothing
        (_,_,_,h:_) -> Just $ T.pack h
    fromDescription :: HandlerHint -> RamlResource -> Either String Handler
    fromDescription hint ramlMethod' = do
      desc <- case r_description ramlMethod' of
        Nothing -> Left "Description of method is empty"
        (Just desc') -> return desc'
      case (foldr (<|>) empty $ map (fromRegex hint) $ T.lines $ desc) of
        Nothing -> Left "Can not find Handler"
        Just handler -> return handler

toYesodResource :: RouteEx ->  Resource String
toYesodResource route =
  Resource {
    resourceName = re_handler route
  , resourcePieces = re_pieces route
  , resourceDispatch =
      Methods {
        methodsMulti = Nothing
      , methodsMethods = map me_method (re_methods route)
      }
  , resourceAttrs = []
  , resourceCheck = True
  }

toRoutesFromString :: String -> [ResourceTree String]
toRoutesFromString ramlStr =
  let eRaml = Y.decodeEither (B.pack ramlStr) :: Either String Raml
      raml = case eRaml of
        Right v -> v
        Left e -> error $ "Invalid raml :" ++ e
      routes = case (routesFromRaml raml) of
        Right v -> v
        Left e -> error $ "Invalid resource : " ++ e
  in map ResourceLeaf $ map toYesodResource routes

toRoutesFromFile :: String -> IO [ResourceTree String]
toRoutesFromFile file = do
  eRaml <- YI.decodeFileEither file
  let raml = case eRaml of
        Right v -> v
        Left e -> error $ "Invalid raml :" ++ show e
      routes = case (routesFromRaml raml) of
        Right v -> v
        Left e -> error $ "Invalid resource : " ++ e
  return $ map ResourceLeaf $ map toYesodResource routes


capitalize :: String -> String
capitalize [] = []
capitalize (h:str) = C.toUpper h:str

toPiece :: T.Text -> Piece String
toPiece str | T.isPrefixOf "/{" str && T.isSuffixOf "}" str= Dynamic $ capitalize $ T.unpack $ T.takeWhile (/= '}') $ T.tail $ T.dropWhile (/= '{') str
            | T.isPrefixOf "/" str = Static $ T.unpack $ T.tail str
            | otherwise = error "Prefix is not '/'."

fromPiece ::  Piece String ->  T.Text
fromPiece (Static str) = "/" <> T.pack str
fromPiece (Dynamic str) = "/#" <> T.pack str

toPieces :: [Path] -> [Piece String]
toPieces paths' = map toPiece paths'

fromPieces ::  [Piece String] ->  [Path]
fromPieces paths' = map fromPiece paths'

toYesodRoutes ::  [RouteEx] ->  T.Text
toYesodRoutes routes = foldr (\a b ->  toRoute a <> "\n" <> b ) "" routes
  where
    toRoute ::  RouteEx ->  T.Text
    toRoute r = foldr (<>) "" (fromPieces (re_pieces r)) <> " " <>
                          T.pack (re_handler r) <> " " <>
                          T.intercalate " " (map (T.pack.me_method) (re_methods r))


parseRamlRoutes :: QuasiQuoter
parseRamlRoutes = QuasiQuoter
    { quoteExp = lift . toRoutesFromString
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined            
    }

parseRamlRoutesFile :: FilePath -> Q Exp
parseRamlRoutesFile file = do
  qAddDependentFile file
  s <- qRunIO $ toRoutesFromFile file
  lift s


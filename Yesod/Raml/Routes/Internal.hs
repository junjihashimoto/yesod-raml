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
import Network.URI

routesFromRaml :: Raml -> Either String [RamlRoute]
routesFromRaml raml = do
  let buri =  T.replace "{version}" (version raml) (baseUri raml)
  uripaths <- case parsePath buri of
    (Just uri) -> return $ fmap ("/" <> ) $ T.split (== '/') $ if T.isPrefixOf "/" uri then T.tail uri else uri
    Nothing -> Left $ "can not parse: " ++  (T.unpack buri)
  v <- forM (M.toList (paths raml)) $ \(k,v) -> do
    routesFromRamlResource v $ uripaths ++ [k]
  return $ concat v
  where
    parsePath uri = fmap T.pack $ fmap uriPath $ parseURI (T.unpack uri)

routesFromRamlResource :: RamlResource -> [Path] -> Either String [RamlRoute]
routesFromRamlResource raml paths' = do
  rrlist <- forM (M.toList (r_paths raml)) $ \(k,v) -> do
    routesFromRamlResource v (paths' ++ [k])
  let rlist = concat rrlist
  case toHandler Nothing raml of
    Right handle -> do
      let methods = map fst $ M.toList (r_methods raml)
      return $ (RamlRoute paths' handle methods):rlist
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

toYesodResource :: RamlRoute -> Resource String
toYesodResource route =
  Resource {
    resourceName = T.unpack (rr_handler route)
  , resourcePieces = toPieces (rr_pieces route)
  , resourceDispatch =
      Methods {
        methodsMulti = Nothing
      , methodsMethods = map (T.unpack.T.toUpper) (rr_methods route)
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

toPieces :: [Path] -> [Piece String]
toPieces paths' = map toPiece paths'


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


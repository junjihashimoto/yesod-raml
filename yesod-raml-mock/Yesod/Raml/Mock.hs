{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Yesod.Raml.Mock (
  parseRamlMockFile
, mockFromRaml
, mockSrcFromRaml
) where

import Yesod.Raml.Routes

import Control.Monad

import Yesod.Core
import Data.Char(toLower)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Yesod.Raml.Type
import Yesod.Raml.Parser ()
import Yesod.Routes.TH.Types
import qualified Data.Text as T
import qualified Data.Yaml.Include as YI


parseRamlMockFile ::  FilePath -> Q [Dec]
parseRamlMockFile file = do
  qAddDependentFile file
  s <- qRunIO $ do
    eRaml <- YI.decodeFileEither file
    let raml = case eRaml of
          Right v -> v
          Left e -> error $ "Invalid raml :" ++ show e
    return $ raml
  mockFromRaml s

mockFromRaml ::  Raml ->  Q [Dec]
mockFromRaml raml = do
  case routesFromRaml raml of
    Left err ->  fail err
    Right routes ->  do
      decs <-  forM routes handlerFromRoute
      return $ concat decs

mockSrcFromRaml ::  Raml ->  IO String
mockSrcFromRaml raml = do
  v <-  runQ $ mockFromRaml raml
  return $ show $ ppr v

handlerFromRoute ::  RouteEx ->  Q [Dec]
handlerFromRoute RouteEx{..} = do
  decs <-  forM re_methods (handlerFromRoute' re_pieces re_handler)
  return $ concat decs

handlerFromRoute' ::  [Piece String] ->  String ->  MethodEx ->  Q [Dec]
handlerFromRoute' paths handler MethodEx{..} = do
  let method = map toLower $ me_method
      handlerFunc =  mkName $ method ++ handler
      args = argsFromPiecies paths
      app = mkName "app"
      ret = mkName "return"
      cont = mkName "TypedContent"
      notfound = mkName "notFound"
      output =
        case me_example of
          Nothing ->  (VarE notfound)
          Just (typ,ex) ->
            (AppE
              (VarE ret)
              (AppE
                (AppE (ConE cont)
                  (LitE (StringL (T.unpack typ))))
                (LitE (StringL (T.unpack ex)))
              )
            )
        
  return [
    SigD handlerFunc (
       ForallT
         [PlainTV app]
#if MIN_VERSION_template_haskell(2,10,0)
         [AppT (ConT ''Yesod) (VarT app)]
#else
         [ClassP ''Yesod [VarT app]]
#endif
         (typeList
           args 
           (AppT
             (AppT
               (AppT (ConT ''HandlerT) (VarT app))
               (ConT ''IO)
             )
             (ConT ''TypedContent)
           )
         )
       ),
    FunD handlerFunc
      [Clause (wildPs args)
        (NormalB output) []
      ]
    ]
  where
    typeList [] v =  v
    typeList (x:xs) v = (AppT (AppT ArrowT (ConT x)) (typeList xs v))
    wildPs xs = map (\_ ->  WildP) xs


argsFromPiecies ::  [Piece String] ->  [Name]
argsFromPiecies [] = []
argsFromPiecies (Static _ : xs ) = argsFromPiecies xs
argsFromPiecies (Dynamic typ : xs ) = mkName typ : argsFromPiecies xs

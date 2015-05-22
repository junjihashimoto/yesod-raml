{-#LANGUAGE OverloadedStrings#-}

module Yesod.Raml.Type where

import Data.Text (Text)
import Data.Map (Map)

type HandlerHint = Text
type Handler = Text
type Path = Text
type Method = Text
type ResponseCode = Text
type ContentType = Text

data RamlResponseBody =
  RamlResponseBody {
    res_schema :: Maybe Text
  , res_example :: Maybe Text
  } deriving (Show,Eq,Ord)

data RamlResponse =
  RamlResponse {
    res_description :: Maybe Text
  , res_body :: Map ContentType RamlResponseBody
  } deriving (Show,Eq,Ord)

data RamlMethod =
  RamlMethod {
    m_responses :: Map ResponseCode RamlResponse
  } deriving (Show,Eq,Ord)

data RamlResource =
  RamlResource {
    r_displayName :: Maybe Text
  , r_description :: Maybe Text
  , r_handler :: Maybe Handler
  , r_methods :: Map Method RamlMethod
  , r_paths :: Map Path RamlResource
  } deriving (Show,Eq,Ord)

data RamlDocumentation = 
  RamlDocumentation {
    doc_title :: Text
  , doc_content :: Text
  } deriving (Show,Eq,Ord)

data Raml =
  Raml {
    title :: Text
  , version :: Text
  , baseUri :: Text
  , documentation :: Maybe [RamlDocumentation]
  , paths :: Map Path RamlResource
  } deriving (Show,Eq,Ord)

data RamlRoute =
  RamlRoute {
    rr_pieces :: [Path]
  , rr_handler :: Text
  , rr_methods :: [Method]
  } deriving (Show,Eq,Ord)


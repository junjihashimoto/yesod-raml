{-#LANGUAGE OverloadedStrings#-}

module Yesod.Raml.Type where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Yesod.Routes.TH.Types (Piece)
import Data.Default

type HandlerHint = Text
type Handler = Text
type Path = Text
type Method = Text
type ResponseCode = Text
type ContentType = Text
type Auth = Text
type HeaderKey = Text
type QueryParameter = Text
type UriParameter = Text
type SchemaKey = Text
type Schema = Text
type Protocol = Text
type MediaType = Text
type ResourceTypeKey = Text
type TraitKey = Text

data RamlResponseBody =
  RamlResponseBody {
    res_schema :: Maybe Text
  , res_example :: Maybe Text
  } deriving (Show,Eq,Ord)

instance Default RamlResponseBody where
  def = RamlResponseBody {
    res_schema  = Nothing
  , res_example = Nothing
  }

data RamlResponse =
  RamlResponse {
    res_description :: Maybe Text
  , res_headers :: Map HeaderKey RamlNamedParameters
  , res_body :: Map ContentType RamlResponseBody
  } deriving (Show,Eq,Ord)

instance Default RamlResponse where
  def = RamlResponse {
    res_description = Nothing
  , res_headers = M.empty
  , res_body = M.empty
  }

data RamlNamedParameters =
  RamlNamedParameters {
    h_displayName :: Maybe Text
  , h_description :: Maybe Text
  , h_type :: Maybe Text
  , h_enum :: [Text]
  , h_pattern :: Maybe Text
  , h_minLength :: Maybe Int
  , h_maxLength :: Maybe Int
  , h_minimum :: Maybe Int
  , h_maximum :: Maybe Int
  , h_example :: Maybe Text
  , h_repeat :: Maybe Bool
  , h_required :: Maybe Bool
  , h_default :: Maybe Text
  } deriving (Show,Eq,Ord)

instance Default RamlNamedParameters where
  def = RamlNamedParameters {
    h_displayName = Nothing
  , h_description = Nothing
  , h_type = Nothing
  , h_enum = []
  , h_pattern = Nothing
  , h_minLength = Nothing
  , h_maxLength = Nothing
  , h_minimum = Nothing
  , h_maximum = Nothing
  , h_example = Nothing
  , h_repeat = Nothing
  , h_required = Nothing
  , h_default = Nothing
  }

data RamlRequestBody =
  RamlRequestBody {
    req_formParameters :: Map QueryParameter RamlNamedParameters
  , req_schema :: Maybe Schema
  , req_example :: Maybe Text
  } deriving (Show,Eq,Ord)

instance Default RamlRequestBody where
  def = RamlRequestBody {
    req_formParameters = M.empty
  , req_schema = Nothing
  , req_example = Nothing
  }

data RamlMethod =
  RamlMethod {
    m_responses :: Map ResponseCode RamlResponse
  , m_description :: Maybe Text
  , m_headers :: Map HeaderKey RamlNamedParameters
  , m_securedBy :: [Auth]
  , m_protocols :: [Protocol]
  , m_queryParameters :: Map QueryParameter RamlNamedParameters
  , m_body :: Map MediaType RamlRequestBody
  , m_is :: [TraitKey]
  } deriving (Show,Eq,Ord)

instance Default RamlMethod where
  def = RamlMethod {
    m_responses = M.empty
  , m_description = Nothing
  , m_headers = M.empty
  , m_securedBy = []
  , m_protocols = []
  , m_queryParameters = M.empty
  , m_body = M.empty
  , m_is = []
  }

data RamlResourceType =
  RamlResourceType {
    rt_usage :: Maybe Text
  , rt_displayName :: Maybe Text
  , rt_description :: Maybe Text
  , rt_methods :: Map Method RamlMethod
  , rt_paths :: Map Path RamlResource
  , rt_uriParameters :: Map UriParameter RamlNamedParameters
  , rt_baseUriParameters :: Map UriParameter RamlNamedParameters
  } deriving (Show,Eq,Ord)

instance Default RamlResourceType where
  def = RamlResourceType {
    rt_usage = Nothing
  , rt_displayName = Nothing
  , rt_description = Nothing
  , rt_methods = M.empty
  , rt_paths = M.empty
  , rt_uriParameters = M.empty
  , rt_baseUriParameters = M.empty
  }

data RamlTrait =
  RamlTrait {
    t_usage :: Maybe Text
  , t_responses :: Map ResponseCode RamlResponse
  , t_description :: Maybe Text
  , t_headers :: Map HeaderKey RamlNamedParameters
  , t_securedBy :: [Auth]
  , t_protocols :: [Protocol]
  , t_queryParameters :: Map QueryParameter RamlNamedParameters
  , t_body :: Map MediaType RamlRequestBody
  } deriving (Show,Eq,Ord)

instance Default RamlTrait where
  def = RamlTrait {
    t_usage = Nothing
  , t_responses = M.empty
  , t_description = Nothing
  , t_headers = M.empty
  , t_securedBy = []
  , t_protocols = []
  , t_queryParameters = M.empty
  , t_body = M.empty
  }

data RamlResource =
  RamlResource {
    r_displayName :: Maybe Text
  , r_description :: Maybe Text
  , r_handler :: Maybe Handler  -- ^ This is used for Yesod Route.
  , r_methods :: Map Method RamlMethod
  , r_paths :: Map Path RamlResource
  , r_uriParameters :: Map UriParameter RamlNamedParameters
  , r_baseUriParameters :: Map UriParameter RamlNamedParameters
  , r_type :: Maybe ResourceTypeKey
  , r_is :: [TraitKey]
  } deriving (Show,Eq,Ord)

instance Default RamlResource where
  def = RamlResource {
    r_displayName = Nothing
  , r_description = Nothing
  , r_handler = Nothing
  , r_methods = M.empty
  , r_paths = M.empty
  , r_uriParameters = M.empty
  , r_baseUriParameters = M.empty
  , r_type = Nothing
  , r_is = []
  }

data RamlDocumentation = 
  RamlDocumentation {
    doc_title :: Text
  , doc_content :: Text
  } deriving (Show,Eq,Ord)

data RamlSecuritySchemes =
  RamlSecuritySchemes {
    ss_description :: Maybe Text
  , ss_type :: Maybe Text
  , ss_describedBy :: Maybe Text
  , ss_settings :: Maybe Text
  } deriving (Show,Eq,Ord)

instance Default RamlSecuritySchemes where
  def = RamlSecuritySchemes {
    ss_description = Nothing
  , ss_type = Nothing
  , ss_describedBy = Nothing
  , ss_settings = Nothing
  }

data Raml =
  Raml {
    title :: Text
  , version :: Text
  , baseUri :: Text
  , baseUriParameters :: Map UriParameter RamlNamedParameters
  , protocols :: [Protocol]
  , mediaType :: Maybe MediaType
  , schemas :: [Map SchemaKey Schema]
  , uriParameters :: Map UriParameter RamlNamedParameters
  , documentation :: [RamlDocumentation]
  , paths :: Map Path RamlResource
  , securitySchemes :: [Map Auth RamlSecuritySchemes]
  , resourceTypes :: [Map ResourceTypeKey RamlResourceType]
  , traits :: [Map TraitKey RamlTrait]
  } deriving (Show,Eq,Ord)

instance Monoid RamlTrait where
  mempty = RamlTrait {
    t_usage = Nothing
  , t_responses = M.empty
  , t_description = Nothing
  , t_headers = M.empty
  , t_securedBy = []
  , t_protocols = []
  , t_queryParameters = M.empty
  , t_body = M.empty
  } 
  mappend a b = RamlTrait {
    t_usage = t_usage a <> t_usage b
  , t_responses = t_responses a <> t_responses b
  , t_description = t_description a <> t_description b
  , t_headers = t_headers a <> t_headers b
  , t_securedBy = t_securedBy a <> t_securedBy b
  , t_protocols = t_protocols a <> t_protocols b
  , t_queryParameters = t_queryParameters a <> t_queryParameters b
  , t_body = t_body a <> t_body b
  } 

data MethodEx =
  MethodEx {
    me_method ::  String  -- ^ Uppercase method name
  , me_example ::  Maybe (ContentType,Text)  -- ^ This is used for output of mock.
  } deriving (Show)

data RouteEx =
  RouteEx {
    re_pieces ::  [Piece String]
  , re_handler ::  String
  , re_methods ::  [MethodEx]
  } deriving (Show)


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Raml.Docs(
  htmlFromRaml
, parseRamlDocsFile
) where

import Yesod.Raml.Parser
import Yesod.Raml.Type
import Text.Hamlet
import Yesod.Markdown
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Yaml.Include as YI
import qualified Data.Yaml as Y
import Data.Monoid
import Language.Haskell.TH.Syntax

import qualified Paths_yesod_raml_docs as D
import Data.Version (showVersion)

yesodRamlDocsVersion :: String
yesodRamlDocsVersion = showVersion D.version

txt2md :: T.Text -> Html
txt2md txt = 
  case (markdownToHtml (Markdown txt)) of
    Left msg -> error $ show msg
    Right md -> md

findAuth :: Auth -> Raml -> Maybe RamlSecuritySchemes
findAuth auth raml = M.lookup auth (foldr1 (<>) (securitySchemes raml))

haveMethodParam :: RamlMethod -> Bool
haveMethodParam method = not ((M.null (m_queryParameters method)) && (M.null (m_headers method)) && (M.null (m_body method)))

haveResourceParam :: RamlResource -> Bool
haveResourceParam resource = not ((M.null (r_uriParameters resource)) && (M.null (r_baseUriParameters resource)))

haveParam :: RamlResource -> RamlMethod -> Bool
haveParam resource method = haveMethodParam method || haveResourceParam resource

haveMethod :: RamlResource -> Bool
haveMethod resource = (not  (M.null (r_methods resource)))

scriptBody :: Html
scriptBody = $(shamletFile "templates/script.hamlet")

styleBody :: Html
styleBody = $(shamletFile "templates/style.hamlet")

htmlFromRaml :: Raml -> Html
htmlFromRaml raml = docs $ genUriParamDescription $ applyTrait $ applyResourceType $ applyVersion raml

docs :: Raml -> Html
docs raml = $(shamletFile "templates/docs.hamlet")

resources :: Raml -> RamlResource -> T.Text -> T.Text -> Html
resources raml res puri ruri = $(shamletFile "templates/resource.hamlet")

items :: T.Text -> RamlNamedParameters -> Html
items key item = $(shamletFile "templates/item.hamlet")

docId :: T.Text -> T.Text 
docId uri =
    T.replace "{" "_"
  $ T.replace "}" "_"
  $ T.replace " " "_"
  $ T.replace "." "_"
  $ T.replace "/" "_" uri


parseRamlDocsFile ::  FilePath -> Q Exp
parseRamlDocsFile file = do
  qAddDependentFile file
  raml <- qRunIO $ do
    eRaml <- YI.decodeFileEither file ::  IO (Either Y.ParseException Raml)
    case eRaml of
          Right v -> return  v
          Left e -> error $ "Invalid raml :" ++ show e
  [|htmlFromRaml raml|]
    

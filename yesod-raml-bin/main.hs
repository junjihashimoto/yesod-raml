{-# LANGUAGE OverloadedStrings #-}

import Yesod.Raml.Type
import Yesod.Raml.Routes
import Yesod.Raml.Docs
import Yesod.Raml.Mock
import qualified Data.Yaml as Y
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html.Renderer.Text
import Options.Applicative

import Yesod.Routes.TH.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.Maybe

data Command =
    ToRoute FilePath FilePath
  | ToHtml FilePath FilePath
  | ToMock FilePath FilePath
  | MockServer Port FilePath
  | Verify FilePath

toroute :: Parser Command
toroute = ToRoute
         <$> (argument str (metavar "RamlFile"))
         <*> (argument str (metavar "RouteFile"))

tohtml :: Parser Command
tohtml = ToHtml
         <$> (argument str (metavar "RamlFile"))
         <*> (argument str (metavar "HtmlFile"))

tomock :: Parser Command
tomock = ToMock
         <$> (argument str (metavar "RamlFile"))
         <*> (argument str (metavar "MockFile"))

mockServer :: Parser Command
mockServer = MockServer
         <$> option auto (long "port"
                          <> short 'p'
                          <> value 3000
                          <> metavar "port")
         <*> (argument str (metavar "RamlFile"))

parse :: Parser Command
parse = subparser $ 
        command "toroute" (info toroute (progDesc "convert raml-file to route-file")) <>
        command "tohtml" (info tohtml (progDesc "convert raml-file to html-file")) <>
        command "tomock" (info tomock (progDesc "convert raml-file to mock-file")) <>
        command "mock-server" (info mockServer (progDesc "run mock-server")) <>
        command "verify" (info verify (progDesc "verify raml-file"))

verify :: Parser Command
verify = Verify <$> (argument str (metavar "RamlFile"))


runCmd :: Command -> IO ()
runCmd (ToRoute ifile ofile) = do
  v <- Y.decodeFileEither ifile :: IO (Either Y.ParseException Raml)
  case v of
    Left e -> print (show e)
    Right raml' ->  do
      case routesFromRaml raml' of
        Left e -> print (show e)
        Right routes ->  T.writeFile ofile (T.fromStrict (toYesodRoutes routes))

runCmd (ToHtml ifile ofile) = do
  v <- Y.decodeFileEither ifile :: IO (Either Y.ParseException Raml)
  case v of
    Left e -> print (show e)
    Right raml' -> T.writeFile ofile (renderHtml (htmlFromRaml raml'))

runCmd (ToMock ifile ofile) = do
  v <- Y.decodeFileEither ifile :: IO (Either Y.ParseException Raml)
  case v of
    Left e -> print (show e)
    Right raml' ->  do
      mockSrcFromRaml raml' >>= writeFile ofile

runCmd (Verify file) = do
  v <- Y.decodeFileEither file :: IO (Either Y.ParseException Raml)
  print v
      
runCmd (MockServer port ifile) = do
  v <- Y.decodeFileEither ifile :: IO (Either Y.ParseException Raml)
  case v of
    Left e -> print (show e)
    Right raml' ->  do
      case routesFromRaml raml' of
        Left e' ->  print (show e')
        Right routes ->  run port (app routes)

comparePath ::  [Piece String] ->  [TS.Text] ->  Bool
comparePath [] [] = True
comparePath [] (_:_) = False
comparePath (_:_)  [] = False
comparePath (Static str:xs) (str':xs') = if (TS.pack str) == str' then comparePath xs xs' else False
comparePath (Dynamic _:xs)  (_:xs') = comparePath xs xs'


app ::  [RouteEx] ->  Application
app routes req respond = do
  let method = requestMethod req
      paths = pathInfo req
      mexample = do
        r <-  listToMaybe $ filter (\r ->  comparePath (re_pieces r) paths)  $ routes
        methodex <-  listToMaybe $ filter  (\m ->  me_method m ==  BC.unpack method ) $ re_methods r
        me_example methodex
  case mexample of
    Nothing ->  respond $ responseLBS status400 [] ""
    Just (ctype,val) ->  respond $ responseLBS status200 [(hContentType,TS.encodeUtf8 ctype)] $ BL.fromStrict $ TS.encodeUtf8 val

opts :: ParserInfo Command
opts = info (parse <**> helper) idm
  
main :: IO ()
main = execParser opts >>= runCmd
  

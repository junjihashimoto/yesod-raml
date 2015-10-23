import Yesod.Raml.Type
import Yesod.Raml.Routes
import Yesod.Raml.Docs
import qualified Data.Yaml as Y
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text

import Options.Applicative

data Command =
    ToRoute FilePath FilePath
  | ToHtml FilePath FilePath
  | Verify FilePath

toroute :: Parser Command
toroute = ToRoute
         <$> (argument str (metavar "RamlFile"))
         <*> (argument str (metavar "RouteFile"))

tohtml :: Parser Command
tohtml = ToHtml
         <$> (argument str (metavar "RamlFile"))
         <*> (argument str (metavar "HtmlFile"))

parse :: Parser Command
parse = subparser $ 
        command "toroute" (info toroute (progDesc "convert raml-file to route-file")) <>
        command "tohtml" (info tohtml (progDesc "convert raml-file to html-file")) <>
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

runCmd (Verify file) = do
  v <- Y.decodeFileEither file :: IO (Either Y.ParseException Raml)
  print v
      

opts :: ParserInfo Command
opts = info (parse <**> helper) idm
  
main :: IO ()
main = execParser opts >>= runCmd
  

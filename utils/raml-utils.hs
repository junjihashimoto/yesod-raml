import Yesod.Raml.Type
import Yesod.Raml.Parser
import qualified Data.Yaml as Y

import Options.Applicative

data Command =
    Verify FilePath

verify :: Parser Command
verify = Verify <$> (argument str (metavar "RamlFile"))

parse :: Parser Command
parse = subparser $ 
        command "verify"      (info verify (progDesc "verify raml-file"))

runCmd :: Command -> IO ()
runCmd (Verify file) = do
  v <- Y.decodeFile file :: IO (Maybe Raml)
  print v

opts :: ParserInfo Command
opts = info (parse <**> helper) idm
  
main :: IO ()
main = execParser opts >>= runCmd
  

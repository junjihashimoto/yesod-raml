module Handler.Board where

import Import
import Yesod.Raml.Parser
import Yesod.Raml.Docs
import Yesod.Raml.Mock
import Data.FileEmbed(embedFile)

getHelpR ::  Handler Html
getHelpR = return  $(parseRamlDocsFile "config/routes.raml")

getRamlR ::  Handler TypedContent
getRamlR = do
  let content = toContent $(embedFile "config/routes.raml")
  return $ TypedContent "application/raml+yaml" content

$(parseRamlMockFile  "config/routes.raml")

-- postInitR ::  Handler TypedContent
-- postInitR = return $ TypedContent "appliation/json" "{hogehoge:222}"

-- getBoardR ::  Session ->  Handler Value
-- getBoardR _ = error "not implemented"

-- postDiskR :: Session ->  XPos ->  YPos ->  Handler Value
-- postDiskR _ _ _ = error "not implemented"

-- getUserR ::  Handler Value
-- getUserR =  error "not implemented"

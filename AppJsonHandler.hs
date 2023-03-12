{-# LANGUAGE OverloadedStrings #-}
{- | Sample Haskell Web Application, module to handle
 -   application/json content type requests.
 -
 -   Based on WAI, implemented with the WARP web server.
 -   See https://www.yesodweb.com/book/web-application-interface
 -   See https://www.stackage.org/package/wai
 -} 

module AppJsonHandler where

import Network.Wai
import Network.HTTP.Types (Header, status200, status404, status500)
import Network.HTTP.Types.Header (Header)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

ctAppJson :: Network.HTTP.Types.Header.Header
ctAppJson = ("Content-Type", "application/json; charset=utf-8")

{- | Lets the visitor / user know that the page they requested is 
 -   not known. The ability exists here to separate responses 
 -   between content types.
 -}
give404 unknownPath = responseBuilder 
                      status404 
                      [ ctAppJson ] 
                      $ mconcat $ map copyByteString
    [ 
        "{ \"status\": 404, \"description\": \"Not Found\", \r\n"
      --note: this will break because unknownPath contains quotes.
      , "\"error\": \"No such path: "
      , BU.fromString $ show unknownPath
      , "\",\r\n"
      , "\"next\": [{\"url\": \"./\", \"title\": \"Home\"}]\r\n"
      , "}" 
    ]

{- | Return the requested static file.
 -}
static path = responseFile
              status200
              [ ]
              (drop 1 $ BU.toString path)
              Nothing

{- | The home page gets shown when the visitor / user first arrives 
 -   onto the web server and did not request any specific page.
 -}
home :: Response
home = responseFile 
       status200 
       [ ctAppJson ] 
       "index.json"
       Nothing

{- | An example response for an app/json content type. -}
hello :: Response
hello = responseBuilder
      status200 
      [ ctAppJson ]
      $ mconcat $ map copyByteString 
        [ 
            "{ \"status\": 200, \"description\": \"OK\",\r\n"
          , "\"title\": \"Hello!\",\r\n"
          , "\"data\": \"Successfully detected the application/json accept request header.\",\r\n"
          , "\"next\": [{\"url\": \"./\", \"title\": \"Home\"}]\r\n"
          , "}" 
        ]


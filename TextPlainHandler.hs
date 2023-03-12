{-# LANGUAGE OverloadedStrings #-}
{- | Sample Haskell Web Application 
 -   Based on WAI, implemented with the WARP web server.
 -   See https://www.yesodweb.com/book/web-application-interface
 -   See https://www.stackage.org/package/wai
 -} 
module TextPlainHandler where

import Network.Wai
import Network.HTTP.Types (hAcceptLanguage, Header, HeaderName, status200, status404, status500)
import Network.HTTP.Types.Header (Header)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

ctTextPlain :: Network.HTTP.Types.Header.Header
ctTextPlain = ("Content-Type", "text/plain; charset=utf-8")

{- | Lets the visitor / user know that the page they requested is 
 -   not known. The ability exists here to separate responses 
 -   between content types.
 -   Note that this does not use any kind of HTML templating. Yet.
 -}
give404 unknownPath = responseBuilder 
                      status404 
                      [ ctTextPlain ]
                      $ mconcat $ Prelude.map copyByteString
    [ 
        "======================================================\r\n"
      , "404 - Not Found\r\n"
      , "------------------------------------------------------\r\n"
      , "No such path: ", BU.fromString $ show unknownPath, ".\r\n"
      , "------------------------------------------------------\r\n"
      , "Go back home.\r\n"
      , "======================================================\r\n"
    ]

{- | In the most basic of set-ups, a static file is attempted
 -   to be returned.
 -   This will fail if that file does not exist.
 -}
static path = responseFile
              status200
              [ ]
              (drop 1 $ BU.toString path)
              Nothing

{- | The home page gets shown when the visitor / user first arrives 
 -   onto the web server and did not request any specific page.
 -   Note that this does not use any kind of HTML templating. Yet.
 -}
home :: Response
home = responseFile 
       status200 
       [ ctTextPlain ]
       "index.txt"
       Nothing



{- | An example response for a text/plain content type. -}
hello :: Response
hello = responseBuilder
      status200 
      [ ctTextPlain ]
      (copyByteString $ BU.fromString "hello")


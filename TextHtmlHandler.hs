{-# LANGUAGE OverloadedStrings #-}
{- | Text/Html response creator for Sample Haskell Web App. 
 -   Based on WAI, implemented with the WARP web server.
 -   See https://www.yesodweb.com/book/web-application-interface
 -   See https://www.stackage.org/package/wai
 -} 

module TextHtmlHandler where

import Network.Wai
import Network.HTTP.Types (Header, hAccept, hAcceptLanguage, HeaderName, status200, status404, status500)
import Network.HTTP.Types.Header (Header)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

ctTextHtml :: Network.HTTP.Types.Header.Header
ctTextHtml = ("Content-Type", "text/html")

{- | Lets the visitor / user know that the page they requested is 
 -   not known. The ability exists here to separate responses 
 -   between content types.
 -   Note that this does not use any kind of HTML templating. Yet.
 -}
give404 unknownPath = responseBuilder 
                      status404 
                      [ ctTextHtml ] 
                      $ mconcat $ map copyByteString
    [ "<!DOCTYPE html><html><head><title>404 - Not Found</title><meta charset='utf-8'/>"
    , "<link rel='stylesheet' media='all' href='/all.css'>"
    , "</head><body>"
    , "<header><hr/><h1>404 - Not Found</h1><hr/></header>"
    , "<p>No such path: ", BU.fromString $ show unknownPath, "</p>"
    , "<p>Go <a href='/'>Home</a></p>"
    , "<hr/></body></html>" ]

{- | Return the requested static file, if possible.
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
       [ ctTextHtml ] 
       "index.html"
       Nothing

{- | An example response for a valid path -}
hello :: Response
hello = responseBuilder
      status200
      [ ctTextHtml ] 
      $ mconcat $ map copyByteString 
      [ 
          "<!DOCTYPE html><html><head><title>Hello!</title><meta charset='utf-8'/>"
        , "<link rel='stylesheet' media='all' href='/all.css'>"
        , "</head><body>"
        , "<header><hr/><h1>Hello!</h1><hr/></header>"
        , "<p>Successfully detected the text/html accept request header.</p>"
        , "<p>Go <a href='/'>Home</a></p>"
        , "<hr/></body></html>"
      ]


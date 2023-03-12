{-# LANGUAGE OverloadedStrings #-}
{- | Sample Haskell Web Application 
 -   Based on WAI, implemented with the WARP web server.
 -   See https://www.yesodweb.com/book/web-application-interface
 -   See https://www.stackage.org/package/wai
 -} 
import Network.Wai
import Network.Wai.Handler.Warp (run, setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Log
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.HTTP.Types (hAccept, hAcceptLanguage, HeaderName, status200, status404, status500)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.List

import qualified HeaderParser as HP
import qualified TextHtmlHandler
import qualified TextPlainHandler
import qualified AppJsonHandler

{- | Entry point of the application. Defines and logs on which 
 -   port to listen, and starts the web server. 
 -}
main :: IO ()
main = do
    -- Probably a good idea later on to read the port from input.
    let port = 8080 
    putStrLn $ "Listening on port " ++ show port
    -- Yes, we want to gzip our output if the client allows it.
    -- Choice 1: run over http, no encryption.
    -- run port $ gzip def router
    -- Choice 2: run over https, with TLS encryption
    runTLS tlsOpts warpOpts router where
      tlsOpts = tlsSettings "public-cert.pem" "secret-key.pem"
      warpOpts = setPort 8080 defaultSettings


{- | Determines which web page to show. By default, the home page 
 -   gets shown. A 404 page gets shown of unknown requests.
 -   We inspect the request content-type to make sure we respond 
 -   with an appropriate type, too.
 -   The fallback handler is plaintext, which routes unknowns 
 -   into a static file retriever.
 -}
router :: Application
router request response = result where 
    accepts = HP.getAcceptRequestHeader (requestHeaders request)
    handler = case (HP.containsCtAppJson accepts) of
        True -> appJsonRouter 
        False -> case (HP.containsCtTextHtml accepts) of 
                 True -> htmlTextRouter
                 False -> plainTextRouter
    result = handler request response

{- | Routes specifically for text/plain content-type requests.
 -   Unknown requests are sent to a static file handler, 
 -   in the hopes that the requested path exists. Otherwise,
 -   WARP will return a 404 not-found.
 -}
plainTextRouter :: Application
plainTextRouter request response = response $
    case rawPathInfo request of
    "/" -> TextPlainHandler.home
    "/hello" -> TextPlainHandler.hello
    unknown -> TextPlainHandler.static unknown

{- | Routes specifically for text/html content-type requests.
 -   Unknown requests are sent to a static file handler, 
 -   in the hopes that the requested path exists. Otherwise,
 -   WARP will return a 404 not-found.
 -
 -   Note: this example routes the same paths for HTML as 
 -   for plaintext. That is not necessary in a real-life
 -   application. We would guess that there is rarely, if 
 -   ever, a web application that duplicates all its files
 -   across various content types.
 -}
htmlTextRouter :: Application
htmlTextRouter request response = response $
    case rawPathInfo request of
    "/" -> TextHtmlHandler.home
    "/hello" -> TextHtmlHandler.hello
    unknown -> TextHtmlHandler.static unknown


{- | Routes specifically for app/json content-type requests.
 -   Unknown requests are sent to a static file handler, 
 -   in the hopes that the requested path exists. Otherwise,
 -   WARP will return a 404 not-found.
 -
 -   Note: this example routes the same paths for json as 
 -   for plaintext. That is not necessary in a real-life
 -   application. 
 -}
appJsonRouter :: Application
appJsonRouter request response = response $
    case rawPathInfo request of
    "/" -> AppJsonHandler.home
    "/hello" -> AppJsonHandler.hello
    unknown -> AppJsonHandler.static unknown

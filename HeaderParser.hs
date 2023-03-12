{-# LANGUAGE OverloadedStrings #-}
{- | Sample Haskell Web Application 
 -   Based on WAI, implemented with the WARP web server.
 -   See https://www.yesodweb.com/book/web-application-interface
 -   See https://www.stackage.org/package/wai
 -} 

module HeaderParser (
           getAcceptRequestHeader
         , containsCtTextHtml
         , containsCtAppJson
       ) where

import Network.Wai
import Network.HTTP.Types (hAccept, hAcceptLanguage, HeaderName)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.List


{- | Find the Accept request header.
 -   Use this on the result of (requestHeaders request).
 -   Then apply one of the containsCt... functions to its result.
 -}
getAcceptRequestHeader :: [(HeaderName, BU.ByteString)] -> BU.ByteString
getAcceptRequestHeader headers = contents where 
       contents = case hasAcceptHeader of 
                  False -> BU.fromString ""
                  True -> snd (head filtered)
       -- should not use length on a unknown length list!
       hasAcceptHeader = 0 /= length filtered
       filtered = filter isAcceptHeader headers
       isAcceptHeader (name, _) = hAccept == name
 
containsCtAppJson :: BU.ByteString -> Bool
containsCtAppJson x = any (== "application/json") xs || any (== ",application/json") xs where
  xs = (Data.List.groupBy (\r s -> s /= ',') (BU.toString x))

{- | Whether the accept request header contains the content type 
 -   "text/html".
 -   Use this on the result of getAcceptRequestHeader.
 -}
containsCtTextHtml :: BU.ByteString -> Bool
containsCtTextHtml x = any (== "text/html") xs || any (== ",text/html") xs where
  xs = (Data.List.groupBy (\r s -> s /= ',') (BU.toString x))


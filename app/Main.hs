{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (fromString)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)

import Network.HTTP.Types.Header(hAcceptLanguage)
import Network.Wai.Middleware.Rewrite(rewritePure)
import Network.Wai.Parse(parseHttpAccept)
import Data.Text.Encoding(decodeLatin1)
import Data.Maybe(fromMaybe)
import Data.List(find)
import qualified Data.ByteString as BS
import Data.Word8(_hyphen)

main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ (rewriteByAcceptLanguage ["en","ja"]) $ staticHttpApp



rewriteByAcceptLanguage :: [BS.ByteString] -> Wai.Middleware
rewriteByAcceptLanguage prepared = rewritePure trans
  where
    trans path headers =
      let path' = do
            header <- lookup hAcceptLanguage headers
            let requested = map takeWhileNotHyphen $ parseHttpAccept header
            language <- find (\ca -> elem ca prepared) requested
            return $ decodeLatin1 language : path
      in fromMaybe path path'
    takeWhileNotHyphen = BS.takeWhile (_hyphen /=)



staticHttpApp :: Wai.Application
staticHttpApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["index.htm"] -- default content


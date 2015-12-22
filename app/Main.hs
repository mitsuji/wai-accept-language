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
import qualified Network.HTTP.Types.Header as H

import Network.Wai.Middleware.Rewrite(rewritePure)
import Network.Wai.Parse(parseHttpAccept)
import Data.Text.Encoding(decodeLatin1)
import Data.Maybe(listToMaybe)
import Debug.Trace(trace)

main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ rewriteByAcceptLanguage $ staticHttpApp



rewriteByAcceptLanguage :: Wai.Middleware
rewriteByAcceptLanguage = rewritePure f
  where
    f path headers = case alHeader of
      Nothing -> path
      Just header -> case listToMaybe $ parseHttpAccept header of
        Nothing -> path
        Just lang -> decodeLatin1 lang : path
      where
        alHeader = lookup H.hAcceptLanguage headers

staticHttpApp :: Wai.Application
staticHttpApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["index.htm"] -- default content

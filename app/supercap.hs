{- |
File        : supercap.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Serve a music library via HTTP.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Config                        (Config (..))
import           Playlist                      (makeAlbumPlaylist, showXspfBS)

import           Network.HTTP.Types.Status     (notFound404)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text                     as T
import           System.Environment            (getArgs)


type Api = SpockM () () () ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    [rootPath, rootUrl] -> do
      let cfg = Config { musicRootPath = rootPath, musicRootUrl = rootUrl }
      spockCfg <- defaultSpockCfg () PCNoDatabase ()
      runSpock 8000 (spock spockCfg (app cfg))
    _ -> error "Usage: supercap <music file root dir> <URL root>"

app :: Config -> Api
app cfg = do
  -- TODO: add prefix for static files
  middleware $ staticPolicy (addBase (musicRootPath cfg))

  get ("playlist" <//> var <//> var) $ \artist album -> do
    pl <- liftIO $ makeAlbumPlaylist cfg artist album
    case pl of
      Right pl' -> do
        setHeader "Content-Type" "application/xspf+xml; charset=utf-8"
        bytes $ showXspfBS cfg pl'
      Left err -> do
        setStatus notFound404
        -- TODO: proper error message
        text $ T.pack $ show err

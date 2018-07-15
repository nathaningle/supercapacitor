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

import           Config                         (Config (..))
import           Playlist                       (Playlist, PlaylistError (..),
                                                 makeAlbumPlaylist, showXspfBS)
import           PlaylistList                   (listAlbums, listArtists)
import           Web                            (albumListPage, artistListPage,
                                                 playlistErrorPage)

import           Network.HTTP.Types             (RequestHeaders, notFound404)
import           Network.Wai.Middleware.Rewrite (PathsAndQueries,
                                                 rewritePureWithQueries)
import           Network.Wai.Middleware.Static  (addBase, staticPolicy)
import           Web.Spock
import           Web.Spock.Config               (PoolOrConn (..),
                                                 defaultSpockCfg)
import           Web.Spock.Lucid                (lucid)


import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           System.Environment             (getArgs)


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
  -- Serve the music files.
  middleware $ rewritePureWithQueries tracksRewrite . staticPolicy (addBase (musicRootPath cfg))

  get ("playlist" <//> var <//> var) $ \artist album -> do
    plResult <- liftIO $ makeAlbumPlaylist cfg artist album
    either playlist404 (playlist cfg) plResult

  get ("playlist" <//> "supercap.css") $
    file "text/css; charset=utf-8" "static/supercap.css"

  get ("playlist" <//> var) $ \artist -> do
    albums <- liftIO $ listAlbums cfg artist
    either playlist404 (lucid . albumListPage artist) albums

  get "playlist" $ do
    artists <- liftIO $ listArtists cfg
    either playlist404 (lucid . artistListPage) artists

-- | Strip the prefix from requests for music files.
tracksRewrite :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
tracksRewrite ("tracks":ps, q) _ = (ps, q)
tracksRewrite psAndQs          _ = psAndQs


-- | Send a 'Playlist' as an XSPF file.
playlist :: MonadIO m => Config -> Playlist -> ActionCtxT ctx m a
playlist cfg pl = do
  setHeader "Content-Type" "application/xspf+xml; charset=utf-8"
  bytes $ showXspfBS cfg pl

-- | TODO: is 404 always appropriate?
playlist404 :: MonadIO m => PlaylistError -> ActionCtxT ctx m a
playlist404 err = do
  setStatus notFound404
  lucid $ playlistErrorPage err

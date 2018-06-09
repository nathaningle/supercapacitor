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
import           Playlist                      (Album, Artist, Playlist,
                                                PlaylistError (..),
                                                makeAlbumPlaylist, showXspfBS)
import           PlaylistList                  (listAlbums, listArtists)

import           Lucid
import           Network.HTTP.Types.Status     (notFound404)
import qualified Network.URI.Encode            as URI
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Web.Spock
import           Web.Spock.Config              (PoolOrConn (..),
                                                defaultSpockCfg)
import           Web.Spock.Lucid               (lucid)


import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           System.Environment            (getArgs)
import           System.FilePath               ((</>))


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
    plResult <- liftIO $ makeAlbumPlaylist cfg artist album
    either playlist404 (playlist cfg) plResult

  get ("playlist" <//> var) $ \artist -> do
    albums <- liftIO $ listAlbums cfg artist
    either playlist404 (lucid . albumListPage artist) albums

  get "playlist" $ do
    artists <- liftIO $ listArtists cfg
    either playlist404 (lucid . artistListPage) artists


-- | Send a 'Playlist' as an XSPF file.
playlist :: MonadIO m => Config -> Playlist -> ActionCtxT ctx m a
playlist cfg pl = do
  setHeader "Content-Type" "application/xspf+xml; charset=utf-8"
  bytes $ showXspfBS cfg pl

-- | TODO: present a proper error message.
playlist404 :: MonadIO m => PlaylistError -> ActionCtxT ctx m a
playlist404 err = do
  setStatus notFound404
  text $ T.pack $ show err

-- | Display a list of available artists.
artistListPage :: [Artist] -> Html ()
artistListPage artists = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ label
  body_ $ do
    h1_ label
    ul_ $ mapM_ (\artist -> li_ (a_ [href_ (href artist)] (toHtml artist))) artists
  where
    href artist = T.pack $ "/playlist" </> URI.encode artist
    label = "Artists"

-- | Display a list of available albums for the given artist.
albumListPage :: Artist -> [Album] -> Html ()
albumListPage artist albums = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ label
  body_ $ do
    p_ $ a_ [href_ "/playlist"] "Back to album list"
    h1_ label
    ul_ $ mapM_ (\album -> li_ (a_ [href_ (href album)] (toHtml album))) albums
  where
    href album = T.pack $ "/playlist" </> URI.encode artist </> URI.encode album
    label = "Albums by " <> toHtml artist

{- |
Module      : PlaylistList
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Describe available playlists.
-}
{-# LANGUAGE RecordWildCards #-}
module PlaylistList where

import           Config           (Config (..))
import           Playlist         (Album, Artist, PlaylistError (..))

import           Control.Monad    (filterM)
import           Data.List        (sort)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  ((</>))


-- | List directories immediately below @path@.
listDirs :: FilePath -> IO (Either PlaylistError [FilePath])
listDirs path = do
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      files <- listDirectory path
      let files' = zip files $ map (path </>) files
      Right . sort . map fst <$> filterM (doesDirectoryExist . snd) files'
    else pure $ Left $ NonexistentDir path

-- | List the available artists.
listArtists :: Config -> IO (Either PlaylistError [Artist])
listArtists Config{..} = listDirs musicRootPath

-- | List the available albums by a given artist.
listAlbums :: Config -> Artist -> IO (Either PlaylistError [Album])
listAlbums Config{..} artist = listDirs $ musicRootPath </> artist

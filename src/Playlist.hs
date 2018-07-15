{- |
Module      : Playlist
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A list of music tracks with metadata.
-}
{-# LANGUAGE RecordWildCards #-}
module Playlist where

import           Config                (Config (..))
import           Track                 (Track (..), TrackError (..),
                                        readTrackFile, toXML)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Either           (partitionEithers)
import           Data.List             (sortOn, stripPrefix)
import qualified Network.URI.Encode    as URI
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.FilePath       (addTrailingPathSeparator, joinPath,
                                        splitDirectories, (</>))

import           Text.XML.Light


type Playlist = [Track]

type Artist = String

type Album = String

-- | Things that can go wrong when generating a 'Playlist'.
data PlaylistError = TrackErrors [TrackError]
                   | NonexistentDir FilePath
                   deriving Show


-- | Make a playlist containing all files that are the immediate contents of a
-- directory.  Note that this does not check that the files are music files,
-- or even that they are regular files — we leave that to 'readTrackFile'.
playlistDir :: FilePath -> IO (Either PlaylistError Playlist)
playlistDir path = do
  files <- listDirectory path
  tracks <- mapM (readTrackFile . (path </>)) files
  pure $ case partitionEithers tracks of
    ([], ts) -> Right $ sortOn trkTrackNum ts
    (es, _ ) -> Left  $ TrackErrors es


-- | Generate a 'Playlist' for a given artist and album.  This assumes that
-- in 'musicRootPath' there exists a directory with the same name as the
-- artist, and in that directory there exists a directory with the same name as
-- the album.
makeAlbumPlaylist :: Config -> Artist -> Album -> IO (Either PlaylistError Playlist)
makeAlbumPlaylist Config{..} artist album = do
  albumDirExists <- doesDirectoryExist albumDirPath
  if albumDirExists
    then playlistDir albumDirPath
    else pure $ Left (NonexistentDir albumDirPath)
  where
    albumDirPath = musicRootPath </> artist </> album


-- | Convert a 'Playlist' to its XSPF representation.
toXspf :: Playlist -> Element
toXspf tracks = Element { elName    = blank_name { qName = "playlist" }
                        , elAttribs = [verAttr, uriAttr]
                        , elContent = [Elem trackList]
                        , elLine    = Nothing
                        }
  where
    verAttr = Attr { attrKey = blank_name { qName = "version" }
                   , attrVal = "1"
                   }
    uriAttr = Attr { attrKey = blank_name { qName = "xmlns" }
                   , attrVal = "http://xspf.org/ns/0/"
                   }
    trackList = blank_element { elName    = blank_name { qName = "trackList" }
                              , elContent = map (Elem . toXML) tracks
                              }

-- | Textual XSPF representation of a 'Playlist'.  Helper for serving via HTTP.
showXspfBS :: Config -> Playlist -> ByteString
showXspfBS cfg tracks = BS.pack $ xmlHeaderUtf8 ++ showElement (toXspfRemote cfg tracks)

-- | The header our XSPF will use.  This differs from that in 'Text.XML.Light'
-- by specifiying the encoding.
xmlHeaderUtf8 :: String
xmlHeaderUtf8 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

-- | Convert a path to a file in the local filesystem to the URL at which we'll
-- serve the file.
pathLocalToRemote :: Config -> FilePath -> Maybe String
pathLocalToRemote Config{..} = fmap toUrlPath . stripPrefix (addTrailingPathSeparator musicRootPath)
  where
    toUrlPath = prependRoot . joinPath . map URI.encode . splitDirectories
    prependRoot p = musicRootUrl </> "tracks" </> p

-- | Convert a 'Playlist' to its XSPF representation with URLs.
toXspfRemote :: Config -> Playlist -> Element
toXspfRemote cfg = toXspf . map updateUrl
  where
    updateUrl track = track { trkHttpUrl = pathLocalToRemote cfg (trkFilePath track) }

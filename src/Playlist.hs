{- |
Module      : Playlist
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A list of music tracks with metadata.
-}
module Playlist where

import           Track                 (Track (..), TrackError (..),
                                        readTrackFile, toXML)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (sortOn)
import           System.Directory      (listDirectory)
import           System.FilePath       ((</>))

import           Text.XML.Light


type Playlist = [Track]


-- | Make a playlist containing all files that are the immediate contents of a
-- directory.  Note that this does not check that the files are music files,
-- or even that they are regular files — we leave that to 'readTrackFile'.
playlistDir :: FilePath -> IO (Either TrackError Playlist)
playlistDir path = do
  files <- listDirectory path
  tracks <- mapM (readTrackFile . (path </>)) files
  pure $ sortOn trkTrackNum <$> sequence tracks


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

-- | Textual XSPF representation of a 'Playlist'.
showXspfBS :: Playlist -> ByteString
showXspfBS tracks = BS.pack $ xmlHeaderUtf8 ++ showElement (toXspf tracks)

-- | The header our XSPF will use.  This differs from that in 'Text.XML.Light'
-- by specifiying the encoding.
xmlHeaderUtf8 :: String
xmlHeaderUtf8 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

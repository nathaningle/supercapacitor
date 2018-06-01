{- |
Module      : Track
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Music track metadata.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Track where

import           Data.Bifunctor             (second)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           System.Exit                (ExitCode (..))
import           System.Process.Typed       (proc, readProcess)
import           Text.Read                  (readMaybe)

import           Text.XML.Light             (CData (..), Content (..),
                                             Element (..), QName (..),
                                             blank_cdata, blank_element,
                                             blank_name)


-- | Metadata describing a single music track or song.
data Track = Track { trkFilePath :: !FilePath
                   , trkDuration :: !(Maybe Int)   -- ^ milliseconds
                   , trkArtist   :: !(Maybe Text)
                   , trkAlbum    :: !(Maybe Text)
                   , trkTitle    :: !(Maybe Text)
                   , trkTrackNum :: !(Maybe Int)
                   , trkYear     :: !(Maybe Int)
                   } deriving Show


-- | Things that can go wrong when processing a 'Track'.
newtype TrackError = FfprobeProcessError String  -- ^ the @ffprobe(1)@ process exited with non-zero status
                   deriving Show


-- | Read music track metadata from a file using FFmpeg's @ffprobe(1)@, which
-- must be on the PATH.
readTrackFile :: FilePath -> IO (Either TrackError Track)
readTrackFile path = do
  (exitcode, outstr, errstr) <- readProcess cfg
  pure $ case exitcode of
    ExitSuccess   -> Right $ parseTrackProbe path outstr
    ExitFailure _ -> Left $ FfprobeProcessError (LBC.unpack errstr)
  where
    -- If you modify this, you'll probably need to also modify 'parseTrackProbe'.
    cfg = proc "ffprobe" ["-loglevel", "error", "-show_format", path]

-- | Parse @ffprobe(1)@ output.
parseTrackProbe :: FilePath -> ByteString -> Track
parseTrackProbe path str = Track { trkFilePath = path
                                 , trkDuration = secondsToMilliseconds <$> lookupRead "duration"
                                 , trkArtist   = lookupText "TAG:artist"
                                 , trkAlbum    = lookupText "TAG:album"
                                 , trkTitle    = lookupText "TAG:title"
                                 , trkTrackNum = lookupRead "TAG:track"
                                 , trkYear     = lookupRead "TAG:date"
                                 }
  where
    m = M.fromList $ mapMaybe splitEquals $ LBC.lines str
    lookupRead k = (readMaybe . LBC.unpack) =<< M.lookup k m
    lookupText k = (decodeUtf8 . LBC.toStrict) <$> M.lookup k m

-- | @ffprobe(1)@ reports duration in decimal seconds, but XSPF specifies
-- integral milliseconds.
secondsToMilliseconds :: Float -> Int
secondsToMilliseconds s = round $ s * 1000

-- | Break a string into key and value parts at the first equals sign.
splitEquals :: ByteString -> Maybe (ByteString, ByteString)
splitEquals str = splitAt' <$> LBC.elemIndex '=' str
  where
    splitAt' idx = second (LBC.dropWhile (== '=')) $ LBC.splitAt idx str


-- | Represent a 'Track' as XML suitable for inclusion in an XSPF playlist.
toXML :: Track -> Element
toXML Track{..} = blank_element { elName    = blank_name { qName = "track" }
                                , elContent = content
                                }
  where
    mkElemText t = Elem . makeXMLLeaf t . T.unpack
    mkElemNum  t = Elem . makeXMLLeaf t . show
    locElem = Elem $ makeXMLLeaf "location" trkFilePath
    content = locElem : catMaybes [ mkElemNum  "duration" <$> trkDuration
                                  , mkElemText "creator"  <$> trkArtist
                                  , mkElemText "album"    <$> trkAlbum
                                  , mkElemText "title"    <$> trkTitle
                                  , mkElemNum  "trackNum" <$> trkTrackNum
                                  ]

-- | Helper to make an XML element containing a single text node.
makeXMLLeaf :: String -> String -> Element
makeXMLLeaf tagname content = blank_element { elName    = blank_name { qName = tagname }
                                            , elContent = [Text (blank_cdata { cdData = content })]
                                            }

{- |
Module      : Web
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Represent lists of playlists as HTML.
-}
{-# LANGUAGE OverloadedStrings #-}
module Web where

import           Playlist           (Album, Artist)

import           Lucid
import qualified Network.URI.Encode as URI

import           Control.Monad      ((<=<))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.FilePath    ((</>))


-- | Head element for the top of each page.
supercapHead :: ToHtml t => t -> Html ()
supercapHead title = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  link_ [href_ "/playlist/supercap.css", rel_ "stylesheet"]
  title_ $ toHtml title

-- | Banner element for the top of each page.
supercapBanner :: Html ()
supercapBanner = header_ $ h1_ "supercapacitor"


-- | Display a list of available artists.
artistListPage :: [Artist] -> Html ()
artistListPage artists = doctypehtml_ $ do
  supercapHead label
  body_ $ do
    supercapBanner
    main_ $ do
      h1_ label
      mapM_ (supercapList href <=< prependTitleLetter) $ NE.groupWith listToMaybe artists
  where
    href artist = T.pack $ "/playlist" </> URI.encode artist
    label = "Artists"

-- | Display a list of available albums for the given artist.
albumListPage :: Artist -> [Album] -> Html ()
albumListPage artist albums = doctypehtml_ $ do
  supercapHead label
  body_ $ do
    supercapBanner
    nav_ $ a_ [href_ "/playlist"] "Back to album list"
    main_ $ do
      h1_ label
      maybe mempty (supercapList href) $ NE.nonEmpty albums
  where
    href album = T.pack $ "/playlist" </> URI.encode artist </> URI.encode album
    label = "Albums by " <> toHtml artist

-- | An unordered list of hyperlinks created from 'String' names using the
-- function provided.
supercapList :: (String -> Text) -> NonEmpty String -> Html ()
supercapList mkHref xs = ul_ [class_ "aalist"] $ mapM_ mkLi $ NE.toList xs
  where
    mkLi :: String -> Html ()
    mkLi x = li_ $ a_ [href_ (mkHref x)] (toHtml x)

-- | Display the first letter of the first list item as a header.
prependTitleLetter :: NonEmpty String -> Html (NonEmpty String)
prependTitleLetter xs = do
  h2_ $ toHtml [firstLetter]
  pure xs
  where
    firstLetter = fromMaybe ' ' $ listToMaybe $ NE.head xs

{- |
Module      : Config
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Common configuration elements.
-}
module Config where


data Config = Config { musicRootPath :: FilePath  -- ^ top-level directory containing artist dirs
                     , musicRootUrl  :: String    -- ^ URL root at which music files are served
                     } deriving Show

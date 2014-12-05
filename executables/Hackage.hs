{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | scan a local index downloaded from Hackage

module Hackage where

import           Edit.Types
import           Edit.Parser

import           Control.Exception
import           Control.Monad
import           Data.Data
import           Data.Either (rights)
import           Data.Foldable
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as T
import           Data.Typeable
import           Shelly
import           System.Environment
import           Text.Parsec

import           Data.Traversable
import           Shelly
import           Text.Parsec

import Prelude hiding
    (foldl, foldl1, foldr, foldr1
    , mapM, mapM_, sequence, sequence_
    , head, tail, last, init, map, (++), (!!), FilePath, lines
    , maximum, maximumBy, readFile, writeFile)

-- | parse the .cabal file for the most recent version of each package.
scanHackage :: FilePath -> Maybe Int -> Sh [Either ParseError [DetailedPackage]]
scanHackage hackagePath limit = do
    paths <- ls hackagePath
    packages <- filterM test_d paths
    traverse parseLatest (case limit of
                           Nothing -> packages
                           Just n -> take n packages)

parseLatest :: FilePath -> Sh (Either ParseError [DetailedPackage])
parseLatest dir = do
    versions <- traverse toTextWarn =<< ls dir
    let newest = newestVersion versions
    fns <- traverse toTextWarn =<< filter (hasExt "cabal") <$> ls (dir </> newest)
    let cabalFileName = case fns of
            [] -> throw . IndexLayoutError $ "packageStat: expected .cabal file in " <> newest
            [c] -> c
            _ -> throw . IndexLayoutError $ "packageStat: only expected one .cabal file in " <> newest
    cabal <- readfile (dir </> newest </> cabalFileName)
    return $ parse detailedParser (T.unpack cabalFileName) cabal

-- | Pick newest version based on standard package numbering.  If we
-- can't make sense of the version number, fall back to lexicographic
-- ordering.
newestVersion :: [Text] -> Text
newestVersion [] = throw $ IndexLayoutError "newestVersion: Cannot pick one from an empty list"
newestVersion vs = case rights $ fmap splitVersion vs of
    [] -> maximum vs
    vs' -> fst . maximumBy (comparing snd) $ zip vs vs'

splitVersion :: Text -> Either String [Int]
splitVersion = traverse (fmap fst . T.decimal) .  T.splitOn "."

data HackageError =
    IndexLayoutError Text
    deriving (Typeable, Data, Show)

instance Exception HackageError

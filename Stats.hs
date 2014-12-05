{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Edit.Types
import           Edit.Parser

import Data.Typeable
import Data.Data
import Data.Either (rights)
import           Control.Exception
import           Data.Foldable
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB

import           Data.Traversable
import           Shelly
import           Text.Parsec

import Prelude hiding
    (foldl, foldl1, foldr, foldr1
    , mapM, mapM_, sequence, sequence_
    , head, tail, last, init, map, (++), (!!), FilePath, lines
    , maximum, maximumBy, readFile, writeFile)

main :: IO ()
main = debugMain
-- main = shelly $ do
--     packages <- take 10 <$> ls (fromText hackagePath)
--     echo $ showT packages
--     cabals <- traverse packageStat packages
--     echo $ showT cabals
--     -- echo_n . textSummary . mconcat $ cabals

debugMain :: IO ()
debugMain = shelly $ do
    let path = "/home/bergey/tmp/index/cereal/0.4.1.0/cereal.cabal"
    echo path
    cabal <- readfile $ fromText path
    echo cabal
    echo "Starting Parser"
    echo . showT $ parse detailedParser (T.unpack path) cabal

hackagePath :: Text
hackagePath = "/home/bergey/tmp/index"

-- packageStat :: FilePath -> Sh PackageStat
packageStat :: FilePath -> Sh String
packageStat dir = do
    versions <- traverse toTextWarn =<< ls dir
    let newest = newestVersion versions
    fns <- traverse toTextWarn =<< filter (hasExt "cabal") <$> ls (dir </> newest)
    let cabalFileName = case fns of
            [] -> throw . IndexLayoutError $ "packageStat: expected .cabal file in " <> newest
            [c] -> c
            _ -> throw . IndexLayoutError $ "packageStat: only expected one .cabal file in " <> newest
    cabal <- readfile (dir </> newest </> cabalFileName)
    case parse detailedParser (T.unpack cabalFileName) cabal of
     Right c -> return $ show c
     Left c -> return $ show c
    --  Right c -> return $ stats c
    --  Left e -> echo (showT e) >> return mempty

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

-- joinVersion :: [Int] -> FilePath
-- joinVersion = fromText . intersperse '.'

data AppError =
    IndexLayoutError Text
    deriving (Typeable, Data, Show)

instance Exception AppError

-- stats :: [DetailedPackage] -> PackageStat
-- stats = HackageStaft 1 globalCol valueCol valueSpace sectionCol where
--   globalCol (GlobalField i _ _ _ _) = Just i
--   globalCol _ = Nothing
--   valueCol (GlobalField i t j _ cs) = Just $ i + T.length t + j
--   valueCol (Section _ _ _  )


-- -- increment :: Int -> IntMap Int -> IntMap Int
-- -- increment k = insertWith (+) k 1

-- percent :: Int -> Int -> Double
-- percent a b = fromIntegral a / fromIntegral b

-- data HackageStat = HackageStat {
--     _packageCount :: Int,
--     _uniformGlobalFieldIndent :: M.IntMap  Int,
--     _uniformFieldValueColumn :: M.IntMap Int,
--     _uniformKeyValueSpacing :: M.IntMap Int,
--     _uniformSectionFieldIndent :: M.IntMap Int,
--     }

-- instance Monoid HackageStat where
--     mempty = HackageStat 0 mempty mempty mempty mempty
--     (HackageStat a b c d) `mappend` (HackageStat e f g h) =
--         HackageStat (a + e) (M.unionWith (+) b f) (M.unionWith (+) c g) (M.unionWith (+) d h)

-- allEqual :: [Maybe Int] -> M.IntMap Int
-- allEqual ms = case catMaybes ms of
--     [] -> Nothing
--     js@(x:_) = case all (==x) js of
--         M.singleton x 1
--         mempty

data PackageStat = PackageStat {
    _packageCount :: Int,
    _executableSections :: Int,
    _librarySections :: Int,
    _flags :: Int,
    _otherSections :: Int
    }

instance Monoid PackageStat where
    mempty = PackageStat 0 0 0 0 0
    (PackageStat a1 b1 c1 d1 e1) `mappend` (PackageStat a2 b2 c2 d2 e2) =
        PackageStat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)

stats :: [DetailedPackage] -> PackageStat
stats ds = (mconcat $ fmap goLine ds) {_packageCount = 1} where
  goLine (Section _ t _ _ _) = case t of
      "executable" -> PackageStat 0 1 0 0 0
      "library" -> PackageStat 0 0 1 0 0
      "flag" -> PackageStat 0 0 0 1 0
      _ -> PackageStat 0 0 0 0 1
  goLine _ = mempty

showT :: Show s => s -> Text
showT = T.pack . show

showB :: Show s => s -> TB.Builder
showB = TB.fromString . show

textSummary :: PackageStat -> Text
textSummary ps = toStrict . TB.toLazyText $
  "found " <> showB (_packageCount ps) <> " packages in " <> TB.fromText hackagePath <> "\n" <>
  "containing:\n" <>
  showB (_executableSections ps) <> " executable sections,\n" <>
  showB (_librarySections ps) <> " library sections,\n" <>
  showB (_flags ps) <> " flag definitions,\n" <>
  "and " <> showB (_otherSections ps) <> " other sections\n"

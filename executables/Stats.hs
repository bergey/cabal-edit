{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Edit.Types
import           Edit.Parser
import Edit.Printer
import Hackage

import System.Environment
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
-- main = debugMain
main = shelly $ do
    args <- liftIO getArgs
    let limit = case args of
            [] -> Nothing
            (a:_) -> Just $ read a
    cabals <- scanHackage (fromText hackagePath) limit
    traverse_ printCabals cabals
    summaries <- traverse packageStat cabals
    echo_n_err . textSummary . mconcat $ summaries

-- debugMain :: IO ()
-- debugMain = shelly $ do
--     let path = "/home/bergey/tmp/index/cereal/0.4.1.0/cereal.cabal"
--     echo path
--     cabal <- readfile $ fromText path
--     echo cabal
--     echo "Starting Parser"
--     echo . showT $ parse detailedParser (T.unpack path) cabal

hackagePath :: Text
hackagePath = "/home/bergey/tmp/index"

packageStat :: Either ParseError [DetailedPackage] -> Sh PackageStat
packageStat (Right c) = return $ stats c
packageStat (Left e) = echo_err (showT e) >> return mempty { _failedParses = 1 }

-- data AppError =
--     IndexLayoutError Text
--     deriving (Typeable, Data, Show)

-- instance Exception AppError

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
    _failedParses :: Int,
    _sourceSections :: Int,
    _executableSections :: Int,
    _librarySections :: Int,
    _testSections :: Int,
    _flags :: Int,
    _otherSections :: [Text]
    }

instance Monoid PackageStat where
    mempty = PackageStat 0 0 0 0 0 0 0 []
    (PackageStat a1 b1 c1 d1 e1 f1 g1 h1) `mappend` (PackageStat a2 b2 c2 d2 e2 f2 g2 h2) =
        PackageStat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2) (f1 + f2) (g1 + g2) (h1 <> h2)

stats :: [DetailedPackage] -> PackageStat
stats ds = (mconcat $ fmap goLine ds) {_packageCount = 1} where
  goLine (Section _ t _ _ _) = case T.toLower t of
      "source-repository" -> mempty { _sourceSections = 1 }
      "executable" -> mempty { _executableSections = 1}
      "library" -> mempty { _librarySections = 1}
      "test-suite" -> mempty { _testSections = 1 }
      "flag" -> mempty { _flags = 1}
      _ -> mempty { _otherSections = [t]}
  goLine _ = mempty

showT :: Show s => s -> Text
showT = T.pack . show

-- showB :: Show s => s -> TB.Builder
-- showB = TB.fromString . show

textSummary :: PackageStat -> Text
textSummary ps = toStrict . TB.toLazyText $
  "\nfound " <> showB (_packageCount ps) <> " packages in " <> TB.fromText hackagePath <> "\n" <>
  "containing:\n" <>
  showB (_executableSections ps) <> " executable sections,\n" <>
  showB (_librarySections ps) <> " library sections,\n" <>
  showB (_flags ps) <> " flag definitions,\n" <>
  "and " <> (showB . length . _otherSections $ ps) <> " other sections\n\n" <>
  "failed to parse " <> showB (_failedParses ps)  <> " packages\n" <>
  "found unknown sections :" <> showB (_otherSections ps) <> "\n\n"

printCabals :: Either ParseError [DetailedPackage] -> Sh ()
printCabals (Left  _) = return ()
printCabals (Right p) = echo . toStrict . TB.toLazyText . debugDetailedPrint $ p

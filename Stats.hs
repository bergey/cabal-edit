module Main where

import Prelude hiding
    (foldl, foldl1, foldr, foldr1
    , mapM, mapM_, sequence, sequence_
    , head, tail, last, init, map, (++), (!!), FilePath, lines)

import Data.Traversable
import Data.Foldable
import Shelly
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec

main :: IO ()
main = return ()

packageStat :: FilePath -> Sh PackageStat
packageStat dir = do
    versions <- ls dir


splitVersion :: Text -> Either String [Int]
splitVersion = traverse T.decimal . splitOn "."

joinVersion :: [Int] -> FilePath
joinVersion = fromText . intersperse '.'

-- data PackageStat =
--     VersionError String |
--     UniformIndent Int |
--     UniformSpace Int

-- data LineStat =
--     GeneralField Int Int | -- ^ first column of field name, value
--     SectionField Int Int | -- ^ first column of field name, value
--     SectionName Int |

--     BlankLine

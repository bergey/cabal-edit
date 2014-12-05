{-# LANGUAGE OverloadedStrings #-}

module Edit.Printer where

import Edit.Types

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import qualified Data.Text.Read as T
import Data.Monoid

debugDetailedPrint :: [DetailedPackage] -> Builder
debugDetailedPrint = foldrm  . map debugDetailedLine

debugDetailedLine :: DetailedPackage -> Builder
debugDetailedLine (GlobalField i n j v ls) = showB (GlobalField i n j v []) <> "\n" <>
                                             (foldrm  $ map debugContinuationLine ls)
debugDetailedLine (Section i n j v ls) = showB (Section i n j v []) <> "\n" <>
                                         (foldrm  $ map debugSectionLine ls)
debugDetailedLine l = showB l <> "\n"

debugSectionLine :: SectionLine -> Builder
debugSectionLine (SectionField i n j v ls) = spaces 4 <> showB (SectionField i n j v []) <> "\n" <>
                                             (foldrm $ map debugContinuationLine ls)
debugSectionLine BlankSLine = spaces 4 <> "BlankSLine" <> "\n"

debugContinuationLine :: ContinuationLine -> Builder
debugContinuationLine cl = spaces 8 <> showB cl <> "\n"

-- indentedLine :: Show s => Int -> s -> Builder
-- indentedLine i s = spaces i <> showB s <> "\n" where

spaces :: Int -> Builder
spaces i = fromString $ replicate i ' '

showB :: Show s => s -> Builder
showB = fromString . show

foldrm :: Monoid m => [m] -> m
foldrm = foldr mappend mempty

-- | Types for detailed & simple parsing of .cabal files.

module Edit.Types where

import Data.Text (Text)

-- | The detailed description of a .cabal file includes the length of
-- each semantic whitespace span on each line.  It should be possible
-- to exactly reproduce any parsed .cabal file from its representation
-- in this type.
data DetailedPackage =
    Comment Int Text|
    GlobalField Int Text Int Text [ContinuationLine] |
    Section Int Text Int Text [SectionField] |
    BlankLine
    deriving (Show, Eq)

data ContinuationLine = ContinuationLine Int Text
    deriving (Show, Eq)

data SectionField = SectionField Int Text Int Text [ContinuationLine]
    deriving (Show, Eq)

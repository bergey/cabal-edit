-- | Parse .cabal files in pedantic detail.

module Edit.Parser where

import Edit.Types

import Text.Parsec hiding (space, spaces, many, (<|>))
-- import Text.Parsec.Char
import Text.Parsec.Text
import Data.Text (Text, pack)
import Control.Applicative
import Data.Functor (void)

spaces :: Parser Int
spaces = do
    ss <- many $ char ' '
    return $ length ss

restOfLine :: Parser Text
restOfLine = pack <$> manyTill anyChar (void endOfLine <|> eof)

fieldName :: Parser Text
fieldName = pack <$> manyTill (char '-' <|> letter) (char ':')

continuationLine :: Int -> Parser ContinuationLine
continuationLine i  = try $ do
    s <- spaces
    if s > i then ContinuationLine s <$> restOfLine
        else fail "not indented enough for continuation line"

sectionField :: Int -> Parser SectionField
sectionField i = try $ do
    s <- spaces
    if s > i then SectionField s <$> fieldName <*> spaces <*> restOfLine <*>
                  many (continuationLine s)
        else fail "not indented enough for section field"

comment :: Parser DetailedPackage
comment = try $ Comment <$> spaces <*> (string "--" *> restOfLine)

globalField :: Parser DetailedPackage
globalField = try $ do
    s <- spaces
    GlobalField s <$> fieldName <*> spaces <*> restOfLine <*>
        many (continuationLine s)

section :: Parser DetailedPackage
section = try $ do
    s <- spaces
    Section s <$> (pack <$> many1 letter) <*> spaces <*> restOfLine <*>
        many (sectionField s)

blankLine :: Parser DetailedPackage
blankLine = spaces *> (void endOfLine <|> eof) *> pure BlankLine

detailedPackageLine :: Parser DetailedPackage
detailedPackageLine =
    label comment "comment" <|>
    label globalField "global field" <|>
    label section "section" <|>
    label blankLine "blank line"

detailedParser :: Parser [DetailedPackage]
detailedParser = many detailedPackageLine

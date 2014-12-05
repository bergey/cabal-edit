{-# LANGUAGE OverloadedStrings #-}

module Main where

import Edit.Types
import Edit.Parser

import Text.Parsec hiding (space, spaces, (<|>))
import Text.Parsec.Text
import Control.Applicative
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

run :: Parser a -> Text -> Either ParseError a
run p = parse p "test"

tests :: TestTree
tests = testGroup "Detailed"
        [ testGroup "Inner Lines"
          [ testCase "continuationLine fails on short indent" $
              assertLeft $ run (continuationLine 4) " one\n"
          , testCase "continuationLine succeeds with good indent" $
              run (continuationLine 2) "    four\n" `assertRight` (ContinuationLine 4 "four")
          , testCase  "sectionField fails on short indent" $
              assertLeft $ run (sectionField 4) " one: two\n"
          , testCase "sectionField succeeds with good indent" $
              run (sectionField 2) "     five: six\n" `assertRight` (SectionField 5 "five" 1 "six" [])
          , testCase "typical sectionField in executable" $
            run (sectionField 0) "  main-is: b.hs\n" `assertRight` SectionField 2 "main-is" 1 "b.hs" []
          ]
        , testGroup "Individual Top Level Constructors"
          [ testCase "comment requires --" $
            assertLeft $ run comment "- bad comment"
          , testCase "comment doesn't include --" $
            run comment "  -- good comment\n" `assertRight` (Comment 2 " good comment")
          , testCase "globalField without leading space" $
            run globalField "name: value\n" `assertRight` (GlobalField 0 "name" 1 "value" [])
          , testCase "globalField without any space" $
            run globalField "name:value\n" `assertRight` (GlobalField 0 "name" 0 "value" [])
          , testCase "globalField with extra space" $
            run globalField "  name:    value\n" `assertRight` (GlobalField 2 "name" 4 "value" [])
          , testCase "globalField with one continuation line" $
            run globalField "description: some\n  words\n" `assertRight`
            GlobalField 0 "description" 1 "some" [ContinuationLine 2 "words"]
          , testCase "globalField with two continuation lines" $
            run globalField "description: some\n  more\n  words\n" `assertRight`
            GlobalField 0 "description" 1 "some" [ContinuationLine 2 "more", ContinuationLine 2 "words"]
          , testCase "globalField doesn't include next line" $
            run globalField "description: some words\nauthor: Daniel Bergey\n" `assertRight`
            GlobalField 0 "description" 1 "some words" []
          , testCase "section accepts executable" $
            run section "executable a\n  main-is: b.hs\n" `assertRight`
            Section 0 "executable" 1 "a" [SectionField 2 "main-is" 1 "b.hs" []]
          , testCase "section accepts flag" $
            run section "flag f\n  default: False\n  manual: True\n" `assertRight`
            Section 0 "flag" 1 "f" [SectionField 2 "default" 1 "False" [], SectionField 2 "manual" 1 "True" []]
            , testCase "section accepts library" $
              run section "library\n  build-depends: array\n" `assertRight`
              Section 0 "library" 0 "" [SectionField 2 "build-depends" 1 "array" []]
          , testCase "blankLine accepts no spaces" $ run (blankLine ()) "\n" `assertRight` ()
          , testCase "blankLine accepts some space" $ run (blankLine ()) "  \n" `assertRight` ()
          , testCase "blankLine rejects letters" $ assertLeft $ run (blankLine ()) "a \n"
          ]
        , testGroup "Top-Level Parser"
          [ testCase "comment; global; blank; section" $
            run detailedParser "--a\nb: c\n\nflag f\n  default: t\n" `assertRight`
            [Comment 0 "a", GlobalField 0 "b" 1 "c" [], BlankLine, Section 0 "flag" 1 "f" [SectionField 2 "default" 1 "t" []]]
          , testCase "two sections" $
            run detailedParser "a\n  b:\n    c\n    d\n  e:v\n\n\nf g\n  h:\n" `assertRight`
            [Section 0 "a" 0 "" [SectionField 2 "b" 0 "" [ContinuationLine 4 "c", ContinuationLine 4 "d"], SectionField 2 "e" 0 "v" [], BlankSectionLine, BlankSectionLine], Section 0 "f" 1 "g" [SectionField 2 "h" 0 "" []]]
          ]
        ]

basic :: TestTree
basic = testGroup "Parsec"
        [  testCase "manyTill consumes final char" $
           run  (manyTill letter (char ':') *> anyChar) "key:v" `assertRight` 'v'
        ]

assertRight :: (Eq b, Show a, Show b) => Either a b -> b -> Assertion
assertRight (Right x) y | x == y = return ()
assertRight (Right x) y = assertFailure $ "Expected " ++ show y ++ " got " ++ show x
assertRight (Left e) y = assertFailure $ show e

assertLeft :: Show b => Either a b -> Assertion
assertLeft (Left _) = return ()
assertLeft (Right x) = assertFailure $ "expected Left, got Right " ++ show x

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

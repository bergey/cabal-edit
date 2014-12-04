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
tests = testGroup "Parsec"
        [  testCase "manyTill consumes final char" $ parse  (manyTill letter (char ':') *> anyChar) "key:v" @?= Right 'v'
        ]

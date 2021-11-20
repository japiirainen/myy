module Main where

import           Test.Tasty

import           Tests.Lex

allTests :: TestTree
allTests = testGroup "Top" [lexTests]

main :: IO ()
main = defaultMain allTests

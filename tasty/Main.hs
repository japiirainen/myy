module Main where

import           Test.Tasty

import           Check
import           Lex
import           Parse

allTests :: TestTree
allTests = testGroup "Top" [lexTests, parseTests, checkTests]

main :: IO ()
main = defaultMain allTests

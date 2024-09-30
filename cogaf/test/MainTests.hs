-- test/MainTests.hs
module Main where

import Test.HUnit
import CellTests
import GridTests
import MyLibTests

-- Main function to run all tests
main :: IO ()
main = do
    _ <- runTestTT $ TestList [cellTests, gridTests, myLibTests]
    return ()


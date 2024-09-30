-- test/MyLibTests.hs
module MyLibTests where

import Test.HUnit
import MyLib
import Grid
import Cell

-- Test cases for the MyLib module
myLibTests :: Test
myLibTests = TestList [
    TestCase (assertEqual "Rendering an empty grid"
        (renderGrid [[Dead, Dead], [Dead, Dead]]) ".\n.\n"),
    TestCase (assertEqual "Rendering a grid with alive cells"
        (renderGrid [[Alive, Dead], [Dead, Alive]]) "O.\n.A\n"),
    TestCase (assertEqual "Rendering a single alive cell"
        (renderGrid [[Alive]]) "O\n"),
    TestCase (assertEqual "Rendering a grid with a single row"
        (renderGrid [[Dead, Alive, Dead]]) ".O.\n"),
    TestCase (assertEqual "Rendering a grid with all alive cells"
        (renderGrid [[Alive, Alive], [Alive, Alive]]) "OO\nOO\n")
    ]

-- Main function to run tests
main :: IO ()
main = do
    _ <- runTestTT myLibTests
    return ()


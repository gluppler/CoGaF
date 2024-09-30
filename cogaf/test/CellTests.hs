-- test/CellTests.hs
module CellTests where

import Test.HUnit
import Cell

-- Test cases for the Cell module
cellTests :: Test
cellTests = TestList [TestCase (assertEqual "Alive cell with 0 neighbors should die" Dead (nextState Alive 0)),
    TestCase (assertEqual "Alive cell with 1 neighbor should die" Dead (nextState Alive 1)),
    TestCase (assertEqual "Alive cell with 2 neighbors should stay alive" Alive (nextState Alive 2)),
    TestCase (assertEqual "Alive cell with 3 neighbors should stay alive" Alive (nextState Alive 3)),
    TestCase (assertEqual "Alive cell with 4 neighbors should die" Dead (nextState Alive 4)),
    TestCase (assertEqual "Dead cell with 3 neighbors should become alive" Alive (nextState Dead 3)),
    TestCase (assertEqual "Dead cell with 2 neighbors should stay dead" Dead (nextState Dead 2)),
    TestCase (assertEqual "Dead cell with 4 neighbors should stay dead" Dead (nextState Dead 4))]

-- Main function to run tests
main :: IO ()
main = do
    _ <- runTestTT cellTests
    return ()


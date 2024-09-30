-- test/GridTests.hs
module GridTests where

import Test.HUnit
import Grid
import Cell

-- Test cases for the Grid module
gridTests :: Test
gridTests = TestList [
    TestCase (assertEqual "Next generation of a single alive cell should be dead"
        (nextGeneration [[Alive]]) [[Dead]]),
    TestCase (assertEqual "Next generation of a block should stay alive"
        (nextGeneration [[Alive, Alive], [Alive, Alive]]) [[Alive, Alive], [Alive, Alive]]),
    TestCase (assertEqual "Next generation of a blinker should toggle"
        (nextGeneration [[Dead, Dead, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]])
        [[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]]),
    TestCase (assertEqual "Next generation of a block pattern should stay alive"
        (nextGeneration [[Alive, Alive, Dead], [Alive, Alive, Dead], [Dead, Dead, Dead]])
        [[Alive, Alive, Dead], [Alive, Alive, Dead], [Dead, Dead, Dead]]),
    TestCase (assertEqual "Next generation of an empty grid should remain empty"
        (nextGeneration [[Dead, Dead], [Dead, Dead]]) [[Dead, Dead], [Dead, Dead]])
    ]

-- Main function to run tests
main :: IO ()
main = do
    _ <- runTestTT gridTests
    return ()


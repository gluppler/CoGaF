-- src/Grid.hs
module Grid (Grid, initializeGrid, nextGeneration, renderGrid) where

import Cell
import System.Random (randomRIO)

type Grid = [[Cell]]

initializeGrid :: Int -> Int -> IO Grid
initializeGrid rows cols = mapM (\_ -> randomRow cols) [1..rows]
    where
        randomRow :: Int -> IO [Cell]
        randomRow n = mapM (\_ -> randomCell) [1..n]
        randomCell :: IO Cell
        randomCell = do
            r <- randomRIO (0, 1) :: IO Int
            return $ if r == 0 then Dead else Alive

renderGrid :: Grid -> String
renderGrid = unlines . map (concatMap show)

nextGeneration :: Grid -> Grid
nextGeneration grid = [[ nextState (grid !! r !! c) (livingNeighbors r c)
                        | c <- [0..cols-1]]
                        | r <- [0..rows-1]]
  where
    rows = length grid
    cols = length (head grid)

    livingNeighbors :: Int -> Int -> Int
    livingNeighbors r c = length $ filter isAlive $ neighbors r c

    neighbors :: Int -> Int -> [Cell]
    neighbors r c = [ grid !! ((r + dr) `mod` rows) !! ((c + dc) `mod` cols)
                    | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0) ]


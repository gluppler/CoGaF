-- src/MyLib.hs
module MyLib (startGame) where

import Grid
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

fps :: Int
fps = 5

startGame :: Grid -> IO ()
startGame grid = gameLoop grid

gameLoop :: Grid -> IO ()
gameLoop grid = do
    putStrLn "\ESC[2J"
    putStrLn $ renderGrid grid
    hFlush stdout
    threadDelay (1000000 `div` fps)
    let nextGrid = nextGeneration grid
    gameLoop nextGrid


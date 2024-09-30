-- app/Main.hs
module Main where

import MyLib
import Grid

rows, cols :: Int
rows = 20
cols = 40

main :: IO ()
main = do
    grid <- initializeGrid rows cols
    startGame grid


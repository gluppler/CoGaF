-- src/Cell.hs
module Cell (Cell(..), nextState, isAlive) where

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
    show Alive = "O"
    show Dead  = "."

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

nextState :: Cell -> Int -> Cell
nextState Alive n
    | n < 2 || n > 3 = Dead
    | otherwise      = Alive
nextState Dead n
    | n == 3         = Alive
    | otherwise      = Dead


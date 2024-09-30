# Conway's Game of Life

## Overview

Conway's Game of Life is a cellular automaton devised by mathematician John Conway. It is a zero-player game, meaning that its evolution is determined by its initial state, requiring no further input. The game consists of a grid of cells that can be either alive or dead, with their next states determined by simple rules based on the states of their neighboring cells. This Haskell implementation demonstrates functional programming principles, emphasizing immutability and recursion.

## Features

- **Cell State Management**: Cells can be in one of two states: Alive (`O`) or Dead (`.`).
- **Grid Representation**: A two-dimensional grid representing the state of the game at any point in time.
- **Next Generation Calculation**: Computes the next state of the grid based on the current configuration and specific rules.
- **Rendering**: Text-based rendering of the grid for visual representation of the game state.
- **Comprehensive Testing**: Unit tests to verify the correctness of cell state transitions and grid functionalities.

## File Structure

```
/conways-game-of-life
│
├── app
│   └── Main.hs          -- Entry point for the game
│
├── src
│   ├── Cell.hs         -- Cell logic and state management
│   ├── Grid.hs         -- Grid representation and generation logic
│   └── MyLib.hs        -- Rendering and game logic
│
└── test
    ├── CellTests.hs    -- Unit tests for Cell module
    ├── GridTests.hs    -- Unit tests for Grid module
    ├── MyLibTests.hs    -- Unit tests for MyLib module
    └── MainTests.hs     -- Aggregated test runner
```

### Logic

#### Cell Logic (`Cell.hs`)

The `Cell` module defines the state of individual cells and the rules for transitioning between states. The key function is `nextState`, which determines the next state of a cell based on its current state and the number of alive neighbors.

```haskell
data Cell = Alive | Dead deriving (Eq)

nextState :: Cell -> Int -> Cell
nextState Alive n | n < 2 = Dead
                  | n == 2 = Alive
                  | n == 3 = Alive
                  | n > 3 = Dead
nextState Dead n | n == 3 = Alive
                 | otherwise = Dead
```

#### Grid Logic (`Grid.hs`)

The `Grid` module manages the grid of cells, calculating the next generation of cells based on the current state of the grid.

```haskell
type Grid = [[Cell]]

nextGeneration :: Grid -> Grid
nextGeneration grid = [[nextState (grid !! r !! c) (countNeighbors r c grid) | c <- [0..(length (head grid) - 1)]] | r <- [0..(length grid - 1)]]
  where
    countNeighbors r c grid = length [() | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0), isAlive (r + dr) (c + dc)]
    isAlive r c = r >= 0 && r < length grid && c >= 0 && c < length (head grid) && (grid !! r !! c == Alive)
```

#### Rendering Logic (`MyLib.hs`)

The `MyLib` module contains functions for rendering the grid as a string for display.

```haskell
renderGrid :: Grid -> String
renderGrid grid = unlines [concat [if cell == Alive then "O" else "." | cell <- row] | row <- grid]
```

## Testing

The project includes unit tests for each module, ensuring the correctness of cell state transitions, grid updates, and rendering functionality.

### Test Files

#### CellTests (`test/CellTests.hs`)

Tests the functionality of cell state management.

#### GridTests (`test/GridTests.hs`)

Tests the grid logic, particularly the calculation of the next generation.

#### MyLibTests (`test/MyLibTests.hs`)

Tests the rendering logic for displaying the grid.

#### MainTests (`test/MainTests.hs`)

Aggregates all tests and runs them.

### Running the Tests

To ensure that the implementation is working correctly, you can run the tests using Cabal:

```bash
cabal test
```

## Getting Started

### Prerequisites

- [Haskell Platform](https://www.haskell.org/platform/) (GHC, Cabal, etc.)
- [Stack](https://docs.haskellstack.org/en/stable/README/) (optional, for building and managing dependencies)

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/yourusername/conways-game-of-life.git
   cd conways-game-of-life
   ```

2. Install dependencies:

   If using Cabal:

   ```bash
   cabal update
   cabal install --only-dependencies
   ```

   If using Stack:

   ```bash
   stack setup
   stack build
   ```

### Running the Game

To run the game, execute the `Main.hs` file:

```bash
cabal run
```

### Usage

Modify the initial state in `Main.hs` to configure the grid. Experiment with different patterns and observe their evolution through generations.

## Contribution

Contributions are welcome! If you have ideas for enhancements, bug fixes, or additional features, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Acknowledgements

Special thanks to John Conway for his pioneering work in cellular automata and to the Haskell community for providing a rich ecosystem for functional programming.

---

module Main where

import           System.Random (getStdGen)

import           Minesweeper   (Grid, Tile, click, randomizeGrid, showGrid)

main :: IO ()
main = do
  g <- getStdGen
  let grid = randomizeGrid 10 g
  putStrLn $ showGrid grid
  play grid

play :: Grid Tile -> IO ()
play grid = do
  (row, col) <- readLn
  let grid' = click row col grid
  putStrLn $ showGrid grid'
  play grid'

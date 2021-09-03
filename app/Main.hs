module Main where

import           Graphics.Vty
import           Graphics.Vty.Input
import           System.Console.ANSI
import           System.Exit         (exitSuccess)
import           System.Random       (getStdGen)

import           Minesweeper         (Grid, Tile, click, randomizeGrid,
                                      showGrid)

size = 10

main :: IO ()
main = do
  clearScreen
  g <- getStdGen
  let grid = randomizeGrid 10 g
  vty <- standardIOConfig >>= mkVty
  putStrLn $ showGrid grid
  play vty grid (0, 0)

play :: Vty -> Grid Tile -> (Int, Int) -> IO ()
play vty grid (r, c) = do

  cursorUp (1 + size * 2 + 2)
  putStrLn $ showGrid grid

  printPosition vty r c

  e <- nextEvent vty
  case e of
    (EvKey KDown _)             -> play vty grid (r + 1, c)
    (EvKey KUp _)               -> play vty grid (r - 1, c)
    (EvKey KRight _)            -> play vty grid (r, c + 1)
    (EvKey KLeft _)             -> play vty grid (r, c - 1)
    (EvKey (KChar ' ') _)       -> play vty (click r c grid) (r, c)
    (EvKey (KChar 'c') [MCtrl]) -> do
      shutdown vty
      putStrLn $ showGrid grid
      exitSuccess
    _                           -> play vty grid (r,c)

printPosition :: Vty -> Int -> Int -> IO ()
printPosition vty r c = do
  saveCursor

  cursorUp (1 + size * 2 + 2)
  cursorDown (1 + 2 * r)
  cursorForward (1 + 2 * c)
  putStrLn "X"
  restoreCursor

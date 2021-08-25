module Minesweeper (click, Grid, randomizeGrid, showGrid, Tile) where

import           Control.Monad.Random (evalRand, fromList)
import           Data.List            (sort)
import           Data.List.Split      (chunksOf)
import           Data.Map             (Map, empty, insert, keys, lookup,
                                       mapWithKey, update)
import           Data.Maybe           (catMaybes, fromMaybe)
import           Prelude              hiding (lookup)
import           System.Random        (Random, RandomGen, random, randomR)
import           Text.Printf          (printf)

type Grid a = Map Int (Map Int a)

showGrid :: Grid Tile -> String
showGrid grid =
    let
        border :: String
        border          = '+' : concat (take (length (keys grid)) (repeat "-+"))

        mapOrdered :: Ord k => (a -> b) -> Map k a -> [b]
        mapOrdered f m  = catMaybes $ map (fmap f . flip lookup m) $ sort $ keys m

        showTile (Closed _)        = " |"
        showTile (Opened (Bomb))   = "b|"
        showTile (Opened (Safe n)) = printf "%d|" n

        showRow :: Map Int Tile -> String
        showRow row = '|' : (concat $ mapOrdered showTile row) ++ ('\n' : border)
    in
        unlines $ border : mapOrdered showRow grid

data Tile
    = Opened Cell
    | Closed Cell
    -- | Flagged Cell
    deriving (Eq, Show)

data Cell
    = Bomb
    | Safe Int
    deriving (Eq, Show)

randomizeGrid :: RandomGen g => Int -> g -> Grid Tile
randomizeGrid size g =
    let
        cells   = weightedList g [(Bomb, 1/5), (Safe 0, 4/5)]
        columns = map listToMap $ chunksOf size cells
        grid    = listToMap $ take size columns
    in
        mapGrid (\_ _ -> Closed) $ assignBombCount grid

assignBombCount :: Grid Cell -> Grid Cell
assignBombCount grid =
    let
        setCount _ _ (Bomb)         = Bomb
        setCount row col (Safe _)   =
            let
                isBomb (Bomb) = True
                isBomb _      = False

                neighbours  = catMaybes $ map (uncurry (lookupGrid grid)) $ getSurrounding row col
                bombCount   = length $ filter isBomb neighbours
            in
                Safe bombCount
    in
        mapGrid setCount grid

listToMap :: [a] -> Map Int a
listToMap xs =
    let
        indexed = zip [0 ..] xs
    in
        foldr (uncurry insert) empty indexed


weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights

click :: Int -> Int -> Grid Tile -> Grid Tile
click row col grid = fromMaybe grid $ do
    rowValues   <- lookup row grid
    tile        <- lookup col rowValues
    case tile of
        (Opened _)          -> return grid
        (Closed t@(Safe 0)) ->
            let
                grid'       = updateGrid row col (Opened t) grid
                surrounding = getSurrounding row col
            in
                return $ clickMultiple grid' surrounding
        (Closed t@(Safe n)) -> return $ updateGrid row col (Opened t) grid
        (Closed t@(Bomb))   -> return $ updateGrid row col (Opened t) grid -- TODO reveal entire grid?
        -- (Flagged c) ->

clickMultiple :: Grid Tile -> [(Int, Int)] -> Grid Tile
clickMultiple = foldr (uncurry click)

updateGrid :: Int -> Int -> a -> Grid a -> Grid a
updateGrid row col t g = update (Just . update (\_ -> Just t) col) row g

lookupGrid :: Grid a -> Int -> Int -> Maybe a
lookupGrid grid row col = lookup row grid >>= lookup col

mapGrid :: (Int -> Int -> a -> b) -> Grid a -> Grid b
mapGrid f = mapWithKey (mapWithKey . f)

getSurrounding :: Int -> Int -> [(Int, Int)]
getSurrounding row col =
    let
        rows    = [(row - 1) .. (row + 1)]
        cols    = [(col - 1) .. (col + 1)]
        coords  = (,) <$> rows <*> cols
    in
        filter (/= (row, col)) coords


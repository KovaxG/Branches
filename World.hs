module World where

import Graphics.Gloss.Data.Picture

import Data.Maybe
import Data.Matrix

import Common

data World = World {
    gameField :: Matrix Square,
    selected :: Maybe Coord
} 

data Square = Square {
    coord :: Coord,
    picture :: Picture,
    unit :: Maybe Unit
} deriving (Show)

data Unit = Unit deriving (Show)

initialState :: World
initialState = World {gameField = allGrass, selected = Nothing}
    where allGrass :: Matrix Square
          allGrass = matrix worldSizeHorizontal 
                            worldSizeVertical
                            tileGenerator
         
          tileGenerator :: Coord -> Square
          tileGenerator (x, y)
            | x == 1 && y == 1 = Square (x, y) grassTile (Just Unit)
            | sum `mod` 4 == 0 = Square (x, y) grassTile Nothing
            | sum `mod` 4 == 1 = Square (x, y) stoneTile Nothing
            | sum `mod` 4 == 2 = Square (x, y) waterTile Nothing
            | otherwise        = Square (x, y) treeTile  Nothing
            where sum = (x ^ y) ^ x * (x - y ^ y)

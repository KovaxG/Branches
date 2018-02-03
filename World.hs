module World where

import Graphics.Gloss.Data.Picture

import Data.Maybe
import Data.Matrix

import Common

data World = World {
    gameField :: Matrix Square,
    info :: Maybe Info
} 

data Square = Square {
    coord :: Coord,
    picture :: Picture,
    unit :: Maybe Unit,
    selected :: Bool
} deriving (Show)

data Info = Info {
    name :: String
}

data Unit = Unit deriving (Show)

initialState :: World
initialState = World { gameField = allGrass, 
                       info = Nothing }
                       
    where allGrass :: Matrix Square
          allGrass = matrix worldSizeHorizontal 
                            worldSizeVertical
                            tileGenerator
         
          tileGenerator :: Coord -> Square
          tileGenerator (x, y)
            | x == 1 && y == 1 = Square (x, y) grassTile (Just Unit) False
            | sum `mod` 4 == 0 = Square (x, y) grassTile Nothing False
            | sum `mod` 4 == 1 = Square (x, y) stoneTile Nothing False
            | sum `mod` 4 == 2 = Square (x, y) waterTile Nothing False
            | otherwise        = Square (x, y) treeTile  Nothing False
            where sum = (x ^ y) ^ x * (x - y ^ y)
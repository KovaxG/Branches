module World (
    World (..),
    Square (..),
    Unit (..),
    initialState,
    clearSelection,
    safeSetElem
) where

import Graphics.Gloss.Data.Picture

import Data.Maybe
import Data.Matrix

import Common


data World = World {
    wGameField :: Matrix Square,
    wSelected :: Maybe Square
} 


data Square = Square {
    sCoord :: Coord,
    sPicture :: Picture,
    sUnit :: Maybe Unit,
    sSelected :: Bool
} deriving (Show, Eq)


data Unit = Unit deriving (Show, Eq)


initialState :: World
initialState = World { wGameField = allGrass, 
                       wSelected = Nothing }
                       
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


clearSelection :: Matrix Square -> Matrix Square
clearSelection = fmap $ \s -> s {sSelected = False}


safeSetElem :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
safeSetElem f (x, y) originalMatrix = maybe outOfBounds changeElem maybeNewValue
    where maybeNewValue = f <$> safeGet x y originalMatrix
          outOfBounds = originalMatrix
          changeElem a = setElem a (x, y) originalMatrix

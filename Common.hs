module Common where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game (png)

import Data.Matrix

type Coord = (Int, Int)

displayWidth  = 600 :: Int
displayHeight = 600 :: Int

gameFieldWidth  = fromIntegral displayWidth  - infoBarHeight :: Float
gameFieldHeight = fromIntegral displayHeight - infoBarHeight :: Float

infoBarWidth  = fromIntegral displayWidth :: Float
infoBarHeight = 150 :: Float

worldSizeHorizontal = 6 :: Int
worldSizeVertical   = 6 :: Int

framesPerSecond = 30 :: Int



pWidth  = gameFieldWidth  / horizontalTileNr :: Float
pHeight = gameFieldHeight / verticalTileNr   :: Float
horizontalTileNr = fromIntegral worldSizeHorizontal :: Float
verticalTileNr   = fromIntegral worldSizeVertical   :: Float


tilePath = "GFX/"          
blankTile = png $ tilePath ++ "Blank.png" :: Picture
grassTile = png $ tilePath ++ "Grass.png" :: Picture
stoneTile = png $ tilePath ++ "Stone.png" :: Picture
waterTile = png $ tilePath ++ "Water.png" :: Picture
treeTile  = png $ tilePath ++ "Tree.png"  :: Picture

unitPic   = png $ tilePath ++ "Unit.png" :: Picture

safeSetElem :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
safeSetElem f (x, y) originalMatrix = maybe outOfBounds changeElem maybeNewValue
    where maybeNewValue = f <$> safeGet x y originalMatrix
          outOfBounds = originalMatrix
          changeElem a = setElem a (x, y) originalMatrix
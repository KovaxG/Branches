module Common where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game (png)

type Coord = (Int, Int)

displayWidth  = 600 :: Int
displayHeight = 600 :: Int
gameFieldWidth  = 600 :: Float
gameFieldHeight = 600 :: Float
framesPerSecond = 30 :: Int
worldSizeHorizontal = 6 :: Int
worldSizeVertical   = 6 :: Int


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

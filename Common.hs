module Common where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Game (png)

type Coord = (Int, Int)

displayWidth  = 600 :: Int
displayHeight = 600 :: Int

fDisplayWidth  = fromIntegral displayWidth  :: Float
fDisplayHeight = fromIntegral displayHeight :: Float

gameFieldWidth  = fDisplayWidth  - infoBarHeight :: Float
gameFieldHeight = fDisplayHeight - infoBarHeight :: Float

infoBarWidth  = fDisplayWidth :: Float
infoBarHeight = 150 :: Float

worldSizeHorizontal = 6 :: Int
worldSizeVertical   = 6 :: Int

framesPerSecond = 30 :: Int


pWidth  = gameFieldWidth  / horizontalTileNr :: Float
pHeight = gameFieldHeight / verticalTileNr   :: Float
horizontalTileNr = fromIntegral worldSizeHorizontal :: Float
verticalTileNr   = fromIntegral worldSizeVertical   :: Float


tilePath = "GFX/"          
grassTile = png $ tilePath ++ "Grass.png" :: Picture
stoneTile = png $ tilePath ++ "Stone.png" :: Picture
waterTile = png $ tilePath ++ "Water.png" :: Picture
treeTile  = png $ tilePath ++ "Tree.png"  :: Picture

unitPic   = png $ tilePath ++ "Unit.png" :: Picture

blankTile = whiteTint $ polygon rectPath
    where whiteTint = color (withAlpha 0.2 white)
          rectPath = rectanglePath pWidth pHeight
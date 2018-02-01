import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Game (png, boundingBox, Size)

import Data.Maybe
import Data.Matrix hiding (trace) 
import Debug.Trace

data World = World {
    gameField :: Matrix Square,
    selected :: Coord
} 

data Square = Square Coord Picture (Maybe Unit) deriving (Show)

data Unit = Unit deriving (Show)

type Coord = (Int, Int)

main :: IO ()
main = play displayMode 
            backgroundColor 
            framesPerSecond 
            initialState 
            render
            eventHandler 
            idleCallback


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


displayMode :: Display
displayMode = InWindow "Game" (displayWidth, displayHeight) (400, 100)


backgroundColor :: Color
backgroundColor = black


initialState :: World
initialState = World {gameField = allGrass, selected = (0,0)}
    where allGrass :: Matrix Square
          allGrass = matrix worldSizeHorizontal 
                            worldSizeVertical
                            tileGenerator
         
          tileGenerator :: (Int, Int) -> Square
          tileGenerator (x, y)
            | x == 1 && y == 1 = Square (x, y) grassTile (Just Unit)
            | sum `mod` 4 == 0 = Square (x, y) grassTile Nothing
            | sum `mod` 4 == 1 = Square (x, y) stoneTile Nothing
            | sum `mod` 4 == 2 = Square (x, y) waterTile Nothing
            | otherwise        = Square (x, y) treeTile  Nothing
            where sum = (x ^ y) ^ x * (x - y ^ y)


tilePath = "GFX/"          
blankTile = png $ tilePath ++ "Blank.png" :: Picture
grassTile = png $ tilePath ++ "Grass.png" :: Picture
stoneTile = png $ tilePath ++ "Stone.png" :: Picture
waterTile = png $ tilePath ++ "Water.png" :: Picture
treeTile  = png $ tilePath ++ "Tree.png"  :: Picture

unitPic   = png $ tilePath ++ "Unit.png" :: Picture


width :: Picture -> Float
width = fst . snd . boundingBox 


height :: Picture -> Float
height = snd . snd . boundingBox          


render :: World -> Picture
render world =  pictures [gf]
    where gf = renderGameField (gameField world) (selected world)


renderGameField :: Matrix Square -> Coord -> Picture
renderGameField matrix selec = translate xOffset yOffset gameField 
    where gameField :: Picture    
          gameField = pictures (squareToPic <$> toList matrix)
    
          xOffset = -(gameFieldWidth /2) - (pWidth /2) :: Float
          yOffset = -(gameFieldHeight/2) - (pHeight/2) :: Float
          
          widthScale  = pWidth /  width  grassTile :: Float
          heightScale = pHeight / height grassTile :: Float
          
          scale :: Picture -> Picture
          scale = Scale widthScale heightScale
          
          trans :: Coord -> Picture -> Picture
          trans (x, y) pic = Translate (fx * pWidth) (fy * pHeight) pic
            where fx = fromIntegral x :: Float
                  fy = fromIntegral y :: Float
          
          squareToPic :: Square -> Picture
          squareToPic (Square coord pic Nothing) 
            | coord == selec = trans coord (scale blankTile)
            | otherwise    = trans coord (scale pic)
          squareToPic (Square coord pic (Just unit)) = 
            pictures [
              trans coord (scale pic),
              trans coord (scale unitPic)
            ]


eventHandler :: Event -> World -> World
eventHandler (EventMotion (fx, fy)) w = w {selected = (x+1, y+1)}
    where x = round (fx + gameFieldWidth  / 2) `div` round pWidth
          y = round (fy + gameFieldHeight / 2) `div` round pHeight
eventHandler e w = trace (show e) w


idleCallback :: Float -> World -> World
idleCallback f w = w

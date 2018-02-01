module Render (render) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game (png, boundingBox, Size)

import Data.Matrix

import Common
import World

render :: World -> Picture
render world =  pictures [gf]
    where gf = renderGameField (gameField world) (selected world)


renderGameField :: Matrix Square -> Coord -> Picture
renderGameField matrix selec = translate xOffset yOffset gameField 
    where gameField :: Picture    
          gameField = pictures (squareToPic <$> toList matrix)
    
          xOffset = -(gameFieldWidth /2) - (pWidth /2) :: Float
          yOffset = -(gameFieldHeight/2) - (pHeight/2) :: Float
          
          scale :: Picture -> Picture
          scale = Scale widthScale heightScale
            where widthScale  = pWidth  / width  grassTile :: Float
                  heightScale = pHeight / height grassTile :: Float
          
          trans :: Coord -> Picture -> Picture
          trans (x, y) pic = Translate (fx * pWidth) (fy * pHeight) pic
            where fx = fromIntegral x :: Float
                  fy = fromIntegral y :: Float
          
          squareToPic :: Square -> Picture
          squareToPic (Square coord _ _) | coord == selec = 
            trans selec (scale blankTile)
          squareToPic (Square coord pic Nothing) = trans coord (scale pic)
          squareToPic (Square coord pic (Just unit)) = 
            pictures [
              trans coord (scale pic),
              trans coord (scale unitPic)
            ]


width :: Picture -> Float
width = fst . snd . boundingBox 


height :: Picture -> Float
height = snd . snd . boundingBox

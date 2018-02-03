module Render (render) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game (png, boundingBox, Size)

import Data.Matrix
import Data.Maybe

import Common
import World

render :: World -> Picture
render world =  pictures [ Translate 0 (infoBarHeight/2) gf, 
                           Translate 0 (-gameFieldHeight/2) ib]
    where gf = renderGameField (gameField world)
          ib = renderInfoBar
    
    
renderInfoBar :: Picture
renderInfoBar = pictures [body]
    where rect = Polygon $ rectanglePath infoBarWidth infoBarHeight
          body = color (greyN 0.5) rect 


renderGameField :: Matrix Square -> Picture
renderGameField matrix = translate xOffset yOffset gameField 
    where gameField :: Picture    
          gameField = pictures (squareToPic <$> toList matrix)
    
          -- Note: Images are drawn in the center of the coordinate
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
          squareToPic (Square coord _ _ True) = trans coord (scale blankTile)
          squareToPic (Square coord pic Nothing _) = trans coord (scale pic)
          squareToPic (Square coord pic (Just unit) _) = 
            pictures [
              trans coord (scale pic),
              trans coord (scale unitPic)
            ]


width :: Picture -> Float
width = fst . snd . boundingBox 


height :: Picture -> Float
height = snd . snd . boundingBox

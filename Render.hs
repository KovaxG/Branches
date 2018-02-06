module Render (
    render
) where

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
    where gf = renderGameField $ wGameField world
          ib = renderInfoBar $ wSelected world


renderInfoBar :: Maybe Square -> Picture
renderInfoBar (Just sqr) = pictures [body, 
                                     picTile,
                                     text]
    where rect = Polygon $ rectanglePath infoBarWidth infoBarHeight
          body = color (greyN 0.5) rect 
          text = Translate (-infoBarWidth/2) 0 $ Scale 0.1 0.1 $ Text $ show sqr
          pic = maybe (sPicture sqr) (\_ -> unitPic) (sUnit sqr)
          picTile = trans pic
            where trans = Translate (-fDisplayWidth/2 + (width pic) /2) 0


renderInfoBar Nothing = pictures [body]
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
          squareToPic (Square coord pic maybeUnit s) = 
            pictures $ [background] ++ unit ++ selection
              where background = trans coord (scale pic)
                    selection = if s == True then [trans coord blankTile] else []
                    unit = maybe [] (\_ -> [trans coord (scale unitPic)]) maybeUnit

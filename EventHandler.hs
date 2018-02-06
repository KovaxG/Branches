module EventHandler (eventHandler) where

import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace
import Data.Matrix hiding (trace)
import Data.Maybe

import World
import Common

eventHandler :: Event -> World -> World
eventHandler (EventKey (MouseButton LeftButton) Down _ (fx,fy)) world = 
    world { wGameField = safeSetElem selectSquare (x, y) clearedField,
            wSelected = safeGet x y clearedField 
          }  
    where clearedField :: Matrix Square
          clearedField = clearSelection (wGameField world)
    
          x = toWorldCoord fx 0.0
          y = toWorldCoord fy (-infoBarHeight/2)    
              
          selectSquare :: Square -> Square
          selectSquare s = s {sSelected = True}

eventHandler (EventKey (MouseButton RightButton) Down _ (fx, fy)) world = 
    world { wGameField = maybe unchanged modified (wSelected world),
            wSelected = Nothing 
          }
    where x = toWorldCoord fx 0.0
          y = toWorldCoord fy (-infoBarHeight/2)   
          
          unchanged = clearSelection (wGameField world)
          
          modified sqr 
            | sUnit sqr == Nothing = unchanged
            | distance (x, y) coord > 1 = unchanged
            | otherwise = safeSetElem placeUnit (x, y) unitRemoved
                where unit = fromJust (sUnit sqr)
                      placeUnit s = s { sUnit = Just Unit }
                      coord = sCoord sqr
                      
                      unitRemoved :: Matrix Square
                      unitRemoved = safeSetElem rmUnit coord unchanged
                        where rmUnit s = s { sUnit = Nothing }

eventHandler (EventKey (SpecialKey KeySpace) Down _ _) world = 
    world { wGameField = clearedField,
            wSelected = Nothing
          }
    where clearedField :: Matrix Square
          clearedField = clearSelection $ wGameField world
          
eventHandler e w = trace (show e) w


toWorldCoord :: Float -> Float -> Int
toWorldCoord fa offset = 1 + round (fa + totalOffset) `div` round pWidth
    where totalOffset = (gameFieldWidth / 2) + offset

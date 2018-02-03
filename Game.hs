import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Game (png, boundingBox, Size)

import Debug.Trace
import Data.Matrix hiding (trace)

import Render
import World
import Common

main :: IO ()
main = play displayMode 
            backgroundColor 
            framesPerSecond 
            initialState 
            render
            eventHandler 
            idleCallback


displayMode :: Display
displayMode = InWindow "Game" (displayWidth, displayHeight) (400, 100)


backgroundColor :: Color
backgroundColor = black


eventHandler :: Event -> World -> World
eventHandler (EventKey (MouseButton LeftButton) Down _ (fx,fy)) world = 
    world {gameField = safeSetElem selectSquare (x, y) clearedField}
    where x = 1 + round (fx + offset) `div` round pWidth
              where offset = gameFieldWidth / 2
          y = 1 + round (fy + offset) `div` round pHeight
              where offset = (gameFieldHeight/2) - (infoBarHeight/2)    
              
          selectSquare :: Square -> Square
          selectSquare s = s {selected = True}
          
          clearSelected :: Matrix Square -> Matrix Square
          clearSelected = fmap $ \s -> s {selected = False}
          
          clearedField :: Matrix Square
          clearedField = clearSelected $ gameField world
              
              
eventHandler e w = trace (show e) w


idleCallback :: Float -> World -> World
idleCallback f w = w

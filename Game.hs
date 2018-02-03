import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Game (png, boundingBox, Size)

import Debug.Trace

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
    world {selected = Just (x+1, y+1)}
    where x = round (fx + offset) `div` round pWidth
              where offset = gameFieldWidth / 2
          y = round (fy + offset) `div` round pHeight
              where offset = (gameFieldHeight/2) - (infoBarHeight/2)
eventHandler e w = trace (show e) w


idleCallback :: Float -> World -> World
idleCallback f w = w

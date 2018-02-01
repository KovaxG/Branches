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
eventHandler (EventMotion (fx, fy)) w = w {selected = (x+1, y+1)}
    where x = round (fx + gameFieldWidth  / 2) `div` round pWidth
          y = round (fy + gameFieldHeight / 2) `div` round pHeight
eventHandler e w = trace (show e) w


idleCallback :: Float -> World -> World
idleCallback f w = w

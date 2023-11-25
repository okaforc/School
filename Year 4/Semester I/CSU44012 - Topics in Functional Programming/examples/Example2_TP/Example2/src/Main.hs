import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


main :: IO ()
main = do
  startGUI defaultConfig buttonUI


buttonUI :: Window -> UI ()
buttonUI window = do
  button <- UI.button #+ [string "Click me"]
  getBody window #+ [return button]

  on UI.click button $ \_ -> do
    getBody window #+ [ UI.div #+ [ string "You clicked me!"] ]


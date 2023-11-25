import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import           Data.IORef
import           Control.Monad.Trans (liftIO)


canvasSize = 400

data Modes = Fill | NoFill


main :: IO ()
main = do
  startGUI defaultConfig setup


setup window = do
  return window # set title "Clickable canvas"

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

  mode <- liftIO $ newIORef Fill
  pos <- liftIO $ newIORef (0,0)

  fillMode <- UI.button #+ [string "Fill"]
  emptyMode <- UI.button #+ [string "Hollow"]
  clear <- UI.button #+ [string "Clear"]

  getBody window #+
    [column [element canvas]
    , element fillMode, element emptyMode, element clear]

  on UI.click clear $ const $
    canvas # UI.clearCanvas
  on UI.click fillMode $ \_ ->
    do liftIO $ writeIORef mode Fill
  on UI.click emptyMode $ \_ ->
      do liftIO $ writeIORef mode NoFill
  on UI.mousemove canvas $ \xy ->
      do liftIO $ writeIORef pos xy
  on UI.click canvas $ \_  ->
    do (x,y) <- liftIO $ readIORef pos
       m <- liftIO $ readIORef mode
       case m of
        Fill -> do
                    canvas # set' UI.fillStyle   (UI.htmlColor "black")
                    canvas # UI.fillRect (x,y) 100 100
        NoFill -> do
                   canvas # set' UI.fillStyle   (UI.htmlColor "white")
                   canvas # UI.fillRect (x,y) 100 100

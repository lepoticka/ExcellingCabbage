import Control.Monad
import Graphics.UI.Threepenny.Core
import qualified EXReactive as R

main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  _     <-return window # set title "Excell"

  event <- liftIO newEvent :: UI(Event (R.Coordinates, Maybe Integer), Handler (R.Coordinates, Maybe Integer))
  displayEvent <- liftIO newEvent :: UI(Event String, Handler String)
  let
      -- coordinates of the cells
      coordinates :: [[R.Coordinates]]
      coordinates = [[(a,b)| a <- [1..5]] |   b <- [1..5]]

      outputsUI :: [[UI Element]]
      outputsUI = map (map $ R.ioCell event $ snd displayEvent ) coordinates

  outputs <- mapM sequence outputsUI

  displayEl <- R.displayElement $ fst displayEvent
  getBody window #+ [
    column [ R.makeGrid outputs displayEl
      ]]

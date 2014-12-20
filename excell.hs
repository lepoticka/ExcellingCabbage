import Control.Monad
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified EXReactive as R

main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Excell"

  event <- liftIO newEvent :: UI(Event (R.Coordinates, Integer), Handler (R.Coordinates, Integer))
  inputs <- replicateM 5 (replicateM 5 UI.input)
  let
      coordinates :: [[R.Coordinates]]
      coordinates = [[(a,b)| a <- [1..5]] | b <- [1..5]]

      cooin :: [[(Element, R.Coordinates)]]
      cooin = zipWith zip inputs coordinates

      outputsUI :: [[UI Element]]
      outputsUI = map ( map(\(a,b) -> R.outputCell a b event) )cooin
      outputsUIp :: UI [[Element]]
      outputsUIp = mapM sequence outputsUI

  outputs <- outputsUIp
      
  getBody window #+ [
    column [ R.makeGrid inputs, R.makeGrid outputs
      ]]

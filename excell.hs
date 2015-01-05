import Control.Monad
import Graphics.UI.Threepenny.Core
import qualified EXReactive as R
import EXData

main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  _     <-return window # set title "Excell"

  -- sepperate events for each cell
  let
      cellEventsUI :: [[UI(Event FeedbackValue, Handler FeedbackValue)]]
      cellEventsUI = [[liftIO newEvent | _ <- [1..5]] | _<- [1..5]]
  cellEvents <- mapM sequence cellEventsUI


  displayEvent <- liftIO newEvent :: UI(Event String, Handler String)
  let
      -- joined event
      joinEvent :: Event [FeedbackValue]
      joinEvent = unions $ concatMap (map fst) cellEvents
      -- coordinates of the cells
      coordinates :: [[R.Coordinates]]
      coordinates = [[(a,b)| a <- [1..5]] |   b <- [1..5]]

      outputsUI :: [[UI Element]]
      outputsUI = map (map ( \(a,b) -> R.ioCell joinEvent b (snd displayEvent) a)) $ zipWith zip coordinates $ fmap (fmap snd) cellEvents

  outputs <- mapM sequence outputsUI

  displayEl <- R.displayElement $ fst displayEvent
  getBody window #+ [
    column [ R.makeGrid outputs displayEl
      ]]

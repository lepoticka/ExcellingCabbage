{-# OPTIONS_HADDOCK ignore-exports #-}
module Main (setup, main) where

import Control.Monad
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as E
import qualified EXReactive as R
import EXData

main :: IO()
main =  startGUI defaultConfig setup
-- We start a server by using the startGUI function.
-- because we didn't put any seetings in defaultConfig, we port from environment 8023, we use address 127.0.0.1

-- Whenever a browser connects to the server, the following function will be executed to start the GUI interaction. It builds the initial HTML page.
setup :: Window -> UI ()
setup window = void $ do
  _     <- return window # set title "Excell"
  -- we set the title of the HTML documentation
  getBody window #+ [E.h1 #+ [string "Reactive Excell"]]


  -- separates events for each cell
  let
      width = 5 :: Int
      height = 5 :: Int
      cellEventsUI :: [[UI(Event FeedbackValue, Handler FeedbackValue)]]
      cellEventsUI = [[liftIO newEvent | _ <- [1..width]] | _<- [1..height]]
  cellEvents <- mapM sequence cellEventsUI


  -- make display event
  displayEvent <- liftIO newEvent :: UI(Event String, Handler String)
  let
      -- joined event
      joinEvent :: Event [FeedbackValue]
      joinEvent = unions $ concatMap (map fst) cellEvents
      -- coordinates of the cells
      coordinates :: [[R.Coordinates]]
      coordinates = [[(a,b)| a <- [1..width]] |   b <- [1..height]]

      outputsUI :: [[UI Element]]
      outputsUI = map (map ( \(a,b) -> R.ioCell joinEvent b (snd displayEvent) a)) $ zipWith zip coordinates $ fmap (fmap snd) cellEvents

  -- get cell elements from UI monad
  outputs <- mapM sequence outputsUI

  -- construct html cell distribution
  displayEl <- R.displayElement $ fst displayEvent
  getBody window #+ [column [ R.makeGrid outputs displayEl]]
-- sign "#+" is combinator that allows us to nest elements quickly in the style of a HTML combination library
-- on the left side of "#+" is the body of the HTML element and we attach to it what is on the right side of "#+"

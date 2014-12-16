import Control.Monad    (void)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified ExcellingCabbage as EC

parserBehavior :: Behavior (String -> EC.Expression)
parserBehavior = pure (EC.processParse . EC.parseArithmetic)
--
evaluateBehavior :: Behavior (EC.Expression -> Integer)
evaluateBehavior = pure EC.evaluate

convertBehavior :: Behavior (Integer -> String)
convertBehavior = pure show

referenceBehavior :: Behavior (EC.Expression -> [(Int, Int)])
referenceBehavior = pure getReference
getReference (EC.Constant _) = []
getReference (EC.Cell a b) = [(a, b)]
getReference (EC.Add a b) = getReference a ++ getReference b
getReference (EC.Sub a b) = getReference a ++ getReference b
getReference (EC.Mult a b) = getReference a ++ getReference b
getReference (EC.Division a b) = getReference a ++ getReference b


outputCell :: (Element, (Int, Int), (Event (Int, Int), Handler (Int, Int))) -> UI Element
outputCell (inputCell, coordinates, joinpair) = do 
  buffer    <- stepper (EC.Constant 0) $ apply parserBehavior $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event EC.Expression, Handler EC.Expression)

  let
      flush = fst flushpair
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  refValueBehavior  <- stepper [] $ apply referenceBehavior flush
  exprBehavior      <- stepper (EC.Constant 0) flush

  outputcell        <- UI.input

  finalpair <- liftIO newEvent :: UI(Event String, Handler String)

  let
      join = fst joinpair
      joinHandle = snd joinpair
      final = fst finalpair
      finalHandler = snd finalpair

      fill :: Behavior ((Int, Int) -> Bool)
      fill = pure (\a b->b `elem` a) <*> refValueBehavior
      filteredEvent :: Event (Int, Int)
      filteredEvent = filterApply fill join

  onEvent (UI.valueChange outputcell) $ \_ -> liftIO $ joinHandle coordinates

  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.EC.evaluate =<< currentValue exprBehavior

  finalBehavior <- stepper "0" final

  _ <- element outputcell # sink value finalBehavior

  return outputcell



main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Excell"

    inputCelica     <- UI.input
    outputCelica    <- UI.input

    getBody window #+ [
            column [
                grid [[string "Input celica", element inputCelica]
                    , [string "Output celica", element outputCelica]]
            ]]

    buffer <- stepper (EC.Constant 0) $ apply parserBehavior $ UI.valueChange inputCelica
    flushpair <- liftIO newEvent :: UI(Event EC.Expression, Handler EC.Expression)

    let 
        flush = fst flushpair
        flushHandler = snd flushpair

    onEvent ( filterE (== 13) (UI.keydown inputCelica)) $ \_ -> liftIO $ flushHandler =<< currentValue buffer

    let
        convertEvent        = apply convertBehavior $ apply evaluateBehavior flush

    convertB         <- stepper "0" convertEvent
    element outputCelica # sink value convertB

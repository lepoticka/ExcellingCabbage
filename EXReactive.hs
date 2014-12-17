module EXReactive(
  outputCell
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified EXParser as P

parserBehavior :: Behavior (String -> P.Expression)
parserBehavior = pure (P.processParse . P.parseArithmetic)
--
referenceBehavior :: Behavior (P.Expression -> [(Int, Int)])
referenceBehavior = pure getReference

getReference :: P.Expression -> [(Int, Int)]
getReference (P.Constant _) = []
getReference (P.Cell a b) = [(a, b)]
getReference (P.Add a b) = getReference a ++ getReference b
getReference (P.Sub a b) = getReference a ++ getReference b
getReference (P.Mult a b) = getReference a ++ getReference b
getReference (P.Division a b) = getReference a ++ getReference b


outputCell :: (Element, (Int, Int), (Event (Int, Int), Handler (Int, Int))) -> UI Element
outputCell (inputCell, coordinates, joinpair) = do 
  buffer    <- stepper (P.Constant 0) $ apply parserBehavior $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event P.Expression, Handler P.Expression)

  let
      flush = fst flushpair
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  refValueBehavior  <- stepper [] $ apply referenceBehavior flush
  exprBehavior      <- stepper (P.Constant 0) flush

  outputcell        <- UI.input

  finalpair <- liftIO newEvent :: UI(Event String, Handler String)

  let
      join = fst joinpair
      joinHandle = snd joinpair
      final = fst finalpair
      finalHandler = snd finalpair

      fill :: Behavior ((Int, Int) -> Bool)
      -- fill = pure (\a b->b `elem` a) <*> refValueBehavior
      fill = pure (flip elem) <*> refValueBehavior
      filteredEvent :: Event (Int, Int)
      filteredEvent = filterApply fill join

  onEvent (UI.valueChange outputcell) $ \_ -> liftIO $ joinHandle coordinates
  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue exprBehavior

  finalBehavior <-stepper "0" final

  onChanges exprBehavior $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue exprBehavior

  _ <- element outputcell # sink value finalBehavior
--
  return outputcell

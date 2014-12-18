module EXReactive(
  outputCell
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified EXParser as P

type Coordinates = (Int, Int)


getReference :: P.Expression -> [Coordinates]
getReference (P.Constant _) = []
getReference (P.Cell a b) = [(a,b)]
getReference (P.Add a b) = getReference a ++ getReference b
getReference (P.Sub a b) = getReference a ++ getReference b
getReference (P.Mult a b) = getReference a ++ getReference b
getReference (P.Division a b) = getReference a ++ getReference b


addToAccumulate :: [(Coordinates, Integer)] -> (Coordinates, Integer) -> [(Coordinates, Integer)]
addToAccumulate [] a = [a]
addToAccumulate (x:xs) a
  | fst x == fst a = a:xs
  | otherwise = x : addToAccumulate xs a


processExpression :: [(Coordinates, Integer)] -> P.Expression -> P.Expression
processExpression _ a@(P.Add _ _) = a
processExpression _ a@(P.Sub _ _) = a
processExpression _ a@(P.Mult _ _) = a
processExpression _ a@(P.Division _ _) = a
processExpression _ a@(P.Constant _) = a
processExpression acc (P.Cell a b)
  | null v = P.Constant 0
  | otherwise = P.Constant $ snd $ head v
 where
   cell = (a,b)
   v = filter (\el -> fst el ==  cell) acc


-- return on enter trigered event and event handler
bufferedEvent :: Element -> UI (Event P.Expression, Handler P.Expression)
bufferedEvent inputCell = do
  buffer    <- stepper (P.Constant 0) $ apply (pure (P.processParse . P.parseArithmetic)) $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event P.Expression, Handler P.Expression)

  let
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  return flushpair


-- configure and return output cell for excell
-- outputCell :: (Element, Coordinates, (Event (Int, Int), Handler (Int, Int))) -> UI Element
outputCell :: (Element, Coordinates, (Event (Coordinates, Integer), Handler (Coordinates, Integer))) -> UI Element
outputCell (inputCell, coordinates, joinpair) = do 

  (flush, _)        <- bufferedEvent inputCell

  refValueBehavior  <- stepper [] $ apply (pure getReference) flush
  exprBehavior      <- stepper (P.Constant 0) flush

  outputcell        <- UI.input

  finalpair <- liftIO newEvent :: UI(Event String, Handler String)

  accpair   <- liftIO newEvent :: UI (Event [(Coordinates, Integer)], Handler [(Coordinates, Integer)])

  let
      join = fst joinpair
      joinHandle = snd joinpair
      final = fst finalpair
      finalHandler = snd finalpair

      fill :: Behavior (Coordinates -> Bool)
      fill = pure (flip elem) <*> refValueBehavior
      filteredEvent :: Event Coordinates
      filteredEvent = filterApply fill $ apply (pure fst) join

      acc = fst accpair
      accHandler = snd accpair
      --
  accumulator   <- stepper [] acc

  let
      addToAccBeh :: Behavior ((Coordinates, Integer) -> [(Coordinates, Integer)])
      addToAccBeh = pure addToAccumulate <*> accumulator

      processExprBeh :: Behavior P.Expression
      processExprBeh = pure processExpression <*> accumulator <*> exprBehavior

  onEvent (apply addToAccBeh join) (liftIO . accHandler)

  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh
  onChanges exprBehavior $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh

  finalBehavior <-stepper "0" final

  onEvent (UI.valueChange outputcell) $ \_ -> liftIO $ joinHandle . (\s -> (coordinates, read s)) =<<  currentValue finalBehavior 
  onChanges exprBehavior $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue exprBehavior


  _ <- element outputcell # sink value finalBehavior
--
  return outputcell

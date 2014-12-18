module EXReactive(
  outputCell
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified EXParser as P

getReference :: P.Expression -> [(Int, Int)]
getReference (P.Constant _) = []
getReference (P.Cell a b) = [(a, b)]
getReference (P.Add a b) = getReference a ++ getReference b
getReference (P.Sub a b) = getReference a ++ getReference b
getReference (P.Mult a b) = getReference a ++ getReference b
getReference (P.Division a b) = getReference a ++ getReference b

-- addToAccumulate :: [((Int, Int), Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
-- addToAccumulate [] a = [a]
-- addToAccumulate (x:xs) a
--   | fst x == fst a = a:xs
--   | otherwise = x : addToAccumulate xs a

-- TODO: add Expression to equation class
-- processExpressions :: [(P.Expression, Integer)] -> P.Expression -> P.Expression
-- processExpressions _ a@(P.Add _ _) = a
-- processExpressions _ a@(P.Sub _ _) = a
-- processExpressions _ a@(P.Mult _ _) = a
-- processExpressions _ a@(P.Division _ _) = a
-- processExpressions _ a@(P.Constant _) = a
-- processExpressions acc e@(P.Cell _ _)
--   | null v = P.Constant 0
--   | otherwise = P.Constant $ snd $ head v
--  where
--    v = filter (\el -> fst el == e) acc

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
outputCell :: (Element, (Int, Int), (Event (Int, Int), Handler (Int, Int))) -> UI Element
outputCell (inputCell, coordinates, joinpair) = do 

  (flush, _)        <- bufferedEvent inputCell

  refValueBehavior  <- stepper [] $ apply (pure getReference) flush
  exprBehavior      <- stepper (P.Constant 0) flush

  outputcell        <- UI.input

  finalpair <- liftIO newEvent :: UI(Event String, Handler String)

  -- accpair   <- liftIO newEvent :: UI (Event [(Cell, Int)], Handler [(Cell, Int)])

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

      -- acc = fst accpair
      -- accHandler = snd accpair
      --
  -- accumulator   <- stepper [] acc
  -- onEvent join $ lift IO $ accHandler . (addToAccumulate =<< currentValue accumulator)

  onEvent (UI.valueChange outputcell) $ \_ -> liftIO $ joinHandle coordinates
  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue exprBehavior
  -- onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate.(proccessExpression =<< currentValue accumulator) =<< currentValue exprBehavior - after equation class is added

  finalBehavior <-stepper "0" final

  onChanges exprBehavior $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue exprBehavior
  -- onChanges exprBehavior $ \_ -> liftIO $ finalHandler.show.P.evaluate.(proccessExpression =<< currentValue accumulator) =<< currentValue exprBehavior - after equation class

  _ <- element outputcell # sink value finalBehavior
--
  return outputcell

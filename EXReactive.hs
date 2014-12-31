module EXReactive(
  ioCell,
  makeGrid,
  Coordinates
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char
import Data.Either
import Control.Monad
import EXData
import qualified EXParser as P


-- make display grid
makeGrid :: [[Element]] -> UI Element
makeGrid field = grid $ letters : field2
  where
    width = length $ head field
    letters = string "" : map (string.(: []).chr.(+) (ord 'a' -1)) [1..width]
    field2 = zipWith (:) (map (string.show) [1..width]) $ map (map element) field


-- get cell values from Expression
getReference :: Expression -> Either ExError References
getReference (Constant _) = Right []
getReference (Cell a b) = Right [(a,b)]
getReference (Add a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Sub a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Mult a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Division a b) = liftM2 (++) (getReference a) $ getReference b


-- accumulate new value
addToAccumulate :: FeedbackValues -> FeedbackValue -> FeedbackValues
addToAccumulate [] a = [a]
addToAccumulate (x:xs) a
  | fst x == fst a = a:xs
  | otherwise = x : addToAccumulate xs a


-- remove cell reference in expression
processExpression :: FeedbackValues -> Expression -> Either ExError Expression
processExpression c (Add a b) = liftM2 Add (processExpression c a) (processExpression c b)
processExpression c (Sub a b) = liftM2 Sub (processExpression c a) (processExpression c b)
processExpression c (Mult a b) = liftM2 Mult (processExpression c a) (processExpression c b)
processExpression c (Division a b) = liftM2 Division (processExpression c a) (processExpression c b)
processExpression _ a@(Constant _) = Right a
processExpression [] (Cell _ _) = Left NoValue
processExpression (x:xs) c@(Cell a b)
  | fst x == (a,b) = Right (Constant (snd x))
  |otherwise = processExpression xs c


-- display nformation to the user
showOutput :: Either ExError Integer -> String
showOutput (Left ParseError ) = "PARSE ERROR"
showOutput (Left EvaluationError) = "EVALUATION ERROR"
showOutput (Left ReferenceError) = "REFERENCE ERROR"
showOutput (Left NoValue) = ""
showOutput (Right a) = show a


-- return on enter trigered Expression and event handler
bufferedEvent :: Element -> UI (Event (Either ExError Expression), Handler (Either ExError Expression))
bufferedEvent inputCell = do
  buffer    <- stepper (Left NoValue) $ apply (pure (P.processParse . P.parseArithmetic)) $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event (Either ExError Expression), Handler (Either ExError Expression))

  let
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  return flushpair


-- return filtered coordinates from joined event
getFilteredEvent :: Coordinates -> Event (Either ExError Expression) -> Event FeedbackValue -> Event FeedbackValue
getFilteredEvent coordinates flush join = do

  refValueBehavior  <- stepper (Left NoValue) $ flip apply flush $ pure $ (=<<) getReference

  let
      checkReference :: Either ExError References -> FeedbackValue -> Bool
      checkReference (Left _) _ = False
      checkReference (Right a) v = fst v `elem` a

      checkReferenceBeh = pure checkReference <*> refValueBehavior

  filterApply checkReferenceBeh $ filterE (\e -> fst e /= coordinates) join


--make accumulate Behavior
makeAccumulator:: Event FeedbackValue -> UI (Behavior FeedbackValues)
makeAccumulator join = do
  accpair   <- liftIO newEvent :: UI (Event FeedbackValues, Handler FeedbackValues)

  let
      acc = fst accpair
      accHandler = snd accpair

  accumulator   <- stepper [] acc

  let
      addToAccBeh :: Behavior ( FeedbackValue -> FeedbackValues)
      addToAccBeh = pure addToAccumulate <*> accumulator

  onEvent (apply addToAccBeh join) (liftIO . accHandler)
  return accumulator

-- configure and return output cell for excell
ioCell :: Coordinates -> (Event FeedbackValue, Handler FeedbackValue) -> UI Element
ioCell coordinates joinpair = do

  -- creating elements and events
  outputcell        <- UI.input
  (flush, _)        <- bufferedEvent outputcell

  exprBehavior      <- stepper (Left NoValue) flush
  finalpair <- liftIO newEvent :: UI(Event String, Handler String)

  let
      join = fst joinpair
      joinHandle = snd joinpair
      final = fst finalpair
      finalHandler = snd finalpair

  -- make accumulator
  accumulator <- makeAccumulator join

  let
      -- make behavior for proccessing expression
      processExprBeh :: Behavior Expression
      processExprBeh = (pure processExpression <*> accumulator) <*> exprBehavior

  -- on filtered event evaluate expression and add it for display at final event
  filteredEvent <- getFilteredEvent coordinates flush join
  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh

  --on change of cell formula evaluate formula and add it for display at final event
  onEvent flush $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh

  -- on new value for display add value to joined event
  finalBehavior <-stepper "0" final
  onEvent final $ \_ -> liftIO $ joinHandle . (\s -> (coordinates, read s)) =<<  currentValue finalBehavior 

  -- display finalBehavior in cell
  _ <- element outputcell # sink value finalBehavior
--
  return outputcell

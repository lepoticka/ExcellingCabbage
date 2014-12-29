module EXReactive(
  ioCell,
  makeGrid,
  Coordinates
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char
import qualified EXParser as P


type Coordinates = (Int, Int)
type References = [Coordinates]
type FeedbackValue = (Coordinates, Integer)
type FeedbackValues = [FeedbackValue]
data ExError = ParseError | EvaluationError | NoValue


-- make display grid
makeGrid :: [[Element]] -> UI Element
makeGrid field = grid $ letters : field2
  where
    width = length $ head field
    letters = string "" : map (string.(: []).chr.(+) (ord 'a' -1)) [1..width]
    field2 = zipWith (:) (map (string.show) [1..width]) $ map (map element) field


-- get cell values from Expression
getReference :: P.Expression -> References
getReference (P.Constant _) = []
getReference (P.Cell a b) = [(a,b)]
getReference (P.Add a b) = getReference a ++ getReference b
getReference (P.Sub a b) = getReference a ++ getReference b
getReference (P.Mult a b) = getReference a ++ getReference b
getReference (P.Division a b) = getReference a ++ getReference b


-- accumulate new value
addToAccumulate :: FeedbackValues -> FeedbackValue -> FeedbackValues
addToAccumulate [] a = [a]
addToAccumulate (x:xs) a
  | fst x == fst a = a:xs
  | otherwise = x : addToAccumulate xs a


-- remove cell reference in expression
processExpression :: FeedbackValues -> P.Expression -> P.Expression
processExpression [] (P.Cell _ _) = P.Constant 0
processExpression _ a@(P.Constant _) = a
processExpression c (P.Add a b) = P.Add (processExpression c a) (processExpression c b)
processExpression c (P.Sub a b) = P.Sub (processExpression c a) (processExpression c b)
processExpression c (P.Mult a b) = P.Mult (processExpression c a) (processExpression c b)
processExpression c (P.Division a b) = P.Division (processExpression c a) (processExpression c b)
processExpression (x:xs) c@(P.Cell a b)
  | fst x == (a,b) = P.Constant (snd x)
  |otherwise = processExpression xs c


-- return on enter trigered Expression and event handler
bufferedEvent :: Element -> UI (Event P.Expression, Handler P.Expression)
bufferedEvent inputCell = do
  buffer    <- stepper (P.Constant 0) $ apply (pure (P.processParse . P.parseArithmetic)) $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event P.Expression, Handler P.Expression)

  let
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  return flushpair


-- return filtered coordinates from joined event
getFilteredEvent :: Coordinates -> Event P.Expression -> Event FeedbackValue -> UI (Event Coordinates)
getFilteredEvent coordinates flush join = do
  refValueBehavior  <- stepper [] $ apply (pure getReference) flush

  let
      fill :: Behavior (Coordinates -> Bool)
      fill = pure (flip elem) <*> refValueBehavior
      filteredEvent :: Event Coordinates
      filteredEvent = filterApply fill $ apply (pure fst) $ filterApply (pure (\s-> fst s /= coordinates)) join

  return filteredEvent


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

  exprBehavior      <- stepper (P.Constant 0) flush
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
      processExprBeh :: Behavior P.Expression
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

module EXReactive(
  ioCell,
  makeGrid,
  Coordinates
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char
import qualified EXParser as P


-- type for denoting cell
type Coordinates = (Int, Int)


-- make display grid
makeGrid :: [[Element]] -> UI Element
makeGrid field = grid $ map string letters : [string ( show num) : map element (field !! (num -1))  | num <- [1..height]]
  where
    letters = "" : [[chr  ( ord 'a' + num - 1 )] | num <- [1..width]]
    height = length field
    width = length $ head field


-- get cell values from Expression
getReference :: P.Expression -> [Coordinates]
getReference (P.Constant _) = []
getReference (P.Cell a b) = [(a,b)]
getReference (P.Add a b) = getReference a ++ getReference b
getReference (P.Sub a b) = getReference a ++ getReference b
getReference (P.Mult a b) = getReference a ++ getReference b
getReference (P.Division a b) = getReference a ++ getReference b


-- accumulate new value
addToAccumulate :: [(Coordinates, Integer)] -> (Coordinates, Integer) -> [(Coordinates, Integer)]
addToAccumulate [] a = [a]
addToAccumulate (x:xs) a
  | fst x == fst a = a:xs
  | otherwise = x : addToAccumulate xs a


-- remove cell reference in expression
processExpression :: [(Coordinates, Integer)] -> P.Expression -> P.Expression
processExpression _ a@(P.Constant _) = a
processExpression c (P.Add a b) = P.Add (processExpression c a) (processExpression c b)
processExpression c (P.Sub a b) = P.Sub (processExpression c a) (processExpression c b)
processExpression c (P.Mult a b) = P.Mult (processExpression c a) (processExpression c b)
processExpression c (P.Division a b) = P.Division (processExpression c a) (processExpression c b)
processExpression [] (P.Cell _ _) = P.Constant 0
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
getFilteredEvent :: Coordinates -> Event P.Expression -> Event (Coordinates, Integer)-> UI (Event Coordinates)
getFilteredEvent coordinates flush join = do
  refValueBehavior  <- stepper [] $ apply (pure getReference) flush

  let
      fill :: Behavior (Coordinates -> Bool)
      fill = pure (flip elem) <*> refValueBehavior
      filteredEvent :: Event Coordinates
      filteredEvent = filterApply fill $ apply (pure fst) $ filterApply (pure (\s-> fst s /= coordinates)) join

  return filteredEvent


--make accumulate Behavior
makeAccumulator:: Event (Coordinates, Integer) -> UI (Behavior [(Coordinates, Integer)])
makeAccumulator join = do
  accpair   <- liftIO newEvent :: UI (Event [(Coordinates, Integer)], Handler [(Coordinates, Integer)])

  let
      acc = fst accpair
      accHandler = snd accpair

  accumulator   <- stepper [] acc

  let
      addToAccBeh :: Behavior ((Coordinates, Integer) -> [(Coordinates, Integer)])
      addToAccBeh = pure addToAccumulate <*> accumulator

  onEvent (apply addToAccBeh join) (liftIO . accHandler)
  return accumulator

-- configure and return output cell for excell
ioCell :: Coordinates -> (Event (Coordinates, Integer), Handler (Coordinates, Integer)) -> UI Element
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

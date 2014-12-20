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
processExpression _ a@(P.Constant _) = a
processExpression c (P.Add a b) = P.Add (processExpression c a) (processExpression c b)
processExpression c (P.Sub a b) = P.Sub (processExpression c a) (processExpression c b)
processExpression c (P.Mult a b) = P.Mult (processExpression c a) (processExpression c b)
processExpression c (P.Division a b) = P.Division (processExpression c a) (processExpression c b)
processExpression [] (P.Cell _ _) = P.Constant 0
processExpression (x:xs) c@(P.Cell a b)
  | fst x == (a,b) = P.Constant (snd x)
  |otherwise = processExpression xs c


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
ioCell :: Coordinates -> (Event (Coordinates, Integer), Handler (Coordinates, Integer)) -> UI Element
ioCell coordinates joinpair = do 

  outputcell        <- UI.input
  (flush, _)        <- bufferedEvent outputcell

  refValueBehavior  <- stepper [] $ apply (pure getReference) flush
  exprBehavior      <- stepper (P.Constant 0) flush


  finalpair <- liftIO newEvent :: UI(Event String, Handler String)
  accpair   <- liftIO newEvent :: UI (Event [(Coordinates, Integer)], Handler [(Coordinates, Integer)])

  let
      join = fst joinpair
      joinHandle = snd joinpair
      final = fst finalpair
      finalHandler = snd finalpair
      acc = fst accpair
      accHandler = snd accpair

      fill :: Behavior (Coordinates -> Bool)
      fill = pure (flip elem) <*> refValueBehavior
      filteredEvent :: Event Coordinates
      filteredEvent = filterApply fill $ apply (pure fst) $ filterApply (pure (\s-> fst s /= coordinates)) join

      --
  accumulator   <- stepper [] acc

  let
      addToAccBeh :: Behavior ((Coordinates, Integer) -> [(Coordinates, Integer)])
      addToAccBeh = pure addToAccumulate <*> accumulator

      processExprBeh :: Behavior P.Expression
      processExprBeh = (pure processExpression <*> accumulator) <*> exprBehavior

  finalBehavior <-stepper "0" final

  onEvent (apply addToAccBeh join) (liftIO . accHandler)
  onEvent filteredEvent $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh
  onEvent flush $ \_ -> liftIO $ finalHandler.show.P.evaluate =<< currentValue processExprBeh
  onEvent final $ \_ -> liftIO $ joinHandle . (\s -> (coordinates, read s)) =<<  currentValue finalBehavior 

  _ <- element outputcell # sink value finalBehavior
--
  return outputcell


makeGrid :: [[Element]] -> UI Element
makeGrid field = grid $ map string letters : [string ( show num) : map element (field !! (num -1))  | num <- [1..height]]
  where
    letters = "" : [[chr  ( ord 'a' + num - 1 )] | num <- [1..width]]
    height = length field
    width = length $ head field

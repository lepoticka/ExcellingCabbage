module EXReactive(
  ioCell,
  makeGrid,
  Coordinates
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char
import Control.Monad
import EXData
import Data.Maybe
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
  | fst x == (a,b) && isJust (snd x) = Right (Constant (fromJust $ snd x))
  | fst x == (a,b) && isNothing (snd x) = Left ReferenceError
  |otherwise = processExpression xs c


-- display nformation to the user
showOutput :: Either ExError Expression -> String
showOutput (Left ParseError ) = "PARSE ERROR"
showOutput (Left EvaluationError) = "EVALUATION ERROR"
showOutput (Left ReferenceError) = "REFERENCE ERROR"
showOutput (Left NoValue) = ""
showOutput (Right (Constant a)) = show a
showOutput (Right (Add _ _)) = "INTERNAL ERROR"
showOutput (Right (Sub _ _)) = "INTERNAL ERROR"
showOutput (Right (Mult _ _)) = "INTERNAL ERROR"
showOutput (Right (Division _ _)) = "INTERNAL ERROR"
showOutput (Right (Cell _ _)) = "INTERNAL ERROR"

toFeedback :: Coordinates -> Either ExError Expression -> FeedbackValue
toFeedback coordinates (Left _) = (coordinates, Nothing)
toFeedback coordinates (Right (Constant a)) = (coordinates, Just a)
toFeedback coordinates (Right (Cell _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Add _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Sub _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Mult _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Division _ _)) = (coordinates, Nothing)


-- return on enter trigered Expression and event handler
bufferedEvent :: Element -> UI (Event (Either ExError Expression), Handler (Either ExError Expression))
bufferedEvent inputCell = do
  buffer    <- stepper (Left NoValue) $ apply (pure P.parseArithmetic) $ UI.valueChange inputCell
  flushpair <- liftIO newEvent :: UI(Event (Either ExError Expression), Handler (Either ExError Expression))

  let
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer

  return flushpair


--make accumulate Behavior
makeAccumulator:: Event FeedbackValue -> UI (Behavior FeedbackValues)
makeAccumulator joinnEvent = do
  accpair   <- liftIO newEvent :: UI (Event FeedbackValues, Handler FeedbackValues)

  let
      acc = fst accpair
      accHandler = snd accpair

  accumulator   <- stepper [] acc

  let
      addToAccBeh :: Behavior ( FeedbackValue -> FeedbackValues)
      addToAccBeh = pure addToAccumulate <*> accumulator

  onEvent (apply addToAccBeh joinnEvent) (liftIO . accHandler)
  return accumulator

-- configure and return output cell for excell
ioCell :: Coordinates -> (Event FeedbackValue, Handler FeedbackValue) -> UI Element
ioCell coordinates joinpair = do

  outputcell    <- UI.input
  (flush, _)    <- bufferedEvent outputcell

  inputHold     <- stepper (Left NoValue) flush

  let
      filteredJoinEvent = filterE (\e -> fst e /= coordinates) $ fst joinpair

  accumulator   <- makeAccumulator filteredJoinEvent

  let
      processExprBeh= pure (>>=) <*> inputHold <*> (pure processExpression <*> accumulator)
      evaluateExprBeh = pure (>>=) <*> processExprBeh <*> pure P.evaluate
      showOutputBeh = pure showOutput <*> evaluateExprBeh
      referenceBeh = pure (>>=) <*> inputHold <*> pure getReference

      checkReference :: Either ExError References -> FeedbackValue -> Bool
      checkReference (Left _) _ = False
      checkReference (Right a) v = fst v `elem` a

      checkReferenceBeh = pure checkReference <*> referenceBeh
      filteredReferenceEvent = filterApply checkReferenceBeh filteredJoinEvent



  _ <- element outputcell # sink value showOutputBeh

  onEvent  filteredReferenceEvent $ \_ -> liftIO . snd joinpair . toFeedback coordinates =<< currentValue evaluateExprBeh
  onEvent  flush  $ \_ -> liftIO . snd joinpair . toFeedback coordinates =<< currentValue evaluateExprBeh

  return outputcell

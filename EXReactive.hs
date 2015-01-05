module EXReactive(
  ioCell,
  makeGrid,
  Coordinates,
  displayElement
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char
import Control.Monad
import EXData
import Data.Maybe
import qualified EXParser as P


-- make display grid
makeGrid :: [[Element]] -> Element -> UI Element
makeGrid field display= grid $ displayL : letters : field2
  where
    width = length $ head field
    letters = string "" : map (string.(: []).chr.(+) (ord 'a' -1)) [1..width]
    field2 = zipWith (:) (map (string.show) [1..width]) $ map (map element) field
    displayL = [string "Formula: ", element display]


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
makeAccumulator:: Event FeedbackValues -> UI (Behavior FeedbackValues)
makeAccumulator joinEvent = do
  accpair   <- liftIO newEvent :: UI (Event FeedbackValues, Handler FeedbackValues)

  let
      acc = fst accpair
      accHandler = snd accpair

  accumulator   <- stepper [] acc

  onEvent joinEvent $ \e -> liftIO . accHandler . flip (foldl addToAccumulate) e =<< currentValue accumulator

  return accumulator

-- configure and return output cell for excell
ioCell :: Event FeedbackValues -> Handler FeedbackValue -> Handler String -> Coordinates -> UI Element
ioCell joinEvent myHandler displayHandler coordinates= do

  outputcell    <- UI.input
  (flush, flushHandle)    <- bufferedEvent outputcell
  inputHold     <- stepper (Left NoValue) flush
--
  let
      filteredJoinEvent = filterE (/= []) $ fmap (filter (\a -> fst a /= coordinates)) joinEvent

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
      filteredReferenceEvent :: Event FeedbackValues
      filteredReferenceEvent = filterE (/=[]) $ (pure filter <*> checkReferenceBeh) <@> filteredJoinEvent


  _ <- element outputcell # sink value showOutputBeh

  onEvent  filteredReferenceEvent $ \_ -> liftIO . myHandler . toFeedback coordinates =<< currentValue evaluateExprBeh
 
  -- display formula
  let
      formatedFormula :: Either ExError Expression -> String
      formatedFormula formula = chr (ord 'a' -1 + fst coordinates) : show (snd coordinates) ++ ": " ++ exprToStr formula

  onEvent flush $ liftIO . displayHandler.formatedFormula
  onEvent (UI.focus outputcell) $ \_ -> liftIO . displayHandler.formatedFormula =<< currentValue inputHold
  onEvent  flush  $ \_ -> liftIO . myHandler . toFeedback coordinates =<< currentValue evaluateExprBeh

  _ <- setKeyIncrement outputcell flushHandle inputHold

  return outputcell


-- convert Expression to string
exprToStr :: Either ExError Expression -> String
exprToStr a@(Left _) = showOutput a
exprToStr a@(Right(Constant _)) = showOutput a
exprToStr (Right(Add a b)) = "(" ++ exprToStr (Right a) ++ " + " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Sub a b)) = "(" ++ exprToStr (Right a) ++ " - " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Mult a b)) = "(" ++ exprToStr (Right a) ++ " * " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Division a b)) = "(" ++ exprToStr (Right a) ++ " / " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Cell a b)) = chr (ord 'a' -1 + a) : show b


-- key arrow increment
setKeyIncrement :: Element -> Handler (Either ExError Expression) -> Behavior (Either ExError Expression) -> UI ()
setKeyIncrement outputcell flushHandle inputHold = do
  -- timer
  timer <- UI.timer # set UI.interval 200
  deltaPair <- liftIO newEvent :: UI(Event Integer, Handler Integer)
  deltaBeh  <- stepper 0 $ fst deltaPair

  let
      updownEvent = fmap (\a -> if a == 38 then 1 else -1) $ filterE (\e -> e == 38 || e == 40) $ UI.keydown outputcell

      runTimer = do
        running <- get UI.running timer
        unless running $ UI.start timer

      incrementExpr :: Either ExError Expression -> Integer -> Either ExError Expression
      incrementExpr (Left a) _ = Left a
      incrementExpr (Right (Constant a)) b = Right (Constant (a+b))
      incrementExpr v@(Right (Add _ _ )) _ = v
      incrementExpr v@(Right (Sub _ _ )) _ = v
      incrementExpr v@(Right (Mult _ _ )) _ = v
      incrementExpr v@(Right (Division _ _ )) _ = v
      incrementExpr v@(Right (Cell _ _ )) _ = v


  -- start/stop timer
  onEvent updownEvent $ const runTimer
  onEvent (filterE (\e -> e == 38 || e == 40) $ UI.keyup outputcell) $ \_ -> UI.stop timer

  -- change delta value
  onEvent updownEvent $ \e -> liftIO $ snd deltaPair e
  onEvent (UI.keyup outputcell) $ \_ -> liftIO $ snd deltaPair 0

  -- increment expression
  onEvent (UI.tick timer) $ \_ -> liftIO $ flushHandle =<< currentValue (pure incrementExpr <*> inputHold <*> deltaBeh)


displayElement :: Event String  -> UI Element
displayElement displayEvent = do
  displayEl    <- UI.input
  displayBeh        <- stepper "" displayEvent
  _                 <- element displayEl # sink value displayBeh
  return displayEl

{-# OPTIONS_HADDOCK ignore-exports #-}

module EXReactive(
  ioCell,
  makeGrid,
  Coordinates,
  displayElement
) where

import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Data.Char
import Control.Monad
import EXData
import Data.Maybe
import qualified EXParser as P

-- | We  display a grid by using function makeGrid that takes a list of lists and Element and returns an UI Element.
makeGrid :: [[Element]] -> Element -> UI Element
-- using function makeGrid enables us library Graphics.UI.Threepenny
makeGrid field display= grid $ displayL : letters : field2
  where
    width = length $ head field
    letters = string "" : map (string.(: []).chr.(+) (ord 'a' -1)) [1..width]
    field2 = zipWith (:) (map (string.show) [1..width]) $ map (map element) field
    displayL = [string "Formula: ", element display]

-- | Function getReference gets cell references from Expression.
getReference :: Expression -> Either ExError References
getReference (Constant _) = Right []
getReference (Cell a b) = Right [(a,b)]
getReference (Add a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Sub a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Mult a b) = liftM2 (++) (getReference a) $ getReference b
getReference (Division a b) = liftM2 (++) (getReference a) $ getReference b
-- so this function checks if this expression depends on other cells

-- | Function addToAccumulate generates updated knowladge about other cells.
addToAccumulate :: FeedbackValues -> FeedbackValue -> FeedbackValues
addToAccumulate [] a = [a]
addToAccumulate (x:xs) a
  | fst x == fst a = a:xs
  | otherwise = x : addToAccumulate xs a

-- | This function generates reference cells in expression from current knowladge.
processExpression :: FeedbackValues -> Expression -> Either ExError Expression 
processExpression c (Add a b) = liftM2 Add (processExpression c a) (processExpression c b)
processExpression c (Sub a b) = liftM2 Sub (processExpression c a) (processExpression c b)
processExpression c (Mult a b) = liftM2 Mult (processExpression c a) (processExpression c b)
processExpression c (Division a b) = liftM2 Division (processExpression c a) (processExpression c b)
processExpression _ a@(Constant _) = Right a
processExpression [] (Cell _ _) = Left ReferenceError
-- it can happen that we come to a cell about which we don't have any information
-- so this function helps us recognize this situation 
-- Note: information is basically everything that you have typed in a cell
processExpression (x:xs) c@(Cell a b)
  | fst x == (a,b) && isJust (snd x) = Right (Constant (fromJust $ snd x))
  | fst x == (a,b) && isNothing (snd x) = Left ReferenceError
  |otherwise = processExpression xs c

-- | Function showOutput displays Information to the user that is in a cell.
showOutput :: Either ExError Expression -> String
-- here we process the final expression and return a string, if its not an error
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

-- | Function toFeedback converts information to feedback information important for other cells.
toFeedback :: Coordinates -> Either ExError Expression -> FeedbackValue             
-- here we return information about changes
toFeedback coordinates (Left _) = (coordinates, Nothing)
toFeedback coordinates (Right (Constant a)) = (coordinates, Just a)
toFeedback coordinates (Right (Cell _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Add _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Sub _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Mult _ _)) = (coordinates, Nothing)
toFeedback coordinates (Right (Division _ _)) = (coordinates, Nothing)

-- | With function bufferedEvent we return on enter trigered event and event handler for error or expression.
bufferedEvent :: Element 
-- an event handler is a function that takes an event value and performs some computation
              -> UI (Event (Either ExError Expression), Handler (Either ExError Expression)) -- ^ created new event
bufferedEvent inputCell = do
  buffer    <- stepper (Left NoValue) $ apply (pure P.parseArithmetic) $ UI.valueChange inputCell
-- With steper function we construct a time-varying function from an initial value and a stream of new values.
-- pure is a function that makes behaviour that has a value of function and valueChange takes an element and creates an event. 
-- apply a time-varying function to a stream of events
  flushpair <- liftIO newEvent :: UI(Event (Either ExError Expression), Handler (Either ExError Expression))

  let
      flushHandle = snd flushpair

  onEvent( filterE (==13) (UI.keydown inputCell)) $ \_ -> liftIO $ flushHandle =<< currentValue buffer
  return flushpair
-- onEvent function register an UI action to be executed whenever the event happens.
-- With function filterE we return event occurrences that fulfill the predicate(and discard the rest), so we return values that are equal 13, which is an ascci value for button ENTER.
-- '=<<' means that we sequently compose two actions, passing any value produced by the first as an argument to the second

-- | Function makeAccumulator makes behavior for accumulating knowladge about other cells.
makeAccumulator:: Event FeedbackValues -> UI (Behavior FeedbackValues)
makeAccumulator joinEvent = do
  accpair   <- liftIO newEvent :: UI (Event FeedbackValues, Handler FeedbackValues)

  let
      acc = fst accpair
      accHandler = snd accpair

  accumulator   <- stepper [] acc
  onEvent joinEvent $ \e -> liftIO . accHandler . flip (foldl addToAccumulate) e =<< currentValue accumulator
  return accumulator


-- | Function ioCell configures and returns output cell for excel.
ioCell :: Event FeedbackValues -> Handler FeedbackValue -> Handler String -> Coordinates -> UI Element
ioCell joinEvent myHandler displayHandler coordinates= do

  outputcell    <- UI.input
  (flush, flushHandle)    <- bufferedEvent outputcell
  inputHold     <- stepper (Left NoValue) flush

  let
      -- filter global event stream for all cell information with respect to coordinates
      filteredJoinEvent = filterE (/= []) $ fmap (filter (\a -> fst a /= coordinates)) joinEvent
      -- filterE we discards empty lists

  -- make accumulator on filtering global event stream
  accumulator <- makeAccumulator filteredJoinEvent

  let
      processExprBeh = pure (>>=) <*> inputHold <*> (pure processExpression <*> accumulator)
      evaluateExprBeh = pure (>>=) <*> processExprBeh <*> pure P.evaluate
      showOutputBeh = pure showOutput <*> evaluateExprBeh
      referenceBeh = pure (>>=) <*> inputHold <*> pure getReference
      -- | Function checkReference checks References for FeedbackValue.
      checkReference :: Either ExError References -> FeedbackValue -> Bool
      -- so here we check if FeedbackValue is in references, if it is not, it will return False
      checkReference (Left _) _ = False
      checkReference (Right a) v = fst v `elem` a

      checkReferenceBeh = pure checkReference <*> referenceBeh
      filteredReferenceEvent :: Event FeedbackValues
      -- we filter references that are referencing to some other references

      -- filter global event stream according to references in expression
      filteredReferenceEvent = filterE (/=[]) $ (pure filter <*> checkReferenceBeh) <@> filteredJoinEvent

  -- sincronize values in cell with values in behavior
  _ <- element outputcell # sink value showOutputBeh

  -- add information about the change of event for propagating changes about current cell
  onEvent  filteredReferenceEvent $ \_ -> liftIO . myHandler . toFeedback coordinates =<< currentValue evaluateExprBeh
  onEvent  flush  $ \_ -> liftIO . myHandler . toFeedback coordinates =<< currentValue evaluateExprBeh
 
  let
      -- | We display a formula with function formatedFormula.
      formatedFormula :: Either ExError Expression -> String
      formatedFormula formula = chr (ord 'a' -1 + fst coordinates) : show (snd coordinates) ++ ": " ++ exprToStr formula

  -- triggers for propagating change to display for formulas
  onEvent flush $ liftIO . displayHandler.formatedFormula
  onEvent (UI.focus outputcell) $ \_ -> liftIO . displayHandler.formatedFormula =<< currentValue inputHold

  -- configure timers and events for changing values with up and down arrow
  _ <- setKeyIncrement outputcell flushHandle inputHold

  return outputcell


-- | Function exprToStr convertes Expression to a string.
exprToStr :: Either ExError Expression -> String
exprToStr a@(Left _) = showOutput a
exprToStr a@(Right(Constant _)) = showOutput a
exprToStr (Right(Add a b)) = "(" ++ exprToStr (Right a) ++ " + " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Sub a b)) = "(" ++ exprToStr (Right a) ++ " - " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Mult a b)) = "(" ++ exprToStr (Right a) ++ " * " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Division a b)) = "(" ++ exprToStr (Right a) ++ " / " ++ exprToStr (Right b) ++ ")"
exprToStr (Right(Cell a b)) = chr (ord 'a' -1 + a) : show b


-- | Function configures increment/decrement with arrow keys.
setKeyIncrement :: Element -> Handler (Either ExError Expression) -> Behavior (Either ExError Expression) -> UI ()
setKeyIncrement outputcell flushHandle inputHold = do
  -- get timer with interval 200 ms
  timer <- UI.timer # set UI.interval 200

  -- We make event/behavior for delta value.
  deltaPair <- liftIO newEvent :: UI(Event Integer, Handler Integer)
  deltaBeh  <- stepper 0 $ fst deltaPair

  let
      -- transform keydown event to delta values
      updownEvent = fmap (\a -> if a == 38 then 1 else -1) $ filterE (\e -> e == 38 || e == 40) $ UI.keydown outputcell

      -- run timer only once when button is pressed
      runTimer = do
        running <- get UI.running timer
        unless running $ UI.start timer

      -- | Function incrementExpr only holds constant function.
      incrementExpr :: Either ExError Expression -> Integer -> Either ExError Expression
      incrementExpr (Left a) _ = Left a
      incrementExpr (Right (Constant a)) b = Right (Constant (a+b))
      incrementExpr v@(Right (Add _ _ )) _ = v
      incrementExpr v@(Right (Sub _ _ )) _ = v
      incrementExpr v@(Right (Mult _ _ )) _ = v
      incrementExpr v@(Right (Division _ _ )) _ = v
      incrementExpr v@(Right (Cell _ _ )) _ = v
      -- this function is returning situations when we only have constant numbers, but constant number can come also as an expression "2+3", "4*5",...


  -- start/stop timer
  onEvent updownEvent $ const runTimer
  onEvent (filterE (\e -> e == 38 || e == 40) $ UI.keyup outputcell) $ \_ -> UI.stop timer

  -- change delta value
  onEvent updownEvent $ \e -> liftIO $ snd deltaPair e
  onEvent (UI.keyup outputcell) $ \_ -> liftIO $ snd deltaPair 0

  -- increment expression
  onEvent (UI.tick timer) $ \_ -> liftIO $ flushHandle =<< currentValue (pure incrementExpr <*> inputHold <*> deltaBeh)


-- | Function generates displayed element.
displayElement :: Event String -> UI Element
displayElement displayEvent = do
  displayEl    <- UI.input
  displayBeh        <- stepper "" displayEvent
  _                 <- element displayEl # sink value displayBeh
  element displayEl # set enabled False

-- We imported two diagrams, that are showing structure of how our program is working. We can think of it as behind the scenes of our code.
-- <<excellingcabbage.pdf>>
-- <<display.pdf>>


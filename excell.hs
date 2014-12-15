import Control.Monad    (void, replicateM)
import Data.Maybe
import Text.Printf
import Safe             (readMay)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified ExcellingCabbage as EC
import Data.Char

parserBehavior :: Behavior (String -> EC.Expression)
parserBehavior = pure (EC.processParse . EC.parseArithmetic)
--
evaluateBehavior :: Behavior (EC.Expression -> Integer)
evaluateBehavior = pure EC.evaluate

convertBehavior :: Behavior (Integer -> String)
convertBehavior = pure show

makeL :: [[Element]] -> UI Element
makeL field = grid $ map string letters : [string ( show num) : map element (field !! (num -1))  | num <- [1..height]]
  where
    letters = "" : [chr  ( ord 'a' + num - 1 ) :  show num | num <- [1..width]]
    height = length field
    width = length $ head field


main::IO()
main =  startGUI defaultConfig $ setup 5 5

setup :: Int -> Int -> Window -> UI ()
setup width heigth window = void $ do
  return window # set title "Excell"

  elementField  <- replicateM heigth $ replicateM width UI.input

  getBody window #+ [ column [makeL elementField]]


    -- inputCelica     <- UI.input
    -- outputCelica    <- UI.input
    --
    -- getBody window #+ [
    --         column [
    --             grid [[string "Input celica", element inputCelica]
    --                 , [string "Output celica", element outputCelica]]
    --         ]]
    --
    -- let 
    --     procEvent           =  apply parserBehavior $ UI.valueChange inputCelica
    --     evalEvent           =  apply evaluateBehavior procEvent
    --     convertEvent        = apply convertBehavior evalEvent
    --
    -- procBehavior            <- stepper (EC.Constant 0) procEvent
    -- evalBehavior            <- stepper 0 evalEvent
    -- convertBehavior         <- stepper "0" convertEvent
    --
    -- -- inputIn <- stepper "0" $ UI.valueChange inputCelica
    --
    -- element outputCelica # sink value convertBehavior

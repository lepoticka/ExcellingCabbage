import Control.Monad    (void)
import Data.Maybe
import Text.Printf
import Safe             (readMay)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified ExcellingCabbage as EC

parserBehavior :: Behavior (String -> EC.Expression)
parserBehavior = pure (EC.processParse . EC.parseArithmetic)
--
evaluateBehavior :: Behavior (EC.Expression -> Integer)
evaluateBehavior = pure EC.evaluate

convertBehavior :: Behavior (Integer -> String)
convertBehavior = pure show



main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Excell"

    inputCelica     <- UI.input
    outputCelica    <- UI.input

    getBody window #+ [
            column [
                grid [[string "Input celica", element inputCelica]
                    , [string "Output celica", element outputCelica]]
            ]]

    let 
        procEvent           =  apply parserBehavior $ UI.valueChange inputCelica
        evalEvent           =  apply evaluateBehavior procEvent
        convertEvent        = apply convertBehavior evalEvent

    procBehavior            <- stepper (EC.Constant 0) procEvent
    evalBehavior            <- stepper 0 evalEvent
    convertBehavior         <- stepper "0" convertEvent

    -- inputIn <- stepper "0" $ UI.valueChange inputCelica

    element outputCelica # sink value convertBehavior

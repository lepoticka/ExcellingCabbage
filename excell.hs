import Control.Monad    (void)
import Data.Maybe
import Text.Printf
import Safe             (readMay)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified ExcellingCabbage as EC


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
        parserBehavior      = pure (EC.parse EC.expr)
        evaluateBehavior    = pure $ show . EC.evaluate

        procEvent           =  apply parserBehavior $ UI.valueChange inputCelica
        evalEvent           =  apply evaluateBehavior procEvent

    procBehavior        <- stepper (EC.Constant 0) procEvent
    evalBehavior        <-  stepper "0" evalEvent

    
    -- inputIn <- stepper "0" $ UI.valueChange inputCelica

    element outputCelica # sink value evalBehavior

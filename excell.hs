import Control.Monad    (void)
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

    buffer <- stepper (EC.Constant 0) $ apply parserBehavior $ UI.valueChange inputCelica
    flushpair <- liftIO newEvent :: UI(Event EC.Expression, Handler EC.Expression)

    let 
        flush = fst flushpair
        flushHandler = snd flushpair

    onEvent ( filterE (==13) (UI.keydown inputCelica)) $ \_ -> liftIO $ flushHandler =<< currentValue buffer

    let
        convertEvent        = apply convertBehavior $ apply evaluateBehavior flush

    convertB         <- stepper "0" convertEvent
    element outputCelica # sink value convertB

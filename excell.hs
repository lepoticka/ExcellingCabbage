import Control.Monad    (void)
import Data.Maybe
import Text.Printf
import Safe             (readMay)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Tuple

import qualified ExcellingCabbage as EC
import Reactive.Threepenny

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

   -- naredi flush event String
    -- event ki reagira na enter
    -- on enter event daj na flushevent buffer
    flushpair <- liftIO newEvent :: UI(Event EC.Expression, Handler EC.Expression)
    let 
        flush = fst flushpair
        flushHandler = snd flushpair

    -- na event podigni event v flush event streamu
    onEvent ( filterE (==13) (UI.keydown inputCelica)) $ \_ -> liftIO $ flushHandler =<< currentValue buffer

    let
        evalEvent           =  apply evaluateBehavior flush
        convertEvent        = apply convertBehavior evalEvent

    convertB         <- stepper "0" convertEvent

    element outputCelica # sink value convertB

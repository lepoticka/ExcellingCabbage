import Control.Monad    (void)
import  qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified EXReactive as R

main::IO()
main =  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Excell"

  inputCelica     <- UI.input
  inputCelica2     <- UI.input
  event <- liftIO newEvent :: UI(Event (Int, Int), Handler (Int,Int))
  outputCelica  <- R.outputCell (inputCelica, (1,1), event)
  outputCelica2  <- R.outputCell (inputCelica2, (1,2), event)

  

  getBody window #+ [
    column [
      grid [[string "Input celica", element inputCelica, element inputCelica2]
        , [string "Output celica", element outputCelica, element outputCelica2]]
      ]]

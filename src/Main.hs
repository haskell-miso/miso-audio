{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bool (bool)
import Miso 
import Miso.Lens
import Miso.String (MisoString)

import Audio

-- TODO detect when audio ended
-- TODO toMisoString format (5.0e-2 -> 0.05) ?

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

theSong :: MisoString
theSong = "short.mp3"

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

newtype Model = Model
  { _modelEnded :: Maybe Bool
  } deriving (Eq)

mkModel :: Model
mkModel = Model Nothing

data Action 
  = ActionAudioClick MisoString
  | ActionAudioEnded

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

modelEnded :: Lens Model (Maybe Bool)
modelEnded = lens _modelEnded $ \record field -> record { _modelEnded = field }

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ div_ []
      [ audio_ [ id_ "audio1", src_ theSong, onEnded ActionAudioEnded ] []
      , button_ [ id_ "button1", onClick (ActionAudioClick "audio1") ] [ text "play" ]
      , text (maybe "" (bool "running" "ended") (model^.modelEnded))
      ]
  ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate ActionAudioEnded = do
  io_ (consoleLog "audio ended")
  modelEnded .= Just True

handleUpdate (ActionAudioClick str) = do
  io_ (consoleLog "audio playing")
  io_ (playStr str)
  modelEnded .= Just False

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  let model = mkModel
  startComponent Component
    { model = model
    , update = handleUpdate
    , view = handleView
    , subs = []
    , events = defaultEvents
    , styles = []
    , mountPoint = Nothing
    , logLevel = Off
    , initialAction = Nothing
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif



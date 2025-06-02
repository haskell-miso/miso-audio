{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (forM_)
-- import Data.Bool (bool)

import Miso 
import Miso.Lens
import Miso.String (MisoString)

import Audio
import Model

-- TODO toMisoString format (5.0e-2 -> 0.05) ?

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

thePlaylist :: [MisoString]
thePlaylist =
  [ "roblox-minecraft-fortnite-video-game-music-299145.mp3"
  , "kids-game-gaming-background-music-297733.mp3"
  , "puzzle-game-bright-casual-video-game-music-249202.mp3"
  , "short.mp3"
  ]

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Action 
  = ActionAudioClick MisoString
  | ActionAudioEnded

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

fmtAudioId :: MisoString -> MisoString
fmtAudioId name = "audio_" <> name

handleView :: Model -> View Action
handleView model = div_ [] 
  [ div_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-audio-test" ] [ text "source" ]
      , text " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-audio-test/" ] [ text "demo" ]
      ]
  , ul_ [] (map fmtAudio (model^.modelPlaylist))
  ]
  where

    fmtAudio filename = 
      let audioId = fmtAudioId filename
      in li_ []
        [ audio_ [ id_ audioId, src_ filename, onEnded ActionAudioEnded ] []
        , button_ [ onClick (ActionAudioClick audioId) ] [ text (playOrPause audioId) ]
        -- , button_ [  ] [ text "reload" ]
        , text (" " <> filename)
        ]

    playOrPause audioId = 
      if model^.modelPlaying == Just audioId then "pause" else " play "

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate ActionAudioEnded = 
  modelPlaying .= Nothing

handleUpdate (ActionAudioClick audioId1) = do
  mPlaying <- use modelPlaying
  case mPlaying of
    Nothing -> do
      io_ (getElementById audioId1 >>= play . Audio)
      modelPlaying .= Just audioId1
    Just audioId0 -> do
      io_ (getElementById audioId0 >>= pause . Audio)
      if audioId0 == audioId1
      then modelPlaying .= Nothing
      else do
        io_ (getElementById audioId1 >>= play . Audio)
        modelPlaying .= Just audioId1

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  let model = mkModel thePlaylist
  let app = defaultComponent model handleUpdate handleView
  startComponent app
    { events = defaultEvents <> audioVideoEvents
    , logLevel = DebugAll
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


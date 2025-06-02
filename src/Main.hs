{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Bool (bool)
import Control.Monad (forM_)
-- import Data.Time.Format
import Miso 
import Miso.Lens
import Miso.String (MisoString)

import Model
import MyMiso

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
  = ActionAskPlay Song
  | ActionAskEnded
  | ActionAskLoad Song

-- ActionAskVolume MisoString
-- ActionSetVolume Double
-- ActionSetEnded Bool
-- ActionSetDuration Double

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ div_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-audio-test" ] [ text "source" ]
      , text " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-audio-test/" ] [ text "demo" ]
      ]
  , ul_ [] (map fmtSong (model^.modelSongs))
  ]
  where

    playOrPause s = 
      let mSong = _playingSong <$> model^.modelPlaying
      in if mSong == Just s then "pause" else " play "

    fmtSong s = li_ [] 
        [ audio_ [ id_ (s^.songAudioId), src_ (s^.songFilename), onEnded ActionAskEnded ] []
        , button_ [ onClick (ActionAskPlay s) ] [ text (playOrPause s) ]
        , button_ [ onClick (ActionAskLoad s) ] [ text "reload" ]
        , text (" " <> s^.songFilename)
        ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionAskPlay s) = do
  mPlaying <- use modelPlaying
  forM_ mPlaying $ \p -> 
    io_ (getElementById (p^.playingSong^.songAudioId) >>= pause . Audio)
  if (_playingSong <$> mPlaying) == Just s
    then modelPlaying .= Nothing
    else do
      modelPlaying .= Just (mkPlaying s)
      -- io (ActionSetDuration <$> duration audio)
      -- io (ActionSetVolume <$> getVolume audio)
      -- io (ActionSetEnded <$> ended audio)
      io_ (getElementById (s^.songAudioId) >>= play . Audio)

handleUpdate ActionAskEnded = 
  modelPlaying .= Nothing

handleUpdate (ActionAskLoad s)  = do
  io_ (getElementById (s^.songAudioId) >>= load . Audio)
  mPlaying <- use modelPlaying
  forM_ mPlaying $ \p -> 
    io_ (getElementById (p^.playingSong^.songAudioId) >>= play . Audio)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  let model = mkModel (mkSong <$> thePlaylist)
      app = defaultComponent model handleUpdate handleView
  startComponent app
    { events = defaultEvents <> audioVideoEvents
    , logLevel = DebugAll
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


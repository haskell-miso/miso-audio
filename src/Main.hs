{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Language.Javascript.JSaddle (JSM)
import Miso 
import Miso.Lens
import Miso.String (MisoString)

import Audio

-- TODO volume slider
-- TODO song info

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

thePlaylist :: [MisoString]
thePlaylist =
  [ "roblox-minecraft-fortnite-video-game-music-299145.mp3"
  , "kids-game-gaming-background-music-297733.mp3"
  , "puzzle-game-bright-casual-video-game-music-249202.mp3"
  ]

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Song = Song
  { _songName :: MisoString
  , _songAudio :: Audio
  } deriving (Eq)

data Model = Model
  { _modelPlaylist :: [Song]
  , _modelPlaying :: Maybe Audio
  , _modelSongInfo :: MisoString
  } deriving (Eq)

data Action 
  = ActionPlay Audio
  | ActionInfo Song

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

songName :: Lens Song MisoString
songName = lens _songName $ \record field -> record { _songName = field }

songAudio :: Lens Song Audio
songAudio = lens _songAudio $ \record field -> record { _songAudio = field }

modelPlaylist :: Lens Model [Song]
modelPlaylist = lens _modelPlaylist $ \record field -> record { _modelPlaylist = field }

modelPlaying :: Lens Model (Maybe Audio)
modelPlaying = lens _modelPlaying $ \record field -> record { _modelPlaying = field }

modelSongInfo :: Lens Model MisoString
modelSongInfo = lens _modelSongInfo $ \record field -> record { _modelSongInfo = field }

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ ul_ [] (map fmtSong $ model^.modelPlaylist)
  , p_ [] [ text (model^.modelSongInfo) ]
  ]
  where

    playOrPause audio = 
      if model^.modelPlaying == Just audio then "pause" else "play"

    fmtSong song@(Song name audio) = li_ [] 
      [ button_ [ onClick (ActionInfo song) ] [ text "info" ]
      , button_ [ onClick (ActionPlay audio) ] [ text (playOrPause audio) ]
      , text " "
      , text name 
      ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionPlay audio) = do
  mCurrentAudio <- use modelPlaying
  forM_ mCurrentAudio $ \currentAudio -> io_ (pause currentAudio)
  if mCurrentAudio == Just audio
    then modelPlaying .= Nothing
    else do
      modelPlaying .= Just audio
      io_ $ play audio

handleUpdate (ActionInfo song) = 
  modelSongInfo .= song^.songName 
  -- TODO paused, ended...

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  songs <- zipWith Song thePlaylist <$> traverse newAudio thePlaylist
  let model = Model songs Nothing ""
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



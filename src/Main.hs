{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Time.Clock
import Data.Time.Format
import Language.Javascript.JSaddle (JSVal(..))
import Miso 
import Miso.Lens
import Miso.String (MisoString, ms)

import Audio

-- TODO volume slider
-- TODO update play/pause button dynamically (ended listener?)

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

instance Eq JSVal where
  JSVal ref1 == JSVal ref2 = ref1 == ref2

instance Eq Audio where
  Audio a1 == Audio a2 = a1 == a2

data Song = Song
  { _songName :: MisoString
  , _songAudio :: Audio
  , _songDuration :: Maybe DiffTime
  } deriving (Eq)

mkSong :: MisoString -> Audio -> Song
mkSong name audio = Song name audio Nothing

data Model = Model
  { _modelPlaylist :: [Song]
  , _modelPlaying :: Maybe Audio
  } deriving (Eq)

data Action 
  = ActionPlay Song
  | ActionSetDuration MisoString Double

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

songName :: Lens Song MisoString
songName = lens _songName $ \record field -> record { _songName = field }

songAudio :: Lens Song Audio
songAudio = lens _songAudio $ \record field -> record { _songAudio = field }

songDuration :: Lens Song (Maybe DiffTime)
songDuration = lens _songDuration $ \record field -> record { _songDuration = field }

modelPlaylist :: Lens Model [Song]
modelPlaylist = lens _modelPlaylist $ \record field -> record { _modelPlaylist = field }

modelPlaying :: Lens Model (Maybe Audio)
modelPlaying = lens _modelPlaying $ \record field -> record { _modelPlaying = field }

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ ul_ [] (map fmtSong $ model^.modelPlaylist)
  ]
  where

    playOrPause audio = 
      if model^.modelPlaying == Just audio then "pause" else "play"

    fmtDuration = maybe "" (ms . formatTime defaultTimeLocale "%H:%02M:%02S")

    fmtSong s@(Song name audio t) = li_ [] 
      [ button_ [ onClick (ActionPlay s) ] [ text (playOrPause audio) ]
      , text (" " <> name <> " " <> fmtDuration t)
      ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionPlay song) = do
  let audio = song^.songAudio
  io (ActionSetDuration (song^.songName) <$> duration audio)
  mCurrentAudio <- use modelPlaying
  forM_ mCurrentAudio $ \currentAudio -> io_ (pause currentAudio)
  if mCurrentAudio == Just audio
    then modelPlaying .= Nothing
    else do
      modelPlaying .= Just audio
      io_ $ play audio

handleUpdate (ActionSetDuration name t) = do
  let dt = realToFrac t
  modelPlaylist %= map (\s -> 
    if s^.songName /= name then s else s & songDuration ?~ dt)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  songs <- zipWith mkSong thePlaylist <$> traverse newAudio thePlaylist
  let model = Model songs Nothing 
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



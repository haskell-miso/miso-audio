{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Monad (forM_)
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
  , _songDuration :: Double
  } deriving (Eq)

mkSong :: MisoString -> Audio -> Song
mkSong name audio = Song name audio 0

data Model = Model
  { _modelPlaylist :: [Song]
  , _modelPlaying :: Maybe Audio
  } deriving (Eq)

data Action 
  = ActionPlay Song
  | ActionSetDuration MisoString Double
  | ActionUpdateSong Song
  | ActionInit

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

songName :: Lens Song MisoString
songName = lens _songName $ \record field -> record { _songName = field }

songAudio :: Lens Song Audio
songAudio = lens _songAudio $ \record field -> record { _songAudio = field }

songDuration :: Lens Song Double
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

    fmtSong s@(Song name audio t) = li_ [] 
      [ button_ [ onClick (ActionPlay s) ] [ text (playOrPause audio) ]
      , text (" " <> name <> " " <> ms t)
      ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionPlay song) = do
  let audio = song^.songAudio
  -- issue ActionInit

  mCurrentAudio <- use modelPlaying
  forM_ mCurrentAudio $ \currentAudio -> io_ (pause currentAudio)
  if mCurrentAudio == Just audio
    then modelPlaying .= Nothing
    else do
      modelPlaying .= Just audio
      io_ $ play audio
      -- issue (ActionUpdateSong song)
      io (ActionSetDuration (song^.songName) <$> duration (song^.songAudio))

handleUpdate (ActionUpdateSong song) = do
  io (ActionSetDuration (song^.songName) <$> duration (song^.songAudio))

handleUpdate (ActionSetDuration name t) = 
  modelPlaylist %= map (\s -> 
    if s^.songName == name then s else s & songDuration .~ t)

handleUpdate ActionInit = do
  songs <- use modelPlaylist
  -- batch (pure . ActionUpdateSong <$> songs)
  forM_ songs (issue . ActionUpdateSong)

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
    -- , initialAction = Just ActionInit
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif



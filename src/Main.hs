{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Time.Clock (DiffTime)
import Data.Time.Format
import Language.Javascript.JSaddle (JSVal(..))
import Miso 
import Miso.Lens
import Miso.String (MisoString, ms, fromMisoStringEither)

import Audio

-- TODO switch "play" button to "pause" when audio ended
-- TODO toMisoString format (5.0e-2 -> 0.05) ?

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
  } deriving (Eq)

data Playing = Playing
  { _playingSong :: Song
  , _playingVolume :: Double
  , _playingDuration :: DiffTime
  } deriving (Eq)

data Model = Model
  { _modelPlaylist :: [Song]
  , _modelPlaying :: Maybe Playing
  } deriving (Eq)

mkModel :: [Song] -> Model
mkModel playlist = Model playlist Nothing

data Action 
  = ActionAskPlay Song
  | ActionSetDuration MisoString Double
  | ActionAskVolume MisoString
  | ActionSetVolume Double

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

songName :: Lens Song MisoString
songName = lens _songName $ \record field -> record { _songName = field }

songAudio :: Lens Song Audio
songAudio = lens _songAudio $ \record field -> record { _songAudio = field }

playingSong :: Lens Playing Song
playingSong = lens _playingSong $ \record field -> record { _playingSong = field }

playingVolume :: Lens Playing Double
playingVolume = lens _playingVolume $ \record field -> record { _playingVolume = field }

playingDuration :: Lens Playing DiffTime
playingDuration = lens _playingDuration $ \record field -> record { _playingDuration = field }

modelPlaylist :: Lens Model [Song]
modelPlaylist = lens _modelPlaylist $ \record field -> record { _modelPlaylist = field }

modelPlaying :: Lens Model (Maybe Playing)
modelPlaying = lens _modelPlaying $ \record field -> record { _modelPlaying = field }

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ ul_ [] (map fmtSong $ model^.modelPlaylist)
  , div_ [] fmtPlaying
  ]

  where

    fmtSong s = li_ [] 
      [ button_ [ onClick (ActionAskPlay s) ] [ text (playOrPause (s^.songAudio)) ]
      , text (" " <> s^.songName)
      ]

    playOrPause audio = 
      let mAudio = _songAudio . _playingSong <$> model^.modelPlaying
      in if mAudio == Just audio then "pause" else "play"

    fmtPlaying = 
      case model^.modelPlaying of
        Nothing -> []
        Just p ->
          [ div_ [] 
              [ text "volume: "
              , text (ms $ p^.playingVolume)
              , input_  [ type_ "range", min_ "0.0", max_ "1.0", step_ "0.05"
                        , value_ (ms $ p^.playingVolume)
                        , onChange ActionAskVolume ]
              ]
          , div_ [] [ text ("duration: " <> fmtDuration (p^.playingDuration)) ]
          ]

    fmtDuration = ms . formatTime defaultTimeLocale "%02M:%02S" 

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionAskPlay song) = do
  let audio = song^.songAudio
  mPlaying <- use modelPlaying
  forM_ mPlaying $ \p -> io_ (pause $ p^.playingSong^.songAudio)
  if (_songAudio . _playingSong <$> mPlaying) == Just audio
    then modelPlaying .= Nothing
    else do
      modelPlaying .= Just (Playing song 0 0)
      io (ActionSetDuration (song^.songName) <$> duration audio)
      io (ActionSetVolume <$> getVolume audio)
      io_ $ play audio

handleUpdate (ActionSetDuration name t) = 
  modelPlaying %= fmap (\p -> p & playingDuration .~ realToFrac t)

handleUpdate (ActionAskVolume str) = 
  forM_ (fromMisoStringEither str) $ \vol -> do
    mPlaying <- use modelPlaying
    forM_ mPlaying $ \p -> do
      io_ (setVolume (p^.playingSong^.songAudio) vol)
      modelPlaying %= fmap (\p -> p & playingVolume .~ vol)

handleUpdate (ActionSetVolume vol) =
  modelPlaying %= fmap (\p -> p & playingVolume .~ vol)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  songs <- zipWith Song thePlaylist <$> traverse newAudio thePlaylist
  let model = mkModel songs
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



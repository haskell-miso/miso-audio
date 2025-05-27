{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Language.Javascript.JSaddle (JSM)
import Miso 
import Miso.Lens
import Miso.String (MisoString)

import Audio

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

playlistFilenames :: [MisoString]
playlistFilenames =
  [ "roblox-minecraft-fortnite-video-game-music-299145.mp3"
  , "kids-game-gaming-background-music-297733.mp3"
  , "puzzle-game-bright-casual-video-game-music-249202.mp3"
  ]

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Model = Model
  { _lengthPlaylist :: Int
  , _playing :: Maybe Audio
  } deriving (Eq)

newtype Assets = Assets
  { _playlist :: [(MisoString, Audio)]
  }

newtype Action 
  = ActionPlay Audio

----------------------------------------------------------------------
-- lenses
----------------------------------------------------------------------

lengthPlaylist :: Lens Model Int
lengthPlaylist = lens _lengthPlaylist $ \record field -> record { _lengthPlaylist = field }

playing :: Lens Model (Maybe Audio)
playing = lens _playing $ \record field -> record { _playing = field }

playlist :: Lens Assets [(MisoString, Audio)]
playlist = lens _playlist $ \record field -> record { _playlist = field }

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Assets -> Model -> View Action
handleView assets model = div_ [] 
  [ ul_ [] (map fmtSong $ assets^.playlist)
  ]
  where

    playOrPause audio = 
      if model^.playing == Just audio then "pause" else "play"

    fmtSong (name, audio) = li_ [] 
      [ button_ [ onClick (ActionPlay audio) ] [ text (playOrPause audio) ]
      , text " "
      , text name 
      ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Assets -> Action -> Effect Model Action

handleUpdate _assets (ActionPlay audio) = do
  mCurrent <- use playing
  forM_ mCurrent $ \current -> io_ (pause current)
  if mCurrent == Just audio
    then playing .= Nothing
    else do
      playing .= Just audio
      io_ $ play audio

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  assets <- Assets . zip playlistFilenames <$> traverse newAudio playlistFilenames
  let model = Model (length $ assets^.playlist) Nothing
  startComponent Component
    { model = model
    , update = handleUpdate assets
    , view = handleView assets
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



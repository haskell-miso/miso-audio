{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle (JSM)
import Miso 
import Miso.Lens
import Miso.String

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

newtype Model = Model
  { _indexPlaylist :: Int
  } deriving (Eq)

indexPlaylist :: Lens Model Int
indexPlaylist = lens _indexPlaylist $ \record field -> record { _indexPlaylist = field }

newtype Assets = Assets
  { _playlist :: [Audio]
  }

playlist :: Lens Assets [Audio]
playlist = lens _playlist $ \record field -> record { _playlist = field }

data Action 
  = ActionReset
  | ActionPlaylist Bool

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Assets -> Model -> View Action
handleView _assets _model = div_ [] 
  [ p_ [] [ "TODO" ]
  ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Assets -> Action -> Effect Model Action

handleUpdate _assets ActionReset = 
  io_ (consoleLog "TODO ActionReset")

handleUpdate _assets (ActionPlaylist _isPaused) = 
  io_ (consoleLog "TODO ActionPlaylist")

{-
handleUpdate res (ActionPlaylist isPaused) = 
  when isPaused $ do
    mIndexPlaylist %= \i -> mod (i+1) (length $ _resPlaylist res)
    withPlaylist res $ \audio -> do
      io_ $ volume audio 0.1
      io_ $ play audio

withPlaylist :: Resources -> (Audio -> Effect Model Action) -> Effect Model Action
withPlaylist res f = do
  i <- use mIndexPlaylist
  traverse_ f $ _resPlaylist res !? i
  
----------------------------------------------------------------------
-- main
----------------------------------------------------------------------
-}

myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  assets <- Assets <$> traverse newAudio playlistFilenames
  let model = Model 0
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




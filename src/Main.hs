{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Data.Time.Format
import Miso 
import Miso.Lens as Lens
import Miso.String (fromMisoStringEither, MisoString, ms)

import Model
import MyMiso

-- TODO toMisoString format (5.0e-2 -> 0.05) ?
-- TODO volume -> getVolume/setVolume ?
-- TODO avoid getElementById calls ?

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
  | ActionAskReload Song
  | ActionAskVolume MisoString
  | ActionSetVolume Double
  | ActionSetDuration Double

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
  , div_ [] fmtPlaying
  ]
  where

    playOrPause s = 
      let mSong = _playingSong <$> model^.modelPlaying
      in if mSong == Just s then "pause" else " play "

    fmtSong s = li_ [] 
        [ audio_ [ id_ (s^.songAudioId), src_ (s^.songFilename), onEnded ActionAskEnded ] []
        , button_ [ onClick (ActionAskPlay s) ] [ text (playOrPause s) ]
        , button_ [ onClick (ActionAskReload s) ] [ text "reload" ]
        , text (" " <> s^.songFilename)
        ]

    fmtDuration = ms . formatTime defaultTimeLocale "%02M:%02S" 

    fmtPlaying = 
      case model^.modelPlaying of
        Nothing -> []
        Just p ->
          [ div_ [] 
              [ input_  [ type_ "range", min_ "0.0", max_ "1.0", step_ "0.05"
                        , value_ (ms $ p^.playingVolume)
                        , onChange ActionAskVolume ]
              ]
          , div_ [] [ text "volume: " , text (ms $ p^.playingVolume) ]
          , div_ [] [ text ("duration: " <> fmtDuration (p^.playingDuration)) ]
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
      io (getElementById (s^.songAudioId) >>= fmap ActionSetVolume . getVolume . Audio)
      io (getElementById (s^.songAudioId) >>= fmap ActionSetDuration . duration . Audio)
      io_ (getElementById (s^.songAudioId) >>= play . Audio)

handleUpdate ActionAskEnded = 
  modelPlaying .= Nothing

handleUpdate (ActionAskReload s)  = do
  io_ (getElementById (s^.songAudioId) >>= load . Audio)
  mSong <- fmap _playingSong <$> use modelPlaying
  when (mSong == Just s) $ 
    -- io_ (getElementById (s^.songAudioId) >>= play . Audio)
    modelPlaying .= Nothing

handleUpdate (ActionAskVolume str) = 
  forM_ (fromMisoStringEither str) $ \vol -> do
    mPlaying <- use modelPlaying
    forM_ mPlaying $ \p -> do
      io_ (getElementById (p^.playingSong^.songAudioId) >>= flip setVolume vol . Audio)
      modelPlaying %= fmap (Lens.set playingVolume vol)

handleUpdate (ActionSetVolume vol) =
  modelPlaying %= fmap (Lens.set playingVolume vol)

handleUpdate (ActionSetDuration t) =
  modelPlaying %= fmap (Lens.set playingDuration (realToFrac t))

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


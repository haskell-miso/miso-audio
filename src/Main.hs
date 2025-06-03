{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Data.Map as Map ((!?), adjust, elems, mapWithKey)
import Data.Time.Format
import Miso
import Miso.Lens as Lens
import Miso.Media
import Miso.String (fromMisoStringEither, MisoString, ms)

import Model

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
-- actions
----------------------------------------------------------------------

data Action 
  = ActionAskPlay SongId
  | ActionAskEnded
  | ActionAskReload SongId
  | ActionAskVolume MisoString
  | ActionAskDuration SongId Media
  | ActionSetDuration SongId Double

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
  , ul_ [] (elems $ mapWithKey fmtSong (model^.modelSongs))
  , div_ [] fmtPlaying
  ]
  where

    -- format a song
    fmtSong sId s = li_ [] 
        [ audio_ 
            [ id_ (sId^.songId)
            , src_ (s^.songFilename)
            , volume_ (s^.songVolume)
            , onEnded ActionAskEnded
            , onCanPlayWith (ActionAskDuration sId) 
            ]
            []
        , button_ 
            [ onClick (ActionAskPlay sId) ]
            [ text (playOrPause sId) ]
        , button_ 
            [ onClick (ActionAskReload sId) ]
            [ text "reload" ]
        , text (" " <> s^.songFilename)
        ]

    playOrPause sId = 
      if (model^.modelPlaying) == Just sId then "pause" else " play "

    -- format the current song, if any
    fmtPlaying = 
      case model^.modelPlaying >>= ((model^.modelSongs) !?) of
        Just p ->
          [ div_ [] 
              [ input_ 
                  [ type_ "range", min_ "0.0", max_ "1.0", step_ "0.05"
                  , value_ (ms $ p^.songVolume)
                  , onChange ActionAskVolume 
                  ]
              ]
          , div_ [] [ text "volume: " , text (ms $ p^.songVolume) ]
          , div_ [] [ text ("duration: " <> fmtDuration (p^.songDuration)) ]
          ]
        _ -> []

    fmtDuration = maybe "" (ms . formatTime defaultTimeLocale "%02M:%02S")

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionAskPlay sId) = do
  -- pause the current song, if any
  mPlaying <- use modelPlaying
  forM_ mPlaying $ \pId -> 
    io_ (getElementById (pId^.songId) >>= pause . Media)
  -- if a new song is selected, play it
  if mPlaying /= Just sId
    then do
      modelPlaying .= Just sId
      io_ (getElementById (sId^.songId) >>= play . Media)
    else modelPlaying .= Nothing

handleUpdate ActionAskEnded = 
  modelPlaying .= Nothing

handleUpdate (ActionAskReload sId)  = do
  -- reload the song
  io_ (getElementById (sId^.songId) >>= load . Media)
  -- if the song is the current song, reset modelPlaying
  mP <- use modelPlaying
  when (mP == Just sId) $ 
    modelPlaying .= Nothing

handleUpdate (ActionAskVolume str) = 
  -- try to parse the input to a number
  forM_ (fromMisoStringEither str) $ \vol -> do
    -- find the current song then adjust its volume in the map
    mPlaying <- use modelPlaying
    forM_ mPlaying $ \pId ->
      modelSongs %= adjust (Lens.set songVolume vol) pId

handleUpdate (ActionAskDuration sId media) =
  -- get media duration then set model
  io (ActionSetDuration sId <$> duration media)

handleUpdate (ActionSetDuration sId t) =
  -- find the song, in the map, and set its duration
  modelSongs %= adjust (Lens.set songDuration (Just $ realToFrac t)) sId

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  let model = mkModel thePlaylist
      app = defaultComponent model handleUpdate handleView
  startComponent app
    { events = defaultEvents <> mediaEvents
    , logLevel = DebugAll
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


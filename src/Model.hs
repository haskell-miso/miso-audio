{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Data.Time.Clock (DiffTime)
import Language.Javascript.JSaddle (JSVal(..))
import Miso 
import Miso.Lens
import Miso.Lens.TH
import Miso.String (MisoString)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Eq JSVal where
  JSVal ref1 == JSVal ref2 = ref1 == ref2

instance Eq Audio where
  Audio a1 == Audio a2 = a1 == a2

-------------------------------------------------------------------------------
-- Song
-------------------------------------------------------------------------------

data Song = Song 
  { _songFilename :: MisoString
  , _songAudioId :: MisoString
  }

instance Eq Song where
  (Song f1 _) == (Song f2 _) = f1 == f2

makeLenses ''Song

mkSong :: MisoString -> Song
mkSong filename = Song filename ("audio_" <> filename)

-------------------------------------------------------------------------------
-- Playing
-------------------------------------------------------------------------------

data Playing = Playing
  { _playingVolume :: Double
  , _playingDuration :: DiffTime
  , _playingSong :: Song
  } deriving (Eq)

makeLenses ''Playing

mkPlaying :: Song -> Playing
mkPlaying = Playing 0 0

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Model = Model
  { _modelPlaying :: Maybe Playing
  , _modelSongs :: [Song]
  } deriving (Eq)

makeLenses ''Model

mkModel :: [Song] -> Model
mkModel = Model Nothing


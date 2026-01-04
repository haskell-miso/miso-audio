-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
-------------------------------------------------------------------------------
module Model where
-------------------------------------------------------------------------------
import Miso
import Data.Map as Map (fromList, Map)
import Data.Time.Clock (DiffTime)
import Miso.Lens (Lens, lens)
import Miso.Lens.TH (makeLenses)
import Miso.Media (Media(..))
import Miso.String (MisoString)
-------------------------------------------------------------------------------
-- SongId
-------------------------------------------------------------------------------
newtype SongId = SongId { _songId :: MisoString }
  deriving (Eq, Ord)
-------------------------------------------------------------------------------
makeLenses ''SongId
-------------------------------------------------------------------------------
mkSongId :: MisoString -> SongId
mkSongId filename = SongId ("audio_" <> filename)
-------------------------------------------------------------------------------
-- Song
-------------------------------------------------------------------------------
data Song = Song 
  { _songFilename :: MisoString
  , _songVolume   :: Double
  , _songDuration :: Maybe DiffTime
  }
  deriving (Eq)
-------------------------------------------------------------------------------
makeLenses ''Song
-------------------------------------------------------------------------------
mkSong :: MisoString -> Song
mkSong filename = Song filename 1 Nothing
-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------
data Model = Model
  { _modelPlaying :: Maybe SongId
  , _modelSongs :: Map SongId Song
  } deriving (Eq)
-------------------------------------------------------------------------------
makeLenses ''Model
-------------------------------------------------------------------------------
mkModel :: [MisoString] -> Model
mkModel filenames = Model Nothing songs
  where
    songs = Map.fromList [ (mkSongId f, mkSong f) | f<-filenames]
-------------------------------------------------------------------------------

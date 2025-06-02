
module Model where

import Miso 
import Miso.Lens
import Miso.Lens.TH
import Miso.String (MisoString)

-- import Audio

data Model = Model
  { _modelPlaylist :: [MisoString]
  , _modelPlaying :: Maybe MisoString
  } deriving (Eq)

mkModel :: [MisoString] -> Model
mkModel playlist = Model playlist Nothing

makeLenses ''Model

{-
modelPlaylist :: Lens Model [MisoString]
modelPlaylist = lens _modelPlaylist $ \record field -> record { _modelPlaylist = field }

modelPlaying :: Lens Model (Maybe MisoString)
modelPlaying = lens _modelPlaying $ \record field -> record { _modelPlaying = field }
-}


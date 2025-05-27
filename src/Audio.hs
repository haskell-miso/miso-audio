
module Audio where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle
import Miso hiding ((<#))
import Miso.String (MisoString, ms)

newtype Audio = Audio JSVal
  deriving (ToJSVal)

newAudio :: MisoString -> JSM Audio
newAudio url = do
  a <- new (jsg "Audio") ([] :: [MisoString])
  o <- makeObject a
  set (ms "src") url o
  pure (Audio a)

play :: Audio -> JSM ()
play (Audio a) = void $ a # "play" $ ()

volume :: Audio -> Double -> JSM ()
volume (Audio a) = a <# "volume"

paused :: Audio -> JSM Bool
paused (Audio a) = do
  value <- a ! "paused"
  fromMaybe False <$> fromJSVal value

pause :: Audio -> JSM ()
pause (Audio a) = void $ a # "pause" $ ()



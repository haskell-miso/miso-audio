module Audio where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle
import Miso hiding ((<#))
import Miso.String (MisoString, ms)

-------------------------------------------------------------------------------
-- already in miso
-------------------------------------------------------------------------------

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


-- TODO replace by setVolume/getVolume ? (volume is both a getter and a setter, in the JS API)
volume :: Audio -> Double -> JSM ()
volume (Audio a) = a <# "volume"

paused :: Audio -> JSM Bool
paused (Audio a) = do
  value <- a ! "paused"
  fromMaybe False <$> fromJSVal value

pause :: Audio -> JSM ()
pause (Audio a) = void $ a # "pause" $ ()

-------------------------------------------------------------------------------
-- tested
-------------------------------------------------------------------------------

duration :: Audio -> JSM Double
duration (Audio a) = do
  value <- a ! "duration"
  fromMaybe 0 <$> fromJSVal value

setVolume :: Audio -> Double -> JSM ()
setVolume (Audio a) = a <# "volume"

getVolume :: Audio -> JSM Double
getVolume (Audio a) = do
  value <- a ! "volume"
  fromMaybe 0 <$> fromJSVal value

load :: Audio -> JSM ()
load (Audio a) = void $ a # "load" $ ()

ended :: Audio -> JSM Bool
ended (Audio a) = do
  value <- a ! "ended"
  fromMaybe False <$> fromJSVal value

-------------------------------------------------------------------------------
-- not tested
-------------------------------------------------------------------------------

onEnded :: action -> Attribute action
onEnded = on (ms "ended") emptyDecoder . const 


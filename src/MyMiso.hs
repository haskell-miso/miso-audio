{-# LANGUAGE OverloadedStrings #-}

module MyMiso where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle
import Miso hiding ((<#))
import Miso.String (MisoString)

-------------------------------------------------------------------------------
-- tested
-------------------------------------------------------------------------------

duration :: Audio -> JSM Double
duration (Audio a) = do
  value <- a ! ("duration" :: MisoString)
  fromMaybe 0 <$> fromJSVal value

setVolume :: Audio -> Double -> JSM ()
setVolume (Audio a) = a <# ("volume" :: MisoString)

getVolume :: Audio -> JSM Double
getVolume (Audio a) = do
  value <- a ! ("volume" :: MisoString)
  fromMaybe 0 <$> fromJSVal value

load :: Audio -> JSM ()
load (Audio a) = void $ a # ("load" :: MisoString) $ ()

ended :: Audio -> JSM Bool
ended (Audio a) = do
  value <- a ! ("ended" :: MisoString)
  fromMaybe False <$> fromJSVal value

-------------------------------------------------------------------------------
-- not tested
-------------------------------------------------------------------------------

onEnded :: action -> Attribute action
onEnded action = on "ended" emptyDecoder $ \() _ -> action

onEndedWith :: (JSVal -> action) -> Attribute action
onEndedWith action = on "ended" emptyDecoder $ \() domRef -> action domRef



module Main where

import Data.Astro.Planet
import Data.Astro.Time.JulianDate()
import Data.Astro.Types()
import Data.List()
import Data.Maybe()
import Data.Time
import FRP.Elerea.Simple
import Graphics.Gloss
import Control.Monad.IO.Class()
import qualified Graphics.Gloss.Interface.IO.Game as G

data PlanetInfo = PlanetInfo {
  piColor :: Color
  } deriving (Show, Eq)

planetInfo :: Planet -> PlanetInfo
planetInfo Mercury = PlanetInfo chartreuse
planetInfo Venus = PlanetInfo magenta
planetInfo Earth = PlanetInfo blue
planetInfo Mars = PlanetInfo red
planetInfo Jupiter = PlanetInfo violet
planetInfo Saturn = PlanetInfo cyan
planetInfo Uranus = PlanetInfo orange
planetInfo Neptune = PlanetInfo aquamarine

data OrreyContext = OrreyContext { osEpoch :: UTCTime }

initialContext :: IO OrreyContext
initialContext = do now <- getCurrentTime
                    return $ OrreyContext { osEpoch = now }
  
type InputEvent = G.Event

renderStatusBar :: OrreyContext -> [Picture]
renderStatusBar cxt = [Translate (-450) (-390) $ Color white $
                       Polygon [(0, 0), (0, 30), (900, 30), (900, 0)]] ++
                      [Translate (-440) (-380) $ Scale 0.1 0.1 $ Text $ "Status bar text"]

renderSun :: [Picture]
renderSun = [Translate 0 0 $ Color yellow $ ThickCircle 1 8]

renderOrrey :: OrreyContext -> Picture 
renderOrrey cxt = Pictures $ renderSun ++
                  renderStatusBar cxt

movePlanets :: Maybe InputEvent -> OrreyContext -> OrreyContext
movePlanets evt cxt = cxt

network :: SignalGen (Signal (Maybe Float))
        -> SignalGen (Signal (Maybe InputEvent))
        -> OrreyContext
        -> SignalGen (Signal Picture)
network _ evt cxt = do
  inputEvent <- evt
  orrey <- transfer cxt movePlanets inputEvent
  return $ renderOrrey <$> orrey

main :: IO ()
main = do
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
  context <- initialContext
               
  recomputePicture <- start $ do network tickGen inputGen context
    
  initialPicture <- recomputePicture
    
  G.playIO (InWindow "Orrey" (900, 780) (0, 0))
    black
    100
    initialPicture
    return
    (\e _ -> do
        inputSink $ Just e
        recomputePicture)
    (\t _ -> do
        tickSink $ Just t
        recomputePicture)
    

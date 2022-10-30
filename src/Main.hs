module Main where

import Data.Astro.Planet
import Data.Astro.Planet.PlanetDetails()
import Data.Astro.Time.JulianDate()
import Data.Astro.Types
import Data.Fixed
import Data.List()
import Data.Maybe()
import Data.Time
import FRP.Elerea.Simple
import Graphics.Gloss
import Control.Monad.IO.Class()
import qualified Graphics.Gloss.Interface.IO.Game as G
  
type InputEvent = G.Event

data CelestialObject = Sun | Moon deriving (Eq, Show)

data PlanetInfo = PlanetInfo {
  piPlanet :: Planet,
  piColor :: Color,
  piSize :: Float
  } deriving (Show, Eq)

planetInfo :: Planet -> PlanetInfo
planetInfo Mercury = PlanetInfo Mercury chartreuse 2
planetInfo Venus = PlanetInfo Venus magenta 2
planetInfo Earth = PlanetInfo Earth blue 2
planetInfo Mars = PlanetInfo Mars red 2
planetInfo Jupiter = PlanetInfo Jupiter violet 2
planetInfo Saturn = PlanetInfo Saturn cyan 2
planetInfo Uranus = PlanetInfo Uranus orange 2
planetInfo Neptune = PlanetInfo Neptune aquamarine 2

planets :: [Planet]
planets = [Mercury, Venus, Earth, Mars, Jupiter, Uranus, Neptune]

data OrreyState = OrreyState {
  osEpoch :: UTCTime,
  osJD :: Double,
  osPlanets :: [PlanetInfo]
  }

initialState :: IO OrreyState
initialState = do now <- getCurrentTime
                  return $ OrreyState {
                    osEpoch = now,
                    osJD = 0,
                    osPlanets = map (\p -> planetInfo p) planets
                    }

trueAnomaly :: Planet -> Double -> Double
trueAnomaly p jd = mod' (jd * rad / tp) rad
                   where rad = 2 * pi
                         tpe = 365.24255 * (pdTp $ j2010PlanetDetails Earth)
                         tp = (pdTp $ j2010PlanetDetails p) * tpe
                        
planetCoordinates :: Planet -> Double -> (Float, Float)
planetCoordinates p v = (realToFrac x, realToFrac y)
                        where a = auToKM $ pdAlpha $ j2010PlanetDetails p
                              au = auToKM $ pdAlpha $ j2010PlanetDetails Earth
                              e = pdE $ j2010PlanetDetails p
                              ω_ = toRadians $ pdOmegaBar $ j2010PlanetDetails p
                              lp = v + ω_
                              r = (a / au) * (1 - e^2) / (1 + e * cos lp)
                              x = r * cos lp
                              y = r * sin lp
                                                 
movePlanets :: Maybe InputEvent -> OrreyState -> OrreyState
movePlanets (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ _)) st = st
movePlanets _ st = st { osJD = (+1) $ osJD st }

renderStatusBar :: OrreyState -> [Picture]
renderStatusBar st = [Translate (-450) (-390) $ Color white $
                       Polygon [(0, 0), (0, 30), (900, 30), (900, 0)]] 
                     {- ++ [Translate (-440) (-380) $ Scale 0.1 0.1 $ Text $
                      ("Earth: " ++ (show x)) ++ "," ++ (show y) ++
                      " v=" ++ (show $ trueAnomaly Earth jd)] -}
                     where jd = osJD st
                           (x, y) = planetCoordinates Earth $ trueAnomaly Earth jd

renderSun :: [Picture]
renderSun = [Translate 0 0 $ Color yellow $ ThickCircle 1 8]

renderPlanet :: Planet -> Double -> Picture
renderPlanet p jd = Translate (x * scl) (y * scl) $ Color clr $ ThickCircle 1 size
                    where (x, y) = planetCoordinates p (trueAnomaly p jd)
                          scl = 70
                          clr = piColor $ planetInfo p
                          size = piSize $ planetInfo p
       
renderOrbit :: Planet -> Picture
renderOrbit p = Translate 0 0 $ Color white $ Circle (aus * 70)
                where aus = realToFrac $ pdAlpha $ j2010PlanetDetails p

renderOrrey :: OrreyState -> Picture 
renderOrrey st = Pictures $ renderSun ++
                 -- ((\p -> renderOrbit p) <$> planets) ++
                 ((\p -> renderPlanet p (osJD st)) <$> planets) ++
                 renderStatusBar st

network :: SignalGen (Signal (Maybe Float))
        -> SignalGen (Signal (Maybe InputEvent))
        -> OrreyState
        -> SignalGen (Signal Picture)
network _ ev st = do
  inputEvent <- ev
  orrey <- transfer st movePlanets inputEvent
  return $ renderOrrey <$> orrey

main :: IO ()
main = do
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
  state <- initialState
               
  recomputePicture <- start $ do network tickGen inputGen state
    
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

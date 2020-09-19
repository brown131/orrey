module Main where

import Data.Astro.Planet
import Data.Astro.Planet.PlanetDetails
import Data.Astro.Time.JulianDate()
import Data.Astro.Types
import Data.List()
import Data.Maybe()
import Data.Time
import FRP.Elerea.Simple
import Graphics.Gloss
import Control.Monad.IO.Class()
import qualified Graphics.Gloss.Interface.IO.Game as G

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
  osPlanets :: [PlanetInfo],
  osTicks :: Int
  }

initialState :: IO OrreyState
initialState = do now <- getCurrentTime
                  return $ OrreyState {
                    osEpoch = now,
                    osPlanets = map (\p -> planetInfo p) planets,
                    osTicks = 0
                    }
  
type InputEvent = G.Event

-- renderEllipse :: Rectangle -> Picture
-- renderEllipse r = (rectPos r)
-- rectSize r

movePlanets :: Maybe InputEvent -> OrreyState -> OrreyState
movePlanets (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ _)) st =
  st { osTicks = (+1) $ osTicks st }
movePlanets _ st = st

renderStatusBar :: OrreyState -> [Picture]
renderStatusBar st = [Translate (-450) (-390) $ Color white $
                       Polygon [(0, 0), (0, 30), (900, 30), (900, 0)]] ++
                     [Translate (-440) (-380) $ Scale 0.1 0.1 $ Text $ ("Ticks: " ++ (show $ osTicks st))]

renderSun :: [Picture]
renderSun = [Translate 0 0 $ Color yellow $ ThickCircle 1 8]

renderOrbit :: Planet -> Picture
renderOrbit p = Translate 0 0 $ Color white $ Circle (aus * 70)
                where aus = realToFrac $ pdAlpha (j2010PlanetDetails p)

-- renderPlanet :: Planet -> OrreyState -> Picture
-- renderPlanet p st = []

renderOrrey :: OrreyState -> Picture 
renderOrrey st = Pictures $ renderSun ++
                 map (\p -> renderOrbit p) planets ++
                 [Translate 0 0 $ Color red $ Arc 45 90 100] ++
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
    

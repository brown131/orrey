-- | Orrery: An interactive visualization of planetary orbits
-- This module creates a real-time 3D visualization of planets in their orbits around the sun
-- using Gloss graphics library and FRP (Functional Reactive Programming) with Elerea

module Main where

-- Astronomy-related imports for calculating planetary positions and properties
import Data.Astro.Planet
import Data.Astro.Planet.PlanetDetails()
import Data.Astro.Time.JulianDate()
import Data.Astro.Types

-- Utility imports for data manipulation
import Data.Fixed
import Data.List()
import Data.Maybe()
import Data.Time

-- FRP and graphics imports
import FRP.Elerea.Simple
import Graphics.Gloss
import Control.Monad.IO.Class()
import qualified Graphics.Gloss.Interface.IO.Game as G

-- | Type alias for input events from the Gloss game interface
type InputEvent = G.Event

-- | Represents celestial objects that could be rendered (currently unused)
data CelestialObject = Sun | Moon deriving (Eq, Show)

-- | Stores visual and physical properties of a planet
data PlanetInfo = PlanetInfo {
  piPlanet :: Planet,      -- ^ The planet type
  piColor :: Color,        -- ^ RGB color for rendering
  piSize :: Float          -- ^ Display size for rendering
  } deriving (Show, Eq)

-- | Maps each planet to its visual properties (color and size)
planetInfo :: Planet -> PlanetInfo
planetInfo Mercury = PlanetInfo Mercury chartreuse 2
planetInfo Venus = PlanetInfo Venus magenta 2
planetInfo Earth = PlanetInfo Earth blue 2
planetInfo Mars = PlanetInfo Mars red 2
planetInfo Jupiter = PlanetInfo Jupiter violet 2
planetInfo Saturn = PlanetInfo Saturn cyan 2
planetInfo Uranus = PlanetInfo Uranus orange 2
planetInfo Neptune = PlanetInfo Neptune aquamarine 2

-- | List of planets to render in the orrery (excludes Earth's satellite Moon)
planets :: [Planet]
planets = [Mercury, Venus, Earth, Mars, Jupiter, Uranus, Neptune]

-- | Represents the current state of the orrery simulation
data OrreyState = OrreyState {
  osEpoch :: UTCTime,      -- ^ Starting time/epoch for the simulation
  osJD :: Double,          -- ^ Current Julian Date (days since epoch)
  osPlanets :: [PlanetInfo] -- ^ Information about each planet to render
  }

-- | Initializes the orrery state with current time and default planet properties
initialState :: IO OrreyState
initialState = do now <- getCurrentTime
                  return $ OrreyState {
                    osEpoch = now,
                    osJD = 0,
                    osPlanets = map (\p -> planetInfo p) planets
                    }

-- | Calculates the true anomaly (position angle in orbit) for a planet at a given Julian Date
-- The true anomaly determines where a planet is positioned in its elliptical orbit
trueAnomaly :: Planet -> Double -> Double
trueAnomaly p jd = mod' (jd * rad / tp) rad
                   where rad = 2 * pi
                         -- tpe: tropical year in days (Earth's orbital period)
                         tpe = 365.24255 * (pdTp $ j2010PlanetDetails Earth)
                         -- tp: planet's orbital period
                         tp = (pdTp $ j2010PlanetDetails p) * tpe
                        
-- | Converts a true anomaly to 2D cartesian coordinates (x, y) for rendering
-- Uses Kepler's equations and orbital elements from J2000 epoch
planetCoordinates :: Planet -> Double -> (Float, Float)
planetCoordinates p v = (realToFrac x, realToFrac y)
                        where -- Semi-major axis in AU
                              a = auToKM $ pdAlpha $ j2010PlanetDetails p
                              -- Earth's semi-major axis (1 AU)
                              au = auToKM $ pdAlpha $ j2010PlanetDetails Earth
                              -- Orbital eccentricity (0 = circular, 1 = parabolic)
                              e = pdE $ j2010PlanetDetails p
                              -- Longitude of perihelion (ω_)
                              ω_ = toRadians $ pdOmegaBar $ j2010PlanetDetails p
                              -- True longitude
                              lp = v + ω_
                              -- Distance from sun using vis-viva equation
                              r = (a / au) * (1 - e^2) / (1 + e * cos lp)
                              -- Convert polar coordinates to cartesian
                              x = r * cos lp
                              y = r * sin lp
                                                 
-- | Handles state updates based on input events (mouse clicks, keyboard)
-- Currently only responds to left mouse click (pauses the simulation)
movePlanets :: Maybe InputEvent -> OrreyState -> OrreyState
movePlanets (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ _)) st = st
movePlanets _ st = st { osJD = (+1) $ osJD st }

-- | Renders the status bar at the bottom of the screen
-- Displays debug information about Earth's position (currently commented out)
renderStatusBar :: OrreyState -> [Picture]
renderStatusBar st = [Translate (-450) (-390) $ Color white $
                       Polygon [(0, 0), (0, 30), (900, 30), (900, 0)]] 
                     {- ++ [Translate (-440) (-380) $ Scale 0.1 0.1 $ Text $
                      ("Earth: " ++ (show x)) ++ "," ++ (show y) ++
                      " v=" ++ (show $ trueAnomaly Earth jd)] -}
                     where jd = osJD st
                           (x, y) = planetCoordinates Earth $ trueAnomaly Earth jd

-- | Renders the sun at the center of the orrery as a yellow circle
renderSun :: [Picture]
renderSun = [Translate 0 0 $ Color yellow $ ThickCircle 1 8]

-- | Renders a single planet as a colored circle at its current orbital position
renderPlanet :: Planet -> Double -> Picture
renderPlanet p jd = Translate (x * scl) (y * scl) $ Color clr $ ThickCircle 1 size
                    where (x, y) = planetCoordinates p (trueAnomaly p jd)
                          scl = 70              -- Scaling factor for display
                          clr = piColor $ planetInfo p
                          size = piSize $ planetInfo p
       
-- | Renders a planet's orbital path as a white circle
renderOrbit :: Planet -> Picture
renderOrbit p = Translate 0 0 $ Color white $ Circle (aus * 70)
                where aus = realToFrac $ pdAlpha $ j2010PlanetDetails p

-- | Combines all rendered elements (sun, planets, orbits, status bar) into a single picture
renderOrrey :: OrreyState -> Picture 
renderOrrey st = Pictures $ renderSun ++
                 -- ((\p -> renderOrbit p) <$> planets) ++  -- Orbit rendering disabled
                 ((\p -> renderPlanet p (osJD st)) <$> planets) ++
                 renderStatusBar st

-- | FRP signal network that manages state updates and rendering
-- Combines input events and time ticks to update the orrery state
network :: SignalGen (Signal (Maybe Float))
        -> SignalGen (Signal (Maybe InputEvent))
        -> OrreyState
        -> SignalGen (Signal Picture)
network _ ev st = do
  inputEvent <- ev
  orrey <- transfer st movePlanets inputEvent
  return $ renderOrrey <$> orrey

-- | Main entry point: sets up the interactive Gloss window and FRP event loop
main :: IO ()
main = do
  -- Create external signal generators for time ticks and input events
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
  state <- initialState
               
  -- Initialize the FRP network with initial state
  recomputePicture <- start $ do network tickGen inputGen state
    
  -- Render the initial picture
  initialPicture <- recomputePicture
    
  -- Launch the Gloss window with event and time handlers
  G.playIO (InWindow "Orrey" (900, 780) (0, 0))
    black                    -- Background color
    100                      -- Frame rate (fps)
    initialPicture           -- Initial render
    return
    -- Input event handler: process mouse/keyboard input
    (\e _ -> do
        inputSink $ Just e
        recomputePicture)
    -- Time tick handler: update simulation on each frame
    (\t _ -> do
        tickSink $ Just t
        recomputePicture)

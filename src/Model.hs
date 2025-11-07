module Model where

import Graphics.Gloss.Interface.IO.Game
import System.Random

------------------------------------------------------------
-- Window setup
------------------------------------------------------------
windowWidth, windowHeight :: Float
windowWidth  = 800
windowHeight = 600

------------------------------------------------------------
-- Ship settings
------------------------------------------------------------
invincibilityDuration :: Float
invincibilityDuration = 2.0

------------------------------------------------------------
-- Bullet settings
------------------------------------------------------------
bulletLifetime :: Float
bulletLifetime = 1.0  -- seconds

------------------------------------------------------------
-- Asteroid splitting constants
------------------------------------------------------------
minAsteroidSize, splitFactor :: Float
minAsteroidSize = 30     -- smallest asteroid radius before it stops splitting
splitFactor     = 0.6    -- size ratio for child asteroids

------------------------------------------------------------
-- Enemy behavior constants
------------------------------------------------------------
enemySpawnScore :: Int
enemySpawnScore = 500     -- spawn an enemy after this score

enemySpeed :: Float
enemySpeed = 70          -- pixels per second

------------------------------------------------------------
-- Screen State
------------------------------------------------------------
data Screen = MainMenu | ControlsScreen | GameScreen deriving (Show, Eq)
------------------------------------------------------------
-- Game model
------------------------------------------------------------
data GameState = GameState
  { player          :: Ship
  , bullets         :: [Bullet]
  , asteroids       :: [Asteroid]
  , enemies         :: [Enemy]
  , score           :: Int
  , isPaused        :: Bool
  , spawnTimer      :: Float
  , enemySpawnTimer :: Float
  , generator       :: StdGen
  , stars           :: [Point]
  , lives           :: Int
  , highScore       :: Int 
  , startHighScore  :: Int
  , currentScreen   :: Screen
  , menuShip        :: Ship
  , menuAsteroid    :: Asteroid
  , menuAsteroid2   :: Asteroid
  , menuAsteroid3   :: Asteroid
  , menuAsteroid4   :: Asteroid
  } deriving (Show, Eq)

------------------------------------------------------------
-- Ship
------------------------------------------------------------
data Ship = Ship
  { position  :: Point
  , velocity  :: Vector
  , angle     :: Float
  , angularV  :: Float
  , thrusting :: Bool
  , iTimer    :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Bullet
------------------------------------------------------------
data Bullet = Bullet
  { bPos  :: Point
  , bVel  :: Vector
  , bTime :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Asteroid
------------------------------------------------------------
data Asteroid = Asteroid
  { aPos       :: Point
  , aVel       :: Vector
  , aSize      :: Float
  , aRotation  :: Float
  , aTexture   :: Int
  } deriving (Show, Eq)

------------------------------------------------------------
-- Enemy (intelligent)
------------------------------------------------------------
data Enemy = Enemy
  { ePos :: Point
  , eVel :: Vector
  , eSize :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Initial state
------------------------------------------------------------
initialState :: GameState
initialState =
  let g = mkStdGen 43
      halfW = windowWidth / 2
      halfH = windowHeight / 2

      generateStars n gen = 
        if n <= 0 
        then ([], gen)
        else 
          let 
            (x, g1) = randomR (-halfW, halfW) gen
            (y, g2) = randomR (-halfH, halfH) g1
            (rest, g_final) = generateStars (n - 1) g2
          in ((x, y) : rest, g_final)

      (starList, finalGen) = generateStars 200 g

      startMenuShip = Ship { position = (-halfW - 50, 100), velocity = (40, -20), angle = -30, thrusting = True }
      startA1 = Asteroid { aPos = (halfW + 100, -100), aVel = (-30, 0), aSize = 40, aRotation = 0, aTexture = 2 }
      startA2 = Asteroid { aPos = (-halfW - 100, 200), aVel = (35, -15), aSize = 60, aRotation = 45, aTexture = 5 }
      startA3 = Asteroid { aPos = (100, halfH + 100), aVel = (-20, -40), aSize = 50, aRotation = 90, aTexture = 7 }
      startA4 = Asteroid { aPos = (-100, -halfH - 50), aVel = (15, 25), aSize = 25, aRotation = -45, aTexture = 1 }

  in GameState
    { player          = Ship { position = (0, 0)
                        , velocity = (0, 0)
                        , angle = 90
                        , angularV = 0
                        , thrusting = False
                        , iTimer = 0.0
                        }
    , bullets         = []
    , asteroids       = initialAsteroids
    , enemies         = []     -- no enemies at start
    , score           = 0
    , isPaused        = False
    , spawnTimer      = 1.0
    , enemySpawnTimer = 1.0
    , generator       = g 
    , stars           = starList
    , lives           = 3
    , highScore       = 0
    , startHighScore  = 0
    , currentScreen   = MainMenu
    , menuShip        = startMenuShip
    , menuAsteroid    = startA1
    , menuAsteroid2   = startA2
    , menuAsteroid3   = startA3
    , menuAsteroid4   = startA4
    }

newGameState :: GameState -> GameState
newGameState gs = initialState 
  { highScore = highScore gs
  , startHighScore = highScore gs
  , currentScreen = GameScreen
  , lives = 3
  }

------------------------------------------------------------
-- Initial asteroids
------------------------------------------------------------
initialAsteroids :: [Asteroid]
initialAsteroids = []

  --[ Asteroid { aPos = (150, 100),  aVel = (-30,  20), aSize = 40 }
  --, Asteroid { aPos = (-200, -150), aVel = (40, -15), aSize = 60 }
  --, Asteroid { aPos = (100, -200), aVel = (-25,  30), aSize = 50 }
  --]

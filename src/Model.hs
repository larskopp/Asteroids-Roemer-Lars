module Model where

import Graphics.Gloss.Interface.IO.Game

------------------------------------------------------------
-- Window setup
------------------------------------------------------------
windowWidth, windowHeight :: Float
windowWidth  = 800
windowHeight = 600

------------------------------------------------------------
-- Bullet settings
------------------------------------------------------------
bulletLifetime :: Float
bulletLifetime = 2.0  -- seconds

------------------------------------------------------------
-- Asteroid splitting constants
------------------------------------------------------------
minAsteroidSize, splitFactor :: Float
minAsteroidSize = 20     -- smallest asteroid radius before it stops splitting
splitFactor     = 0.6    -- size ratio for child asteroids

------------------------------------------------------------
-- Enemy behavior constants
------------------------------------------------------------
enemySpawnScore :: Int
enemySpawnScore = 50     -- spawn an enemy after this score

enemySpeed :: Float
enemySpeed = 60          -- pixels per second

------------------------------------------------------------
-- Game model
------------------------------------------------------------
data GameState = GameState
  { player     :: Ship
  , bullets    :: [Bullet]
  , asteroids  :: [Asteroid]
  , enemies    :: [Enemy]
  , score      :: Int
  , isPaused   :: Bool
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
  { aPos  :: Point
  , aVel  :: Vector
  , aSize :: Float
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
initialState = GameState
  { player     = Ship { position = (0, 0)
                      , velocity = (0, 0)
                      , angle = 90
                      , angularV = 0
                      , thrusting = False
                      }
  , bullets    = []
  , asteroids  = initialAsteroids
  , enemies    = []     -- no enemies at start
  , score      = 0
  , isPaused   = False
  }

------------------------------------------------------------
-- Initial asteroids
------------------------------------------------------------
initialAsteroids :: [Asteroid]
initialAsteroids =
  [ Asteroid { aPos = (150, 100),  aVel = (-30,  20), aSize = 40 }
  , Asteroid { aPos = (-200, -150), aVel = (40, -15), aSize = 60 }
  , Asteroid { aPos = (100, -200), aVel = (-25,  30), aSize = 50 }
  ]

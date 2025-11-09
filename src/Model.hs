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
-- Explosion settings
------------------------------------------------------------
explosionLifetime :: Float 
explosionLifetime = 1 -- 1 second animation

------------------------------------------------------------
-- Bullet settings
------------------------------------------------------------
playerBulletLifetime :: Float
playerBulletLifetime = 1.0  -- seconds

enemyBulletLifetime :: Float
enemyBulletLifetime = 2.0

enemyBulletSpeed :: Float
enemyBulletSpeed = 250

------------------------------------------------------------
-- Asteroid splitting constants
------------------------------------------------------------
minAsteroidSize, splitFactor :: Float
minAsteroidSize = 30
splitFactor     = 0.6

------------------------------------------------------------
-- Enemy behavior constants
------------------------------------------------------------
enemySpawnScore :: Int
enemySpawnScore = 500

shooterSpawnScore :: Int
shooterSpawnScore = 2500

enemySpeed :: Float
enemySpeed = 70

shooterFireRate :: Float
shooterFireRate = 1.0

------------------------------------------------------------
-- Screen State
------------------------------------------------------------
data Screen = MainMenu | ControlsScreen | GameScreen
  deriving (Show, Eq)

------------------------------------------------------------
-- Game model
------------------------------------------------------------
data GameState = GameState
  { player            :: Ship
  , bullets           :: [Bullet]
  , asteroids         :: [Asteroid]
  , enemies           :: [Enemy]
  , shooters          :: [Shooter]
  , enemyBullets      :: [EnemyBullet]
  , explosions        :: [Explosion]
  , score             :: Int
  , isPaused          :: Bool
  , spawnTimer        :: Float
  , enemySpawnTimer   :: Float
  , shooterSpawnTimer :: Float
  , generator         :: StdGen
  , stars             :: [Point]
  , lives             :: Int
  , highScore         :: Int
  , startHighScore    :: Int
  , currentScreen     :: Screen
  , menuShip          :: Ship
  , menuEnemy         :: Enemy
  , menuShooter       :: Shooter
  , menuAsteroid      :: Asteroid
  , menuAsteroid2     :: Asteroid
  , menuAsteroid3     :: Asteroid
  , menuAsteroid4     :: Asteroid
  , hasSavedHighScore :: Bool
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

data EnemyBullet = EnemyBullet
  { ebPos  :: Point
  , ebVel  :: Vector
  , ebTime :: Float
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
-- Enemy
------------------------------------------------------------
data Enemy = Enemy
  { ePos   :: Point
  , eVel   :: Vector
  , eSize  :: Float
  , eAngle :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Shooter
------------------------------------------------------------
data Shooter = Shooter
  { sPos       :: Point
  , sVel       :: Vector
  , sSize      :: Float
  , sFireTimer :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Explosion
------------------------------------------------------------
data Explosion = Explosion
  { exPos  :: Point
  , exTime :: Float
  , exSize :: Float
  } deriving (Show, Eq)

------------------------------------------------------------
-- Initial state
------------------------------------------------------------
initialState :: StdGen -> GameState
initialState g =
  let 
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

      startMenuShip = Ship { position = (-halfW - 50, 100)
                           , velocity = (40, -20)
                           , angle = -30
                           , thrusting = True
                           , iTimer = 0.0
                           , angularV = 0.0 }

      startA1 = Asteroid { aPos = (halfW + 150, -100)
                         , aVel = (-40, 10)
                         , aSize = 40
                         , aRotation = 0
                         , aTexture = 2 }

      startA2 = Asteroid { aPos = (-halfW - 300, 200)
                         , aVel = (35, -15)
                         , aSize = 60
                         , aRotation = 45
                         , aTexture = 5 } 

      startA3 = Asteroid { aPos = (100, halfH + 400)
                         , aVel = (-20, -40)
                         , aSize = 50
                         , aRotation = 90
                         , aTexture = 7 } 

      startA4 = Asteroid { aPos = (-100, -halfH - 500)
                         , aVel = (15, 25)
                         , aSize = 25
                         , aRotation = -45
                         , aTexture = 1 } 

      startEnemy = Enemy { ePos = (halfW + 250, halfH + 100)
                         , eVel = (-50, -30)
                         , eSize = 25.0
                         , eAngle = 10.0 }

      startShooter = Shooter { sPos = (-halfW - 150, -250)
                             , sVel = (30, 10)
                             , sSize = 30.0
                             , sFireTimer = shooterFireRate }

  in GameState
    { player            = Ship { position = (0, 0)
                               , velocity = (0, 0)
                               , angle = 90
                               , angularV = 0
                               , thrusting = False
                               , iTimer = 0.0 }
    , bullets           = []
    , asteroids         = []
    , enemies           = []
    , shooters          = []
    , enemyBullets      = []
    , explosions        = []
    , score             = 0
    , isPaused          = False
    , spawnTimer        = 0.5
    , enemySpawnTimer   = 1.0
    , shooterSpawnTimer = 1.0
    , generator         = finalGen
    , stars             = starList
    , lives             = 3
    , highScore         = 0
    , startHighScore    = 0
    , currentScreen     = MainMenu
    , menuShip          = startMenuShip
    , menuEnemy         = startEnemy
    , menuShooter       = startShooter
    , menuAsteroid      = startA1
    , menuAsteroid2     = startA2
    , menuAsteroid3     = startA3
    , menuAsteroid4     = startA4
    , hasSavedHighScore = False
    }


------------------------------------------------------------
-- Create a new game while keeping high score and RNG
------------------------------------------------------------
newGameState :: GameState -> GameState
newGameState gs =
  let g   = generator gs
      hs  = highScore gs
      shs = startHighScore gs
  in (initialState g)
        { highScore = hs
        , startHighScore = shs
        , currentScreen = GameScreen
        , hasSavedHighScore = False
        }

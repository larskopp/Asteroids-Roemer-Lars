module Update (step) where

import Graphics.Gloss
import Model
import System.Random

------------------------------------------------------------
-- Main step function
------------------------------------------------------------
step :: Float -> GameState -> GameState
step dt gs
  | isPaused gs = gs
  | otherwise   =
      handleCollisions $
      (spawnAsteroid dt) $
      (spawnEnemies dt) $
      gs { player    = moveShip dt (player gs)
         , bullets   = updateBullets dt (bullets gs)
         , asteroids = updateAsteroids dt (asteroids gs)
         , enemies   = updateEnemies dt (player gs) (enemies gs)
         }


------------------------------------------------------------
-- Ship movement
------------------------------------------------------------
moveShip :: Float -> Ship -> Ship
moveShip dt ship =
  let (x, y)     = position ship
      (vx, vy)   = velocity ship
      a          = angle ship
      av         = angularV ship
      a'         = a + av * dt
      thrustMag  = if thrusting ship then 200 else 0
      rad        = a' * pi / 180
      ax         = thrustMag * cos rad
      ay         = thrustMag * sin rad
      vx' = (vx + ax * dt) * drag
      vy' = (vy + ay * dt) * drag
      x'  = wrap windowWidth  (x + vx' * dt)
      y'  = wrap windowHeight (y + vy' * dt)
  in ship { position = (x', y'), velocity = (vx', vy'), angle = a' }

drag :: Float
drag = 0.99

------------------------------------------------------------
-- Bullets
------------------------------------------------------------
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets dt =
  filter ((< bulletLifetime) . bTime)
  . map (moveBullet dt)

moveBullet :: Float -> Bullet -> Bullet
moveBullet dt b =
  let (x, y) = bPos b
      (vx, vy) = bVel b
      x' = wrap windowWidth  (x + vx * dt)
      y' = wrap windowHeight (y + vy * dt)
  in b { bPos = (x', y'), bTime = bTime b + dt }

------------------------------------------------------------
-- Asteroids
------------------------------------------------------------
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids dt = map (moveAsteroid dt)

getNewSpawnRate :: StdGen -> Int -> (Float, StdGen)
getNewSpawnRate g currentScore =
  let
    minSpawnTime = 1.0
    maxSpawnTime = 5.0
    scoreFactor = min (fromIntegral currentScore / 10000.0) 1.0 

    lowerBound = minSpawnTime 
    upperBound = maxSpawnTime - (maxSpawnTime - minSpawnTime) * scoreFactor

  in randomR (lowerBound, upperBound) g

spawnAsteroid :: Float -> GameState -> GameState
spawnAsteroid dt gs = 
  let 
    newTimer = spawnTimer gs - dt
  in 
    if newTimer <= 0
      then 
        let 
          g = generator gs
          sc = score gs
          (nextSpawnTime, g') = getNewSpawnRate g sc 
          (newA, g'') = newAsteroid g'
        in 
          gs { asteroids = newA : asteroids gs
             , spawnTimer = nextSpawnTime
             , generator = g''
             }
      else gs { spawnTimer = newTimer }

newAsteroid :: StdGen -> (Asteroid, StdGen)
newAsteroid g = 
  let
    halfWidth  = windowWidth / 2
    halfHeight = windowHeight / 2
    buffer = 100.0

    -- Genereer de willekeurige rand (gebruikt g, geeft g1 terug)
    (edge, g1) = randomR (0 :: Int, 3) g

    -- Gebruik de generator in de return, en de inkomende g1 voor de randomR calls
    (startX, startY, g2) = case edge of
      0 -> -- Bovenrand
        let (x, g_next) = randomR (-halfWidth, halfWidth) g1
        in (x, halfHeight + buffer, g_next)
      1 -> -- Onderrand
        let (x, g_next) = randomR (-halfWidth, halfWidth) g1
        in (x, -halfHeight - buffer, g_next)
      2 -> -- Linkerrand
        let (y, g_next) = randomR (-halfHeight, halfHeight) g1
        in (-halfWidth - buffer, y, g_next)
      3 -> -- Rechterrand
        let (y, g_next) = randomR (-halfHeight, halfHeight) g1
        in (halfWidth + buffer, y, g_next)
      _ -> (0, 0, g1)

    startPos = (startX, startY)

    -- Gebruik g2 als de basis voor de volgende willekeurige getallen
    (minV, maxV) = (40, 100)
    (baseSpeed, g3) = randomR (minV, maxV) g2

    (minSize, maxSize) = (40, 60)
    (size, g4) = randomR (minSize, maxSize) g3
    
    (targetX, g5) = randomR (-halfWidth, halfWidth) g4
    (targetY, g6) = randomR (-halfHeight, halfHeight) g5
    
    dx = targetX - startX
    dy = targetY - startY
    
    magnitude = sqrt (dx*dx + dy*dy)
    velX = baseSpeed * (dx / magnitude)
    velY = baseSpeed * (dy / magnitude)
    
  in (Asteroid { aPos = startPos
               , aVel = (velX, velY)
               , aSize = size
               }, g6)


moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid dt a =
  let (x, y) = aPos a
      (vx, vy) = aVel a
      x' = wrap windowWidth  (x + vx * dt)
      y' = wrap windowHeight (y + vy * dt)
  in a { aPos = (x', y') }
------------------------------------------------------------
-- Enemies
------------------------------------------------------------
updateEnemies :: Float -> Ship -> [Enemy] -> [Enemy]
updateEnemies dt ship = map (chasePlayer dt (position ship))

chasePlayer :: Float -> Point -> Enemy -> Enemy
chasePlayer dt (px, py) e =
  let (x, y) = ePos e
      dx = px - x
      dy = py - y
      distToPlayer = sqrt (dx*dx + dy*dy) + 1
      vx = enemySpeed * dx / distToPlayer
      vy = enemySpeed * dy / distToPlayer
      x' = wrap windowWidth  (x + vx * dt)
      y' = wrap windowHeight (y + vy * dt)
  in e { ePos = (x', y'), eVel = (vx, vy) }


newEnemy :: StdGen -> (Enemy, StdGen)
newEnemy g =
  let
    halfWidth  = windowWidth / 2
    halfHeight = windowHeight / 2
    buffer = 100.0
    eSize = 25.0 

    (edge, g1) = randomR (0 :: Int, 3) g

    (startX, startY, g2) = case edge of
      0 -> -- Bovenrand 
        let (x, g_temp) = randomR (-halfWidth, halfWidth) g1
        in (x, halfHeight + buffer, g_temp)
      1 -> -- Onderrand 
        let (x, g_temp) = randomR (-halfWidth, halfWidth) g1
        in (x, -halfHeight - buffer, g_temp)
      2 -> -- Linkerrand
        let (y, g_temp) = randomR (-halfHeight, halfHeight) g1
        in (-halfWidth - buffer, y, g_temp)
      3 -> -- Rechterrand
        let (y, g_temp) = randomR (-halfHeight, halfHeight) g1
        in (halfWidth + buffer, y, g_temp)
      _ -> (0, 0, g1)

    startPos = (startX, startY)
 in (Enemy { ePos = startPos, eVel = (0, 0), eSize = eSize }, g2)

getNewEnemySpawnRate :: StdGen -> Int -> (Float, StdGen)
getNewEnemySpawnRate g currentScore =
  let
    minTime = 1.0
    maxTime = 5.0

    scoreFactor = min (fromIntegral currentScore / 10000.0) 1.0 
    
    upperBound = maxTime - (maxTime - minTime) * scoreFactor
    
  in randomR (minTime, upperBound) g


spawnEnemies :: Float -> GameState -> GameState
spawnEnemies dt gs
  | score gs >= enemySpawnScore && null (enemies gs) = -- Spawn new enemies when score threshold reached
    let 
      newTimer = enemySpawnTimer gs - dt
    in 
      if newTimer <=0 
        then 
          let 
            g = generator gs
            sc = score gs
            (nextSpawnTime, g1) = getNewEnemySpawnRate g sc
            (newE, g2) = newEnemy g1 
          in
            gs { enemies = [newE]
               , enemySpawnTimer = nextSpawnTime 
               , generator = g2 }
        else gs { enemySpawnTimer = newTimer}
  | otherwise = gs


------------------------------------------------------------
-- Asteroid Score, Collisions and splitting
------------------------------------------------------------
getAsteroidScore :: Asteroid -> Int
getAsteroidScore a
    | aSize a >= 40.0 = 20    -- big asteroid 
    | aSize a >= 25.0 = 50    -- medium asteroid
    | otherwise       = 100   -- small asteroid

handleCollisions :: GameState -> GameState
handleCollisions gs =
  let (bs1, as1, sc1) = collideAll (bullets gs) (asteroids gs) (score gs)
      (bs2, es1, sc2) = collideEnemies bs1 (enemies gs) sc1
      enemyHit        = any (\e -> dist (ePos e) (position (player gs)) < eSize e) es1
      asteroidHit     = any (\a -> dist (aPos a) (position (player gs)) < 10 + aSize a) as1
      shipHit         = enemyHit || asteroidHit
  in if shipHit
     then initialState  {generator = generator gs} -- reset game if ship hit by enemy
     else gs { bullets = bs2, asteroids = as1, enemies = es1, score = sc2 }

------------------------------------------------------------
-- Bullet vs Asteroid collisions
------------------------------------------------------------
collideAll :: [Bullet] -> [Asteroid] -> Int -> ([Bullet], [Asteroid], Int)
collideAll bs as sc =
  let hits = [ (b,a)
             | b <- bs, a <- as
             , dist (bPos b) (aPos a) < aSize a
             ]
      hitBullets    = map fst hits
      hitAsteroids  = map snd hits
      bs'           = filter (`notElem` hitBullets) bs
      survivors     = filter (`notElem` hitAsteroids) as
      splitChildren = concatMap splitAsteroid hitAsteroids
      scoreIncrease = sum (map getAsteroidScore hitAsteroids)
      sc'           = sc + scoreIncrease
  in (bs', survivors ++ splitChildren, sc')

splitAsteroid :: Asteroid -> [Asteroid]
splitAsteroid a
  | aSize a <= minAsteroidSize = []
  | otherwise =
      let (vx, vy) = aVel a
          newSize  = aSize a * splitFactor
          a1 = a { aVel = ( vy, -vx ), aSize = newSize }
          a2 = a { aVel = (-vy,  vx ), aSize = newSize }
      in [a1, a2]

------------------------------------------------------------
-- Bullet vs Enemy collisions
------------------------------------------------------------
collideEnemies :: [Bullet] -> [Enemy] -> Int -> ([Bullet], [Enemy], Int)
collideEnemies bs es sc =
  let hits = [ (b,e)
             | b <- bs, e <- es
             , dist (bPos b) (ePos e) < eSize e
             ]
      hitBullets   = map fst hits
      hitEnemies   = map snd hits
      bs'          = filter (`notElem` hitBullets) bs
      survivors    = filter (`notElem` hitEnemies) es
      sc'          = sc + 200 * length hitEnemies
  in (bs', survivors, sc')

------------------------------------------------------------
-- Utilities
------------------------------------------------------------
dist :: Point -> Point -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

wrap :: Float -> Float -> Float
wrap limit coord
  | coord >  limit / 2 = coord - limit
  | coord < -limit / 2 = coord + limit
  | otherwise          = coord

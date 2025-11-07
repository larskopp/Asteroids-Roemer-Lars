module Update (step) where

import Graphics.Gloss
import Model
import System.Random

------------------------------------------------------------
-- Main step function
------------------------------------------------------------
step :: Float -> GameState -> GameState
step dt gs
  | currentScreen gs == MainMenu || currentScreen gs == ControlsScreen = updateMenu dt gs
  | isPaused gs = gs
  | otherwise   =
     let
     player' = (player gs) {iTimer = max 0 (iTimer (player gs) - dt) }
     gs' = gs {player = player'}
     in
      handleCollisions $
      (spawnAsteroid dt) $
      (spawnEnemies dt) $
      gs { player    = moveShip dt (player')
         , bullets   = updateBullets dt (bullets gs)
         , asteroids = updateAsteroids dt (asteroids gs)
         , enemies   = updateEnemies dt (player gs) (enemies gs)
         }

------------------------------------------------------------
-- Menu animation
------------------------------------------------------------
updateMenu :: Float -> GameState -> GameState
updateMenu dt gs =
    let 
        halfW = windowWidth / 2
        halfH = windowHeight / 2

        ship = menuShip gs
        (sx, sy) = position ship
        (svx, svy) = velocity ship
        sx' = sx + svx * dt -- FIX: Gebruik svx en svy
        sy' = sy + svy * dt

        ship' = if sx' > halfW + 50 || sy' < -halfH - 50
                then ship { position = (-halfW - 50, 150), angle = -30, thrusting = True }
                else ship { position = (sx', sy'), angle = -30, thrusting = True }

        updateAndRespawnAsteroid a resetPos resetRot =
          let 
              (ax, ay) = aPos a
              (avx, avy) = aVel a
              ax' = ax + avx * dt
              ay' = ay + avy * dt
              aRot' = aRotation a + dt * 10
              
              -- Respawn conditie: buiten links/rechts of boven/onder
              isOutside = ax' < -halfW - 100 || ax' > halfW + 100 || ay' < -halfH - 100 || ay' > halfH + 100
          in 
              if isOutside
              then a { aPos = resetPos, aRotation = resetRot }
              else a { aPos = (ax', ay'), aRotation = aRot' }

        a1' = updateAndRespawnAsteroid (menuAsteroid gs)  (halfW + 100, -100) 0
        a2' = updateAndRespawnAsteroid (menuAsteroid2 gs) (-halfW - 100, 200) 45
        a3' = updateAndRespawnAsteroid (menuAsteroid3 gs) (100, halfH + 100) 90
        a4' = updateAndRespawnAsteroid (menuAsteroid4 gs) (-200, -halfH - 50) (-45)
                    
    in gs { menuShip = ship', 
            menuAsteroid = a1',
            menuAsteroid2 = a2',
            menuAsteroid3 = a3',
            menuAsteroid4 = a4' }

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
      0 -> -- Top
        let (x, g_next) = randomR (-halfWidth, halfWidth) g1
        in (x, halfHeight + buffer, g_next)
      1 -> -- Bottem 
        let (x, g_next) = randomR (-halfWidth, halfWidth) g1
        in (x, -halfHeight - buffer, g_next)
      2 -> -- Left
        let (y, g_next) = randomR (-halfHeight, halfHeight) g1
        in (-halfWidth - buffer, y, g_next)
      3 -> -- Right
        let (y, g_next) = randomR (-halfHeight, halfHeight) g1
        in (halfWidth + buffer, y, g_next)
      _ -> (0, 0, g1)

    startPos = (startX, startY)

    (minV, maxV) = (40, 100)
    (baseSpeed, g3) = randomR (minV, maxV) g2

    (minSize, maxSize) = (40, 60)
    (size, g4) = randomR (minSize, maxSize) g3
    
    (randRot, g5) = randomR (0.0, 360.0) g4 
    (randTex, g6) = randomR (1 :: Int, 7 :: Int) g5

    (targetX, g7) = randomR (-halfWidth, halfWidth) g6
    (targetY, g8) = randomR (-halfHeight, halfHeight) g7
    
    dx = targetX - startX
    dy = targetY - startY
    
    magnitude = sqrt (dx*dx + dy*dy)
    velX = baseSpeed * (dx / magnitude)
    velY = baseSpeed * (dy / magnitude)
    
  in (Asteroid { aPos = startPos
               , aVel = (velX, velY)
               , aSize = size
               , aRotation = randRot  -- Veld toevoegen
               , aTexture = randTex   -- Veld toevoegen
               }, g8)


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
      0 -> -- Top
        let (x, g_temp) = randomR (-halfWidth, halfWidth) g1
        in (x, halfHeight + buffer, g_temp)
      1 -> -- Bottem
        let (x, g_temp) = randomR (-halfWidth, halfWidth) g1
        in (x, -halfHeight - buffer, g_temp)
      2 -> -- Left
        let (y, g_temp) = randomR (-halfHeight, halfHeight) g1
        in (-halfWidth - buffer, y, g_temp)
      3 -> -- Right
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

resetPlayer :: GameState -> GameState
resetPlayer gs =
  let 
    newLives = lives gs - 1
    currentScore = score gs
    newHighScore = max (highScore gs) currentScore
    
    isGameOver = newLives <= 0
    
    baseState = initialState
    
    gameOverState = baseState 
                    { 
                    generator = generator gs,
                    startHighScore = highScore gs,
                    score = currentScore,
                    highScore = newHighScore,
                    lives = 0,
                    isPaused  = True,
                    currentScreen = GameScreen
                    }
    
    respawnState = gs { 
      player = (player gs) { 
        position = (0, 0), 
        velocity = (0, 0),
        angle = 0,
        angularV = 0,
        iTimer = invincibilityDuration
      },
      bullets = [], 
      lives = newLives,
      highScore = highScore gs
    }
    
  in if isGameOver
      then gameOverState
      else respawnState

handleCollisions :: GameState -> GameState
handleCollisions gs =
  let g_in = generator gs
      (bs1, as1, sc1, g_out) = collideAll g_in (bullets gs) (asteroids gs) (score gs)
      (bs2, es1, sc2) = collideEnemies bs1 (enemies gs) sc1

      isVulnerable    = iTimer (player gs) <=0
      enemyHit        = any (\e -> dist (ePos e) (position (player gs)) < eSize e) es1
      asteroidHit     = any (\a -> dist (aPos a) (position (player gs)) < 10 + aSize a) as1
      shipHit         = enemyHit || asteroidHit

      newState        = gs { bullets = bs2, asteroids = as1, enemies = es1, score = sc2, generator = g_out}
  in if shipHit && isVulnerable
     then resetPlayer  newState -- reset player if ship hit by enemy
     else newState

------------------------------------------------------------
-- Bullet vs Asteroid collisions
------------------------------------------------------------
collideAll :: StdGen -> [Bullet] -> [Asteroid] -> Int -> ([Bullet], [Asteroid], Int, StdGen)
collideAll g bs as sc =
  let hits = [ (b,a)
             | b <- bs, a <- as
             , dist (bPos b) (aPos a) < aSize a
             ]
      hitBullets    = map fst hits
      hitAsteroids  = map snd hits
      bs'           = filter (`notElem` hitBullets) bs
      survivors     = filter (`notElem` hitAsteroids) as
      (splitChildren, g_out) = foldl splitAsteroidAndChain ([], g) hitAsteroids
      scoreIncrease = sum (map getAsteroidScore hitAsteroids)
      sc'           = sc + scoreIncrease
  in (bs', survivors ++ splitChildren, sc', g_out)

splitAsteroidAndChain :: ([(Asteroid)], StdGen) -> Asteroid -> ([(Asteroid)], StdGen)
splitAsteroidAndChain (accAsteroids, g_in) a =
  let (newAsteroids, g_out) = splitAsteroid g_in a
  in (accAsteroids ++ newAsteroids, g_out ) 

splitAsteroid :: StdGen -> Asteroid -> ([Asteroid], StdGen)
splitAsteroid g a
  | aSize a <= minAsteroidSize = ([], g)
  | otherwise =
      let (vx, vy) = aVel a
          newSize  = aSize a * splitFactor

          (r1, g1) = randomR (0.0, 360.0) g
          (t1, g2) = randomR (1 :: Int, 7 :: Int) g1

          (r2, g3) = randomR (0.0, 360.0) g2
          (t2, g4) = randomR (1 :: Int, 7 :: Int) g3

          a1 = a { aVel = ( vy, -vx ), aSize = newSize, aRotation = r1, aTexture = t1 }
          a2 = a { aVel = (-vy,  vx ), aSize = newSize, aRotation = r2, aTexture = t2 }
      in ([a1, a2], g4)

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

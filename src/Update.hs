module Update (step) where

import Graphics.Gloss
import Model

------------------------------------------------------------
-- Main step function
------------------------------------------------------------
step :: Float -> GameState -> GameState
step dt gs
  | isPaused gs = gs
  | otherwise   =
      handleCollisions $
      spawnEnemies $
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

-- Spawn new enemies when score threshold reached
spawnEnemies :: GameState -> GameState
spawnEnemies gs
  | score gs >= enemySpawnScore && null (enemies gs) =
      gs { enemies = [Enemy { ePos = (300, 200), eVel = (0, 0), eSize = 25 }] }
  | otherwise = gs

------------------------------------------------------------
-- Collisions and splitting
------------------------------------------------------------
handleCollisions :: GameState -> GameState
handleCollisions gs =
  let (bs1, as1, sc1) = collideAll (bullets gs) (asteroids gs) (score gs)
      (bs2, es1, sc2) = collideEnemies bs1 (enemies gs) sc1
      shipHit         = any (\e -> dist (ePos e) (position (player gs)) < eSize e) es1
  in if shipHit
     then initialState  -- reset game if ship hit by enemy
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
      sc'           = sc + 10 * length hitAsteroids
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
      sc'          = sc + 50 * length hitEnemies
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

module View (draw) where

import Graphics.Gloss
import Model

------------------------------------------------------------
-- Main draw function
------------------------------------------------------------
draw :: GameState -> Picture
draw gs = pictures
  [ drawShip (player gs)
  , pictures (map drawBullet (bullets gs))
  , pictures (map drawAsteroid (asteroids gs))
  , pictures (map drawEnemy (enemies gs))
  , drawScore (score gs)
  ]

------------------------------------------------------------
-- Draw ship
------------------------------------------------------------
drawShip :: Ship -> Picture
drawShip ship =
  translate x y $
  rotate (-(angle ship)) $
  color white $
  polygon [(-10,-10),(20,0),(-10,10)]
  where
    (x, y) = position ship

------------------------------------------------------------
-- Draw bullet
------------------------------------------------------------
drawBullet :: Bullet -> Picture
drawBullet b =
  translate x y $
  color orange $
  thickCircle 1 2
  where
    (x, y) = bPos b

------------------------------------------------------------
-- Draw asteroid
------------------------------------------------------------
drawAsteroid :: Asteroid -> Picture
drawAsteroid a =
  translate x y $
  color (greyN 0.6) $
  circleSolid (aSize a)
  where
    (x, y) = aPos a

------------------------------------------------------------
-- Draw enemy (👾)
------------------------------------------------------------
drawEnemy :: Enemy -> Picture
drawEnemy e =
  translate x y $
  color red $
  polygon [(-10,-10),(10,-10),(0,15)]
  where
    (x, y) = ePos e

------------------------------------------------------------
-- Draw score
------------------------------------------------------------
drawScore :: Int -> Picture
drawScore sc =
  translate (-windowWidth/2 + 20) (windowHeight/2 - 40) $
  scale 0.15 0.15 $
  color white $
  text ("Score: " ++ show sc)

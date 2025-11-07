module View (draw) where

import Graphics.Gloss
import Model

------------------------------------------------------------
-- Main draw function
------------------------------------------------------------
draw :: GameState -> Picture
draw gs = pictures
  [ drawStars (stars gs)
  ,  drawShip (player gs)
  , pictures (map drawBullet (bullets gs))
  , pictures (map drawAsteroid (asteroids gs))
  , pictures (map drawEnemy (enemies gs))
  , drawScore (score gs)
  ]

------------------------------------------------------------
--Draw stars
------------------------------------------------------------

drawStars :: [Point] -> Picture
drawStars points =
  color (greyN 0.2) $
  pictures $ map drawStar points
  where
    drawStar (x, y) = translate x y $ circleSolid 1

------------------------------------------------------------
-- Draw ship
------------------------------------------------------------
drawShip :: Ship -> Picture
drawShip ship =
  translate x y $
  rotate (-(angle ship)) $
  pictures
    [
      drawThrust ship ,
      color white $
      polygon [(-10,-10),(20,0),(-10,10)]
    ]
  where
    (x, y) = position ship

drawThrust :: Ship -> Picture
drawThrust ship
  | thrusting ship =
    let 
      flickerFactor = (sin (positionX / 100) + 1) / 2 
      positionX     = fst (position ship)

      outerLength  = 13 + 5 * flickerFactor
      outerBaseY   = 7 
      
      innerLength  = outerLength * 0.7 
      innerBaseY   = 4
      
      baseX = -10
      
      innerFlame = 
        [ (baseX, -innerBaseY)
        , (baseX - innerLength, 0)
        , (baseX, innerBaseY)
        ]
      
      outerFlame = 
        [ (baseX, -outerBaseY)
        , (baseX - outerLength, 0)
        , (baseX, outerBaseY)
        ]
        
      outerColor = mixColors (1 - flickerFactor) flickerFactor red orange
      innerColor = mixColors flickerFactor (1 - flickerFactor) yellow orange
      
    in 
      pictures
      [ color outerColor (polygon outerFlame)
      , color innerColor (polygon innerFlame)
      ]

  | otherwise = blank

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
  translate (-windowWidth /2 + 20) (windowHeight/2 - 40) $
  scale 0.15 0.15 $
  color white $
  text ("Score: " ++ show sc)

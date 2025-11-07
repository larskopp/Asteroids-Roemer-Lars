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
  rotate (-(aRotation a)) $
  scale scaleFactor scaleFactor $
  color color_scheme $
  case aTexture a of
    1 -> asteroidShape1
    2 -> asteroidShape2
    3 -> asteroidShape3
    4 -> asteroidShape4
    5 -> asteroidShape5
    6 -> asteroidShape6
    7 -> asteroidShape7    
    _ -> circleSolid 1
  where
    (x, y) = aPos a
    scaleFactor = (aSize a) / 50.0 
    color_scheme = greyN 0.6
    
    asteroidShape1 = polygon
      [ (60, 10), (20, 35), (-10, 40), (-55, 0), (-40, -20), (-5, -45), (30, -30) ]
    
    asteroidShape2 = polygon 
      [ (50, 0), (30, 30), (0, 40), (-30, 20), (-50, 0), (-20, -30), (10, -40), (40, -20) ]
    
    asteroidShape3 = polygon 
      [ (40, 40), (15, 50), (-10, 45), (-45, 20), (-50, -10), (-40, -40), (-20, -50), (10, -40), (40, -30), (55, 0) ]

    asteroidShape4 = polygon
      [ (45, 10), (10, 50), (-40, 40), (-50, -10), (-30, -45), (10, -35) ]
    
    asteroidShape5 = polygon
      [ (50, 10), (35, 40), (0, 30), (-20, 20), (-40, -5), (-30, -30), (0, -40), (30, -35), (45, -15) ]

    asteroidShape6 = polygon
      [ (40, 0), (35, 40), (-30, 45), (-40, 5), (-20, -50), (20, -40) ]

    asteroidShape7 = polygon
      [ (40, 20), (20, 40), (-10, 35), (-30, 10), (-45, -20), (-25, -40), (10, -50), (35, -30) ]

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

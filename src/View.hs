module View (draw) where

import Graphics.Gloss
import Model

------------------------------------------------------------
-- Custom Colors
------------------------------------------------------------
darkRed :: Color
darkRed = makeColor 0.7 0.0 0.0 1.0

------------------------------------------------------------
-- Main draw function
------------------------------------------------------------
draw :: GameState -> Picture
draw gs 
  | lives gs <= 0 = drawGameOverScreen gs
  | otherwise     = drawGame gs
------------------------------------------------------------
-- Game draw
------------------------------------------------------------
drawGame :: GameState -> Picture
drawGame gs = pictures
  [ drawStars (stars gs)
  , drawShip (player gs)
  , pictures (map drawBullet (bullets gs))
  , pictures (map drawAsteroid (asteroids gs))
  , pictures (map drawEnemy (enemies gs))
  , drawScore (score gs)
  , drawLives (lives gs)
  , drawHighScore (highScore gs)
  ]

------------------------------------------------------------
-- Game Over screen
------------------------------------------------------------
drawGameOverScreen :: GameState -> Picture
drawGameOverScreen gs =
    let 
        currentScore = score gs
        highScoreVal = startHighScore gs
        isNewHighScore = currentScore > highScoreVal
        
        displayedHighScore = max currentScore highScoreVal

        oldHighScoreText = if currentScore > highScoreVal
                           then "Previous High Score: " ++ show highScoreVal
                           else "High Score: " ++ show highScoreVal

        gameOverText = translate (-200) 150 $ scale 0.5 0.5 $ color white $ text "GAME OVER"

        scoreText = translate (-150) 50 $ scale 0.2 0.2 $ color white $ text $ "SCORE: " ++ show currentScore

        highScoreText = translate (-200) 0 $ scale 0.2 0.2 $ color white $ text oldHighScoreText

        newHighScoreBanner = translate (-300) (-80) $ scale 0.3 0.3 $ color yellow $ text "NEW HIGH SCORE!"

        restartText = translate (-250) (-200) $ scale 0.15 0.15 $ color (greyN 0.5) $ text "Press R to start a new game"

        elements = [gameOverText, scoreText, highScoreText, restartText]

        finalElements = if currentScore > highScoreVal then newHighScoreBanner : elements else elements

    in pictures finalElements
    

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
  let
    invincibilityTime = iTimer ship
    isInvincible = invincibilityTime > 0
    blinkOn = round (invincibilityTime * 4) `mod` 2 == 0
    
    shipPicture = pictures
             [ 
               drawThrust ship,  -- Teken de vlammen
               color white $ 
               polygon [(-10,-10),(20,0),(-10,10)] -- Het schip zelf
             ]
  in
    if isInvincible && not blinkOn
      then blank
      else translate x y $
           rotate (-(angle ship)) $
           shipPicture
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

------------------------------------------------------------
-- Draw high score
------------------------------------------------------------
drawHighScore :: Int -> Picture
drawHighScore hsc =
  let
    internalScale = 0.12
    displayScale = 1.2 -- Schaal de hele structuur licht op

    textPicture = 
      scale internalScale internalScale $
      color (greyN 0.8) $
      text ("High Score: " ++ show hsc)

  in
    translate (windowWidth/2 - 530) (windowHeight/2 - 65) $
    scale displayScale displayScale $
    translate (-25 / internalScale) 0 $
    textPicture

------------------------------------------------------------
-- Draw lives
------------------------------------------------------------

drawLives :: Int -> Picture
drawLives numLives =
  let
    heartRedPixels = 
      [ (-3, 0), (-3, -1), (-3, -2)
      , (-2, 1), (-2, 0), (-2, -1), (-2, -2), (-2, -3)
      , (-1, 1), (-1, 0), (-1, -1), (-1, -2), (-1, -3), (-1, -4)
      , (0, 0), (0,-1), (0, -2), (0, -3), (0, -4), (0,-5)
      , (1, -1), (1, -2), (1, -3), (1, -4), (1, -5), (1, -6)
      , (2, -1), (2, -2), (2, -3), (2, -4), (2, -5), (2, -6), (2, -7)
      , (3, 0), (3, -1), (3, -2), (3, -3), (3, -4), (3, -5), (3, -6), (3, -7)
      , (4, 1), (4, 0), (4, -1), (4, -2), (4, -3), (4, -4), (4, -5), (4, -6)
      , (5, 1), (5, -1), (5, -2), (5, -3), (5, -4), (5, -5)
      , (6, 1), (6, 0), (6, -2), (6, -3), (6, -4)
      , (7, 0), (7, -1), (7, -2), (7, -3)
      ]

    heartShinePixels = 
      [ (5, 0), (6, -1) ]

    shadow = 
      [ (-4,0), (-4,-1), (-4,-2), (-4, -3)
      , (-3, 1), (-3, -3), (-3,-4)
      , (-2, -4), (-2,-5)
      , (-1, -5), (-1, -6)
      , (0, -6), (0, -7)
      , (1, -7), (1,-8)
      , (2, -8)
      ]

    heartBorder = 
      [ (-5,0), (-5, -1), (-5,-2), (-5,-3)
      , (-4, 1), (-4, -4)
      , (-3, 2), (-3, -5)
      , (-2, 2), (-2, -6)
      , (-1, 2), (-1, -7)
      , (0, 1), (0, -8)
      , (1, 0), (1, -9)
      , (2, 0), (2, -9)
      , (3, 1), (3, -8)
      , (4, 2), (4, -7)
      , (5, 2), (5, -6)
      , (6, 2), (6, -5)
      , (7, 1), (7, -4)
      , (8, 0), (8, -1), (8,-2), (8, -3)
      ]

    pixelSize = 2.0 

    drawPixels :: Color -> [(Float, Float)] -> Picture
    drawPixels c points = 
        color c $
        pictures [ translate (x * pixelSize) (y * pixelSize) (rectangleSolid pixelSize pixelSize) | (x, y) <- points ]

    pixelHeart :: Picture
    pixelHeart = 
        translate 0 (-1 * pixelSize) $
        pictures [ 
             drawPixels black heartBorder
           , drawPixels darkRed shadow
           , drawPixels red heartRedPixels 
           , drawPixels white heartShinePixels
        ]

    spacing = 45
    
    hearts = 
      [ translate (x * spacing) 0 pixelHeart
      | i <- [1..numLives]
      , let x = fromIntegral i * 0.75 -- Compacte positionering
      ]
  in
    translate (-windowWidth/2-5) (windowHeight/2 - 80) $
    pictures hearts
module View (draw) where

import Graphics.Gloss
import Model

------------------------------------------------------------
-- Custom Colors
------------------------------------------------------------
darkRed :: Color
darkRed = makeColor 0.7 0.0 0.0 1.0
lightGrey :: Color
lightGrey = makeColor 0.8 0.8 0.8 1.0 
pauseOverlayColor :: Color
pauseOverlayColor = makeColor 1.0 1.0 1.0 0.1

enemyBulletColor :: Color
enemyBulletColor = makeColor 0.0 1.0 1.0 1.0

shooterSpots :: Color 
shooterSpots = makeColor 0.52 0.79 0.98 1.0
shooterHull :: Color
shooterHull = makeColor 0.32 0.43 0.51 1.0
shooterWindow :: Color
shooterWindow = makeColor 0.75 0.91 0.99 1.0
shooterRing :: Color
shooterRing = makeColor 0.42 0.49 0.52 1.0

------------------------------------------------------------
-- Main draw function
------------------------------------------------------------
draw :: GameState -> Picture
draw gs = 
  case currentScreen gs of  
    MainMenu          -> drawMainMenu gs
    ControlsScreen    -> drawControlsScreen gs
    GameScreen
      | lives gs <= 0 -> drawGameOverScreen gs
      | otherwise     -> drawGame gs

------------------------------------------------------------
-- Main Menu screen
------------------------------------------------------------
drawMainMenu :: GameState -> Picture
drawMainMenu gs = pictures
  [ drawStars (stars gs)

  , drawMenuShip (menuShip gs)
  , drawAsteroid (menuAsteroid gs)
  , drawAsteroid (menuAsteroid2 gs)
  , drawAsteroid (menuAsteroid3 gs)
  , drawAsteroid (menuAsteroid4 gs)

  , translate (-175) 150 $ scale 0.5 0.5 $ color white $ text "ASTEROIDS"
  , translate (-80) 50 $ scale 0.15 0.15 $ color lightGrey $ text ("High Score: " ++ show (highScore gs))
  , translate (-63) (-50) $ pictures 
    [ 
      translate (-80) (-10) $ scale 0.15 0.15 $ color white $ text "Press N for a New Game" 
    ]
  , translate (-53) (-150) $ pictures 
    [ 
      translate (-80) (-10) $ scale 0.15 0.15 $ color white $ text "Press C for the Controls" 
    ]
  ]
  where
    drawButton txt = pictures 
      [ translate (-80) (-10) $ scale 0.15 0.15 $ color white $ text txt ]

------------------------------------------------------------
-- Controls Screen
------------------------------------------------------------
drawControlsScreen :: GameState -> Picture
drawControlsScreen gs = pictures
  [ drawStars (stars gs)

  , drawMenuShip (menuShip gs)
  , drawAsteroid (menuAsteroid gs)
  , drawAsteroid (menuAsteroid2 gs)
  , drawAsteroid (menuAsteroid3 gs)
  , drawAsteroid (menuAsteroid4 gs)
  
  , translate (-300) 200 $ scale 0.3 0.3 $ color white $ text "CONTROLS"
  
  , translate (-300) 100 $ drawControl "W" "Thrust"
  , translate (-300) 50 $ drawControl "A" "Rotate Left"
  , translate (-300) 0 $ drawControl "D" "Rotate right"
  , translate (-300) (-50) $ drawControl "Space" "Shoot"
  , translate (-300) (-100) $ drawControl "P" "Pause / Resume"
  
  , translate (-250) (-200) $ scale 0.15 0.15 $ color lightGrey $ text "Press M to return to Main Menu"
  ]
  where
    drawControl key action = pictures
      [ translate 0 0 $ scale 0.15 0.15 $ color yellow $ text key
      , translate 150 0 $ scale 0.15 0.15 $ color white $ text action
      ]
------------------------------------------------------------
-- Draw Game
------------------------------------------------------------
drawGame :: GameState -> Picture
drawGame gs 
  | isPaused gs = drawPauseScreen gs
  | otherwise = pictures
  [ drawStars (stars gs)
  , drawShip (player gs)
  , pictures (map drawBullet (bullets gs))
  , pictures (map drawAsteroid (asteroids gs))
  , pictures (map drawEnemy (enemies gs))
  , pictures (map drawShooter (shooters gs))
  , pictures (map drawEnemyBullet (enemyBullets gs))
  , pictures (map drawExplosion (explosions gs))
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
        regularHighScoreText = "High Score: " ++ show highScoreVal
        previousHighScoreText = "Previous High Score: " ++ show highScoreVal

        gameOverText = translate (-210) 150 $ scale 0.5 0.5 $ color white $ text "GAME OVER"
        scoreText = translate (-80) 50 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ show currentScore
        highScoreText = 
            if isNewHighScore
            then translate (-160) 0 $ scale 0.2 0.2 $ color white $ text previousHighScoreText
            else translate (-120) 0 $ scale 0.2 0.2 $ color white $ text regularHighScoreText
        newHighScoreBanner = translate (-190) (-80) $ scale 0.3 0.3 $ color yellow $ text "NEW HIGH SCORE!"

        restartText = pictures
          [
            translate (-160) (-200) $ scale 0.15 0.15 $ color (greyN 0.5) $ text "Press N to start a new game"
          , translate (-175) (-250) $ scale 0.15 0.15 $ color (greyN 0.5) $ text "Press M to return to Main Menu"
          ]

        elements = [gameOverText, scoreText, highScoreText, restartText]

        finalElements = if currentScore > highScoreVal then newHighScoreBanner : elements else elements

    in pictures finalElements
    
------------------------------------------------------------
-- Pause Screen
------------------------------------------------------------
drawPauseScreen :: GameState -> Picture
drawPauseScreen gs = pictures
  [ drawGame (gs { isPaused = False })
  , color pauseOverlayColor $ rectangleSolid windowWidth windowHeight
  , translate (-180) 50 $ scale 0.7 0.7 $ color white $ text "PAUSED"
  , translate (-100) (-50) $ scale 0.15 0.15 $ color lightGrey $ text "Press P to resume"
  , translate (-170) (-100) $ scale 0.15 0.15 $ color lightGrey $ text "Press M to return to Main Menu"
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
  let
    invincibilityTime = iTimer ship
    isInvincible = invincibilityTime > 0
    blinkOn = round (invincibilityTime * 4) `mod` 2 == 0
    
    shipPicture = pictures
             [ 
               drawThrust ship,
               color white $ 
               polygon [(-10,-10),(20,0),(-10,10)] 
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
-- Menu Ship Drawing
------------------------------------------------------------
drawMenuShip :: Ship -> Picture
drawMenuShip ship = 
  translate x y $
  rotate (-(angle ship)) $
  scale 1.5 1.5 $
  pictures 
    [ drawThrust ship
    , color white $ 
      polygon [(-10,-10),(20,0),(-10,10)]
    ]
  where
    (x, y) = position ship
------------------------------------------------------------
-- Draw bullets
------------------------------------------------------------
drawBullet :: Bullet -> Picture
drawBullet b =
  translate x y $
  color orange $
  thickCircle 1 2
  where
    (x, y) = bPos b

drawEnemyBullet :: EnemyBullet -> Picture
drawEnemyBullet eb =
  translate x y $
  color enemyBulletColor $
  circleSolid 5
  where
    (x, y) = ebPos eb

------------------------------------------------------------
-- Draw Explosion
--------------------------------------------
drawExplosion :: Explosion -> Picture
drawExplosion ex =
  let
    (px, py) = exPos ex
    time = exTime ex
    t = time / explosionLifetime 
    
    scaleFactor = 1.0 + t * 2.0
    opacity = 1.0 - t           
    
    getParticleColor p = 
        case p `mod` 3 of
            0 -> red
            1 -> yellow
            2 -> orange
            _ -> white
        
    particleCount = 24
    maxRadius     = 20.0

    particles = [
        let 
          p_float = fromIntegral p
          angle_rad = p_float * 2 * pi / fromIntegral particleCount
          
          distance = t * maxRadius

          p_x = cos angle_rad * distance
          p_y = sin angle_rad * distance
          pixelSize_t = 3.0 * (1.0 - t * 0.7)
        in
        translate px py $
        color (getParticleColor p) $
        translate p_x p_y $
        rectangleSolid pixelSize_t pixelSize_t 
        | p <- [0..particleCount - 1]
      ]
      
  in pictures particles


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
pixel :: Float -> Point -> Picture
pixel pixelSize (x, y) = translate (x * pixelSize) (y * pixelSize) $ rectangleSolid pixelSize pixelSize

drawEnemy :: Enemy -> Picture
drawEnemy e =
  translate x y $
  color red $
  polygon [(-10,-10),(10,-10),(0,15)]
  where
    (x, y) = ePos e

drawShooter :: Shooter -> Picture
drawShooter s =
    translate x y $
    scale 2.0 2.0 $
    drawPixelArt 
  where
    (x, y) = sPos s
    pixelSize = 1.0

    spots =
      [ (-18, 0), (-18,-1)
      , (-17, 1), (-17, 0), (-17, -1), (-17, -2)
      , (-16, 1), (-16, 0), (-16, -1), (-16, -2)
      , (-15, 0), (-15, -1)
      , (-12, 0), (-12, -1)
      , (-11, 1), (-11, 0), (-11, -1), (-11, -2)
      , (-10, 1), (-10, 0), (-10, -1), (-10, -2)
      , (-9, 0), (-9, -1)
      , (-5, 0), (-5, -1)
      , (-4, 1), (-4, 0), (-4, -1), (-4, -2)
      , (-3 ,1), (-3, 0), (-3, -1), (-3, -2)
      , (-2, 0), (-2, -1) 
      , (2, 0), (2, -1)
      , (3, 1), (3, 0), (3, -1), (3, -2)
      , (4, 1), (4, 0), (4, -1), (4, -2)
      , (5, 0), (5,-1)
      , (9, 0), (9, -1)
      , (10, 1), (10, 0), (10, -1), (10, -2)
      , (11, 1), (11, 0), (11, -1), (11, -2)
      , (12, 0), (12, -1)
      , (15, 0), (15, -1)
      , (16, 1), (16, 0), (16, -1), (16, -2)
      , (17, 1), (17, 0), (17, -1), (17, -2)
      , (18, 0), (18, -1)
      ]

    edge = 
      [ (-18, 1), (-18, -2)
      , (-17, 2), (-17, -3)
      , (-16, 3), (-16, -4)
      , (-15, 4), (-15, 3), (-15, -4), (-15, -5)
      , (-14, 5), (-14, 3), (-14, -4), (-14, -6)
      , (-13, 5), (-13, 3), (-13, -4), (-13, -6)
      , (-12, 5), (-12, 3), (-12, -4), (-12, -7)
      , (-11, 8), (-11, 7), (-11, 6), (-11, 5), (-11, 3), (-11, -4), (-11, -7)
      , (-10, 10), (-10, 9), (-10, 5), (-10, 3), (-10, -4), (-10, -5), (-10, -8)
      , (-9, 12), (-9, 11), (-9, 6), (-9, 5), (-9, 3), (-9, -4), (-9, -6), (-9, -8)
      , (-8, 13), (-8, 7), (-8, 5), (-8, 3), (-8, -4), (-8, -7), (-8, -8)
      , (-7, 14), (-7, 7), (-7, 5), (-7, 3), (-7, -4), (-7, -8), (-7, -9)
      , (-6, 14), (-6, 6), (-6, 5), (-6, 3), (-6, -4), (-6, -9)
      , (-5, 15), (-5, 11), (-5, 10), (-5, 9), (-5, 5), (-5, 3), (-5, -4), (-5, -5), (-5, -9)
      , (-4, 15), (-4, 12), (-4, 8), (-4, 7), (-4, 5), (-4, 3), (-4, -4), (-4, -6), (-4, -7), (-4, -10)
      , (-3, 16), (-3, 13), (-3, 10), (-3, 9), (-3, 6), (-3, 5), (-3, 3), (-3, -4), (-3, -8), (-3, -9), (-3,-10)
      , (-2, 16), (-2, 13), (-2, 10), (-2, 9), (-2, 8), (-2, 5), (-2, 3), (-2, -4), (-2, -10)
      , (-1, 16), (-1, 14), (-1, 9), (-1, 8), (-1, 5), (-1, 3), (-1, -4), (-1, -10)
      , (0, 16), (0, 14), (0, 6), (0, 5), (0, 3), (0, -4), (0, -5), (0, -6), (0, -7), (0, -8), (0, -9), (0, -10)
      , (18, 1), (18, -2)
      , (17, 2), (17, -3)
      , (16, 3), (16, -4)
      , (15, 4), (15, 3), (15, -4), (15, -5)
      , (14, 5), (14, 3), (14, -4), (14, -6)
      , (13, 5), (13, 3), (13, -4), (13, -6)
      , (12, 5), (12, 3), (12, -4), (12, -7)
      , (11, 8), (11, 7), (11, 6), (11, 5), (11, 3), (11, -4), (11, -7)
      , (10, 10), (10, 9), (10, 5), (10, 3), (10, -4), (10, -5), (10, -8)
      , (9, 12), (9, 11), (9, 6), (9, 5), (9, 3), (9, -4), (9, -6), (9, -8)
      , (8, 13), (8, 7), (8, 5), (8, 3), (8, -4), (8, -7), (8, -8)
      , (7, 14), (7, 7), (7, 5), (7, 3), (7, -4), (7, -8), (7, -9)
      , (6, 14), (6, 6), (6, 5), (6, 3), (6, -4), (6, -9)
      , (5, 15), (5, 11), (5, 10), (5, 9), (5, 5), (5, 3), (5, -4), (5, -5), (5, -9)
      , (4, 15), (4, 12), (4, 8), (4, 7), (4, 5), (4, 3), (4, -4), (4, -6), (4, -7), (4, -10)
      , (3, 16), (3, 13), (3, 10), (3, 9), (3, 6), (3, 5), (3, 3), (3, -4), (3, -8), (3, -9), (3,-10)
      , (2, 16), (2, 13), (2, 10), (2, 9), (2, 8), (2, 5), (2, 3), (2, -4), (2, -10)
      , (1, 16), (1, 14), (1, 9), (1, 8), (1, 5), (1, 3), (1, -4), (1, -10)
      ]

    ring = 
      [ (-16, 2), (-16, -3)
      , (-15, 2), (-15, 1), (-15, -2), (-15, -3)
      , (-14, 2), (-14, 1), (-14, 0), (-14, -1), (-14, -2), (-14, -3)
      , (-13, 2), (-13, 1), (-13, 0), (-13, -1), (-13, -2), (-13, -3)
      , (-12, 2), (-12, 1), (-12, -2), (-12, -3)
      , (-11, 2), (-11, -3)
      , (-10, 2), (-10, -3)
      , (-9, 2), (-9, 1), (-9, -2), (-9, -3)
      , (-8, 2), (-8, 1), (-8,0), (-8, -1), (-8, -2), (-8, -3)
      , (-7, 2), (-7, 1), (-7, 0), (-7, -1), (-7, -2), (-7, -3)
      , (-6, 2), (-6, 1), (-6, 0), (-6, -1), (-6, -2), (-6, -3)
      , (-5, 2), (-5, 1), (-5, -2), (-5, -3)
      , (-4, 2), (-4, -3)
      , (-3, 2), (-3, -3)
      , (-2, 2), (-2, 1), (-2, -2), (-2, -3)
      , (-1, 2), (-1, 1), (-1, 0), (-1, -1), (-1, -2), (-1, -3)
      , (0, 2), (0,1), (0, 0), (0, -1), (0, -2), (0, -3)
      , (16, 2), (16, -3)
      , (15, 2), (15, 1), (15, -2), (15, -3)
      , (14, 2), (14, 1), (14, 0), (14, -1), (14, -2), (14, -3)
      , (13, 2), (13, 1), (13, 0), (13, -1), (13, -2), (13, -3)
      , (12, 2), (12, 1), (12, -2), (12, -3)
      , (11, 2), (11, -3)
      , (10, 2), (10, -3)
      , (9, 2), (9, 1), (9, -2), (9, -3)
      , (8, 2), (8, 1), (8, 0), (8, -1), (8, -2), (8, -3)
      , (7, 2), (7, 1), (7, 0), (7, -1), (7, -2), (7, -3)
      , (6, 2), (6, 1), (6, 0), (6, -1), (6, -2), (6, -3)
      , (5, 2), (5, 1), (5, -2), (5, -3)
      , (4, 2), (4, -3)
      , (3, 2), (3, -3)
      , (2, 2), (2, 1), (2, -2), (2, -3)
      , (1, 2), (1, 1), (1, 0), (1, -1), (1, -2), (1, -3)
      ]
    
    hull =
      [ (-14, 4), (-13, 4), (-12, 4), (-11, 4), (-10, 4), (-9, 4), (-8,4), (-7, 4), (-6, 4), (-5, 4), (-4, 4), (-3, 4), (-2,4), (-1, 4), (0,4)
      , (14, 4), (13, 4), (12, 4), (11, 4), (10, 4), (9, 4), (8, 4), (7, 4), (6, 4), (5, 4), (4, 4), (3, 4), (2, 4), (1, 4), (0, 4)
      , (-14,-5), (-13, -5), (-12, -5), (-12, -6), (-11, -5), (-11, -6), (-10, -6), (-10, -7), (-9, -7)
      , (-9, -5), (-8, -5), (-8, -6), (-7, -5), (-7, -6), (-7, -7), (-6, -5), (-6, -6), (-6, -7), (-6, -8), (-5, -6), (-5,-7), (-5, -8), (-4, -8), (-4, -9)
      , (-4, -5), (-3, -5), (-3, -6), (-3, -7), (-2, -5), (-2, -6), (-2, -7), (-2, -8), (-2, -9), (-1, -5), (-1, -6), (-1, -7), (-1, -8), (-1, -9)
      , (-14, 4), (-13, 4), (-12, 4), (-11, 4), (-10, 4), (-9, 4), (-8, 4), (-7, 4), (-6, 4), (-5, 4), (-4, 4), (-3, 4), (-2, 4), (-1, 4), (0, 4)
      , (14, -5), (13, -5), (12, -5), (12, -6), (11, -5), (11, -6), (10, -6), (10, -7), (9, -7)
      , (9, -5), (8, -5), (8, -6), (7, -5), (7, -6), (7, -7), (6, -5), (6, -6), (6, -7), (6, -8), (5, -6), (5, -7), (5, -8), (4, -8), (4, -9)
      , (4, -5), (3, -5), (3, -6), (3, -7), (2, -5), (2, -6), (2, -7), (2, -8), (2, -9), (1, -5), (1, -6), (1, -7), (1, -8), (1, -9)  
      ]

    window = 
      [ (-10, 8), (-10,7), (-10,6)
      , (-9, 10), (-9, 9), (-9, 8), (-9, 7)
      , (-8, 12), (-8, 11), (-8, 10), (-8, 9), (-8, 8)
      , (-7, 13), (-7, 12), (-7, 11), (-7, 10), (-7, 9), (-7, 8)
      , (-6, 13), (-6, 12), (-6, 11), (-6, 10), (-6, 9), (-6,8), (-6, 7)
      , (-5, 14), (-5, 13), (-5, 12), (-5, 8), (-5, 7), (-5, 6)
      , (-4, 14), (-4, 13), (-4, 6)
      , (-3, 15), (-3, 14)
      , (-2, 15), (-2, 14)
      , (-1, 15)
      , (0,15)
      , (10, 8), (10, 7), (10, 6)
      , (9, 10), (9, 9), (9, 8), (9, 7)
      , (8, 12), (8, 11), (8, 10), (8, 9), (8, 8)
      , (7, 13), (7, 12), (7, 11), (7, 10), (7, 9), (7, 8)
      , (6, 13), (6, 12), (6, 11), (6, 10), (6, 9), (6, 8), (6, 7)
      , (5, 14), (5, 13), (5, 12), (5, 8), (5, 7), (5, 6)
      , (4, 14), (4, 13), (4, 6)
      , (3, 15), (3, 14)
      , (2, 15), (2, 14)
      , (1, 15)
      ]
    
    alien =
      [ (-8, 6), (-7, 6)
      , (-4, 11), (-4, 10), (-4, 9)
      , (-3, 12), (-3, 11), (-3, 8), (-3, 7)
      , (-2, 12), (-2, 11), (-2, 7), (-2,6)
      , (-1, 13), (-1, 12), (-1, 11), (-1, 10), (-1, 7), (-1, 6)
      , (0, 13), (0, 12), (0, 11), (0, 10), (0, 9), (0, 8), (0, 7)
      , (8, 6), (7, 6)
      , (4, 11), (4, 10), (4, 9)
      , (3, 12), (3, 11), (3, 8), (3, 7)
      , (2 , 12), (2, 11), (2, 7), (2, 6)
      , (1, 13), (1, 12), (1, 11), (1, 10), (1, 7), (1, 6)
      ]
    
    light = 
      [(-4, -11), (-3, -11), (-2,-11), (-1, -11), (0, -11), (1, -11), (2, -11), (3, -11), (4, -11)
      ]
    
    drawPixelArt = pictures $
        [ color shooterSpots $ pixel pixelSize pt | pt <- spots ] ++
        [ color black $ pixel pixelSize pt | pt <- edge ] ++
        [ color shooterRing $ pixel pixelSize pt | pt <- ring ] ++
        [color shooterHull $ pixel pixelSize pt | pt <- hull] ++
        [color green $ pixel pixelSize pt | pt <- alien] ++
        [color yellow $ pixel pixelSize pt | pt <- light] ++
        [color shooterWindow $ pixel pixelSize pt | pt <- window]

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
    displayScale = 1.2 

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
      , let x = fromIntegral i * 0.75
      ]
  in
    translate (-windowWidth/2-5) (windowHeight/2 - 80) $
    pictures hearts
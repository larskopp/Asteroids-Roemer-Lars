module Input (handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Model

handleInput :: Event -> GameState -> GameState
handleInput ev gs = 
  case ev of
    
    -- Restart (R)
    EventKey (Char 'r') Down _ _ -> handleRestart gs
    EventKey (Char 'R') Down _ _ -> handleRestart gs
    
    -- Pause Toggle (P)
    EventKey (Char 'p') Down _ _ -> togglePause gs
    EventKey (Char 'P') Down _ _ -> togglePause gs

    _ -> 
      if isPaused gs
      then gs 
      else handleRunningInput ev gs

handleRunningInput :: Event -> GameState -> GameState
handleRunningInput ev gs = case ev of
   -- Rotate left/right (A/D)
   EventKey (Char 'a') Down _ _ -> setAngularV  200 gs    -- left
   EventKey (Char 'd') Down _ _ -> setAngularV (-200) gs  -- right
   EventKey (Char 'a') Up   _ _ -> stopTurn gs  200
   EventKey (Char 'd') Up   _ _ -> stopTurn gs (-200)

   EventKey (Char 'A') Down _ _ -> setAngularV  200 gs    -- left
   EventKey (Char 'D') Down _ _ -> setAngularV (-200) gs  -- right
   EventKey (Char 'A') Up   _ _ -> stopTurn gs  200
   EventKey (Char 'D') Up   _ _ -> stopTurn gs (-200)

   -- Thrust toggle (W)
   EventKey (Char 'w') Down _ _ -> gs { player = (player gs) { thrusting = True } }
   EventKey (Char 'w') Up   _ _ -> gs { player = (player gs) { thrusting = False } }

   EventKey (Char 'W') Down _ _ -> gs { player = (player gs) { thrusting = True } }
   EventKey (Char 'W') Up   _ _ -> gs { player = (player gs) { thrusting = False } }

   -- Shooting (Space)
   EventKey (SpecialKey KeySpace) Down _ _ -> shootBullet gs

   _ -> gs

------------------------------------------------------------
-- Helpers
------------------------------------------------------------
handleRestart :: GameState -> GameState
handleRestart gs
  | isPaused gs && lives gs <= 0 = initialState { highScore = highScore gs } -- Reset game but keep high score
  | otherwise                    = gs

togglePause :: GameState -> GameState
togglePause gs
  | lives gs <= 0 = gs 
  | otherwise     = gs { isPaused = not (isPaused gs) }

setAngularV :: Float -> GameState -> GameState
setAngularV av gs = gs { player = (player gs) { angularV = av } }

stopTurn :: GameState -> Float -> GameState
stopTurn gs av =
  let ship = player gs
  in if angularV ship == av
     then gs { player = ship { angularV = 0 } }
     else gs

shootBullet :: GameState -> GameState
shootBullet gs =
  let ship = player gs
      (x, y) = position ship
      a = angle ship * pi / 180
      speed = 400
      vel = (speed * cos a, speed * sin a)
      b = Bullet { bPos = (x, y), bVel = vel, bTime = 0 }
  in gs { bullets = b : bullets gs }

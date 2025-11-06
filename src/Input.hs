module Input (handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Model

handleInput :: Event -> GameState -> GameState
handleInput ev gs = case ev of
  -- Pause
  EventKey (Char 'p') Down _ _ ->
    gs { isPaused = not (isPaused gs) }

  EventKey (Char 'P') Down _ _ ->
    gs { isPaused = not (isPaused gs) }

  -- Rotate left/right (A/D)
  EventKey (Char 'a') Down _ _ -> setAngularV  200 gs   -- left
  EventKey (Char 'd') Down _ _ -> setAngularV (-200) gs -- right
  EventKey (Char 'a') Up   _ _ -> stopTurn gs  200
  EventKey (Char 'd') Up   _ _ -> stopTurn gs (-200)

  EventKey (Char 'A') Down _ _ -> setAngularV  200 gs   -- left
  EventKey (Char 'D') Down _ _ -> setAngularV (-200) gs -- right
  EventKey (Char 'A') Up   _ _ -> stopTurn gs  200
  EventKey (Char 'D') Up   _ _ -> stopTurn gs (-200)

  -- Thrust toggle
  EventKey (Char 'w') Down _ _ -> gs { player = (player gs) { thrusting = True } }
  EventKey (Char 'w') Up   _ _ -> gs { player = (player gs) { thrusting = False } }

  EventKey (Char 'W') Down _ _ -> gs { player = (player gs) { thrusting = True } }
  EventKey (Char 'W') Up   _ _ -> gs { player = (player gs) { thrusting = False } }

  -- Shooting
  EventKey (SpecialKey KeySpace) Down _ _ -> shootBullet gs

  _ -> gs

------------------------------------------------------------
-- Helpers
------------------------------------------------------------
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

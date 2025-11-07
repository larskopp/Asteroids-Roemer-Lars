module Input (handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Model

handleInput :: Event -> GameState -> GameState
handleInput ev gs = 
  case currentScreen gs of
    MainMenu       -> handleMainMenuInput ev gs
    ControlsScreen -> handleControlsInput ev gs
    GameScreen     -> handleGameInput ev gs

------------------------------------------------------------
-- Main Menu
------------------------------------------------------------
handleMainMenuInput :: Event -> GameState -> GameState
handleMainMenuInput ev gs = case ev of
  -- Start new game 
  EventKey (Char 'n') Down _ _ -> newGameState gs
  EventKey (Char 'N') Down _ _ -> newGameState gs
  
  -- Show controls
  EventKey (Char 'c') Down _ _ -> gs { currentScreen = ControlsScreen }
  EventKey (Char 'C') Down _ _ -> gs { currentScreen = ControlsScreen }
  
  _ -> gs

------------------------------------------------------------
-- Controls Screen
------------------------------------------------------------
handleControlsInput :: Event -> GameState -> GameState
handleControlsInput ev gs = case ev of
  EventKey (Char 'm') Down _ _ -> gs { currentScreen = MainMenu }
  EventKey (Char 'M') Down _ _ -> gs { currentScreen = MainMenu } -- Back to main menu
  _ -> gs
------------------------------------------------------------
-- Pause Screen
------------------------------------------------------------
handlePausedInput :: Event -> GameState -> GameState
handlePausedInput ev gs =
    case ev of
        -- M to return to Main Menu
        EventKey (Char 'm') Down _ _ -> gs { currentScreen = MainMenu, isPaused = False }
        EventKey (Char 'M') Down _ _ -> gs { currentScreen = MainMenu, isPaused = False }
        
        -- P to resume
        EventKey (Char 'p') Down _ _ -> togglePause gs
        EventKey (Char 'P') Down _ _ -> togglePause gs

        _ -> gs

------------------------------------------------------------
-- Game Over screen
------------------------------------------------------------
handleGameOverInput :: Event -> GameState -> GameState
handleGameOverInput ev gs =
    case ev of
        -- N to start a new game
        EventKey (Char 'n') Down _ _ -> newGameState gs
        EventKey (Char 'N') Down _ _ -> newGameState gs
        
        -- M to return to main menu
        EventKey (Char 'm') Down _ _ -> gs { currentScreen = MainMenu }
        EventKey (Char 'M') Down _ _ -> gs { currentScreen = MainMenu }
        
        _ -> gs
        
------------------------------------------------------------
-- Game Screen
------------------------------------------------------------
handleGameInput :: Event -> GameState -> GameState
handleGameInput ev gs
  | lives gs <= 0 && isPaused gs = handleGameOverInput ev gs
  | otherwise =
    case ev of
    
      -- Main Menu
      EventKey (Char 'm') Down _ _ -> handleRestart gs
      EventKey (Char 'M') Down _ _ -> handleRestart gs
  
      -- Pause 
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

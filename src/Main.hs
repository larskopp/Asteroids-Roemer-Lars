module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Directory (doesFileExist)
import System.IO
import Data.List (sortOn)
import Control.Exception (evaluate)
import Model
import View
import Input
import Update
import Data.Maybe (mapMaybe)


------------------------------------------------------------
-- Window setup
------------------------------------------------------------
window :: Display
window = InWindow "Asteroids" (800, 600) (100, 100)

background :: Color
background = black

fps :: Int
fps = 60

------------------------------------------------------------
-- High Score File Handling (top 5)
------------------------------------------------------------
highScoreFile :: FilePath
highScoreFile = "highscores.txt"

loadHighScores :: IO [Int]
loadHighScores = do
  exists <- doesFileExist highScoreFile
  if not exists
    then return []
    else do
      handle <- openFile highScoreFile ReadMode
      contents <- hGetContents handle
      scores <- evaluate (length contents `seq` contents)
      let parseSafe s = case reads s of
                          [(n, "")] -> Just n
                          _         -> Nothing
          result = take 5 (reverse (sortOn id (mapMaybe parseSafe (lines scores))))
      hClose handle
      return result

saveHighScores :: [Int] -> IO ()
saveHighScores scores = do
  let output = unlines (map show (take 5 (reverse (sortOn id scores))))
  evaluate (length output)
  handle <- openFile highScoreFile WriteMode
  hPutStr handle output
  hFlush handle 
  hClose handle
------------------------------------------------------------
-- IO Wrappers for Gloss
------------------------------------------------------------
drawIO :: GameState -> IO Picture
drawIO = return . draw

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO ev gs = do
  let gs' = handleInput ev gs
  if lives gs' <= 0 && not (hasSavedHighScore gs')
    then do
      currentScores <- loadHighScores
      let updated = take 5 (reverse (sortOn id (score gs' : currentScores)))
      saveHighScores updated
      return gs' { highScore = maximum updated, hasSavedHighScore = True }
    else return gs'

stepIO :: Float -> GameState -> IO GameState
stepIO dt gs = return (step dt gs)

------------------------------------------------------------
-- Main
------------------------------------------------------------
main :: IO ()
main = do
  g <- getStdGen
  scores <- loadHighScores
  let hs = if null scores then 0 else maximum scores
  let initial = (initialState g) { highScore = hs, startHighScore = hs }
  playIO window background fps initial drawIO handleInputIO stepIO

module Main where

import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Input
import Update
import System.Random

window :: Display
window = InWindow "Asteroids (scaffold)" (800, 600) (100, 100)

background :: Color
background = black

fps :: Int
fps = 60

main :: IO ()
main = do -- AANPASSING: Maak main een IO actie
  g <- getStdGen
  play window background fps (initialState g) draw handleInput step


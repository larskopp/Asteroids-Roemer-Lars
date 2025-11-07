module Main where

import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Input
import Update

window :: Display
window = InWindow "Asteroids (scaffold)" (800, 600) (100, 100)

background :: Color
background = black

fps :: Int
fps = 120

main :: IO ()
main = play window background fps initialState draw handleInput step

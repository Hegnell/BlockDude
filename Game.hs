module Game where

import Graphics.Rendering.OpenGL
import Levels


data PlayerDirection = PLeft | PRight deriving (Eq, Show)

data PlayerPosition = PlayerPosition {
  x :: Int,
  y :: Int
} deriving Show

data Player = Player {
  pos :: PlayerPosition,
  direction :: PlayerDirection
} deriving Show


createPlayer :: Levels.Level -> Player
createPlayer level = Player {
  pos = PlayerPosition {x = sx, y = sy},
  direction = PRight
} where
  (sx, sy) = startPos level



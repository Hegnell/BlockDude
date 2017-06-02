module Levels (
  Level(..),
  levels,
  getObject
) where


data Level = Level {
  layout :: [String],
  startPos :: (Int, Int),
  number :: Int
} deriving Show


-- A list of all the levels in the game.
levels = [Level {layout =["                ",
                          "                ",
                          "                ",
                          "                ",
                          "                ",
                          "xx              ",
                          "xxx             ",
                          "xxxx   x   x   e",
                          "xxxxx xxx xxx xx",
                          "xxxxxxxxxxxxxxxx"],
                startPos = (0,6),
                number = 1}
        ]

getObject :: Level -> Int -> Int -> Char
getObject level x y =
  if withinBounds x y
    then (layout level !! y) !! x
    else ' '


withinBounds :: Int -> Int -> Bool
withinBounds x y =
  if 0 <= x && x <= 15 && 0 <= y && y <= 9
    then True
    else False
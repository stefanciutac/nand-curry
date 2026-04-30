module Logic
  ( rightShift,
    leftShift,
  )
where

import Control.Arrow (ArrowChoice (left))
import Gates
import Register

-- Shifters
rightShift :: [Bool] -> Bool -> ([Bool], Bool)
rightShift bus carry = ([carry] ++ [bus !! i | i <- [0 .. 6]], bus !! 7)

leftShift :: [Bool] -> Bool -> ([Bool], Bool)
leftShift bus carry = ([bus !! i | i <- [1 .. 7]] ++ [carry], bus !! 0)

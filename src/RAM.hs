module RAM
  ( rwMem,
  )
where

import Gates
import Register

isS :: [Bool] -> Bool -> Int -> Int -> Bool
isS address s across down = andGate s (andGate ((decoder4X16 [address !! i | i <- [0 .. 3]]) !! across) ((decoder4X16 [address !! i | i <- [4 .. 7]]) !! down))

isE :: [Bool] -> Bool -> Int -> Int -> Bool
isE address e across down = andGate e (andGate ((decoder4X16 [address !! i | i <- [0 .. 3]]) !! across) ((decoder4X16 [address !! i | i <- [4 .. 7]]) !! down))

rwMem :: [Bool] -> [Bool] -> Bool -> Bool -> [[Bool]] -> ([Bool], [[Bool]])
rwMem address byte s e prevState =
  ( head [memReg byte (isS address s across down) (isE address e across down) (prevState !! (across * down + across)) | across <- [0 .. 7], down <- [0 .. 7]],
    [if isS address s a d == False then prevState !! (a * d + a) else byte | a <- [0 .. 15], d <- [0 .. 15]]
  )

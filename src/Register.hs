module Register
    ( memReg
    , decoder2X4
    , decoder3X8
    , decoder4X16
    ) where

import Gates

-- Register
memBit :: Bool -> Bool -> Bool -> Bool
memBit i s prevState = nandGate (nandGate i s) (nandGate (nandGate (nandGate i s) s) prevState)

memByte :: [Bool] -> Bool -> [Bool] -> [Bool]
memByte byte s prevState = [memBit (byte !! index) s (prevState !! index) | index <- [0..7]]

memEn :: [Bool] -> Bool -> [Bool]
memEn byte e = [andGate bit e | bit <- byte]

memReg :: [Bool] -> Bool -> Bool -> [Bool] -> [Bool]
memReg byte s e prevState = memEn (memByte byte s prevState) e

-- Decoders
decoder2X4 :: Bool -> Bool -> [Bool]
decoder2X4 a b = [andGate (notGate a) (notGate b),
                  andGate (notGate a) b,
                  andGate a (notGate b),
                  andGate a b]

decoder3X8 :: Bool -> Bool -> Bool -> [Bool]
decoder3X8 a b c = [and3Gate (notGate a) (notGate b) (notGate c),
                    and3Gate (notGate a) (notGate b) c,
                    and3Gate (notGate a) b (notGate c),
                    and3Gate (notGate a) b c,
                    and3Gate a (notGate b) (notGate c),
                    and3Gate a (notGate b) c,
                    and3Gate a b (notGate c),
                    and3Gate a b c]

decoder4X16 :: [Bool] -> [Bool]
decoder4X16 [a, b, c, d] = [and4Gate (notGate a) (notGate b) (notGate c) (notGate d),
                       and4Gate (notGate a) (notGate b) (notGate c) d,
                       and4Gate (notGate a) (notGate b) c (notGate d),
                       and4Gate (notGate a) (notGate b) c d,
                       and4Gate (notGate a) b (notGate c) (notGate d),
                       and4Gate (notGate a) b (notGate c) d,
                       and4Gate (notGate a) b c (notGate d),
                       and4Gate (notGate a) b c d,
                       and4Gate a (notGate b) (notGate c) (notGate d),
                       and4Gate a (notGate b) (notGate c) d,
                       and4Gate a (notGate b) c (notGate d),
                       and4Gate a (notGate b) c d,
                       and4Gate (notGate a) (notGate b) c d,
                       and4Gate a b (notGate c) d,
                       and4Gate a b c (notGate d),
                       and4Gate a b c d]

module Gates
    ( nandGate
    , andGate
    , notGate
) where

-- NAND gate
nandGate :: Bool -> Bool -> Bool
nandGate a b = if a == True && b == True then False else True

-- Other logic gates
andGate :: Bool -> Bool -> Bool
andGate a b = nandGate (nandGate a b) (nandGate a b)

notGate :: Bool -> Bool
notGate a = nandGate a a


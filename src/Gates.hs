module Gates
    ( nandGate
    , andGate
    , notGate
    , orGate
    , xorGate
    , and3Gate
    , and4Gate
) where

-- NAND gate
nandGate :: Bool -> Bool -> Bool
nandGate a b = if a == True && b == True then False else True

-- Other logic gates
andGate :: Bool -> Bool -> Bool
andGate a b = nandGate (nandGate a b) (nandGate a b)

notGate :: Bool -> Bool
notGate a = nandGate a a

orGate :: Bool -> Bool -> Bool
orGate a b = nandGate (notGate a) (notGate b)

xorGate :: Bool -> Bool -> Bool
xorGate a b = nandGate (nandGate (notGate a) b) (nandGate (notGate b) a)

and3Gate :: Bool -> Bool -> Bool -> Bool
and3Gate a b c = andGate (andGate a b) c

and4Gate :: Bool -> Bool -> Bool -> Bool -> Bool
and4Gate a b c d = andGate (andGate a b) (andGate c d)

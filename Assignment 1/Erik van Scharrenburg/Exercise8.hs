module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew b = b `elem` (boys \\ [Carl, Matthew])
accuses Peter   b = b `elem` [Matthew, Jack]
accuses Jack    b = not (accuses Matthew b)
                 && not (accuses Peter b)
accuses Arnold  b = xor (accuses Matthew b) (accuses Peter b)
accuses Carl    b = not (accuses Arnold b)

xor a b = (a || b) && not (a && b)

accusers :: Boy -> [Boy]
accusers b = filter (`accuses` b) boys

guilty, honest :: [Boy]
guilty = filter accusedByThree boys
  where accusedByThree x = length (accusers x) == 3
honest = filter accusedGuilty boys
  where accusedGuilty x = any (accuses x) guilty

-- Time: 1 hour and 30 minutes

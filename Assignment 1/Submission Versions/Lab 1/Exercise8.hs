import Test.QuickCheck
import Data.List

--Exercise 8

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses:: Boy -> Boy -> Bool
accuses Matthew name
        |elem name names = True
        |otherwise = False
        where names = [Peter, Jack, Arnold]

accuses Peter name
        |elem name names = True
        |otherwise = False
        where names = [Matthew, Jack]

accuses Jack name = not(accuses Matthew name)
                 && not(accuses Peter name)

accuses Arnold name = ((accuses Matthew name) || (accuses Peter name))
                      && not ((accuses Matthew name) && (accuses Peter name))

accuses Carl name = not (accuses Arnold name)


accusers:: Boy -> [Boy]
accusers name = [x | x <- boys, (accuses x name)]


guilty::[Boy]
guilty = filter (\x -> length (accusers x) >= 3) boys


honest::[Boy]
honest = filter (\x -> any (accuses x) guilty) boys

--The time spent on this exercise was around 4 hours

-- Joana Correia Magalhães Sousa
-- We chose this implemenation because the code is easy to read and the accuses
-- function is a good and clear translation of the information in the puzzle.

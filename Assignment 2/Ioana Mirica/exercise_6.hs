module Lab2 where
import Data.Char
import Test.QuickCheck

changeForLower c = 
    if (ord c + 13) > 122 then chr ((ord c + 13) - 26) else chr (ord c + 13)

changeForUpper c = 
    if (ord c + 13) > 90 then chr ((ord c + 13) - 26) else chr (ord c + 13)

rot13 :: String -> String
rot13 text =
    map (\c -> if isLower c == True then changeForLower c else changeForUpper c) text


--Didn't manage to do the quickCheck
--Used on the lower and uper case chars of the alphabet
--Time: 1h 15min
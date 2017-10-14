module Exercise7 where
import Data.List
import System.Random
import Lecture6

-- Exercise 7 - Erik van Scharrenburg
findRandomPrimePair :: IO (Integer, Integer)
findRandomPrimePair = do p <- rndPrime
                         q <- rndPrime
                         if p /= q then return (p, q)
                         else findRandomPrimePair
  where rndPrime = randomRIOWhere keyLengthBounds (primeMR 1)
        keyLengthBounds = (2 ^ (keyLength - 1), 2 ^ keyLength - 1)
        keyLength = 128

randomRIOWhere :: Random a => (a, a) -> (a -> IO Bool) -> IO a
randomRIOWhere t p = do x <- randomRIO t
                        b <- p x
                        if b then return x
                        else randomRIOWhere t p

exercise7 = do pp <- findRandomPrimePair
               print ("Prime pair:  " ++ show pp)
               let pub = uncurry rsaPublic pp
               print ("Public key:  " ++ show pub)
               let prv = uncurry rsaPrivate pp
               print ("Private key: " ++ show prv)
               msg <- randomRIO (10 ^ 3, 10 ^ 12)
               print ("Message: " ++ show msg)
               let encMsg = rsaEncode pub msg
               print ("Encoded message: " ++ show encMsg)
               let decMsg = rsaDecode prv encMsg
               print ("Decoded message: " ++ show decMsg)
               print ("Original == Decoded: " ++ show (msg == decMsg))

-- Time: 1 hour and 30 minutes

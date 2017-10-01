module Lab4 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

random_data_generator :: IO (Set Int)
random_list :: Int -> IO [Int]

random_list 0 = return ([])
random_list n = do r <- getStdRandom (randomR (0, 100))
                   rl <- random_list (n - 1)
                   return (r:rl)

random_data_generator = do rnd_length <- getStdRandom (randomR (0, 100))
                           list <- random_list rnd_length
                           return (list2set list)

instance Arbitrary a => Arbitrary (Set a) where arbitrary = do l <- arbitrary
                                                               return (list2set l)
						   
random_test_quickCheck = generate arbitrary :: IO (Set Int)

--https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
--Time spent: 2h12min
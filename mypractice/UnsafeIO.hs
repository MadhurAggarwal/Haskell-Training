module UnsafeIO where 

import System.IO.Unsafe 


-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c 
-- fmap :: (a -> b) -> IO a -> IO b

-- unsafePerformIO :: IO a -> a
-- we can use it to get only a from IO a 
-- But it is better to use >>= liftA2 or fmap where it is avaiable only temporarily and final result is still expected an IO action
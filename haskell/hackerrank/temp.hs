import           Control.Monad       (forM_)
import qualified Data.HashMap.Strict as HM
import           Debug.Trace         (traceShow)
--q = [ 2,1,5,3,4 ] :: [Int]
--q = [ 2,5,1,3,4 ] :: [Int]
--q = [1,2,5,3,7,8,6,4] :: [Int]
--q = [1,2,5,3,4,7,8,6] :: [Int]
--q = [1,2,3,4,5,6,7,8] :: [Int]
--q = [5,1,2,3,7,8,6,4] :: [Int]

minimumBribes q = case countBribes (zip q [1..]) (Just 0) (HM.fromList (zip q [0..])) of
  Nothing     -> putStrLn "Too chaotic"
  Just number -> print number

--countBribes :: [(Int,Int)] -> Maybe Int -> [Int] -> Maybe Int
countBribes [] acc _ = acc
countBribes ((v,i):xs) (Just acc) q
  | v-i > 2 = Nothing
  | otherwise = countBribes xs (Just $ bribes+acc) q
  where bribes = sum [if (q HM.! j > v) then 1 else 0 | j <- arr]
        arr = [max 0 (v-2)..(i-1)]

main :: IO ()
main = do
    file <- readFile "/home/ezemtsov/Desktop/programming-languages/haskell/hackerrank/input09.txt"
    let input = lines file
        t = read $ head input :: Int
    forM_ (filter odd [1..length $ tail input]) $ \t_itr -> do
        let n = input !! t_itr
            qTemp = input !! (t_itr + 1)
            q = map (read :: String -> Int) . words $ qTemp
        -- print q
        minimumBribes q

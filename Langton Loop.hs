-- Valarie Sheffey
-- CS 456
-- Langton Loop


import qualified Data.Map.Strict as Map
import System.Console.ANSI
import System.IO
import Data.Array
import Data.Char




main = do
  clearScreen
  hideCursor
  table <- readFile "langtonTransitionTable.txt"
  let pairings = valNkeys table
  putStrLn $ show pairings
  --let transitionMap = buildMap pairings Map.empty
  --putStrLn $ Map.showTree transitionMap
  --langtonLoop (inset (digitToIntArray $ lulzToArray startLoop) (emptyArray 0 100 100)) transitionMap


  
startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]




emptyArray init n m = array b [(i,init) | i <- range b]
    where b = ((0,0),(n-1,m-1))


          
lulzToArray :: [[Char]] -> Array (Int,Int) Char
lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(n,n))
          n = length xs - 1




--digitToIntArray = fmap digitToInt
intToDigitArray = fmap intToDigit




printIntArray :: Array (Int,Int) Int -> IO ()
printIntArray = printArray . intToDigitArray




printArray :: Array (Int,Int) Char -> IO ()
printArray a = mapM_ putStrLn (arrayToLulz a)




inset a a' = a' // assocs a




langtonLoop a m = do
  setCursorPosition 0 0
  printArrayFat $ fmap intToLangtonArray a
  langtonLoop (langtonUpdate a m) m




putStrFatLn cs = do
  sequence_ [putChar c >> putChar ' ' | c <- cs]
  putChar '\n'




printArrayFat a = mapM_ putStrFatLn (arrayToLulz a)




arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]




rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a




row a j = [a ! (j,k) | k <- colIndices a]




intToLangtonArray :: Int -> Char
intToLangtonArray x
  | x <= 0 = ' '
  | x <= 1 = '1'
  | x <= 2 = '2'
  | x <= 3 = '3'
  | x <= 4 = '4'
  | x <= 5 = '5'
  | x <= 6 = '6'
  | otherwise  = '7'




--langtonUpdate :: Array (Int,Int) Int -> Map.Map [Char] [Char] -> Array (Int,Int) Int
langtonUpdate a m = zipWithArray update a (neighborsArray a) -- will return updated loop
  where update arr1 arr2  =
         digitToInt $ head $ removeJust (head arr2) (Map.lookup arr2 m)


          
removeJust x (Just a) = a
removeJust x Nothing = [x]




neighbors a (j,k) =
   -- [Center, Top, Right, Bottom, Left]
   concat $ map show [a % (j,k), a % (j - 1, k + 0), a % (j + 0, k + 1), a % (j + 1, k + 0), a % (j + 0, k - 1)]
 


neighborsArray a = array (bounds a) [ (i, neighbors a (j,k)) | i@(j,k) <- indices a]


  
zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int,Int) b -> Array (Int,Int) c
zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) (b ! (j,k)) ) | i@(j,k) <- indices a]




(%) :: Array (Int,Int) a -> (Int,Int) -> a
a % (j,k) = a ! (j `mod` (m+1),k `mod` (n+1)) where (m,n) = snd $ bounds a




buildMap [] map = map
buildMap vals map =
  buildMap (drop 1 vals) (Map.insert (fst $ vals !! 0) (snd $ vals !! 0 ) map)


  
valNkeys tab = fmap (\x -> (take 5 x, drop 5 x)) (lines tab)


addRotations :: [(String, String)] -> [(String, String)]
addRotations ls = concat [r (a,b) |(a,b) <- ls]
  where
     r(x:xs,y) = [(l,y) | l <- (map (x:) (rotations xs))]




-- rot "ABCD" => "BCDA"
rot [] = []
rot [x] = [x]
rot (x:xs) = xs ++ [x]




--rotate 2 "ABCD" => "CDAB"
rotate 0 xs = xs
rotate n xs = rotate (n-1) (rot xs)




--rotations "ABCD" => ["ABCD","BCDA","CDAB","DABC"]
rotations xs = map (\n -> rotate n xs) [0..(length xs) - 1]
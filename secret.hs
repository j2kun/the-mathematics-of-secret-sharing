module Main where

import System.IO
import System.Random
import Data.Char
import Data.Bits
import Data.Ratio

type Point = (Rational, Rational)
type Polynomial = [Rational] --Polynomials are represented in ascending degree order


addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly [] [] = []
addPoly [] xs = xs
addPoly xs [] = xs
addPoly (x:xs) (y:ys) = (x+y) : (addPoly xs ys)


multPoly :: Polynomial -> Polynomial -> Polynomial
multPoly [] [] = []
multPoly [] _ = []
multPoly _ [] = []
multPoly xs ys = foldr addPoly [] $ map (multNShift ys) $ zip xs [0..]


multNShift :: Polynomial -> (Rational, Int) -> Polynomial
multNShift xs (y, shift) = 
    (replicate shift 0) ++ ( map ((*) y) xs)


share :: Rational -> Integer -> Int -> IO [Point]
share secret k r = do 
  generator <- getStdGen
  let poly = makePolynomial secret r generator
      ys = map (eval poly) $ map toRational [1..k] 
  return $ zip [1..] ys


reconstruct :: [Point] -> Rational 
reconstruct = head . findPolynomial


makePolynomial :: Rational -> Int -> StdGen -> Polynomial
makePolynomial secret r generator = 
  secret : map toRational (take (r-1) $ randomRs (1, (numerator(2*secret))) generator)


findPolynomial :: [Point] -> Polynomial
findPolynomial points = 
      let term (i, (xi, yi)) = 
            let prodTerms = map (\ (xj, _) -> [-xj/(xi - xj), 1/(xi - xj)]) $ allBut i points
            in multPoly [yi] $ foldl multPoly [1] prodTerms  
      in foldl addPoly [] $ map term $ zip [0..] points
                            

allBut :: Integer -> [a] -> [a]
allBut i list = snd $ unzip $ filter (\ (index,_) -> i /= index) $ zip [0..] list


eval :: Polynomial -> Rational -> Rational 
eval poly x =  foldr (\ coeff ans -> ans * x + coeff) 0 poly


encode :: String -> Integer
encode str = let nums = zip [0..] $ map (toInteger . ord) str
                 integers = map (\(i, n) -> shift n (i*8)) nums
             in foldl (+) 0 integers


decode :: Integer -> String
decode 0 = ""
decode num = if num < 0 
             then error "Can't decode a negative number"
             else chr (fromInteger (num .&. 127)) : (decode $ shift num (-8))


example msg k r = let secret = toRational $ encode msg 
                  in do points <- share secret k r 
                        putStrLn $ show $ numerator secret
                        putStrLn $ show $ map (\(x,y) -> (numerator x, numerator y)) points
                        let subset = take r points
                            encodedSecret = eval (findPolynomial subset) 0
                        putStrLn $ show $ numerator encodedSecret
                        putStrLn $ decode $ numerator encodedSecret 


module Encode (
    encodeid,
    decodeid,
    encodeb1,
    decodeb1,
    encodeb2,
    decodeb2,  
    encodeb8,
    decodeb8,
    --encodeb10,
    encodeb16,
    decodeb16,
    encodeb32,
    decodeb32,
    --encodeb58,
    encodeb64,
    decodeb64
) where

import Data
import Data.Char (ord , chr ,intToDigit, digitToInt)
import Data.List (elemIndices)
import Numeric (showIntAtBase)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- general use functions

strconv :: String -> [Int]   -- takes a String and returns a list of integers representing decimal encoding of each ASCII char, UTF8 support not included yet
strconv  = map ord 

decconv :: [Int] -> String   -- inverse of strconv
decconv  = map chr 



bconv' :: Int -> [Int] -> [Int]  -- takes an integer and returns a list of boolean integers in the form of [0,1,1,0] this imitates base 10 to 2 conversion (3 -> [1,1])   
bconv' 0  b = b 
bconv' a  b = bconv' (quot a 2)  (mod a 2 : b)
padhelper a = if length  a == 8             -- pads boolean integer lists so they end up with 8 elements  ([1,0,1,0] -> [0,0,0,0,1,0,1,0])
            then  a 
            else padhelper (0 : a)
bconv a = padhelper (bconv' a [])               



dconv' :: Int -> [Int] -> Int -> Int  --inverse function of bconv ([0,0,1,0,0,1,1,0] -> 38)
dconv' a b c = if null b
               then a 
               else dconv' (a + ((2 ^ c) * head b) ) (tail b) (c+1)
dconv a = dconv' 0 (reverse a) 0


reshape1' :: [[Int]] -> [Int] -> Int -> [[Int]]      -- inverse function of concat splits a list of integers into a list of b sized lists of integers
reshape1' a b c   
                  | null b = a
                  | length b < c = a ++ [b] 
                  | otherwise = reshape1' (a ++ [take c b])  (drop c b) c

reshape1  = reshape1' []              

replicate1 a = replicate a '1'                       -- helper function to facilitate calling replicate as an argument to map


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- central encoding functions for bases 8 16 32 and 64; bases 58 and 10 are treated as a special case 
-- (a) list of boolean integers
-- (s) number of octets to encode for each block
-- (c) number of encoded characters to produce for each block
-- (l) bit lenght of encoded characters
-- (d) padding flag 
-- (e) container for the output of basesEncode1
-- (n) index of base charset in baseIndex (from 1 to 13)

   
basesEncode1 :: [Int] -> Int -> Int -> Int -> Bool -> [Int] -> Int -> String
basesEncode1 a s c l d e n  
                      | d = 
                             if not (null a)
                             then basesEncode1 (drop (8 * s) a) s c l d (e ++ helper1 a s c l) n
                             else map (helper2 n) e
                            
                      | not (null a) =
                                   basesEncode1 (drop (8 * s) a) s c l d (e ++ helper1NoPad a s c l) n
                      | otherwise = map (helper2 n) e 

basesDecode1 :: [Int] -> Int -> Int -> Bool -> String            
basesDecode1 a l c d = if d  || mod (length a) c == 0
                       then helper5 (map helper4 (reshape1 (concatMap (helper3 l) a) 8))
                       else helper5 (map (chr . dconv) 
                                      
                                              (reshape1 
                                                  (take (length (concatMap (helper3 l) a) - mod 
                                                                                                  (length (concatMap (helper3 l) a))
                                                                                                  8)
                                                        (concatMap (helper3 l) a))   8) )                                                         


helper1 :: [Int] -> Int -> Int -> Int -> [Int]          -- helper to basesEncode1
helper1 q s c l = if length q >= (8 * s)     
                then map dconv (reshape1 (take (8 * s) q) l)
                else    (++) (map dconv
                                 (reshape1 
                                        (take ((div (length q) l + 1) * l)  
                                              (q ++ replicate (8 * s - length q) 0))
                                        l ))
                              (replicate (div (8 * s - length q) l )  (-1) )   

helper1NoPad :: [Int] -> Int -> Int -> Int -> [Int]     -- no-pad version of helper1
helper1NoPad q s c l = if length q >= (8 * s)    
                       then map dconv (reshape1 (take (8 * s) q) l)
                       else (map dconv (reshape1 
                                                (take 
                                                       ( l * (div (length q) l + 1) )   (q ++ replicate ((8 * s) - length q) 0)) 
                                       l))  
 
helper2 :: Int -> Int -> Char          -- takes a base index from 1 (baseid) to 13 (base64url) and a decimal value and returns the conversion of the value in that base
helper2 s q = if q == (-1)
                 then '=' 
                 else head (drop q (head  (drop (s - 1) baseIndex) !! 1)) 

helper2i :: Int -> Char ->Int           --inverse of helper2
helper2i s q = if q == '='
                 then (-1)
                 else  head (elemIndices q  (head (tail (head (drop (s-1) baseIndex)))))

helper3 :: Int -> Int -> [Int]
helper3 c a = if a /= (-1)
              then drop (8-c) (bconv a)
              else replicate c (-1)

helper4 :: [Int] -> Char
helper4 a = if  (-1) `notElem` a
            then  chr (dconv a) 
            else ' '

helper5 a = if last a == ' '
            then helper5 (reverse (drop 1 (reverse a)))
            else a
              
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--helper functions for base 58 

--helper581 :: Int -> Int -> [Int] -> Int
--helper581 a b c =  if length c /= 0
--                   then helper581 (a + ((head c) * (16 ^ (b*8))))  (b+1) (drop 1 c) 
--                   else a    

--helper582 :: [Int] -> Int -> [Int] 
--helper582 a c = if c /= 0
--                  then helper582 ((mod c 58) : a) (div c 58) 
--                  else a
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--helper functions for base 1

helper11 :: String -> [String] -> [Int] -> Int -> [String]
helper11 a b c d = if length a > 1
                   then helper11 (drop (head c - d) a) (b ++ [take (head c- (d+1)) a]) (tail c) (head c) 
                   else drop 1 b
                 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- other base encoding/decoding functions



encodeid :: String -> String 
encodeid a = a

decodeid :: String -> String
decodeid a = a


encodeb1 ::  String -> String
encodeb1  =  concatMap ((++ "/") . replicate1 . (+ 1) . ord )  

decodeb1 :: String -> String
decodeb1 a = map (chr . length . drop 1) (helper11 a [""] (elemIndices '/' a) (-1) )

encodeb2 ::  String -> String
encodeb2 a =  map intToDigit (concatMap bconv (strconv a)) 

decodeb2 :: String -> String
decodeb2 a =  map (chr . dconv) (reshape1 (map digitToInt a) 8)

encodeb8 :: String -> Bool -> String
encodeb8 a b = basesEncode1 (concatMap bconv (strconv a)) 3 8 3 b [] 4

decodeb8 :: String -> Bool -> String
decodeb8 a  =  basesDecode1 (map (helper2i 4) a) 3 8 

--encodeb10 :: String -> String
--encodeb10 a  = showIntAtBase 10 ("0123456789" !!) (encodeb16 a True)  ""

encodeb16 :: String -> Bool -> String
encodeb16 a b = basesEncode1 (concatMap bconv (strconv a)) 1 2 4 b [] 6

decodeb16 :: String -> Bool -> String
decodeb16 a  =  basesDecode1 (map (helper2i 4) a) 4 2 

encodeb32 :: String -> Bool -> Int -> String
encodeb32 a b  = basesEncode1 (concatMap bconv (strconv a)) 5 8 5  b [] 

decodeb32 :: String -> Int -> Bool -> String
decodeb32 a b  = basesDecode1 (map (helper2i b) a) 5 8 

--encodeb58flickr :: String -> Int -> String                                              -- (b) is either 10 for base58flickr or 11 for base58btc
--encodeb58flickr a b =  showIntAtBase 58 ("123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" !!)  (encodeb16 a True) ""  

--encodeb58btc :: String -> Int -> String                                              -- (b) is either 10 for base58flickr or 11 for base58btc
--encodeb58btc a b =  showIntAtBase 58 ("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" !!)  (encodeb16 a True)  ""

encodeb64 :: String -> Bool -> Int -> String
encodeb64 a b  = basesEncode1 (concatMap bconv (strconv a)) 3 4 6  b [] 

decodeb64 :: String -> Int -> Bool -> String
decodeb64 a b  = basesDecode1 (map (helper2i b) a) 6 4 








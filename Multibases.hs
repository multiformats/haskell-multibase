module Multibases (multiEncode,
                       multiDecode
                      )where

import Data.Char (ord , chr ,intToDigit, digitToInt, toLower)
import Data.List (elemIndices)
import Numeric (showIntAtBase)



multiEncode :: String -> String -> Bool -> Bool -> String 
multiEncode a b c d = if  a `elem` map head baseIndex
                     then if d 
                          then  case a of 
                                 "baseid"    -> "\x00" ++ b
                                 "base1"     -> last (head (tail baseIndex )) ++ ( '/' : encodeb1 b)
                                 "base2"     -> head (last  (baseIndex !! 2)) : encodeb2 b
                                 "base8"     -> head (last (baseIndex !! 3)) : encodeb8 b c
                                 --"base10"    -> (head (head (reverse (head (drop 4 baseIndex))))) : (encodeb10 b)
                                 "base16"    -> head (last (baseIndex !! 5))  : encodeb16 b c
                                 "base32"    -> head (last (baseIndex !! 6))  : encodeb32 b c 7
                                 "base32hex" -> head (last (baseIndex !! 7))  : encodeb32 b c 8
                                 "base32z"   -> head (last (baseIndex !! 8))  : encodeb32 b c 9
                                 --"base58flickr" -> (head (head (reverse (head (drop 9 baseIndex))))) : (encodeb58 b 10)
                                 --"base58btc" -> (head (head (reverse (head (drop 10 baseIndex))))) : (encodeb58 b 11)
                                 "base64"    -> head (last (baseIndex !! 11)) : encodeb64 b c 12
                                 "base64url" -> head (last (baseIndex !! 12)) : encodeb64 b c 13
                          else case a of 
                                 "baseid"    -> "\x00" ++ b
                                 "base1"     -> head (last (tail baseIndex)) ++ ( '/' : encodeb1 b)
                                 "base2"     -> head (last (baseIndex !! 2)) : encodeb2 b
                                 "base8"     -> map toLower (head (last (baseIndex !! 3)) : encodeb8 b c)
                                 --"base10"    -> (head (head (reverse (head (drop 4 baseIndex))))) : (encodeb10 b)
                                 "base16"    -> map toLower (head (last (baseIndex !! 5)) : encodeb16 b c)
                                 "base32"    -> map toLower (head (last (baseIndex !! 6)) : encodeb32 b c 7)
                                 "base32hex" -> map toLower (head (last (baseIndex !! 7)) : encodeb32 b c 8)
                                 "base32z"   -> map toLower (head (last (baseIndex !! 8)) : encodeb32 b c 9)
                                 --"base58flickr" -> (head (head (reverse (head (drop 9 baseIndex))))) : (encodeb58 b 10)
                                 --"base58btc" -> (head (head (reverse (head (drop 10 baseIndex))))) : (encodeb58 b 11)
                                 "base64"    -> map toLower (head (last (baseIndex !! 11)) : encodeb64 b c 12)
                                 "base64url" -> map toLower (head (last (baseIndex !! 12)) : encodeb64 b c 13)  
 
                     else "selected base encoding is not supported"


multiDecode b = if '=' `elem` b   then multiDecode'  b True   else multiDecode'  b False 
multiDecode' :: String -> Bool -> String 
multiDecode' b c =  if head b `elem` map (head . last) baseIndex
                    then 
                        case head b of  
                          '\x00' -> decodeid (drop 1 b)
                          '1'    -> decodeb1 (drop 1 b)
                          '0'    -> decodeb2 (drop 1 b)
                          '7'    ->  decodeb8 (drop 1 b) c
                          'F'    ->  decodeb16 (drop 1 b)c 
                          'f'    ->  decodeb16 (drop 1 b)c 
                          'B'    ->  decodeb32 (drop 1 b) 7 c 
                          'b'    ->  decodeb32 (drop 1 b) 7 c 
                          'V'    ->  decodeb32 (drop 1 b) 8 c 
                          'v'    ->  decodeb32 (drop 1 b) 8 c 
                          'h'    ->  decodeb32 (drop 1 b) 9 c 
                          'M'    ->  decodeb64 (drop 1 b) 12 c 
                          'm'    ->  decodeb64 (drop 1 b) 12 c 
                          'U'    ->  decodeb64 (drop 1 b) 13 c 
                          'u'    ->  decodeb64 (drop 1 b) 13 c 
                    else "unrecognized format, please use a supported multibase-encoded String"   
                 


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

charsid        = "\0"
chars1         = "1"
chars2         = "01"
chars8         = "01234567"
chars10        = "0123456789"
chars16        = "0123456789ABCDEF"
chars32        = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
chars32hex     = "0123456789ABCDEFGHIJKLMNOPQRSTUV" 
chars32z       = "YBNDRFG8EJKMCPQXOT1UWISZA345H769"
chars58flickr  = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
chars58btc     = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
chars64        = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
chars64url     = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"


baseIndex = [ ["baseid"     , charsid       , "\x00" ] ,  
            ["base1"        , chars1        , "1"     ] , 
            ["base2"        , chars2        , "0"     ] ,
            ["base8"        , chars8        , "7"     ] , 
            ["base10"       , chars10       , "9"     ] ,  
            ["base16"       , chars16       , "Ff"    ] ,
            ["base32"       , chars32       , "Bb"    ] ,
            ["base32hex"    , chars32hex    , "Vv"    ] ,
            ["base32z"      , chars32z      , "h"     ] ,
            ["base58flickr" , chars58flickr , "Z"     ] ,
            ["base58btc"    , chars58btc    , "z"     ] ,   
            ["base64"       , chars64       , "Mm"     ] ,
            ["base64url"    , chars64url    , "Uu"     ] ]   


 

 

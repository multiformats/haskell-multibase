module Multibase (multiEncode,
                       multiDecode
                      )where



import System.Environment
import Data
import Encode
import Data.Char (toLower)


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
                 





 

 

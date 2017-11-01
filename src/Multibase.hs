module Multibase (
                  appendPrefix,
                  removePrefix,
                  getPrefix,
                  multiEncode,
                  multiDecode
                  ) where

import qualified Data.Word as W
import qualified Data.ByteString as BStr
import qualified Data.Map as M
import MultibaseEncode


appendPrefix ::  String -> BStr.ByteString -> BStr.ByteString
appendPrefix a b =  BStr.append (BStr.pack $ snd $  fst $ (multibases M.! a)) b 

removePrefix :: BStr.ByteString -> BStr.ByteString
removePrefix a = BStr.pack $ tail (BStr.unpack a)

getPrefix :: BStr.ByteString -> [W.Word8]
getPrefix a = [head $ BStr.unpack a]
  
multiEncode :: String -> BStr.ByteString -> Bool -> BStr.ByteString
multiEncode a b c =  appendPrefix a (encodeByteString b ( snd (multibases M.! a)) (fst $ fst (multibases M.! a)) c)

multiDecode :: BStr.ByteString -> BStr.ByteString
multiDecode a = decodeByteString (removePrefix a) (snd (multibases M.! (baseNumMap M.! (getPrefix a)))) (fst $ fst (multibases M.! (baseNumMap M.! (getPrefix a))))  0
  
multibases :: M.Map  String ((BStr.ByteString , [W.Word8]), Integer)
multibases = M.fromList [("base id" , (( word8Range ,   [0] ), 256 )) ,
                         ("base 2",(( base2   , [48]) , 2)) ,
                         ("base 8",(( base8   , [55]) , 8)) ,
                         ("base 10",(( base10   , [57]) , 10)) ,    
                         ("base 16",(( base16  , [102]) , 16)) ,
                         ("base 32",(( base32  , [98]) , 32)), 
                         ("base 32z",(( base32z  , [104]) , 32)), 
                         ("base 32hex",(( base32hex  , [118]) , 32 )),
                         ("base 58btc",(( base58btc  , [122]) , 58)) ,
                         ("base 58flickr",(( base58flickr  , [91]) , 58 )) ,
                         ("base 64",(( base64  , [109]) , 64)) ,
                         ("base 64URL" , (( base64URL  , [117]) , 64)) ]
                         
baseNumMap = M.fromList [([0] , "base id") , ([48] , "base 2") , ([55] , "base 8") , ([57] , "base 10") ,( [102] , "base 16"), ([98] , "base 32" ), ([104] , "base 32z") ,( [118] , "base 32hex")  ,( [122] , "base 58btc" ),( [91] , "base 58flickr") , ([109] , "base 64") ,( [117] , "base 64URL")]

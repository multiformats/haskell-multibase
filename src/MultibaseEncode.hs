module MultibaseEncode(
                 encodeByteString,
                 decodeByteString,
                 word8Range,
                 baseid,
                 base2,
                 base8,
                 base10,
                 base16,
                 base32,
                 base32hex,
                 base32z,
                 base58btc,
                 base58flickr,
                 base64,
                 base64URL
                 )  where

import qualified Data.ByteString as BStr
import qualified Data.Bits as Bin
import qualified Data.Map.Strict as Map
import qualified Data.Word as W


decodeByteString :: BStr.ByteString -> Integer -> BStr.ByteString -> Int ->  BStr.ByteString
decodeByteString byteString prevBase prevByteset recurrCount
   | (Map.member prevBase bNumToBlO) && (mod (length (filter (/= 61) (BStr.unpack byteString))) (bNumToBlO Map.! prevBase) /= 0) =
               decodeByteString  ( BStr.pack ((filter (/= 61) (BStr.unpack byteString)) ++ [lookupTable prevBase prevByteset Map.! 0] )) prevBase prevByteset (recurrCount + 1)
   | otherwise = BStr.pack $ reverse $ drop recurrCount (reverse $ encode (map (invertedTable prevBase prevByteset Map.!) (BStr.unpack byteString) ) 256 word8Range prevBase )


encodeByteString :: BStr.ByteString -> Integer -> BStr.ByteString -> Bool -> BStr.ByteString
encodeByteString byteString base byteset padFlag
     | (Map.member base  bNumToBl) &&  (mod (BStr.length byteString) (bNumToBl Map.! base)  /= 0)  =  BStr.pack $  padCatcher ( BStr.unpack byteString) base byteset padFlag (padHelper (BStr.unpack byteString) base)

     | otherwise = BStr.pack $ encode (BStr.unpack byteString) base byteset 256



padHelper w8List base  =  (bNumToBl Map.! base) - (mod (length w8List) (bNumToBl Map.! base))




padCatcher :: [W.Word8] -> Integer -> BStr.ByteString -> Bool -> Int -> [W.Word8]    
padCatcher w8List base byteSet padFlag pHn
         | padFlag  = reverse ( (++) (replicate pHn (61 :: W.Word8) ) (drop  pHn (reverse (encodeWithPadding w8List base byteSet pHn))))

         | otherwise = reverse (drop   pHn (reverse (encodeWithPadding w8List base byteSet pHn)))


encodeWithPadding :: [W.Word8] -> Integer -> BStr.ByteString -> Int  -> [W.Word8]                   
encodeWithPadding  w8List base byteSet pHn = encode (w8List ++ replicate pHn (0 :: W.Word8) ) base byteSet 256                    



encode :: [W.Word8] -> Integer -> BStr.ByteString -> Integer -> [W.Word8]
encode byteString base byteSet prevBase  =
                map (lookupTable base byteSet Map.!) (integerListToWord8List  $ convertInteger [] ( convertIntegerList 0 ( word8ListToIntegerList byteString) prevBase) base)

                

-- Integer base conversion ---------------------------------------------------------------------------------------------------------------------------------------------------

convertInteger :: [Integer] -> Integer -> Integer -> [Integer]
convertInteger oW eW base
            | eW == 0  = oW
            | otherwise =  convertInteger (mod eW base :  oW) (quot eW base) base



convertIntegerList :: Integer -> [Integer] -> Integer -> Integer
convertIntegerList oW eW base  
            | null eW   = oW
            | otherwise = convertIntegerList ((oW * base ) + head eW) (Prelude.tail eW) base



--- ByteString base encoding--------------------------------------------------------------------------------------------------------------------------------------------------

word8ListToIntegerList :: [W.Word8] -> [Integer]
word8ListToIntegerList  = map toInteger 



integerListToWord8List :: [Integer] -> [W.Word8]
integerListToWord8List  = map fromInteger 



--------useful maps and map builders -----------------------------------------------------------------------------------------------------------------------------------------
lookupTable base byteSet =  Map.fromList (BStr.zip (BStr.take (fromInteger base) word8Range) byteSet)

invertedTable base byteSet =  Map.fromList (map (\(a , b) -> (b , a)) (Map.toList (lookupTable base byteSet)) )

word8Range = BStr.pack [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                        21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                        41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                        61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                        81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100, 
                        101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,
                        121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,
                        141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,
                        161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,
                        181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,
                        201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,
                        221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,
                        241,242,243,244,245,246,247,248,249,250,251,252,253,254,255]


bNumToBl  =  Map.fromList [(16,1),(32,5),(64,3),(128,7)]

bNumToBlO  =  Map.fromList [(16,2),(32,8),(64,4),(128,8)]

---- bytesets ----------------------------------------------------------------------------------------------------------------------------------------------------------------

baseid = word8Range

base2 = BStr.pack [48,49]

base8  = BStr.pack [48,49,50,51,52,53,54,55]

base10 = BStr.pack [48,49,50,51,52,53,54,55,56,57]

base16 = BStr.pack [48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70]

base32 = BStr.pack [65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                         81,82,83,84,85,86,87,88,89,90,50,51,52,53,54,55]

base32hex = BStr.pack [48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                         81,82,83,84,85,86]                  

base32z   = BStr.pack [121,98,110,100,114,102,103,56,101,106,107,109,99,112,113,120,111,116,49,117,119,105,115,122,97,51,52,
                        53,104,55,54,57]                      
                         
base58btc = BStr.pack [49,50,51,52,53,54,55,56,57,65,66,67,68,69,70,71,72,74,75,76,77,78,80,
                         81,82,83,84,85,86,87,88,89,90,97,98,99,100, 
                         101,102,103,104,105,106,107,109,110,111,112,113,114,115,116,117,118,119,120,
                         121,122]

base58flickr = BStr.pack [49,50,51,52,53,54,55,56,57,97,98,99,100, 
                         101,102,103,104,105,106,107,109,110,111,112,113,114,115,116,117,118,119,120,
                         121,122,65,66,67,68,69,70,71,72,74,75,76,77,78,80,
                         81,82,83,84,85,86,87,88,89,90]

base64 = BStr.pack [65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                         81,82,83,84,85,86,87,88,89,90,97,98,99,100, 
                         101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,
                         121,122,48,49,50,51,52,53,54,55,56,57,43,47]

base64URL = BStr.pack [65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                         81,82,83,84,85,86,87,88,89,90,97,98,99,100, 
                         101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,
                         121,122,48,49,50,51,52,53,54,55,56,57,45,95]

                    












 




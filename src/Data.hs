module Data
 (baseIndex
 ) where

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

 
      
            



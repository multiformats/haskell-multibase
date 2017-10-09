# haskell-multibase

Cabal library name: Multibases

WARNING : This package is not complete yet.

TODO : finnish implementing base 58 and 10 encodings

to build the downloaded package :

cd to the extracted package directory

    cabal configure --prefix=$HOME --user 
    cabal build
    cabal install
    
This library currently comes with one module Multibase which contains two functions :
    
     multiEncode :: Strinng -> String -> Bool -> Bool -> String
     multiEncode "baseX" "String to encode" padding(Bool) uppercase(Bool) = "encodedString"  
and
 
     multiDecode :: String -> String 
     multiDecode "multi-encoded String to decode"
    
which detects automatically the base which was used for the multiEncode

Supported bases so far:

"baseid" 
"base1" 
"base2" 
"base8" 
"base10" (to come) 
"base16" 
"base32" 
"base32hex" 
"base32z" 
"base58flickr" (to come) 
"base58btc" (to come) 
"base64" 
"base64url" 

see https://github.com/multiformats/multibase/blob/master/README.md for more informations on the project itself.




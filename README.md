# haskell-multibase

[![](https://img.shields.io/badge/project-multiformats-blue.svg?style=flat-square)](https://github.com/multiformats/multiformats)
[![](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)


> Implementation of [multibase](https://github.com/multiformats/multibase) -self identifying base encodings- in Haskell.

## Install

This library can be built with cabal:

     cd path/to/extracted/library
     cabal build
if you want to install it:
     
     cabal install
     
## Usage

This library exports the Multibase module which comes with five functions:

      multiEncode :: String -> Data.ByteString.ByteString -> Bool ->  Data.ByteString.ByteString 
     
      -- takes a base encoding and a byteString and returns the encoded byteString with its base prefix
      -- Bool is the padding flag (makes a difference only for bases for which padding is standardized)
      
      decodeByteString :: Data.ByteString.ByteString -> Data.ByteString.ByteString 
     
      -- takes a multibase-encoded byteString and decodes it (UTF-8), prefix is used to determine encoding
     
      appendPrefix :: String -> Data.ByteString.ByteString -> Data.ByteString.ByteString 
     
      -- appends a multibase prefix to a byteString without encoding it
     
      removePrefix :: Data.ByteString.ByteString -> Data.ByteString.ByteString 
     
      -- just removes the prefix, be careful not to use it on a non-prefixed byteString
     
String can be : "base id" , "base 2" , "base 8" , "base 10" , "base16" , "base 32" , "base 32z" , "base 32hex" , "base 58btc" ,
                "base 58flickr" , "base 64" , "base64URL"
               

Please check [multibase](https://github.com/multiformats/multibase/) readme for more information.

## License 

[MIT](LICENSE) © 2017 multiformats

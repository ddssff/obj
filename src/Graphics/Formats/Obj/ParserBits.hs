{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj.ParserBits
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Common pieces of parsers for obj model parsing
----------------------------------------------------------------------
module Graphics.Formats.Obj.ParserBits
       (unsafeReadFloat,unsafeRFloat
       ,anyOf
       ,consumeWS,firstWord, bsWords,removeComments,parseName) where

import Foreign
import Foreign.C.String

import Data.ByteString.Internal
import qualified Data.ByteString.Char8  as CBS
import qualified Data.ByteString.Unsafe as BS

import Data.Maybe

import Control.Applicative hiding ((<|>))
import Control.Applicative.Infix

unsafeReadFloat :: CBS.ByteString -> Float
unsafeReadFloat =
  fst . maybe (error "unsafeReadFloat: No float to read.") id . unsafeRFloat

foreign import ccall unsafe "stdlib.h strtof" 
    c_strtof :: CString -> Ptr CString -> IO Float

-- | Bare bones, unsafe wrapper for strtof. This provides a non-copying
--   direct parsing of Float values from a ByteString. It uses strtof
--   directly on the bytestring buffer. strtof requires the string to be
--   null terminated, or for a guarantee that parsing will find a floating
--   point value before the end of the string.
--   Taken from Bytestring's ReadDouble
unsafeRFloat :: ByteString -> Maybe (Float, ByteString)
unsafeRFloat b | CBS.null b = Nothing
unsafeRFloat b = inlinePerformIO $
    alloca $ \resptr ->
    BS.unsafeUseAsCString b $ \ptr ->
      do -- copy just the bytes we want to parse
        d      <- c_strtof ptr resptr  -- 
        newPtr <- peek resptr
        return $! case d of
            0 | newPtr == ptr -> Nothing
            _ | otherwise  ->
                    let rest = BS.unsafeDrop (newPtr `minusPtr` ptr) b
                        z    = realToFrac d
                    in  z `seq` rest `seq` Just $! (z, rest)
{-# INLINE unsafeReadFloat #-}

anyOf :: [a -> Bool] -> a -> Bool
anyOf = foldr (liftA2 (||)) (const False)

bsWords :: CBS.ByteString -> [CBS.ByteString]
bsWords =
  filter ((>0) . CBS.length) . CBS.splitWith ((==' ') <^(||)^> (=='\t'))

removeComments :: CBS.ByteString -> CBS.ByteString
removeComments bs = case CBS.split '#' bs of
                      []    -> CBS.empty
                      (x:_) -> x

consumeWS :: CBS.ByteString -> CBS.ByteString
consumeWS = CBS.dropWhile ((==' ') <^(||)^> (=='\t'))

firstWord :: CBS.ByteString -> CBS.ByteString
firstWord = CBS.takeWhile (not . anyOf [(==' '), (=='\t'), (=='\n'), (=='\r')])

parseName :: CBS.ByteString -> CBS.ByteString
parseName = firstWord . consumeWS

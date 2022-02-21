{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Mtl.Parse
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Mtl format parser using bytestrings
----------------------------------------------------------------------
module Graphics.Formats.Mtl.Parse where

import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy  as LBS

import Data.Binary
import Data.Binary.Get

import qualified Data.Map as M

import Graphics.Rendering.OpenGL

import Graphics.Formats.Mtl.Contents
import Graphics.Formats.Obj.ParserBits

import Control.Monad

instance Binary MtlFile where
  put (MF x) = forM_ (M.elems x) put
  get =
    return . MF
           . buildMap
           . map decodeMtl
           . chunk (CBS.pack "newmtl")
           . CBS.concat
           . LBS.toChunks =<< getRemainingLazyByteString

instance (Show a, Binary a) => Binary (Color4 a) where
  put (Color4 r g b a) = put (show r) >> put ' ' >> put (show g) >> put ' ' >>
                         put (show b) >> put ' ' >> put (show a)
  get = undefined

buildMap :: [Material] -> M.Map CBS.ByteString Material
buildMap x = M.fromList $ zip (map name x) x

chunk :: CBS.ByteString -> CBS.ByteString -> [CBS.ByteString]
chunk x y = h : if CBS.null t then [] else chunk x (CBS.drop (CBS.length x) t)
            where
              (h,t) = CBS.breakSubstring x y

instance Binary Material where
  put m = do put "newmtl " >> put (name           m) >> put '\n'
             put "Ka "     >> put (ambientColour  m) >> put '\n'
             put "Kd "     >> put (diffuseColour  m) >> put '\n'
             put "Ks "     >> put (specularColour m) >> put '\n'
  get = undefined

decodeMtl :: CBS.ByteString -> Material
decodeMtl =   foldr ($) emptyMat . map decodeLine . CBS.lines

decodeLine :: CBS.ByteString -> Material -> Material
decodeLine = decodeLine' . consumeWS . removeComments

decodeLine' :: CBS.ByteString -> Material -> Material
decodeLine' s =
  if CBS.length s > 0 then
    case s of
      _ | (CBS.pack "Ka")     `CBS.isPrefixOf` s ->
        colour  setAmbient          (CBS.drop 2 s)
      _ | (CBS.pack "Kd")     `CBS.isPrefixOf` s ->
        colour  setDiffuse          (CBS.drop 2 s)
      _ | (CBS.pack "Ks")     `CBS.isPrefixOf` s ->
        colour  setSpecular         (CBS.drop 2 s)
      _ | (CBS.pack "map_Ka") `CBS.isPrefixOf` s ->
        applyTex setAmbientTexName  (CBS.drop 6 s)
      _ | (CBS.pack "map_Kd") `CBS.isPrefixOf` s ->
        applyTex setDiffuseTexName  (CBS.drop 6 s)
      _ | (CBS.pack "map_Ks") `CBS.isPrefixOf` s ->
        applyTex setSpecularTexName (CBS.drop 6 s)
      x ->
        flip setName . parseName $ x
  else id

colour :: (a -> Color4 GLfloat -> c) -> CBS.ByteString -> a -> c
colour f = (flip f) . makeColour . map unsafeReadFloat . bsWords

applyTex :: (a -> String -> c) -> CBS.ByteString -> a -> c
applyTex f = (flip f) . CBS.unpack . parseName

makeColour :: [Float] -> Color4 GLfloat
makeColour [r,g,b]     = Color4 r g b 1
makeColour (r:g:b:a:_) = Color4 r g b a
makeColour x           = error (show x)

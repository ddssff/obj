{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphcs.Formats.Mtl.Contents
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Mtl file content description
----------------------------------------------------------------------
module Graphics.Formats.Mtl.Contents
       (MtlFile(..),Material(..)
       ,setName,setMatFile
       ,setAmbient       ,setDiffuse       ,setSpecular
       ,setAmbientTexName,setDiffuseTexName,setSpecularTexName
       ,loadTextures
       ,emptyMat,whiteMat) where

import           Data.Map            (Map)
import qualified Data.Map         as M
import qualified Data.Traversable as T
import           Data.List

import Foreign hiding (newArray)

import Data.Array.Unboxed

import Graphics.Rendering.OpenGL
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Instances.OpenGL ()

import Codec.Image.DevIL

import qualified Data.ByteString.Char8 as CBS

newtype MtlFile = MF (Map CBS.ByteString Material)
                  deriving Show

data Material = Mat {name    :: CBS.ByteString
                    ,matFile :: FilePath
                    ,ambientColour  :: Color4 GLfloat
                    ,diffuseColour  :: Color4 GLfloat
                    ,specularColour :: Color4 GLfloat
                    ,ambientTex     :: Either String TextureObject
                    ,diffuseTex     :: Either String TextureObject
                    ,specularTex    :: Either String TextureObject}
                deriving (Show,Eq,Ord)

instance Arbitrary Material where
  arbitrary = liftM2 setName
                     (liftM2 setMatFile
                             (liftM2 setDiffuse
                                     (liftM2 setAmbient
                                             (liftM2 setSpecular
                                                     (return emptyMat)
                                                     arbitrary)
                                             arbitrary)
                                     arbitrary)
                             (anyList nonSpace))
                     (CBS.pack <$> anyList nonSpace)
instance CoArbitrary Material where
  coarbitrary m =   coarbitrary (CBS.unpack $ name m)
                  . coarbitrary (matFile m)
                  . coarbitrary (ambientColour  m)
                  . coarbitrary (diffuseColour  m)
                  . coarbitrary (specularColour m)
                  . coarbitrary (ambientTex     m)
                  . coarbitrary (diffuseTex     m)
                  . coarbitrary (specularTex    m)

loadTextures :: (FilePath -> IO (Maybe FilePath))
             -> MtlFile
             -> IO ([FilePath],MtlFile)
loadTextures f (MF ms) =
  do loaded <- mmapM (loadMtlTextures f) $ ms
     return (filter (/= "") . nub . concat . M.elems . M.map fst $ loaded
            ,MF $ M.map snd loaded)

mmapM :: (Monad m) => (a -> m b) -> Map k a -> m (Map k b)
mmapM f = T.sequence . M.map f 

loadMtlTextures :: (FilePath -> IO (Maybe FilePath))
                -> Material
                -> IO ([FilePath],Material)
loadMtlTextures f m =
  do at <- maybeLoadTex (ambientTex  m)
     dt <- maybeLoadTex (diffuseTex  m)
     st <- maybeLoadTex (specularTex m)
     let missing = missing' at ++ missing' dt ++ missing' st
     return (missing,m {ambientTex = at, diffuseTex = dt, specularTex = st})
  where
    missing' :: Either String TextureObject -> [String]
    missing' (Left x)  = [x]
    missing' (Right _) = []
    maybeLoadTex :: Either String TextureObject
                 -> IO (Either String TextureObject)
    maybeLoadTex mat =
      case mat of
        Left "" -> return $ Left ""
        Left x  -> do fn <- f x
                      (case fn of
                         Just fn' ->
                           do t <- loadTexture fn'
                              return $ Right t
                         Nothing ->
                           return $ Left x)
        Right x -> return $ Right x   

loadTexture :: FilePath -> IO TextureObject
loadTexture f = buildTexture =<< readImage f

{- Andy Gill's hacky texture loading code -}
buildTexture :: UArray (Int,Int,Int) Word8 -> IO TextureObject
buildTexture arr =
  do let (width,height,mindepth) =
           case bounds arr of
             ((mw,mh,md),(w,h,_)) -> (w + 1 - mw,h + 1 - mh, md)
     p <- mallocBytes (width * height * 4)
     sequence_
       [ do pokeElemOff p (off+0) (arr ! (w,h,mindepth  ))
            pokeElemOff p (off+1) (arr ! (w,h,mindepth+1))
            pokeElemOff p (off+2) (arr ! (w,h,mindepth+2))
            pokeElemOff p (off+3) (arr ! (w,h,mindepth+3))
       | (off,(w,h)) <- zip [0,4 ..] [ (w,h) | w <- [ 0 .. width - 1 ]
                                             , h <- [ 0 .. height  - 1 ]]
       ]
      
     texName <- liftM head (genObjectNames 1)
     textureBinding Texture2D $= Just texName
     textureFilter  Texture2D $= ((Linear', Nothing), Linear')
     
     textureWrapMode Texture2D S $= (Repeated, Repeat)
     textureWrapMode Texture2D T $= (Repeated, Repeat) 
     
     let pd = PixelData RGBA UnsignedByte p
     texImage2D Texture2D
                NoProxy
                0
                RGBA'
                (TextureSize2D (fromIntegral width) (fromIntegral height))
                0
                pd
     return texName

setName :: Material -> CBS.ByteString -> Material
setName m n = m {name = n}

setMatFile :: Material -> FilePath -> Material
setMatFile m f = m {matFile = f}

setAmbient :: Material -> Color4 GLfloat -> Material
setAmbient m c = m {ambientColour = c}

setDiffuse :: Material -> Color4 GLfloat -> Material
setDiffuse m c = m {diffuseColour = c}

setSpecular :: Material -> Color4 GLfloat -> Material
setSpecular m c = m {specularColour = c}

setAmbientTexName :: Material -> String -> Material
setAmbientTexName m t = m {ambientTex = Left t}

setDiffuseTexName :: Material -> String -> Material
setDiffuseTexName m t = m {diffuseTex = Left t}

setSpecularTexName :: Material -> String -> Material
setSpecularTexName m t = m {specularTex = Left t}

emptyMat :: Material
emptyMat = Mat {name           = CBS.pack ""
               ,matFile        = ""
               ,ambientColour  = Color4 0.0 0.0 0.0 0.0
               ,diffuseColour  = Color4 0.0 0.0 0.0 0.0
               ,specularColour = Color4 0.0 0.0 0.0 0.0
               ,ambientTex     = Left ""
               ,diffuseTex     = Left ""
               ,specularTex    = Left ""}

whiteMat :: Material
whiteMat = Mat {name           = CBS.pack "white"
               ,matFile        = ""
               ,ambientColour  = Color4 1   1   1   1
               ,diffuseColour  = Color4 0.5 0.5 0.5 1
               ,specularColour = Color4 0   0   0   1
               ,ambientTex     = Left ""
               ,diffuseTex     = Left ""
               ,specularTex    = Left ""}

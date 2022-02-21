{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj.Contents
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Describes the concrete syntax of an Obj file
----------------------------------------------------------------------
module Graphics.Formats.Obj.Contents where

import Test.QuickCheck
import Test.QuickCheck.Instances

import Control.Applicative.Infix

import qualified Data.ByteString.Char8 as CBS

newtype ObjFile = OF [Statement]
                  deriving (Show,Eq)

instance Arbitrary ObjFile where
  arbitrary          = OF <$> arbitrary
instance CoArbitrary ObjFile where
  coarbitrary (OF x) = coarbitrary x

data Statement = V      !Float !Float !Float !Float
               | VN     !Float !Float !Float
               | VT     !Float !Float !Float
               | P      ![Int]
               | L      ![VDouble]
               | F      ![VTriple]
               | G      ![Group]
               | SG     !Int
               | MtlLib ![CBS.ByteString]
               | UseMtl !CBS.ByteString
                 deriving (Show,Read,Eq)

data VTriple = VTr !Int !(Maybe Int) !(Maybe Int)
               deriving (Eq,Ord,Show,Read)
data VDouble = VD  !Int !(Maybe Int)
               deriving (Eq,Ord,Show,Read)
type Group   = CBS.ByteString

instance Arbitrary Statement where
  arbitrary =
    oneof [V  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          ,VN <$> arbitrary <*> arbitrary <*> arbitrary
          ,VT <$> arbitrary <*> arbitrary <*> arbitrary
          ,P  <$> (nonEmpty nonZero_)
          ,L  <$> (nonEmpty arbitrary)
          ,F  <$> (nonEmpty arbitrary)
          ,(G . map CBS.pack) <$> (nonEmpty (nonEmpty (notOneof " \t\n\r#")))
          ,SG <$> positive]
instance CoArbitrary Statement where
  coarbitrary (V x y z w) =
    coarbitrary x . coarbitrary y . coarbitrary z . coarbitrary w
  coarbitrary (VN x y z)  =
    coarbitrary x . coarbitrary y . coarbitrary z
  coarbitrary (VT x y z)  =
    coarbitrary x . coarbitrary y . coarbitrary z
  coarbitrary (P n)       = coarbitrary n
  coarbitrary (L n)       = coarbitrary n
  coarbitrary (F n)       = coarbitrary n
  coarbitrary (G n)       = coarbitrary (map CBS.unpack n)
  coarbitrary (SG g)      = coarbitrary g
  coarbitrary (UseMtl xs) = coarbitrary (CBS.unpack xs)
  coarbitrary (MtlLib x)  = coarbitrary (map CBS.unpack x)
  
instance Arbitrary VTriple where
  arbitrary = VTr <$> positive <*> maybeGen positive <*> maybeGen positive
instance CoArbitrary VTriple where
  coarbitrary (VTr v t n) = coarbitrary v . coarbitrary t . coarbitrary n

instance Arbitrary VDouble where
  arbitrary            = VD <$> positive <*> maybeGen positive
instance CoArbitrary VDouble where
  coarbitrary (VD v t) = coarbitrary v . coarbitrary t

isNormal, isTexCoord, isVertex, isPoints , isLines :: Statement -> Bool
isFace  , isObject  , isUseMtl, isSmoothG          :: Statement -> Bool

isNormal   (VN _ _ _  ) = True
isNormal   _            = False
isTexCoord (VT _ _ _  ) = True
isTexCoord _            = False
isVertex   (V  _ _ _ _) = True
isVertex   _            = False
isPoints   (P  _      ) = True
isPoints   _            = False
isLines    (L  _      ) = True
isLines    _            = False
isFace     (F  _      ) = True
isFace     _            = False
isUseMtl   (UseMtl _  ) = True
isUseMtl   _            = False
isSmoothG  (SG _      ) = True
isSmoothG  _            = False
isObject                = isFace <^(||)^> isLines <^(||)^> isPoints

contentsTests :: IO ()
contentsTests = return ()

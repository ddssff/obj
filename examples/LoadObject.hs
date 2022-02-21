{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  LoadObject
-- Copyright   :  (c) Anygma BVBA and Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Based on BSD Licenced Source code by Conal Elliott and Andy J Gill.
--
-- Load an Obj model at a specified scale, and spin it round on screen
----------------------------------------------------------------------

module Main where

import System
import System.FilePath

import Data.Monoid
import Control.Applicative
import Control.Applicative.Infix

import FRP.Reactive
import FRP.Reactive.GLUT.Adapter
import Graphics.FieldTrip

import Graphics.Formats
import Graphics.Formats.Obj

import Graphics.Rendering.OpenGL

import Codec.Image.DevIL

-- | Load an object at a specified scale and render it.
main :: IO ()
main =
  do [file,scale] <- System.getArgs
     ilInit
     simpleInit ("ObjModel: " ++ file)
     lightModelTwoSide $= Enabled
     dl <- objFromFile file [fst (splitFileName file)] >>= displayListR
     adapt . spinningR (read scale) . renderableG $ dl

-- | Produces a Behavior of rendering actions based on a piece of geometry.
--   The geometry is rendered spinning at a specified scale.
spinningR :: Double -> Geometry3 -> UI -> Behavior Action
spinningR scale g = const (render3 <$> spinningG scale g)

-- | Produces a Behavior of geometry based on transforming a static piece of
--   Geometry.  The transformation spins the geometry over time.
spinningG :: Double -> Geometry3 -> Behavior Geometry3
spinningG scale g  = spinning scale <^(*%)^> pure g

-- | A Behavior of transformations.  Specifies how something should be moved
--   at any time point.  This Behavior describes a spinning motion.
spinning :: Double -> Behavior (Transform3 Double)
spinning scale = f <$> time
 where
   f t =           translate3       (Vector3 (0::Double) 0 (2*sin (-t/5) - 3))
         `mappend` rotate3    t     (Vector3 0.2 0.2 0.3)
         `mappend` uscale3    scale

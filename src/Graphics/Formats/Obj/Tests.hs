{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj.Tests
-- Copyright   :  (c) Thomas Davie 2008
-- License     :  MIT
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Testing for Obj model loading
----------------------------------------------------------------------
module Graphics.Formats.Obj.Tests (runTests) where

import Graphics.Formats.Obj.Contents
import Graphics.Formats.Obj.Parse
import Graphics.Formats.Obj.ObjModel

runTests :: IO ()
runTests = contentsTests >> parseTests >> geometryTests
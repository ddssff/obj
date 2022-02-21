{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Obj model loader
----------------------------------------------------------------------
module Graphics.Formats.Obj
       (objFromFile
       ,ObjModel
       ) where

import Data.Binary

import System.Directory hiding (findFile)
import System.FilePath

import qualified Data.Map as M
import Data.Maybe hiding (fromJust)

import Graphics.Formats.Obj.Parse
import Graphics.Formats.Obj.ObjModel
import Graphics.Formats.Mtl.Contents
import Graphics.Formats.Mtl.Parse    ()

import qualified Data.ByteString.Char8 as CBS

-- | Loads an Obj model from a file given a list of search paths to find
--   materials and textures at.
objFromFile :: FilePath -> [FilePath] -> IO ObjModel
objFromFile x sps =
  do -- Parse the obj
     cs <- decodeFile x
     -- Find the material files and parse them too
     let mtlfs = map CBS.unpack $ mtllibs cs
     files <- mapM (findFile sps) mtlfs
     mapM_ (putStrLn . ("Warning: File not found: " ++) . fst)
       . filter ((==Nothing) . snd)
       $ zip mtlfs files
     parsedMats <- mapM decodeFile (catMaybes files)
     let ms = MF . M.unions . map (\(MF a) -> a) $ parsedMats
     -- Now find all the textures and load them
     (missingTexFiles,ms') <- loadTextures (findFile sps) ms
     mapM_ (putStrLn . ("Warning: File not found: " ++)) missingTexFiles
     -- Put everything together and hand it back
     return $ geometry cs ms'

-- | Finds a file given a list of search paths to look for it at.
--   The first file found is returned.
findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile sps f = (listToMaybe . catMaybes) <$> (mapM (tryFile f) sps)

-- | Checks if a file exists at a specified search path
--   If it does, returns that filepath in a maybe
tryFile :: FilePath -> FilePath -> IO (Maybe FilePath)
tryFile x y = do e <- doesFileExist (y </> x)
                 if e then return . Just $ (y </> x) else return Nothing

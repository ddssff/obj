{-# OPTIONS_GHC -Wall -fno-warn-orphans -funbox-strict-fields #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj.Parse
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Obj file parsing
----------------------------------------------------------------------
module Graphics.Formats.Obj.Parse (parseTests,mtllibs) where

import Graphics.Formats.Obj.Contents
import Graphics.Formats.Obj.ParserBits

import Test.QuickCheck

import Data.Maybe hiding (fromJust)

import           Data.Binary hiding (putList)
import           Data.Binary.Get
import           Data.Binary.Put

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as CBS

import Control.Monad

instance Binary ObjFile where
  put (OF sts) =
    forM_ sts put
  get =
    return . OF
           . catMaybes
           . map decodeStmt
           . CBS.lines
           . CBS.concat
           . LBS.toChunks =<< getRemainingLazyByteString

instance Binary Statement where
  put (V x y z w) =
    do putString "v "
       putShow x >> put ' '
       putShow y >> put ' '
       putShow z >> put ' '
       putShow w >> put ' '
       put '\n'
  put (VN x y z) =
    do putString "vn "
       putShow x >> put ' '
       putShow y >> put ' '
       putShow z >> put ' '
       put '\n'
  put (VT x y z) =
    do putString "vt "
       putShow x >> put ' '
       putShow y >> put ' '
       putShow z >> put ' '
       put '\n'
  put (P is) = put 'p'  >> putList putShow   is     >> put '\n'
  put (L is) = put 'l'  >> putList putDouble is     >> put '\n'
  put (F is) = put 'f'  >> putList putTriple is     >> put '\n'
  put (G gs) = put 'g'  >> putList putByteString gs >> put '\n'
  put (SG g) = putString "s " >> putShow g          >> put '\n'
  put (MtlLib m) = put "mtllib"  >> putList putByteString m >> put '\n'
  put (UseMtl m) = put "usemtl " >> putByteString m         >> put '\n'
  get = undefined


putString :: String -> Put
putString = putByteString . CBS.pack

putShow :: Show a => a -> Put
putShow = putString . show

putList :: (a -> Put) -> [a] -> Put
putList f x = forM_ x (\i -> put ' ' >> f i)

putDouble :: VDouble -> Put
putDouble (VD x (Just y)) = putShow x >> put '/' >> putShow y
putDouble (VD x Nothing ) = putShow x

putTriple :: VTriple -> Put
putTriple (VTr v t n) =
  putShow v >>
    case (t,n) of
      (Nothing,Just n') -> putString "//" >> putShow n'
      _                 -> put' t >> put' n
  where
    put' x = case x of
               Nothing -> return ()
               Just x' -> put '/' >> putShow x'

decodeStmt :: CBS.ByteString -> Maybe Statement
decodeStmt = decodeStmt' . consumeWS . removeComments

decodeStmt' :: CBS.ByteString -> Maybe Statement
decodeStmt' s =
  if CBS.length s > 0 then
    case CBS.head s of
      'p' -> Just . P      . runParse parsePoints                   $ s
      'l' -> Just . L      . runParse parseLines                    $ s
      'f' -> Just . F      . runParse parseFace                     $ s
      'g' -> Just . G      . runParse parseGroups                   $ s
      's' -> Just . SG     . runParse parseSmoothGroup              $ s
      _ | (CBS.pack "vn")     `CBS.isPrefixOf` s
          -> Just          . runParse parseNormal      . CBS.tail   $ s
      _ | (CBS.pack "vt")     `CBS.isPrefixOf` s
          -> Just          . runParse parseTexCoord    . CBS.tail   $ s
      'v' -> Just          . runParse parseVertex                   $ s
      _ | (CBS.pack "mtllib") `CBS.isPrefixOf` s
          -> Just . MtlLib . runParse parseMtlLib      . CBS.drop 5 $ s
      _ | (CBS.pack "usemtl") `CBS.isPrefixOf` s
          -> Just . UseMtl . runParse parseUseMtl      . CBS.drop 5 $ s
      _   -> Nothing
    else Nothing

runParse :: (CBS.ByteString -> a) -> CBS.ByteString -> a
runParse x = x . consumeWS . CBS.tail

if' :: Bool -> a -> a -> a
if' c t e = if c then t else e

parsePoints :: CBS.ByteString -> [Int]
parseLines  :: CBS.ByteString -> [VDouble]
parseFace   :: CBS.ByteString -> [VTriple]
parsePoints = map unsafeReadInt . bsWords
parseLines  = map readDouble    . bsWords
parseFace   = map readTriple    . bsWords

parseGroups      :: CBS.ByteString -> [CBS.ByteString]
parseSmoothGroup :: CBS.ByteString -> Int
parseGroups      = map parseName . bsWords
parseSmoothGroup = if' <$> (== CBS.pack "off") <*> (const 0) <*> unsafeReadInt

parseMtlLib :: CBS.ByteString -> [CBS.ByteString]
parseUseMtl :: CBS.ByteString -> CBS.ByteString
parseMtlLib = map parseName    . bsWords
parseUseMtl = parseName . head . bsWords

parseNormal   :: CBS.ByteString -> Statement
parseTexCoord :: CBS.ByteString -> Statement
parseVertex   :: CBS.ByteString -> Statement
parseNormal   s = let Just (x,s' ) = unsafeRFloat s
                      Just (y,s'') = unsafeRFloat s'
                      Just (z,_  ) = unsafeRFloat s''
                  in VN x y z
parseTexCoord s = let Just (x,s') = unsafeRFloat s
                      y           = unsafeRFloat s'
                  in case y of
                       Just (y',r) -> case unsafeRFloat r of
                                        Just (z,_) -> VT x y' z
                                        Nothing    -> VT x y' 0
                       Nothing     -> VT x 0 0
parseVertex   s = let Just (x,s'  ) = unsafeRFloat s
                      Just (y,s'' ) = unsafeRFloat s'
                      Just (z,s''') = unsafeRFloat s''
                      w             = unsafeRFloat s'''
                  in case w of
                       Just (w',_) -> V x y z w'
                       Nothing     -> V x y z 1


unsafeReadInt :: CBS.ByteString -> Int
unsafeReadInt x = case CBS.readInt x of
                    Just (i,_) -> i
                    Nothing    -> error "unsafeReadInt: No integer to read."

readDouble :: CBS.ByteString -> VDouble
readDouble x =
  if CBS.length b > 1
    then VD (unsafeReadInt a) (Just . unsafeReadInt $ CBS.tail b)
    else VD (unsafeReadInt a) Nothing
  where
    (a,b) = CBS.break (=='/') x

-- | Read a vertex/texcoord/normal triple.
--   Triples can take these forms:
--   v, v/t, v//n, v/t/n
readTriple :: CBS.ByteString -> VTriple
readTriple vtns = 
  VTr v t n
  where
    (vs,tnr) = CBS.break (=='/') vtns
    (ts,nr ) = if CBS.length tnr > 0
                 then CBS.break (=='/') . CBS.tail $ tnr
                 else (CBS.empty, CBS.empty)
    ns       = if CBS.length nr > 0
                 then CBS.tail nr
                 else CBS.empty
    
    v        = unsafeReadInt vs
    t        = getMaybeInt ts
    n        = getMaybeInt ns
    
    getMaybeInt x = if CBS.length x > 0
                      then Just $ unsafeReadInt x
                      else Nothing

mtllibs :: ObjFile -> [CBS.ByteString]
mtllibs (OF f) = concatMap stmtMtlLibs f

stmtMtlLibs :: Statement -> [CBS.ByteString]
stmtMtlLibs (MtlLib xs) = xs
stmtMtlLibs _ = []

prop_parseUnParse :: ObjFile -> Bool
prop_parseUnParse x =
  (decode . encode $ x) == x

parseTests :: IO ()
parseTests = do putStr "prop_parseUnParse: "
                quickCheck prop_parseUnParse

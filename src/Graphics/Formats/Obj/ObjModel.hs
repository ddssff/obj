{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.Obj.ObjModel
-- Copyright   :  (c) Anygma BVBA and Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Describes an Obj Model
----------------------------------------------------------------------
module Graphics.Formats.Obj.ObjModel
       (geometryTests
       ,ObjModel()
       ,geometry,objFile
       ,renderONormals
       ) where

import Graphics.Formats
import Graphics.Formats.Obj.Contents
import Graphics.Formats.Mtl.Contents

import Graphics.Rendering.OpenGL

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Instances

import Data.List
import Data.Array
import Data.Function
import qualified Data.Map    as M
import           Data.IntMap    (IntMap)
import qualified Data.IntMap as IM

import Control.Monad

data ObjModel = OM BufferSet [FObject]
                   deriving (Show)

type BufferSet = (VertexBuffer, TexCoordBuffer, NormalBuffer)
data NVTriple = NVT !Int !(Maybe Int) !Int
                deriving (Eq, Ord, Show)

type VertexBuffer   = Array Int (Vertex4   GLfloat)
type TexCoordBuffer = Array Int (TexCoord2 GLfloat)
type NormalBuffer   = Array Int (Normal3   GLfloat)

data Object a = OFace     Material [a]
              | OQuad     Material [a]
              | OTriangle Material [a]
              | OLine              [VDouble]
              | OPoint             [Int]
                  deriving (Show,Eq,Ord)
type FObject = Object NVTriple
type IObject = Object VTriple

instance EqProp ObjModel where
  m =-= m' = property (normalForm m == normalForm m')

normalForm :: ObjModel -> (BufferSet,[FObject])
normalForm (OM (vb,tb,nb) os) =
  let lb = fst . bounds
      ub = snd . bounds
      indicies = (uncurry enumFromTo) . bounds
      mkArray a =
        let b = lb a
        in  array (0,ub a - lb a) [(i - b,a ! i) | i <- indicies a]
      stripMat (OFace     _ bs) = OFace emptyMat bs
      stripMat (OTriangle _ bs) = OFace emptyMat bs
      stripMat (OQuad     _ bs) = OFace emptyMat bs
      stripMat x                = x
  in  ((mkArray vb,mkArray tb,mkArray nb)
      ,sort $ offsetObjects (lb vb) (lb nb) (lb tb) (map stripMat os))

instance Eq ObjModel where
  (OM bs os) == (OM bs' os') = bs == bs' && os == os'

instance Arbitrary ObjModel where
  arbitrary = OM <$> ((>**<) arbitrary arbitrary arbitrary) <*> arbitrary
instance CoArbitrary ObjModel where
  coarbitrary (OM (v,t,n) os) =
    coarbitrary v . coarbitrary t . coarbitrary n . coarbitrary os

instance Arbitrary (FObject) where
  arbitrary = oneof [OFace     <$> (return whiteMat) <*> (nonEmpty arbitrary)
                    ,OTriangle <$> (return whiteMat)
                               <*> (setLength 3 arbitrary)
                    ,OQuad     <$> (return whiteMat)
                               <*> (setLength 4 arbitrary)
                    ,OLine     <$> (nonEmpty arbitrary)
                    ,OPoint    <$> (nonEmpty positive)]
instance CoArbitrary (FObject) where
  coarbitrary (OFace     _ n) = coarbitrary n
  coarbitrary (OTriangle _ n) = coarbitrary n
  coarbitrary (OQuad     _ n) = coarbitrary n
  coarbitrary (OLine       n) = coarbitrary n
  coarbitrary (OPoint      n) = coarbitrary n

instance Arbitrary NVTriple where
  arbitrary = NVT <$> positive <*> maybeGen positive <*> positive
instance CoArbitrary NVTriple where
  coarbitrary (NVT v t n) = coarbitrary v . coarbitrary t . coarbitrary n

instance Renderable ObjModel where
  render (OM bs os) = 
    let (textured,unTextured) = partition isTextured os
        texturedGroups =   groupBy ((==)    `on` texID)
                         . sortBy  (compare `on` texID)
                         $ textured
    in preservingAttrib
         [AllServerAttributes]
         (do colorMaterial $= Just (Front, AmbientAndDiffuse)
             mapM_ (renderOs bs) (unTextured:texturedGroups))

renderOs :: BufferSet -> [FObject] -> IO ()
renderOs bs os =
  case os of
    []    -> return ()
    (o:_) -> do setTexturing o
                renderList bs (filter isTriangle os) Triangles
                renderList bs (filter isQuad     os) Quads
                mapM_ (renderObject bs) (filter isPolygon os)
{-                setColour (Color4 (0.0 :: GLfloat) 1.0 0.0 1.0)
                          (Color4 (0.0 :: GLfloat) 1.0 0.0 1.0)
                          (Color4 (0.0 :: GLfloat) 1.0 0.0 1.0)
                mapM_ (renderONormals bs) os-}

material :: Object a -> Maybe Material
material (OFace     m _) = Just m
material (OTriangle m _) = Just m
material (OQuad     m _) = Just m
material _               = Nothing

points :: Object a -> Maybe [a]
points (OFace     _ ps) = Just ps
points (OTriangle _ ps) = Just ps
points (OQuad     _ ps) = Just ps
points _                = Nothing

faceTexture :: Object a -> Either String TextureObject
faceTexture = maybe (Left "") diffuseTex . material

faceColour :: Object a -> IO ()
faceColour f = 
  either (const (setColour (ambientColour  m)
                           (diffuseColour  m)
                           (specularColour m)))
         (const (setColour (Color4 (1.0 :: GLfloat) 1.0 1.0 1.0)
                           (Color4 (1.0 :: GLfloat) 1.0 1.0 1.0)
                           (Color4 (1.0 :: GLfloat) 1.0 1.0 1.0)))
         (diffuseTex m)
  where
    m = maybe (error "Face has no material") id $ material f

texID :: Object a -> GLuint
texID = either (const 0) unTexObj . faceTexture
        where
          unTexObj (TextureObject x) = x

setTexturing :: Object a -> IO ()
setTexturing o =
  case faceTexture o of
    Left _  ->    texture        Texture2D $= Disabled
    Right t -> do texture        Texture2D $= Enabled
                  textureBinding Texture2D $= Just t

isTextured :: Object a -> Bool
isTextured = either (const False) (const True) . faceTexture

renderList :: BufferSet -> [FObject] -> PrimitiveMode -> IO ()
renderList  bs x f =
  do renderPrimitive f $ forM_ x (renderCalls bs)

renderCalls :: BufferSet -> FObject -> IO ()
renderCalls bs f =
  do faceColour f
     maybe (return ()) (mapM_ (renderOperation3 bs)) $ points f

renderObject :: BufferSet -> FObject -> IO ()
renderObject bs (OFace     _ ps) = 
  renderPrimitive Polygon   $ forM_ ps (renderOperation3 bs)
renderObject bs (OTriangle _ ps) =
  renderPrimitive Triangles $ forM_ ps (renderOperation3 bs)
renderObject bs (OQuad     _ ps) =
  renderPrimitive Quads     $ forM_ ps (renderOperation3 bs)
renderObject bs (OLine       ps) =
  renderPrimitive LineStrip $ forM_ ps (renderOperation2 bs)
renderObject bs (OPoint      ps) =
  renderPrimitive Points    $ forM_ ps (renderOperation1 bs)

renderOperation3 :: BufferSet -> NVTriple -> IO ()
renderOperation3 (vs,ts,ns) (NVT v t n) =
  let nop = normal (ns ! n) >> top
            {-color ((\(Normal3 x y z) -> Color4 (clip x) (clip y) (clip z) 1.0) (ns ! n)) >> top -}
      top = maybe vop ((>> vop) . texCoord . (ts !)) t
      vop = vertex (vs ! v)
  in nop

{-clip :: GLfloat -> GLfloat
clip x = let v = (x + 1.0 / 2)
         in (if v < 0 then 0 else if v > 1 then 1 else v)-}

renderOperation2 :: BufferSet -> VDouble -> IO ()
renderOperation2 (vs,ts,_) (VD v t) =
  let top = maybe vop ((>> vop) . texCoord . (ts !)) t
      vop = vertex (vs ! v)
  in top

renderOperation1 :: BufferSet -> Int -> IO ()
renderOperation1 bs v = renderOperation2 bs (VD v Nothing)

isTriangle :: Object a -> Bool
isTriangle (OTriangle _ _) = True
isTriangle _               = False

isQuad :: Object a -> Bool
isQuad (OQuad _ _ ) = True
isQuad _            = False

isPolygon :: Object a -> Bool
isPolygon (OFace _ _) = True
isPolygon _           = False

setColour :: (ColorComponent a, ColorComponent b, ColorComponent c) =>
             Color4 a -> Color4 b -> Color4 c -> IO ()
setColour _ d _ = color d
{-  do materialAmbient  Front $= a
       materialDiffuse  Front $= d
       materialSpecular Front $= s -}

renderONormals :: BufferSet -> FObject -> IO ()
renderONormals bs o =
  maybe (return ())
        (renderPrimitive Lines . mapM_ (renderNormals bs)) $ points o

renderNormals :: BufferSet -> NVTriple -> IO ()
renderNormals (vs,_,ns) (NVT v _ n) =
  vertex (vs ! v) >> vertex ((vs ! v) ..+^^ (ns ! n))

(..+^^) :: Vertex4 GLfloat -> Normal3 GLfloat -> Vertex4 GLfloat
(..+^^) (Vertex4 x y z w) (Normal3 i j k) = Vertex4 (x+i) (y+j) (z+k) w

geometry :: ObjFile -> MtlFile -> ObjModel
geometry (OF f) mtls =
  OM bs (unsmoothedObjects ++ smoothedObjects)
  where
    vertexBuffer   = listArray (1,length vertexList    ) vertexList
    normalBuffer   = listArray (1,length fullNormalList) fullNormalList
    texCoordBuffer = listArray (1,length texCoordList  ) texCoordList
    vertexList     = map vToVertex    . filter isVertex   $ f
    normalList     = map vnToNormal   . filter isNormal   $ f
    fullNormalList = sNormalList ++ unsmoothedNormals
    sNormalList    = normalList ++ smoothedNormals
    texCoordList   = map vtToTexCoord . filter isTexCoord $ f
    bs             = (vertexBuffer,texCoordBuffer,normalBuffer)

    unsmoothedGroup = maybe [] id (IM.lookup 0 objects)
    (unsmoothedObjects,unsmoothedNormals,numNormals) =
      foldl' (objectNormals (length sNormalList + numNormals + 1) vertexBuffer)
             ([],[],0)
             unsmoothedGroup

    smoothingGroups = IM.elems . IM.filterWithKey (\k _ -> k /= 0) $ objects
    (smoothedObjects,smoothedNormals,numsNormals) =
      foldl' (smoothGroup (length normalList + numsNormals + 1) vertexBuffer)
             ([],[],0)
             smoothingGroups

    objects = fst6 $ foldl' (addObj mtls) (IM.empty,whiteMat,0,1,1,1) f

fst6 :: (a,b,c,d,e,f) -> a
fst6 (x,_,_,_,_,_) = x

addObj :: MtlFile
       -> (IntMap [IObject],Material,Int,Int,Int,Int)
       -> Statement
       -> (IntMap [IObject],Material,Int,Int,Int,Int)
addObj (MF mtls) (os,_,sg,vc,nc,tc) (UseMtl m) =
  maybe (error ("Material not found: " ++ show m))
        (\mtl' -> (os,mtl',sg,vc,nc,tc))
        (M.lookup m mtls)
addObj _   (os,cm,sg,vc,nc,tc) (P ps) =
  (IM.insertWith (++) sg [OPoint (map (mkAbs vc) ps)] os, cm,sg,vc,nc,tc)
addObj _   (os,cm,sg,vc,nc,tc) (L ps) =
  (IM.insertWith (++) sg [OLine (absoluteRefs2 vc tc ps)] os, cm,sg,vc,nc,tc)
addObj _   (os,cm,sg,vc,nc,tc) (F ps) =
  case length ps of
    3 -> (IM.insertWith (++) sg [OTriangle cm (absoluteRefs3 vc nc tc ps)] os
         ,cm,sg,vc,nc,tc)
    4 -> (IM.insertWith (++) sg [OQuad     cm (absoluteRefs3 vc nc tc ps)] os
         ,cm,sg,vc,nc,tc)
    _ -> (IM.insertWith (++) sg [OFace     cm (absoluteRefs3 vc nc tc ps)] os
         ,cm,sg,vc,nc,tc)
addObj _   (os,cm,sg,vc,nc,tc) (V _ _ _ _) = (os,cm,sg,vc+1,nc  ,tc  )
addObj _   (os,cm,sg,vc,nc,tc) (VN _ _ _)  = (os,cm,sg,vc  ,nc+1,tc  )
addObj _   (os,cm,sg,vc,nc,tc) (VT _ _ _)  = (os,cm,sg,vc  ,nc  ,tc+1)
addObj _   (os,cm,_ ,vc,nc,tc) (SG s)      = (os,cm,s ,vc  ,nc  ,tc  )
addObj _   (os,cm,sg,vc,nc,tc) _           = (os,cm,sg,vc  ,nc  ,tc  )

objectNormals :: Int -> VertexBuffer -> ([FObject],[Normal3 GLfloat],Int)
                                     -> IObject
                                     -> ([FObject],[Normal3 GLfloat],Int)
objectNormals loff vb (os,ns,nns) o =
  (go : os, gns ++ ns, nns + vl)
  where
    go = applyNormals normalMap o
    normalMap = IM.fromList $ zip vs [(loff - nns - vl)..]
    gns =   map (normalise . uncurry (flip crossProduct) . createVectors)
          . take vl
          . makeTripples
          . map (vb !)
          . drop (vl - 1)
          . cycle
          $ vs
    vl = length vs
    vs = objVerticies o
    makeTripples (x:y:z:xs) = (x,y,z) : makeTripples (y:z:xs)
    makeTripples _ = error "not enough elements in list for makeTriples."

smoothGroup :: Int -> VertexBuffer -> ([FObject],[Normal3 GLfloat],Int)
                                   -> [IObject]
                                   -> ([FObject],[Normal3 GLfloat],Int)
smoothGroup loff vb (os,ns,nns) g =
  (gos ++ os, gns ++ ns, nns + lgn)
  where
    gos       = map (applyNormals normalMap) g
    normalMap = IM.fromList $ zip vs [(loff - nns - lgn)..]
    gns       = map (makeNormal vb g) vs
    lgn       = length gns
    vs        = concatMap objVerticies g

applyNormals :: IntMap Int -> IObject -> FObject
applyNormals m (OFace     mat vs) = OFace     mat (appNorms m vs)
applyNormals m (OTriangle mat vs) = OTriangle mat (appNorms m vs)
applyNormals m (OQuad     mat vs) = OQuad     mat (appNorms m vs)
applyNormals _ (OLine         vs) = OLine  vs
applyNormals _ (OPoint        vs) = OPoint vs

appNorms :: IntMap Int -> [VTriple] -> [NVTriple]
appNorms m vs =
  map (\(VTr v t n) -> case n of
                         Just n' -> NVT v t n'
                         Nothing -> case v `IM.lookup` m of
                                      Just n' -> NVT v t n'
                                      Nothing -> error "Didn't gen normal")
      vs

makeNormal :: VertexBuffer -> [IObject] -> Int -> Normal3 GLfloat
makeNormal vb os =
  averageVec . map (uncurry (flip crossProduct) . createVectors
                                                . lookupVerticies vb)
             . (findVertexNeighbors os)
  where
    findVertexNeighbors :: [IObject] -> Int -> [(Int,Int,Int)]
    findVertexNeighbors objs v = foldr (findVertexPair v) [] objs
    
    findVertexPair :: Int -> IObject -> [(Int,Int,Int)] -> [(Int,Int,Int)]
    findVertexPair v (OFace     _ vtripples) x = findVP v vtripples x
    findVertexPair v (OTriangle _ vtripples) x = findVP v vtripples x
    findVertexPair v (OQuad     _ vtripples) x = findVP v vtripples x
    findVertexPair _ _                       x = x

    findVP v vtripples ns =
      maybe ns (:ns) (find3 v (map trippleVertex vtripples))
      where
        find3  :: Int -> [Int] -> Maybe (Int,Int,Int)
        find3  x ys = find3' x (length ys) $ cycle ys
        find3' :: Int -> Int -> [Int] -> Maybe (Int,Int,Int)
        find3' _ 0 _  = Nothing
        find3' x r (l:c:n:ys)
          | x == c    = Just (l,c,n)
          | otherwise = find3' x (r-1) (c:n:ys)
        find3' _ _ _  =
          error "find3' called incorrectly.  Input list not infinite."
    
    lookupVerticies :: VertexBuffer
                    -> (Int,Int,Int)
                    -> (Vertex4 GLfloat,Vertex4 GLfloat,Vertex4 GLfloat)
    lookupVerticies buff (a,b,c) = (buff ! a, buff ! b, buff ! c)
    
createVectors :: (Vertex4 GLfloat,Vertex4 GLfloat,Vertex4 GLfloat)
              -> (Normal3 GLfloat,Normal3 GLfloat)
createVectors (a,b,c) = (a .-. b,c .-. b)

crossProduct :: Num a => Normal3 a -> Normal3 a -> Normal3 a
crossProduct (Normal3 x y z) (Normal3 x' y' z') =
  Normal3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

averageVec :: Floating a => [Normal3 a] -> Normal3 a
averageVec [] = error "Average vectors: no vectors to average."
averageVec xs = normalise (   (sumVec (map normalise xs))
                           ^/ (fromIntegral $ length xs))

normalise :: Floating a => Normal3 a -> Normal3 a
normalise x = x ^/ (mag x)

sumVec :: Num a => [Normal3 a] -> Normal3 a
sumVec = foldr1 (^+^)

mag :: Floating a => Normal3 a -> a
mag (Normal3 x y z) = sqrt (x * x + y * y + z * z)

(^+^) :: Num a => Normal3 a -> Normal3 a -> Normal3 a
(^+^) (Normal3 x y z) (Normal3 x' y' z') =
  Normal3 (x + x') (y + y') (z + z')
(.-.) :: Num a => Vertex4 a -> Vertex4 a -> Normal3 a
(.-.) (Vertex4 x y z w) (Vertex4 x' y' z' w') =
  Normal3 (x * w - x' * w') (y * w - y' * w') (z * w - z' * w')

(^/) :: Floating a => Normal3 a -> a -> Normal3 a
v ^/ s = v ^* (1 / s)
(^*) :: Num a => Normal3 a -> a -> Normal3 a
(Normal3 x y z) ^* s = Normal3 (x * s) (y * s) (z * s)

objVerticies :: IObject -> [Int]
objVerticies (OFace     _ vs) = map trippleVertex vs
objVerticies (OTriangle _ vs) = map trippleVertex vs
objVerticies (OQuad     _ vs) = map trippleVertex vs
objVerticies _ = []

mkAbs :: Int -> Int -> Int
mkAbs c x = if x < 0 then c+x else x

absoluteRefs2 :: Int -> Int -> [VDouble] -> [VDouble]
absoluteRefs2 c c' =
  map (\(VD x y) -> VD (mkAbs c x) (mkAbs c' <$> y))

absoluteRefs3 :: Int -> Int -> Int -> [VTriple] -> [VTriple]
absoluteRefs3 c c' c'' =
  map (\(VTr x y z) -> VTr (mkAbs c x) (mkAbs c' <$> y) (mkAbs c'' <$> z))

trippleVertex :: VTriple -> Int
trippleVertex (VTr v _ _) = v

offsetObjects :: Int -> Int -> Int -> [FObject] -> [FObject]
offsetObjects vo no to = map (offsetObj vo no to)
offsetObj :: Int -> Int -> Int -> FObject -> FObject
offsetObj vo to no (OFace    m ts) =
  OFace m     $ map (\(NVT v t n) -> NVT (v-vo)
                                         (t >>= return . ((flip (-)) to))
                                         (n-no)) ts
offsetObj vo no to (OTriangle m ts) =
  OTriangle m $ map (\(NVT v t n) -> NVT (v-vo)
                                         (t >>= return . ((flip (-)) to))
                                         (n-no)) ts
offsetObj vo no to (OQuad     m ts) =
  OQuad m     $ map (\(NVT v t n) -> NVT (v-vo)
                                         (t >>= return . ((flip (-)) to))
                                         (n-no)) ts
offsetObj vo _  to (OLine       ts) =
  OLine $ map (\(VD v t) -> VD (v-vo) (t >>= return . ((flip (-)) to))) ts
offsetObj vo _  _  (OPoint      ts) =
  OPoint $ map ((flip (-)) vo) ts

objFile :: ObjModel -> ObjFile
objFile (OM (vb,tb,nb) os) =
  OF $ concat [writeBuffer vertexToV    vb
              ,writeBuffer texCoordToVT tb
              ,writeBuffer normalToVN   nb
              ,map writeObject (offsetObjects (-1) (-1) (-1) os)]
  where
    writeBuffer :: (a -> Statement) -> Array Int a -> [Statement]
    writeBuffer f = map f . elems
    writeObject :: FObject -> Statement
    writeObject (OFace     _ fs) = F (map nvToVTriple fs)
    writeObject (OTriangle _ fs) = F (map nvToVTriple fs)
    writeObject (OQuad     _ fs) = F (map nvToVTriple fs)
    writeObject (OLine ls)       = L ls
    writeObject (OPoint ps)      = P ps
    
    nvToVTriple (NVT v t n) = VTr v t (Just n)

vnToNormal :: Statement -> Normal3 GLfloat
vnToNormal (VN i j k)   = Normal3 i j k
vnToNormal _            = error "Obj statement was not a normal."

vtToTexCoord :: Statement -> TexCoord2 GLfloat
vtToTexCoord (VT u v _) = TexCoord2 u v
vtToTexCoord _          = error "Obj statement was not a texture coordinate."

vToVertex :: Statement -> Vertex4 GLfloat
vToVertex (V x y z w)   = Vertex4 x y z w
vToVertex _             = error "Obj statement was not a vertex."

vertexToV :: Vertex4 GLfloat -> Statement
vertexToV (Vertex4 x y z w)  = V x y z w

normalToVN :: Normal3 GLfloat -> Statement
normalToVN (Normal3 i j k)   = VN i j k

texCoordToVT :: TexCoord2 GLfloat -> Statement
texCoordToVT (TexCoord2 u v) = VT u v 0.0

prop_geomUnGeom :: ObjModel -> Property
prop_geomUnGeom x =
  ((((flip geometry) (MF M.empty)) . objFile $ x) =-= x)

geometryTests :: IO ()
geometryTests = do putStr "prop_geomUnGeom:   "
                   quickCheck prop_geomUnGeom

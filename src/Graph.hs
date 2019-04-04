{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module Graph where

import           Data.Data
import           Data.Graph
import           Data.Vector                 as V
import           GHC.Generics

import           VectorSpace

import           Control.Parallel.Strategies

newtype Vec2 = Vec2 (Float, Float) deriving (Typeable, Show, Generic, Data)
instance VectorSpace Vec2 where
  type Scalar Vec2 = Float

  (Vec2 (x, y)) *- s = Vec2 (x*s, y*s)
  s -* (Vec2 (x, y)) = Vec2 (x*s, y*s)

  (Vec2 (x, y)) /- s = Vec2 (x/s, y/s)
  s -/ (Vec2 (x, y)) = Vec2 (x/s, y/s)

  (Vec2 (x,y)) <+> (Vec2 (x',y')) = Vec2 (x+x',y+y')
  (Vec2 (x,y)) <-> (Vec2 (x',y')) = Vec2 (x-x',y-y')
  (Vec2 (x,y)) <.> (Vec2 (x',y')) = x*x' + y*y'

data GNode = GNode { pos :: Vec2, vel :: Vec2 } deriving (Show)
type GraphState = (Graph, Vector GNode)

updateGraphState dt (g, vec) = (g, vec')
  where
    last = V.length vec - 1
    vec' = V.zipWith update (fromList [0..last]) vec `using` parTraversable rseq
    update i = updatePos i . updateVel i

    norme v = sqrt $ norme2 v
    norme2 v = v <.> v
    normalize v = v /- norme v
    diffdecay2 u v = let uv = u <-> v in normalize uv /- norme2 uv

    replC = 10
    attrC :: Float
    attrC = 0.001

    repulsion i up =
      Prelude.foldl (<+>) (Vec2 (0,0)) [ diffdecay2 up (pos (vec ! iv)) | iv <- [0..last], iv /= i ]
    attraction i up =
      Prelude.foldl (<+>) (Vec2 (0,0)) [ spring up (pos (vec ! iv)) | (i', iv) <- edges g, i' == i ]
      where spring u v = let vec = u <-> v; d = norme vec in log (d / 100) -* normalize vec

    totalForces i v = (replC -* repulsion i v) <-> (attrC -* attraction i v)

    frottement v@(Vec2 (x, y)) = let d = norme2 v in
      if d > coef * coef then Vec2 (x - signum x * coef, y - signum y * coef)
      else Vec2 (0, 0)
      where coef = 0.0001

    updateVel i v = v { vel = frottement $ vel v <+> totalForces i (pos v) }
    updatePos i v = v { pos = pos v <+> (vel v *- dt) }

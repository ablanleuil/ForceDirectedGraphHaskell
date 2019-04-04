{-# LANGUAGE TypeFamilies #-}

module VectorSpace where

class VectorSpace v where
  type Scalar v :: *

  (*-) :: v -> Scalar v -> v
  (-*) :: Scalar v -> v -> v

  (/-) :: v -> Scalar v -> v
  (-/) :: Scalar v -> v -> v

  (<+>) :: v -> v -> v
  (<->) :: v -> v -> v
  (<.>) :: v -> v -> Scalar v

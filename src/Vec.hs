{-# LANGUAGE DeriveFunctor #-}

module Vec
  ( Vec (Vec),
    zero,
    one,
    from,
    vec,
    getX,
    getY,
    getZ,
    dot,
    cross,
    unit,
    (.+.),
    (.-.),
    (.*.),
    (./.),
    Point3,
    Vec3,
    len,
    lenSquared,
  )
where

data Vec a = Vec
  { getX :: !a,
    getY :: !a,
    getZ :: !a
  }
  deriving (Show, Functor)

instance Num a => Num (Vec a) where
  {-# INLINE (+) #-}
  Vec x1 y1 z1 + Vec x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)
  {-# INLINE (-) #-}
  Vec x1 y1 z1 - Vec x2 y2 z2 = Vec (x1 - x2) (y1 - y2) (z1 - z2)
  {-# INLINE (*) #-}
  Vec x1 y1 z1 * Vec x2 y2 z2 = Vec (x1 * x2) (y1 * y2) (z1 * z2)
  abs v = fmap abs v
  signum v = fmap signum v
  fromInteger n = from (fromInteger n)

type Vec3 = Vec Double

type Point3 = Vec Double

vec :: Num a => a -> a -> a -> Vec a
vec = Vec

from :: Num a => a -> Vec a
from n = vec n n n

zero :: Num a => Vec a
zero = from 0

one :: Num a => Vec a
one = from 1

{-# INLINE dot #-}
dot :: Num a => Vec a -> Vec a -> a
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

{-# INLINE cross #-}
cross :: Num a => Vec a -> Vec a -> Vec a
cross (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec x y z
  where
    x = y1 * z2 - z1 * y2
    y = z1 * x2 - x1 * z2
    z = x1 * y2 - y1 * x2

{-# INLINE (.+.) #-}
(.+.) :: (Num a) => Vec a -> a -> Vec a
(.+.) v x = fmap (+ x) v

{-# INLINE (.-.) #-}
(.-.) :: (Num a) => Vec a -> a -> Vec a
(.-.) v x = fmap (`subtract` x) v

{-# INLINE (.*.) #-}
(.*.) :: (Num a) => Vec a -> a -> Vec a
(.*.) v x = fmap (* x) v

{-# INLINE (./.) #-}
(./.) :: (Fractional a) => Vec a -> a -> Vec a
(./.) v x = fmap (/ x) v

{-# INLINE lenSquared #-}
lenSquared :: Num a => Vec a -> a
lenSquared v = dot v v

{-# INLINE len #-}
len :: Vec Double -> Double
len = sqrt . lenSquared

{-# INLINE unit #-}
unit :: Vec Double -> Vec Double
unit v = v ./. len v

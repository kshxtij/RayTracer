module Scenes (genRandomScene) where

import Colors (toColor)
import Hittable
  ( Checkered (Checkered),
    Dielectric (Dielectric),
    HittableList,
    Lambertian (Lambertian),
    Material (Material),
    Metal (Metal),
    Sphere (Sphere),
    toHittableList,
  )
import Samplings (sampleFraction, sampleFraction3, samplePoint2)
import System.Random (mkStdGen)
import Vec (len, vec)

genRandomScene :: Int -> HittableList Sphere
genRandomScene seed = toHittableList allObjects
  where
    allObjects = [groundObject] ++ randomObjects ++ fixedObjects
    groundObject = Sphere (vec 0 (-1000) 0) 1000 (Material (Checkered (toColor $ vec 0 0 0) (toColor $ vec 1 1 1)))
    randomObjects = fst $ foldl mkObject ([], mkStdGen seed) $ (,) <$> [-11 .. 11] <*> [-11 .. 11]
    mkObject (objects, g) (a, b) = (maybe objects (: objects) object, g2)
      where
        ((chooseMat, x, z), g1) = sampleFraction3 g
        center = vec (a + 0.9 * x) 0.2 (b + 0.9 * z)
        (object, g2) = if len (center - vec 4 0.2 0) > 0.9 then createRandomObject g1 else (Nothing, g1)
        createRandomObject g3 = (Just obj, g5)
          where
            ((p1, p2), g4) = samplePoint2 g3
            (d1, g5) = sampleFraction g4
            mat
              | chooseMat < 0.8 = Material $ Lambertian (toColor $ p1 * p2)
              | chooseMat < 0.95 = Material $ Metal ((\i -> 0.5 + 0.5 * i) <$> toColor p1) (0.5 * d1)
              | otherwise = Material $ Dielectric 1.5
            obj = Sphere center 0.2 mat
    fixedObjects =
      [ Sphere (vec 0 1 0) 1.0 (Material (Dielectric 1.5)),
        Sphere (vec (-4) 1 0) 1.0 (Material (Lambertian (toColor $ vec 0.4 0.2 0.1))),
        Sphere (vec 4 1 0) 1.0 (Material (Metal (toColor $ vec 0.7 0.6 0.5) 0.0))
      ]

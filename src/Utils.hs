module Utils
  where
import Linear
import Graphics.Rendering.OpenGL.GL

toV3 :: Floating a => (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV3s :: Floating a => ((a, a, a),(a, a, a),(a, a, a)) -> (V3 a, V3 a, V3 a)
toV3s (a1, a2, a3) = (toV3 a1, toV3 a2, toV3 a3)

scaleV3s :: Floating a => a -> (V3 a, V3 a, V3 a) -> (V3 a, V3 a, V3 a)
scaleV3s mu (a1, a2, a3) = (mu *^ a1, mu *^ a2, mu *^ a3)

toVx3 :: Floating a => V3 a -> Vertex3 a
toVx3 (V3 x y z) = Vertex3 x y z

--toVx3s :: Floating a => (V3 a, V3 a, V3 a) -> (Vertex3 a, Vertex3 a, Vertex3 a)
--toVx3s (a1, a2, a3) = (toVx3 a1, toVx3 a2, toVx3 a3)

triangleNormal :: Floating a => (V3 a, V3 a, V3 a) -> Normal3 a
triangleNormal (a1, a2, a3) =
  Normal3 x y z
  where
    V3 x y z = signorm $ cross (a2 ^-^ a1) (a3 ^-^ a1)

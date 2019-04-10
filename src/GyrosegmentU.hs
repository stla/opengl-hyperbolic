module GyrosegmentU
  ( gyrosegment )
  where
import Linear

betaF :: Floating a => a -> V3 a -> a
betaF s v = 1 / sqrt(1 + quadrance v / s)

gyroadd :: Floating a => a -> V3 a -> V3 a -> V3 a
gyroadd s x y =
  (1 + bX/(1+bX) * dot x y / s / s + (1-bY)/bY) *^ x ^+^ y
  where
    bX = betaF s x
    bY = betaF s y

gyroscalar :: Floating a => a -> a -> V3 a -> V3 a
gyroscalar s r x = (s/xnorm * sinh(r*asinh(xnorm/s))) *^ x
  where
    xnorm = norm x

gyrovector :: Floating a => a -> V3 a -> V3 a -> V3 a
gyrovector s x y = gyroadd s (negated x) y

gyroABt :: Floating a => a -> V3 a -> V3 a -> a -> V3 a
gyroABt s a b t = gyroadd s a (gyroscalar s t (gyrovector s a b))

gyrosegment :: Floating a => a -> V3 a -> V3 a -> Int -> [V3 a]
gyrosegment s a b n = map (gyroABt s a b) ts
  where
    n' = realToFrac n
    ts = [realToFrac i / n' | i <- [0 .. n]]

module GyroU
  ( gyrosubdiv )
  where
import Linear

gammaF :: Floating a => a -> V3 a -> a
gammaF s v = 1 / sqrt(1 - quadrance v / s)

betaF :: Floating a => a -> V3 a -> a
betaF s v = 1 / sqrt(1 + quadrance v / s)

phiEU :: Floating a => a -> V3 a -> V3 a
phiEU s v = gammaF s v *^ v

phiUE :: Floating a => a -> V3 a -> V3 a
phiUE s v = betaF s v *^ v

gyromidpointE :: Floating a => a -> V3 a -> V3 a -> V3 a
gyromidpointE s va vb =
  ((ga *^ va) ^+^ (gb *^ vb)) ^/ (ga + gb)
  where
    ga = gammaF s va
    gb = gammaF s vb

gyromidpointU :: Floating a => a -> V3 a -> V3 a -> V3 a
gyromidpointU s va vb = phiEU s (gyromidpointE s (phiUE s va) (phiUE s vb))

gyrosubdiv0 :: Floating a => a -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv0 s (a1, a2, a3) =
  [(a1, m12, m13), (a2, m23, m12), (a3, m13, m23), (m12, m23, m13)]
  where
    m12 = gyromidpointU s a1 a2
    m13 = gyromidpointU s a1 a3
    m23 = gyromidpointU s a2 a3

gyrosubdiv :: Floating a => Int -> a -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv depth s (a1, a2, a3) = go depth [(a1, a2, a3)]
  where
    go n list | n == 0 = list
              | otherwise = concatMap (gyrosubdiv0 s) (go (n-1) list)

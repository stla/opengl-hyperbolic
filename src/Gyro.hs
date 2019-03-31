module Gyro
  ( gyrosubdiv )
  where
import Linear

gamma2 :: Floating a => V3 a -> a
gamma2 v = 1 / (1 - quadrance v)

scalarM :: Floating a => a -> V3 a -> V3 a
scalarM r v = tanh(r * atanh(norm v)) *^ signorm v

gyromidpoint :: Floating a => V3 a -> V3 a -> V3 a
gyromidpoint va vb =
  scalarM 0.5 (((ga2 *^ va) ^+^ (gb2 *^ vb)) ^/ (ga2 + gb2 - 1))
  where
    ga2 = gamma2 va
    gb2 = gamma2 vb

gyrocentroid :: Floating a => V3 a -> V3 a -> V3 a -> V3 a
gyrocentroid va vb vc =
  scalarM 0.5 (((ga2 *^ va) ^+^ (gb2 *^ vb) ^+^ (gc2 *^ vc)) ^/ (ga2 + gb2 + gc2 - 1.5))
  where
    ga2 = gamma2 va
    gb2 = gamma2 vb
    gc2 = gamma2 vc

gyrosubdiv0 :: Floating a => (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv0 (a1, a2, a3) =
  [(g, a1, m12), (g, m12, a2), (g, a2, m23),
   (g, m23, a3), (g, a3, m13), (g, m13, a1)]
  where
    m12 = gyromidpoint a1 a2
    m13 = gyromidpoint a1 a3
    m23 = gyromidpoint a2 a3
    g = gyrocentroid a1 a2 a3

gyrosubdiv :: Floating a => Int -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv depth (a1, a2, a3) = go depth [(a1, a2, a3)]
  where
    go n list | n == 0 = list
              | otherwise = concatMap gyrosubdiv0 (go (n-1) list)

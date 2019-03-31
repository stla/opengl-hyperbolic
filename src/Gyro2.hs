module Gyro2
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

gyrosubdiv0 :: Floating a => (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv0 (a1, a2, a3) =
  [(a1, m12, m13), (a2, m23, m12), (a3, m13, m23), (m12, m23, m13)]
  where
    m12 = gyromidpoint a1 a2
    m13 = gyromidpoint a1 a3
    m23 = gyromidpoint a2 a3

gyrosubdiv :: Floating a => Int -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
gyrosubdiv depth (a1, a2, a3) = go depth [(a1, a2, a3)]
  where
    go n list | n == 0 = list
              | otherwise = concatMap gyrosubdiv0 (go (n-1) list)

module Icosidodecahedron where

import Gyro2
import Utils
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.IORef
import Graphics.Rendering.OpenGL.Capture (capturePPM)
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Linear hiding (rotate, perspective, lookAt)
import System.Directory (doesDirectoryExist)
import System.IO.Unsafe
import Text.Printf

white, black, red, blue, grey :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
red = Color4 1 0 0 1
blue = Color4 0 0 1 1
grey = Color4 0.35 0.35 0.35 1

type Point = (GLdouble, GLdouble, GLdouble)

phi :: GLdouble
phi = (1 + sqrt 5) / 2

points :: [Point]
points = [
    (0,0,2*phi),
    (0,2*phi,0),
    (2*phi,0,0),
    (0,0,-2*phi),
    (0,-2*phi,0),
    (-2*phi,0,0),
    (1,phi,phi2),
    (1,phi,-phi2),
    (1,-phi,phi2),
    (-1,phi,phi2),
    (1,-phi,-phi2),
    (-1,phi,-phi2),
    (-1,-phi,phi2),
    (-1,-phi,-phi2),
    (phi,phi2,1),
    (phi,-phi2,1),
    (-phi,phi2,1),
    (phi,phi2,-1),
    (-phi,-phi2,1),
    (phi,-phi2,-1),
    (-phi,phi2,-1),
    (-phi,-phi2,-1),
    (phi2,1,phi),
    (-phi2,1,phi),
    (phi2,1,-phi),
    (phi2,-1,phi),
    (-phi2,1,-phi),
    (-phi2,-1,phi),
    (phi2,-1,-phi),
    (-phi2,-1,-phi)
  ]
  where
    phi2 = phi*phi

faces :: [[Int]]
faces =
  [
    [6, 9, 0],
    [3, 11, 7],
    [16, 23, 9],
    [22, 14, 6],
    [12, 8, 0],
    [3, 10, 13],
    [20, 16, 1],
    [11, 26, 20],
    [19, 15, 4],
    [8, 15, 25],
    [2, 22, 25],
    [1, 14, 17],
    [17, 24, 7],
    [28, 24, 2],
    [28, 19, 10],
    [4, 18, 21],
    [27, 23, 5],
    [27, 18, 12],
    [5, 26, 29],
    [13, 21, 29],
    [9, 6, 14],
    [14, 16, 9],
    [1, 16, 14],
    [5, 23, 20],
    [20, 26, 5],
    [23, 16, 20],
    [0, 8, 25],
    [25, 22, 6],
    [25, 6, 0],
    [18, 15, 8],
    [4, 15, 18],
    [8, 12, 18],
    [17, 22, 2],
    [14, 22, 17],
    [2, 24, 17],
    [11, 20, 17],
    [17, 7, 11],
    [17, 20, 1],
    [28, 10, 3],
    [3, 7, 28],
    [7, 24, 28],
    [28, 25, 15],
    [15, 19, 28],
    [2, 25, 28],
    [21, 13, 10],
    [10, 19, 21],
    [21, 19, 4],
    [9, 23, 27],
    [0, 9, 27],
    [27, 12, 0],
    [29, 26, 11],
    [29, 11, 3],
    [3, 13, 29],
    [18, 27, 29],
    [29, 21, 18],
    [29, 27, 5]
  ]

triangles1 :: [(Point, Point, Point)]
triangles1 = map getPoints faces
  where
    getPoints face =
      (points !! (face !! 0), points !! (face !! 1), points !! (face !! 2))

triangles :: [(V3 GLdouble, V3 GLdouble, V3 GLdouble)]
triangles = map (scaleV3s (0.95/2/phi) . toV3s) triangles1

meshes :: [[(V3 GLdouble, V3 GLdouble, V3 GLdouble)]]
meshes = map (gyrosubdiv 5) triangles

data Context = Context
  { contextRot1 :: IORef GLfloat
  , contextRot2 :: IORef GLfloat
  , contextRot3 :: IORef GLfloat
  , contextZoom :: IORef Double
  }

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  zoom <- get (contextZoom context)
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (\mesh -> renderPrimitive Triangles $ do
    materialDiffuse Front $= blue
    mapM_ drawTriangle mesh) meshes
  swapBuffers
  where
    drawTriangle (v1,v2,v3) = do
      normal $ triangleNormal (v1, v2, v3)
      vertex $ toVx3 v1
      vertex $ toVx3 v2
      vertex $ toVx3 v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w' / h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard ::
     IORef GLfloat
  -> IORef GLfloat
  -> IORef GLfloat -- rotations
  -> IORef GLdouble -- zoom
  -> IORef Bool -- animation
  -> IORef Bool -- save
  -> IORef Int -- delay
  -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim save delay c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.25)
    'l' -> zoom $~! subtract 0.25
    'q' -> leaveMainLoop
    'a' -> anim $~! not
    's' -> save $~! not
    'o' -> delay $~! (+5000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-5000)
    _ -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle ::
     IORef Bool
  -> IORef Bool
  -> IORef Int
  -> IORef Int
  -> IORef GLfloat
  -> IdleCallback
idle anim save delay snapshot rot2 = do
  a <- get anim
  ss <- get snapshot
  s <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && ss < 180) $ do
      let ppm = printf "ppm/pic%04d.ppm" ss
      (>>=) capturePPM (B.writeFile ppm)
      print ss
      snapshot $~! (+ 1)
    rot2 $~! (+ 2)
    _ <- threadDelay d
    postRedisplay Nothing
  return ()


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hyperbolic dodecahedron"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialDiffuse Front $= white
  materialEmission Front $= Color4 0 0 0 0
  materialSpecular Front $= white
  materialShininess Front $= 10
  lighting $= Enabled
  lightModelAmbient $= grey
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-500) 1
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 5000
  displayCallback $=
    display
      Context
      { contextRot1 = rot1
      , contextRot2 = rot2
      , contextRot3 = rot3
      , contextZoom = zoom
      }
  reshapeCallback $= Just (resize 0)
  keyboardCallback $=
    Just (keyboard rot1 rot2 rot3 zoom anim save delay)
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot rot2)
  putStrLn
    "*** Hyperbolic dodecahedron ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop

module Dodecahedron where

import Gyro
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

points :: [Point]
points = [
    ( a,  a,  a),
    ( a,  a, -a),
    ( a, -a,  a),
    (-a, -a,  a),
    (-a,  a, -a),
    (-a,  a,  a),
    ( 0,  b, -c),
    ( 0, -b, -c),
    ( 0, -b,  c),
    ( c,  0, -b),
    (-c,  0, -b),
    (-c,  0,  b),
    ( b,  c,  0),
    ( b, -c,  0),
    (-b, -c,  0),
    (-b,  c,  0),
    ( 0,  b,  c),
    ( a, -a, -a),
    ( c,  0,  b),
    (-a, -a, -a)
  ]
  where
    phi = (1+ sqrt 5)/2
    a = 1 / sqrt 3
    b = a / phi
    c = a * phi

faces :: [[Int]]
faces =
  [
   [13, 2, 3],
   [3, 14, 13],
   [3, 2, 8],
   [17, 1, 9],
   [6, 1, 17],
   [17, 7, 6],
   [6, 7, 19],
   [19, 4, 6],
   [10, 4, 19],
   [13, 14, 19],
   [19, 17, 13],
   [7, 17, 19],
   [15, 1, 6],
   [12, 1, 15],
   [6, 4, 15],
   [15, 0, 12],
   [15, 16, 0],
   [5, 16, 15],
   [18, 1, 12],
   [12, 0, 18],
   [9, 1, 18],
   [0, 16, 18],
   [8, 2, 18],
   [18, 16, 8],
   [18, 2, 13],
   [18, 17, 9],
   [13, 17, 18],
   [8, 16, 11],
   [11, 16, 5],
   [11, 3, 8],
   [14, 3, 11],
   [11, 19, 14],
   [10, 19, 11],
   [11, 4, 10],
   [5, 15, 11],
   [11, 15, 4]
  ]

triangles1 :: [(Point, Point, Point)]
triangles1 = map getPoints faces
  where
    getPoints face =
      (points !! (face !! 0), points !! (face !! 1), points !! (face !! 2))

triangles :: [(V3 GLdouble, V3 GLdouble, V3 GLdouble)]
triangles = map (scaleV3s 0.9 . toV3s) triangles1

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

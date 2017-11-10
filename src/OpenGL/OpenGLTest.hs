module OpenGL.OpenGLTest where

import Graphics.UI.GLUT

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

displayc :: DisplayCallback
displayc = do
  clear [ColorBuffer]
  let c = Cube 1.0
  renderObject Solid c
  renderPrimitive LineStrip $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

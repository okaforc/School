module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rect, ellipse, polygon, imageMask,
  identity, translate, rotate, scale, shear, (<+>), (<++>),
  -- inside)  where
  insideColour, deg2Rad)  where
import Codec.Picture

-- import Data.Geometry.Polygon

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
-- cross (Vector a b) (Vector a' b') = a * a' + b * b'
cross (Vector a b) (Vector a' b') = a * b' - b * a'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

deg2Rad :: Double -> Double
deg2Rad deg = deg * (pi/180)

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector
type Edge = (Point, Point)
type Colour = (Pixel8, Pixel8, Pixel8)
type ColourShape = (Shape, Colour)

point :: Double -> Double -> Point
point = vector

edge :: Point -> Point -> Edge
edge p1 p2 = (p1, p2)


data Shape = Empty
           | Circle
           | Square
           | Polygon [Point]
           | ImageMask  (Transform, ColourShape) (Transform, ColourShape)
             deriving Show

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square
imageMask = ImageMask

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Shear Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
shear = Shear
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1
t0 <++> (t1, s1) = (Compose t0 t1, s1)


transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Shear (Vector tx ty)) (Vector px py) = Vector (px - (py * tx)) (py - (px * ty))
transform (Rotate m)                 p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p
-- transform (Compose2 t1 [(t2, s2):ss])            p = transform t2 $ transform t1 p


-- Drawings

-- type Drawing = [(Transform,Shape)]

-- -- interpretation function for drawings

-- inside :: Point -> Drawing -> Bool
-- inside p d = any (inside1 p) d

-- inside1 :: Point -> (Transform, Shape) -> Bool
-- inside1 p (t,s) = insides (transform t p) s


-- type Drawing = [(Transform,Shape)]
type Drawing = [(Transform, ColourShape)]

-- interpretation function for drawings

-- inside :: Point -> Drawing -> Bool
-- inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, ColourShape) -> Colour
inside1 p (t,(s,c)) = if transform t p `insides` s then c else (0,0,0)


insideColour :: Point -> Drawing -> Colour
insideColour p d = firstColour $ map (inside1 p) d -- head $ map (approxinside1 p) d 
                   where firstColour :: [Colour] -> Colour
                         firstColour [] = (0, 0, 0)
                         firstColour [x]      = x -- Down to the last shape? Use it's colour
                         firstColour ((0, 0, 0):xs) = firstColour xs -- skip any 100's unless we're at the end
                         firstColour (x:_)    = x -- if you find an "inside" 

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` Polygon ps = pointInPoly ps p
p `insides` (ImageMask (t1,(s1,_)) (t2,(s2,_))) = (transform t1 p `insides` s1) && (transform t2 p `insides` s2)

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 20), circle)


rect :: Double -> Double -> Colour -> (Transform, ColourShape)
rect w h c = (Scale (Vector w h), (square, c))

-- rect :: Double -> Double -> (Transform, Shape)
-- rect w h = (Scale (Vector w h), square)

ellipse :: Double -> Double -> Colour -> (Transform, ColourShape)
ellipse w h c = (Scale (Vector w h), (circle, c))

polygon :: [Point] -> Shape
polygon = Polygon
-- polygon [point 0 1, point 0 2, point 1 1, point 1 2] = Polygon

-- crossProduct :: Vector -> Vector -> Int
-- crossProduct (x1,y1) (x2,y2) = (x1*y2) - (y1*x2)





-- The following code has been taken from https://gist.github.com/lojic/6734952 and modified:
-- Transform a list of points into a list of edges
polyEdges :: [Point] -> [ Edge ]
polyEdges (p1:p2:ps) = (p1,p2) : polyEdges (p2:ps)
polyEdges _ = []

-- Compute the two vectors on which we'll compute the cross product              
vectors :: (Point, Point) -> Point -> (Point, Point)
vectors (a, b) c = (point (getX b - getX a) (getY b - getY a), point (getX c - getX a) (getY c - getY a))

-- Indicate whether the point is within the convex polygon
pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly p = all (<= 0) normals
  where normals = map (\ edge -> uncurry cross (vectors edge p)) (polyEdges poly)

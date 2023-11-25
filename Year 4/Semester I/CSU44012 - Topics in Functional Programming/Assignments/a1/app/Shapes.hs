{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rect, ellipse, polygon, imageMask,
  identity, translate, rotate, scale, shear, (<+>), (<++>),
  -- inside)  where
  colourAtPoint, deg2Rad)  where
import Codec.Picture

-- import Data.Geometry.Polygon

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

-- perp dot product. also the cross product where both z components are 0
cross :: Vector -> Vector -> Double
-- cross (Vector a b) (Vector a' b') = a * a' + b * b'
cross (Vector x y) (Vector x' y') = x * y' - y * x'

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

($+) :: Point -> Point -> Point
(Vector ax ay) $+ (Vector bx by) = point (ax+bx) (ay+by)

($$+) :: Matrix -> Matrix -> Matrix
(Matrix (Vector a1 b1) (Vector c1 d1)) $$+ (Matrix (Vector a2 b2) (Vector c2 d2)) = Matrix (Vector (a1+a2) (b1+b2)) (Vector (c1+c2) (d1+d2))


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

imageMask :: (Transform, ColourShape) -> (Transform, ColourShape) -> Shape
imageMask = ImageMask

polygon :: [Point] -> Shape
polygon = Polygon

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

-- optimisations: combine adjacent transforms of matching types
transform (Compose Identity Identity)            p = p
transform (Compose (Translate t1) (Translate t2))            p = transform (Translate (t1 $+ t2)) p
transform (Compose (Scale t1) (Scale t2))            p = transform (Scale (t1 $+ t2)) p
transform (Compose (Shear t1) (Shear t2))            p = transform (Shear (t1 $+ t2)) p
transform (Compose (Rotate m1) (Rotate m2))            p = transform (Rotate (m1 $$+ m2)) p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- transform (Compose2 t1 [(t2, s2):ss])            p = transform t2 $ transform t1 p


type Drawing = [(Transform, ColourShape)]

-- interpretation function for drawings

-- inside :: Point -> Drawing -> Bool
-- inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, ColourShape) -> Colour
inside1 p (t,(s,c)) = if transform t p `insides` s
    then c
    else (0, 0, 0) -- (0, 0, 0) does not appear in the drawing. black would be (1, 1, 1) or something smaller

-- Given a point and a drawing, determine the color at that point. (modified from mandlebrot 3 and changed the name for *aesthetic* reasons)
colourAtPoint :: Point -> Drawing -> Colour
colourAtPoint p d = c $ map (inside1 p) d
                   where c :: [Colour] -> Colour
                         c [] = (0, 0, 0) -- blank if no shape
                         c [x] = x -- use the only colour available
                         c ((0, 0, 0):xs) = c xs -- recurse through all colours in the drawing, ignoring blanks
                         c (x:_) = x -- return the first non-blank colour inside the shape

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

ellipse :: Double -> Double -> Colour -> (Transform, ColourShape)
ellipse w h c = (Scale (Vector w h), (circle, c))




-- The following code has been taken from Brian Adkin (https://gist.github.com/lojic/6734952) and modified:
-- Transform a list of points into a list of edges
polyEdges :: [Point] -> [ Edge ]
polyEdges (p1:p2:ps) = (p1,p2) : polyEdges (p2:ps)
polyEdges _ = []

-- Setup the two points on which we'll compute the cross product
-- ((x1, y1), (x2, y2)), (x, y) => (x2 - x1, y2 - y1), (x - x1, y - y1)
-- cross => (x2 - x1) * (y - y1) - (y2 - y1) * (x - x1)
crossSetup :: (Point, Point) -> Point -> (Point, Point)
crossSetup (a, b) c = (point (getX b - getX a) (getY b - getY a), point (getX c - getX a) (getY c - getY a))

-- Indicate whether the point is within the convex polygon
-- point_in_poly = true
-- for each edge (\edge) in poly (polyEdges poly -> [Edges])
--    normal = cross product (cross) on the two points forming edge (crossSetup edge p)
--    point_in_poly &&= normal <= 0 (clockwise)
-- return point_in_poly
pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly p = all (<= 0) normals
  where normals = map (\edge -> uncurry cross (crossSetup edge p)) (polyEdges poly)

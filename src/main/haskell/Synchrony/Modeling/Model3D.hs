module Synchrony.Modeling.Model3D
  ( Angle(..)
  , Angle3D(..)
  , Color
  , Length(..)
  , Method(..)
  , Model(..)
  , Polygon3D
  , Units(..)
  , Vector2D(..)
  , Vector3D(..)
  , (@+)
  , (@-)
  , (@*)
  , (@/)
  , (@%)
  , absolute
  , angleDegrees
  , angleValue
  , backward
  , calipers
  , color
  , down
  , forward
  , guess
  , laser
  , left
  , lengthValue
  , micrometer
  , minus90
  , neg
  , opaqueRed
  , opaqueOrange
  , opaqueYellow
  , opaqueGreen
  , opaqueBlue
  , opaquePurple
  , opaquePink
  , opaqueWhite
  , opaqueGray
  , opaqueBlack
  , opaqueBrown
  , clearBlue
  , person
  , plane
  , plus180
  , plus90
  , polygonPrism
  , polyPrism
  , right
  , rotate
  , rotateX
  , rotateY
  , rotateZ
  , ruler
  , shrink
  , smidge -- TODO: don't expose
  , smidge2
  , smidgeBigger
  , smidgeNegative
  , sphere
  , square
  , squareRoot
  , tape
  , toOpenSCAD
  , translate
  , unspecified
  , up
  , zero
  , zeroAngle
  ) where

import qualified Data.Colour             as DC
import qualified Data.List               as L
import qualified Graphics.OpenSCAD       as OS
import qualified Data.Colour.SRGB.Linear as RGB
--import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Fixed as DF


-- | An absolute angle in radians
data Angle = Angle Double deriving Show

data Angle3D = Angle3D Angle Angle Angle deriving Show

type Color = DC.AlphaColour Double

data Length = Absolute Double
            | DividedBy Length Length
            | Max Length Length
            | Measurement Double Method
            | Min Length Length
            | Minus Length Length
            | Modulo Length Length
            | Plus Length Length
            | SquareRoot Length
            | Times Length Length
            deriving Show

data Method
  = Ruler -- A ruler with 1 millimeter increments. Measurements may be much less accurate due to "eyeballing".
  | Laser -- A laser level in combination with a tape measure. To compose two laser lengths, they must share the same laser reference.
  | Method String -- Another method (with a descriptive string)
  | Micrometer -- A US micrometer with an accuracy of >= 0.01 inches (0.254 millimeters)
  | MillimeterCalipers -- Calipers with an accuracy of >= 1 millimeter
  | Tape -- A measuring tape with 1 millimeter increments. Measurements are typically much less accurate than 1mm due to "eyeballing" as well as bending of the tape.
  | Unspecified deriving Show

data Model
  = Box {
      boxX :: Length,
      boxY :: Length,
      boxZ :: Length }
  | Color Color Model
  | Cube Length
  | Cylinder {
      cylinderRadius :: Length,
      cylinderHeight :: Length }
  | Difference Model Model
  | Polyhedron [Polygon3D]
  | Rectangle Length Length
  | Rotate Angle3D Model
  | Sphere Length
  | Translate Vector3D Model
  | Union [Model]
  deriving Show

type Polygon3D = [Vector3D]

data Units = Centimeters | Inches

data Vector2D = Vector2D {
  vector2dX :: Length,
  vector2dY :: Length
  } deriving Show

data Vector3D = Vector3D Length Length Length deriving Show

--(:+) :: Length -> Length
--(:+) = Plus

infixl 1 @+
(@+) = Plus

infixl 2 @-
(@-) = Minus

infixl 3 @*
(@*) = Times

infixl 4 @/
(@/) = DividedBy

infixl 4 @%
(@%) = Modulo

absolute :: Double -> Length
absolute = Absolute

angleDegrees :: Angle -> Double
angleDegrees a = 180 * (angleValue a) / pi

angleValue :: Angle -> Double
angleValue (Angle d) = d

backward :: Length -> Model -> Model
backward l = translate zero (neg l) zero

calipers :: Double -> Length
calipers d = Measurement d MillimeterCalipers

color :: Double -> Double -> Double -> Double -> Color
color r g b a = DC.withOpacity (RGB.rgb r g b) a

down :: Length -> Model -> Model
down l = translate zero zero (neg l)

forward :: Length -> Model -> Model
forward l = translate zero l zero

guess :: Double -> Length
guess d = Measurement d Unspecified

laser :: Double -> Length
laser d = Measurement d Laser

left :: Length -> Model -> Model
left l = translate (neg l) zero zero

lengthValue :: Length -> Double
lengthValue l = case l of
  Absolute d -> d
  DividedBy l d -> (lengthValue l) / (lengthValue d)
  Max l1 l2 -> max (lengthValue l1) (lengthValue l2)
  Measurement d _ -> d
  Min l1 l2 -> min (lengthValue l1) (lengthValue l2)
  Minus l1 l2 -> (lengthValue l1) - (lengthValue l2)
  Modulo l d -> DF.mod' (lengthValue l) (lengthValue d)
  Plus l1 l2 -> (lengthValue l1) + (lengthValue l2)
  SquareRoot l -> sqrt (lengthValue l)
  Times l d -> (lengthValue l) * (lengthValue d)

micrometer :: Double -> Length
micrometer d = Measurement d Micrometer

minus90 = Angle $ 0 - pi/2

neg :: Length -> Length
neg l = Absolute 0 @- l

opaqueRed = DC.opaque OS.red
opaqueOrange = DC.opaque OS.orange
opaqueYellow = DC.opaque OS.yellow
opaqueGreen = DC.opaque OS.green
opaqueBlue = DC.opaque OS.blue
opaquePurple = DC.opaque OS.purple
opaquePink = DC.opaque OS.pink
opaqueWhite = DC.opaque OS.white
opaqueGray = DC.opaque OS.gray
opaqueBlack = DC.opaque DC.black :: Color
opaqueBrown = DC.opaque OS.brown

clearBlue = DC.withOpacity (RGB.rgb 0 0 1) 0.5

person :: Units -> Model
person units = Color opaqueOrange $ Union [head, torso, legs]
  where
    legs = Cylinder (absolute legsRadius) (absolute $ height - 2*headRadius)
    torso = up (absolute $ (height - 2*headRadius) / 2) $
      Cylinder (absolute torsoRadius) (absolute $ (height - 2*headRadius) /2)
    head = up (absolute $ height - headRadius) $ Sphere (absolute headRadius)
    headRadius = convert 20
    height = convert 183
    legsRadius = convert 20
    torsoRadius = convert 30
    convert l = case units of
      Centimeters -> l
      Inches -> l / 2.54

plane :: Length -> Length -> Model
plane x y = Box x y smidge

plus180 = Angle pi
plus90 = Angle $ pi/2

-- TODO: possibly superseded by the (much simpler) polyPrism
polygonPrism :: Length -> [Vector2D] -> Model
polygonPrism thickness points = Difference hullRect (Union $ fmap toSlice pairs)
  where
    minX = unspecified $ minimum $ fmap (lengthValue . vector2dX) points
    minY = unspecified $ minimum $ fmap (lengthValue . vector2dY) points
    maxX = unspecified $ maximum $ fmap (lengthValue . vector2dX) points
    maxY = unspecified $ maximum $ fmap (lengthValue . vector2dY) points
    margin = unspecified $ max (lengthValue $ maxX @- minX) (lengthValue $ maxY @- minY)
    hullRect = right minX $ forward minY $
      Box (maxX @- minX) (maxY @- minY) thickness
    pairs = zipWith (\p1 p2 -> (p1, p2)) points $ rollDown points
    angle p1 p2 = if dx' /= 0
        then if dx' < 0 then a + pi else a
        else if dy' >= 0
        then pi/2
        else -pi/2
      where
        a = atan $ dy' / dx'
        dx' = dx p1 p2
        dy' = dy p1 p2
    dx p1 p2 = (lengthValue $ vector2dX p2) - (lengthValue $ vector2dX p1)
    dy p1 p2 = (lengthValue $ vector2dY p2) - (lengthValue $ vector2dY p1)
    dist p1 p2 = unspecified $ sqrt $ (dx' * dx') + (dy' * dy')
      where
        dx' = dx p1 p2
        dy' = dy p1 p2
    toSlice :: (Vector2D, Vector2D) -> Model
    toSlice (p1, p2) = right (vector2dX p1) $ forward (vector2dY p1) $ down smdg $
        rotate (Angle 0) (Angle 0) (Angle $ angle p1 p2) $
          left margin $
            Box (dist p1 p2 @+ margin @* absolute 2) margin (thickness @+ smdg @* absolute 2)
      where
        smdg = thickness @/ absolute 10

polyPrism :: Length -> [Vector2D] -> Model
polyPrism height points = Polyhedron $ bottomFace:topFace:sideFaces
  where
    bottomPoint (Vector2D x y) = Vector3D x y $ absolute 0
    topPoint (Vector2D x y) = Vector3D x y height
    bottomFace = bottomPoint <$> points
    topFace = topPoint <$> points
    sideFaces = L.zipWith (\p1 p2 -> [bottomPoint p1, bottomPoint p2, topPoint p2, topPoint p1]) points (rollDown points)

right :: Length -> Model -> Model
right l = translate l zero zero

rollDown :: [a] -> [a]
rollDown l = L.drop 1 l ++ L.take 1 l

rotate :: Angle -> Angle -> Angle -> Model -> Model
rotate x y z m = Rotate (Angle3D x y z) m

rotateX :: Angle -> Model -> Model
rotateX a = rotate a zeroAngle zeroAngle

rotateY :: Angle -> Model -> Model
rotateY a = rotate zeroAngle a zeroAngle

rotateZ :: Angle -> Model -> Model
rotateZ a = rotate zeroAngle zeroAngle a

ruler :: Double -> Length
ruler d = Measurement d Ruler

shrink :: Model -> Model
shrink m = case m of
  Box len wid high -> translate smidge smidge smidge $
    Box (len @- smidge @* absolute 2) (wid @- smidge @* absolute 2) (high @- smidge @* absolute 2)
  _ -> m

smidge = Absolute 0.1
smidge2 = Absolute 0.2

smidgeBigger :: Model -> Model
smidgeBigger m = case m of
  Box len wid high -> translate smidgeNegative smidgeNegative smidgeNegative $
    Box (len @+ smidge @* absolute 2) (wid @+ smidge @* absolute 2) (high @+ smidge @* absolute 2)
  _ -> m

smidgeNegative = Absolute (0-0.1)

sphere :: Length -> Model
sphere = Sphere

square :: Length -> Length -> Model
square x y = Box x y smidge

squareRoot :: Length -> Length
squareRoot = SquareRoot

tape :: Double -> Length
tape d = Measurement d Tape

translate :: Length -> Length -> Length -> Model -> Model
translate x y z m = Translate (Vector3D x y z) m

toOpenSCAD :: Model -> OS.Model OS.Vector3d
toOpenSCAD m = case m of
    Box x y z -> OS.box (lengthValue x) (lengthValue y) (lengthValue z)
    Color c m -> OS.transparent c $ toOpenSCAD m
    Cube xyz -> OS.cube (lengthValue xyz)
    Cylinder r h -> OS.cylinder (lengthValue r) (lengthValue h) OS.def
    Polyhedron faces -> OS.polyhedron convexity (encodeFace <$> faces)
      where
        encodeFace points = encodePoint <$> points
        encodePoint (Vector3D x y z) = (lengthValue x, lengthValue y, lengthValue z)
        convexity = 10
    Rectangle w f ->  OS.solid $ OS.rectangle (lengthValue w) (lengthValue f)
    Rotate a m -> OS.rotate (fromAngle3D a) (toOpenSCAD m)
    Sphere r -> OS.sphere (lengthValue r) OS.def
    Translate v m -> OS.translate (fromVector v) (toOpenSCAD m)
    Union models -> OS.union $ fmap toOpenSCAD models
    Difference m1 m2 -> OS.difference (toOpenSCAD m1) (toOpenSCAD m2)
  where
    fromVector (Vector3D x y z) = (lengthValue x, lengthValue y, lengthValue z)
    fromAngle3D (Angle3D x y z) = (angleDegrees x, angleDegrees y, angleDegrees z)

unspecified :: Double -> Length
unspecified d = Measurement d Unspecified

up :: Length -> Model -> Model
up l = translate zero zero l

zero = Absolute 0

zeroAngle = Angle 0

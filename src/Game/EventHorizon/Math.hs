module Game.EventHorizon.Math
  ( -- * 3D Vectors
    Vec3 (..)
  , vec3
  , vZero
  , vNeg
  , (^+^)
  , (^-^)
  , (*^)
  , (^*)
  , (^/)
  , (<.>)
  , (><)
  , vLen
  , vLenSq
  , vNorm
  , vDist
  , vLerp

    -- * Quaternions
  , Quat (..)
  , quatIdentity
  , quatFromAxisAngle
  , quatFromEuler
  , quatMul
  , quatNorm
  , quatConj
  , quatInv
  , quatRotate
  , (#>)
  , quatSlerp
  , forwardOf
  , rightOf
  , upOf

    -- * Pose
  , Pose (..)
  , at
  , atRot

    -- * Colors
  , Color (..)
  , rgb
  , white
  , black
  , red
  , green
  , blue
  , yellow
  , cyan
  , orange
  , purple
  , brown
  , gold
  , gray
  , darkGray
  , lightGray
  , skyBlue
  , forestGreen
  , slate
  ) where

-- | A 3D vector (right-handed: +X right, +Y up, -Z forward).
--
-- Supports standard 'Num' arithmetic (@+@, @-@, @negate@, component-wise @*@, @0@)
-- as well as vector-space operators ('^+^', '^-^', '*^', '^*', '^/', '<.>', '><').
data Vec3 = Vec3 !Double !Double !Double
  deriving (Eq, Show)

instance Num Vec3 where
  Vec3 x1 y1 z1 + Vec3 x2 y2 z2 = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  Vec3 x1 y1 z1 - Vec3 x2 y2 z2 = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  Vec3 x1 y1 z1 * Vec3 x2 y2 z2 = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = let d = fromInteger n in Vec3 d d d

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

vZero :: Vec3
vZero = Vec3 0 0 0

vNeg :: Vec3 -> Vec3
vNeg = negate

infixl 6 ^+^, ^-^
infixl 7 *^, ^*, ^/, <.>, ><

-- | Vector addition.
(^+^) :: Vec3 -> Vec3 -> Vec3
(^+^) = (+)

-- | Vector subtraction.
(^-^) :: Vec3 -> Vec3 -> Vec3
(^-^) = (-)

-- | Left scalar multiplication.
(*^) :: Double -> Vec3 -> Vec3
s *^ Vec3 x y z = Vec3 (s * x) (s * y) (s * z)

-- | Right scalar multiplication.
(^*) :: Vec3 -> Double -> Vec3
Vec3 x y z ^* s = Vec3 (x * s) (y * s) (z * s)

-- | Scalar division.
(^/) :: Vec3 -> Double -> Vec3
Vec3 x y z ^/ s = Vec3 (x / s) (y / s) (z / s)

-- | Dot product.
(<.>) :: Vec3 -> Vec3 -> Double
Vec3 x1 y1 z1 <.> Vec3 x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

-- | Cross product.
(><) :: Vec3 -> Vec3 -> Vec3
Vec3 x1 y1 z1 >< Vec3 x2 y2 z2 =
  Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- | Squared Euclidean length.
vLenSq :: Vec3 -> Double
vLenSq v = v <.> v

-- | Euclidean length.
vLen :: Vec3 -> Double
vLen = sqrt . vLenSq

-- | Normalize a vector, returning 'vZero' if near-zero.
vNorm :: Vec3 -> Vec3
vNorm v =
  let l = vLen v
   in if l < 1e-9 then vZero else v ^/ l

-- | Euclidean distance between two points.
vDist :: Vec3 -> Vec3 -> Double
vDist a b = vLen (a - b)

-- | Linear interpolation between two vectors.
vLerp :: Double -> Vec3 -> Vec3 -> Vec3
vLerp t a b = a + t *^ (b - a)

-- | Unit quaternion @(x, y, z, w)@.
--
-- Forms a 'Semigroup' and 'Monoid' under rotation composition ('<>' and 'mempty').
data Quat = Quat !Double !Double !Double !Double
  deriving (Eq, Show)

instance Semigroup Quat where
  (<>) = quatMul

instance Monoid Quat where
  mempty = quatIdentity

quatIdentity :: Quat
quatIdentity = Quat 0 0 0 1

quatFromAxisAngle :: Vec3 -> Double -> Quat
quatFromAxisAngle axis angle =
  let Vec3 ax ay az = vNorm axis
      half = angle * 0.5
      s = sin half
      c = cos half
   in quatNorm (Quat (ax * s) (ay * s) (az * s) c)

-- | Build a quaternion from Euler angles @(rx, ry, rz)@ in radians (XYZ order).
quatFromEuler :: Vec3 -> Quat
quatFromEuler (Vec3 rx ry rz) =
  let qx = quatFromAxisAngle (Vec3 1 0 0) rx
      qy = quatFromAxisAngle (Vec3 0 1 0) ry
      qz = quatFromAxisAngle (Vec3 0 0 1) rz
   in quatNorm (qx <> qy <> qz)

quatMul :: Quat -> Quat -> Quat
quatMul (Quat x1 y1 z1 w1) (Quat x2 y2 z2 w2) =
  Quat
    (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
    (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
    (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2)
    (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)

quatNorm :: Quat -> Quat
quatNorm (Quat x y z w) =
  let n = sqrt (x * x + y * y + z * z + w * w)
   in if n < 1e-12 then quatIdentity else Quat (x / n) (y / n) (z / n) (w / n)

quatConj :: Quat -> Quat
quatConj (Quat x y z w) = Quat (-x) (-y) (-z) w

-- | Inverse of a unit quaternion.
quatInv :: Quat -> Quat
quatInv = quatConj . quatNorm

-- | Rotate a 'Vec3' by a unit 'Quat'.
quatRotate :: Quat -> Vec3 -> Vec3
quatRotate (Quat qx qy qz qw) v =
  let qv = Vec3 qx qy qz
      t = 2 *^ (qv >< v)
   in v + qw *^ t + (qv >< t)

infixr 7 #>

-- | Infix operator for rotating a vector by a quaternion.
(#>) :: Quat -> Vec3 -> Vec3
(#>) = quatRotate

-- | Spherical linear interpolation between two unit quaternions.
quatSlerp :: Double -> Quat -> Quat -> Quat
quatSlerp t (Quat x1 y1 z1 w1) (Quat x2 y2 z2 w2) =
  let rawDot = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
      (bx, by, bz, bw, d) =
        if rawDot < 0
          then (-x2, -y2, -z2, -w2, -rawDot)
          else (x2, y2, z2, w2, rawDot)
   in if d > 0.9995
        then
          quatNorm $
            Quat
              (x1 + t * (bx - x1))
              (y1 + t * (by - y1))
              (z1 + t * (bz - z1))
              (w1 + t * (bw - w1))
        else
          let theta = acos (min 1.0 d)
              sinT = sin theta
              s1 = sin ((1 - t) * theta) / sinT
              s2 = sin (t * theta) / sinT
           in Quat
                (s1 * x1 + s2 * bx)
                (s1 * y1 + s2 * by)
                (s1 * z1 + s2 * bz)
                (s1 * w1 + s2 * bw)

-- | Local forward direction (@-Z@) rotated by quaternion.
forwardOf :: Quat -> Vec3
forwardOf q = q #> Vec3 0 0 (-1)

-- | Local right direction (@+X@) rotated by quaternion.
rightOf :: Quat -> Vec3
rightOf q = q #> Vec3 1 0 0

-- | Local up direction (@+Y@) rotated by quaternion.
upOf :: Quat -> Vec3
upOf q = q #> Vec3 0 1 0

-- | Position and orientation in 3D space.
data Pose = Pose
  { posePos :: !Vec3
  , poseRot :: !Quat
  }
  deriving (Eq, Show)

-- | Pose at a position with identity rotation.
at :: Vec3 -> Pose
at p = Pose p quatIdentity

-- | Pose at a position with an explicit rotation.
atRot :: Vec3 -> Quat -> Pose
atRot = Pose

-- | RGB color with channels in @[0, 1]@.
data Color = Color !Double !Double !Double
  deriving (Eq, Show)

rgb :: Double -> Double -> Double -> Color
rgb = Color

white, black, red, green, blue, yellow, cyan, orange, purple, brown, gold :: Color
white = Color 0.96 0.96 0.98
black = Color 0.05 0.05 0.07
red = Color 0.88 0.22 0.22
green = Color 0.22 0.78 0.36
blue = Color 0.22 0.50 0.92
yellow = Color 0.96 0.85 0.24
cyan = Color 0.18 0.84 0.88
orange = Color 0.95 0.54 0.16
purple = Color 0.62 0.32 0.88
brown = Color 0.48 0.30 0.16
gold = Color 0.95 0.76 0.18

gray, darkGray, lightGray, skyBlue, forestGreen, slate :: Color
gray = Color 0.52 0.55 0.60
darkGray = Color 0.22 0.24 0.28
lightGray = Color 0.78 0.80 0.84
skyBlue = Color 0.48 0.76 0.96
forestGreen = Color 0.16 0.48 0.22
slate = Color 0.32 0.37 0.44

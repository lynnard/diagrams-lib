{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Attributes
-- Copyright   :  (c) 2013-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered. This module defines /Textures/ (Gradients and Colors) in two
-- dimensions. Like the attributes defined in the Diagrams.Attributes module,
-- all attributes defined here use the 'Last' or 'Recommend' /semigroup/ structure.
-- 'FillColor' and 'LineColor' attributes are provided so that backends that
-- don't support gradients need not be concerned with using textures. Backends
-- should only implement color attributes or textures attributes, not both.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Attributes (
  -- * Textures
    Texture(..), solid, _SC, _AC, _LG, _RG, defaultLG, defaultRG
  , GradientStop(..), stopColor, stopFraction, mkStops
  , SpreadMethod(..), lineLGradient, lineRGradient

  -- ** Linear Gradients
  , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
  , lGradSpreadMethod, mkLinearGradient

  -- ** Radial Gradients
  , RGradient(..), rGradStops, rGradTrans
  , rGradCenter0, rGradRadius0, rGradCenter1, rGradRadius1
  , rGradSpreadMethod, mkRadialGradient

  -- ** Line texture
  , LineTexture(..), _LineTexture, getLineTexture, lineTexture, lineTextureA
  , mkLineTexture, _lineTexture

  -- ** Line color
  , lineColor, lc, lcA

  -- ** Fill texture
  , FillTexture(..), _FillTexture, getFillTexture, fillTexture
  , mkFillTexture, _fillTexture, _fillTextureR

  -- ** Fill color
  , fillColor, fc, fcA, recommendFillColor

  -- * Compilation utilities
  , splitTextureFills

  ) where

import           Control.Lens                hiding (transform)
import           Data.Colour                 hiding (AffineSpace, over)
import           Data.Data
import           Data.Default.Class
import           Data.Monoid.Recommend
import           Data.Semigroup

import           Diagrams.Attributes
import           Diagrams.Attributes.Compile
import           Diagrams.Core
import           Diagrams.Core.Types         (RTree)
import           Diagrams.Located            (unLoc)
import           Diagrams.Path               (Path, pathTrails)
import           Diagrams.Trail              (isLoop)
import           Diagrams.TwoD.Types
import           Diagrams.Util


-----------------------------------------------------------------
--  Gradients  --------------------------------------------------
-----------------------------------------------------------------

-- | A gradient stop contains a color and fraction (usually between 0 and 1)
data GradientStop d = GradientStop
  { _stopColor    :: SomeColor
  , _stopFraction :: d
  }

-- makeLensesWith (lensRules & generateSignatures .~ False) ''GradientStop
stopColor f_a37vr (GradientStop x1_a37vs x2_a37vu)
  = fmap
      (\ y1_a37vv -> GradientStop y1_a37vv x2_a37vu) (f_a37vr x1_a37vs)
{-# INLINE stopColor #-}
stopFraction f_a37vB (GradientStop x1_a37vD x2_a37vE)
  = fmap
      (\ y1_a37vF -> GradientStop x1_a37vD y1_a37vF) (f_a37vB x2_a37vE)
{-# INLINE stopFraction #-}

-- | A color for the stop.
stopColor :: Lens' (GradientStop n) SomeColor

-- | The fraction for stop.
stopFraction :: Lens' (GradientStop n) n

-- | The 'SpreadMethod' determines what happens before 'lGradStart' and after
--   'lGradEnd'. 'GradPad' fills the space before the start of the gradient
--   with the color of the first stop and the color after end of the gradient
--   with the color of the last stop. 'GradRepeat' restarts the gradient and
--   'GradReflect' restarts the gradient with the stops in reverse order.
data SpreadMethod = GradPad | GradReflect | GradRepeat

-- | Linear Gradient
data LGradient n = LGradient
  { _lGradStops        :: [GradientStop n]
  , _lGradStart        :: Point V2 n
  , _lGradEnd          :: Point V2 n
  , _lGradTrans        :: Transformation V2 n
  , _lGradSpreadMethod :: SpreadMethod }

type instance V (LGradient n) = V2
type instance N (LGradient n) = n

-- makeLensesWith (lensRules & generateSignatures .~ False) ''LGradient
lGradEnd
  f_a37Lv
  (LGradient x1_a37Lw x2_a37Lx x3_a37Ly x4_a37Lz x5_a37LA)
  = fmap
      (\ y1_a37LB
         -> LGradient x1_a37Lw x2_a37Lx y1_a37LB x4_a37Lz x5_a37LA)
      (f_a37Lv x3_a37Ly)
{-# INLINE lGradEnd #-}
lGradSpreadMethod
  f_a37LC
  (LGradient x1_a37LD x2_a37LE x3_a37LF x4_a37LG x5_a37LH)
  = fmap
      (\ y1_a37LI
         -> LGradient x1_a37LD x2_a37LE x3_a37LF x4_a37LG y1_a37LI)
      (f_a37LC x5_a37LH)
{-# INLINE lGradSpreadMethod #-}
lGradStart
  f_a37LJ
  (LGradient x1_a37LK x2_a37LL x3_a37LM x4_a37LN x5_a37LO)
  = fmap
      (\ y1_a37LP
         -> LGradient x1_a37LK y1_a37LP x3_a37LM x4_a37LN x5_a37LO)
      (f_a37LJ x2_a37LL)
{-# INLINE lGradStart #-}
lGradStops
  f_a37LQ
  (LGradient x1_a37LR x2_a37LT x3_a37LU x4_a37LV x5_a37LW)
  = fmap
      (\ y1_a37LZ
         -> LGradient y1_a37LZ x2_a37LT x3_a37LU x4_a37LV x5_a37LW)
      (f_a37LQ x1_a37LR)
{-# INLINE lGradStops #-}
lGradTrans
  f_a37M4
  (LGradient x1_a37M5 x2_a37M7 x3_a37M8 x4_a37Mb x5_a37Mc)
  = fmap
      (\ y1_a37Md
         -> LGradient x1_a37M5 x2_a37M7 x3_a37M8 y1_a37Md x5_a37Mc)
      (f_a37M4 x4_a37Mb)
{-# INLINE lGradTrans #-}

instance Fractional n => Transformable (LGradient n) where
  transform = over lGradTrans . transform

-- | A list of stops (colors and fractions).
lGradStops :: Lens' (LGradient n) [GradientStop n]

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
lGradTrans :: Lens' (LGradient n) (Transformation V2 n)

-- | The starting point for the first gradient stop. The coordinates are in
--   'local' units and the default is (-0.5, 0).
lGradStart :: Lens' (LGradient n) (Point V2 n)

-- | The ending point for the last gradient stop.The coordinates are in
--   'local' units and the default is (0.5, 0).
lGradEnd :: Lens' (LGradient n) (Point V2 n)

-- | For setting the spread method.
lGradSpreadMethod :: Lens' (LGradient n) SpreadMethod

-- | Radial Gradient
data RGradient n = RGradient
  { _rGradStops        :: [GradientStop n]
  , _rGradCenter0      :: Point V2 n
  , _rGradRadius0      :: n
  , _rGradCenter1      :: Point V2 n
  , _rGradRadius1      :: n
  , _rGradTrans        :: Transformation V2 n
  , _rGradSpreadMethod :: SpreadMethod }

-- makeLensesWith (lensRules & generateSignatures .~ False) ''RGradient
rGradCenter0
  f_a37WK
  (RGradient x1_a37WL
             x2_a37WM
             x3_a37WN
             x4_a37WP
             x5_a37WQ
             x6_a37WR
             x7_a37WS)
  = fmap
      (\ y1_a37WT
         -> RGradient
              x1_a37WL y1_a37WT x3_a37WN x4_a37WP x5_a37WQ x6_a37WR x7_a37WS)
      (f_a37WK x2_a37WM)
{-# INLINE rGradCenter0 #-}
rGradCenter1
  f_a37WU
  (RGradient x1_a37WV
             x2_a37WW
             x3_a37WX
             x4_a37WY
             x5_a37WZ
             x6_a37X0
             x7_a37X1)
  = fmap
      (\ y1_a37X2
         -> RGradient
              x1_a37WV x2_a37WW x3_a37WX y1_a37X2 x5_a37WZ x6_a37X0 x7_a37X1)
      (f_a37WU x4_a37WY)
{-# INLINE rGradCenter1 #-}
rGradRadius0
  f_a37X3
  (RGradient x1_a37X4
             x2_a37X5
             x3_a37X6
             x4_a37X7
             x5_a37X8
             x6_a37X9
             x7_a37Xa)
  = fmap
      (\ y1_a37Xb
         -> RGradient
              x1_a37X4 x2_a37X5 y1_a37Xb x4_a37X7 x5_a37X8 x6_a37X9 x7_a37Xa)
      (f_a37X3 x3_a37X6)
{-# INLINE rGradRadius0 #-}
rGradRadius1
  f_a37Xc
  (RGradient x1_a37Xd
             x2_a37Xe
             x3_a37Xf
             x4_a37Xh
             x5_a37Xi
             x6_a37Xj
             x7_a37Xk)
  = fmap
      (\ y1_a37Xm
         -> RGradient
              x1_a37Xd x2_a37Xe x3_a37Xf x4_a37Xh y1_a37Xm x6_a37Xj x7_a37Xk)
      (f_a37Xc x5_a37Xi)
{-# INLINE rGradRadius1 #-}
rGradSpreadMethod
  f_a37Xn
  (RGradient x1_a37Xo
             x2_a37Xp
             x3_a37Xq
             x4_a37Xr
             x5_a37Xs
             x6_a37Xt
             x7_a37Xu)
  = fmap
      (\ y1_a37Xv
         -> RGradient
              x1_a37Xo x2_a37Xp x3_a37Xq x4_a37Xr x5_a37Xs x6_a37Xt y1_a37Xv)
      (f_a37Xn x7_a37Xu)
{-# INLINE rGradSpreadMethod #-}
rGradStops
  f_a37Xx
  (RGradient x1_a37Xy
             x2_a37XA
             x3_a37XB
             x4_a37XC
             x5_a37XE
             x6_a37XF
             x7_a37XG)
  = fmap
      (\ y1_a37XH
         -> RGradient
              y1_a37XH x2_a37XA x3_a37XB x4_a37XC x5_a37XE x6_a37XF x7_a37XG)
      (f_a37Xx x1_a37Xy)
{-# INLINE rGradStops #-}
rGradTrans
  f_a37XI
  (RGradient x1_a37XJ
             x2_a37XK
             x3_a37XL
             x4_a37XM
             x5_a37XN
             x6_a37XO
             x7_a37XP)
  = fmap
      (\ y1_a37XQ
         -> RGradient
              x1_a37XJ x2_a37XK x3_a37XL x4_a37XM x5_a37XN y1_a37XQ x7_a37XP)
      (f_a37XI x6_a37XO)
{-# INLINE rGradTrans #-}

type instance V (RGradient n) = V2
type instance N (RGradient n) = n

instance Fractional n => Transformable (RGradient n) where
  transform = over rGradTrans . transform

-- | A list of stops (colors and fractions).
rGradStops :: Lens' (RGradient n) [GradientStop n]

-- | The center point of the inner circle.
rGradCenter0 :: Lens' (RGradient n) (Point V2 n)

-- | The radius of the inner cirlce in 'local' coordinates.
rGradRadius0 :: Lens' (RGradient n) n

-- | The center of the outer circle.
rGradCenter1  :: Lens' (RGradient n) (Point V2 n)

-- | The radius of the outer circle in 'local' coordinates.
rGradRadius1 :: Lens' (RGradient n) n

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
rGradTrans :: Lens' (RGradient n) (Transformation V2 n)

-- | For setting the spread method.
rGradSpreadMethod :: Lens' (RGradient n) SpreadMethod

-----------------------------------------------------------------
--  Textures  ---------------------------------------------------
-----------------------------------------------------------------

-- | A Texture is either a color 'SC', linear gradient 'LG', or radial gradient 'RG'.
--   An object can have only one texture which is determined by the 'Last'
--   semigroup structure.
data Texture n = SC SomeColor | LG (LGradient n) | RG (RGradient n)
  deriving Typeable

type instance V (Texture n) = V2
type instance N (Texture n) = n

-- makePrisms ''Texture
_SC :: forall n_a37Y8. Prism' (Texture n_a37Y8) SomeColor
_SC
  = prism
      (\ x_a38kV -> SC x_a38kV)
      (\ x_a38kW
         -> case x_a38kW of
              SC y1_a38kX -> Right y1_a38kX
              _ -> Left x_a38kW )
_LG :: forall n_a37Y8. Prism' (Texture n_a37Y8) (LGradient n_a37Y8)
_LG
  = prism
      (\ x_a38lr -> LG x_a38lr)
      (\ x_a38ls
         -> case x_a38ls of
              LG y1_a38lt -> Right y1_a38lt
              _ -> Left x_a38ls )
_RG :: forall n_a37Y8. Prism' (Texture n_a37Y8) (RGradient n_a37Y8)
_RG
  = prism
      (\ x_a38lv -> RG x_a38lv)
      (\ x_a38lw
         -> case x_a38lw of
              RG y1_a38lx -> Right y1_a38lx
              _ -> Left x_a38lw )

-- | Prism onto an 'AlphaColour' 'Double' of a 'SC' texture.
_AC :: Prism' (Texture n) (AlphaColour Double)
_AC = _SC . _SomeColor

instance Floating n => Transformable (Texture n) where
  transform t (LG lg) = LG $ transform t lg
  transform t (RG rg) = RG $ transform t rg
  transform _ sc      = sc

-- | Convert a solid colour into a texture.
solid :: Color a => a -> Texture n
solid = SC . SomeColor

-- | A default is provided so that linear gradients can easily be created using
--   lenses. For example, @lg = defaultLG & lGradStart .~ (0.25 ^& 0.33)@. Note that
--   no default value is provided for @lGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultLG :: Fractional n => Texture n
defaultLG = LG LGradient
  { _lGradStops        = []
  , _lGradStart        = mkP2 (-0.5) 0
  , _lGradEnd          = mkP2 0.5 0
  , _lGradTrans        = mempty
  , _lGradSpreadMethod = GradPad
  }

-- | A default is provided so that radial gradients can easily be created using
--   lenses. For example, @rg = defaultRG & rGradRadius1 .~ 0.25@. Note that
--   no default value is provided for @rGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultRG :: Fractional n => Texture n
defaultRG = RG RGradient
  { _rGradStops        = []
  , _rGradCenter0      = mkP2 0 0
  , _rGradRadius0      = 0.0
  , _rGradCenter1      = mkP2 0 0
  , _rGradRadius1      = 0.5
  , _rGradTrans        = mempty
  , _rGradSpreadMethod = GradPad
  }

-- | A convenient function for making gradient stops from a list of triples.
--   (An opaque color, a stop fraction, an opacity).
mkStops :: [(Colour Double, d, Double)] -> [GradientStop d]
mkStops = map (\(x, y, z) -> GradientStop (SomeColor (withOpacity x z)) y)

-- | Make a linear gradient texture from a stop list, start point, end point,
--   and 'SpreadMethod'. The 'lGradTrans' field is set to the identity
--   transfrom, to change it use the 'lGradTrans' lens.
mkLinearGradient :: Num n => [GradientStop n] -> Point V2 n -> Point V2 n -> SpreadMethod -> Texture n
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end mempty spreadMethod)

-- | Make a radial gradient texture from a stop list, radius, start point,
--   end point, and 'SpreadMethod'. The 'rGradTrans' field is set to the identity
--   transfrom, to change it use the 'rGradTrans' lens.
mkRadialGradient :: Num n => [GradientStop n] -> Point V2 n -> n
                  -> Point V2 n -> n -> SpreadMethod -> Texture n
mkRadialGradient stops c0 r0 c1 r1 spreadMethod
  = RG (RGradient stops c0 r0 c1 r1 mempty spreadMethod)

-- Line Texture --------------------------------------------------------

-- | The texture with which lines are drawn.  Note that child
--   textures always override parent textures.
--   More precisely, the semigroup structure on line texture attributes
--   is that of 'Last'.
newtype LineTexture n = LineTexture (Last (Texture n))
  deriving (Typeable, Semigroup)
instance (Typeable n) => AttributeClass (LineTexture n)

type instance V (LineTexture n) = V2
type instance N (LineTexture n) = n

_LineTexture :: Iso (LineTexture n) (LineTexture n')
                    (Texture n)     (Texture n')
_LineTexture = iso getLineTexture (LineTexture . Last)

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Floating n => Transformable (LineTexture n) where
  transform t (LineTexture (Last tx)) = LineTexture (Last $ transform t tx)

instance Default (LineTexture n) where
  def = _LineTexture . _SC ## SomeColor black

mkLineTexture :: Texture n -> LineTexture n
mkLineTexture = LineTexture . Last

getLineTexture :: LineTexture n -> Texture n
getLineTexture (LineTexture (Last t)) = t

lineTexture :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => Texture n -> a -> a
lineTexture = applyTAttr . LineTexture . Last

lineTextureA :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => LineTexture n -> a -> a
lineTextureA = applyTAttr

_lineTexture :: (Floating n, Typeable n) => Lens' (Style V2 n) (Texture n)
_lineTexture = atTAttr . anon def isDef . _LineTexture
  where
    isDef = anyOf (_LineTexture . _AC) (== opaque black)

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (InSpace V2 n a, Color c, Typeable n, Floating n, HasStyle a) => c -> a -> a
lineColor = lineTexture . SC . SomeColor

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).  See comment in 'lineColor' about backends.
lc :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).  See comment in 'lineColor'
--   about backends.
lcA :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => AlphaColour Double -> a -> a
lcA = lineColor

-- | Apply a linear gradient.
lineLGradient :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => LGradient n -> a -> a
lineLGradient g = lineTexture (LG g)

-- | Apply a radial gradient.
lineRGradient :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => RGradient n -> a -> a
lineRGradient g = lineTexture (RG g)

-- Fill Texture --------------------------------------------------------

-- | The texture with which objects are filled.
--   The semigroup structure on fill texture attributes
--   is that of 'Recommed . Last'.
newtype FillTexture n = FillTexture (Recommend (Last (Texture n)))
  deriving (Typeable, Semigroup)

instance Typeable n => AttributeClass (FillTexture n)

_FillTexture :: Iso' (FillTexture n) (Recommend (Texture n))
_FillTexture = iso getter setter
  where
    getter (FillTexture (Recommend (Last t))) = Recommend t
    getter (FillTexture (Commit    (Last t))) = Commit t
    setter (Recommend t) = FillTexture (Recommend (Last t))
    setter (Commit t)    = FillTexture (Commit (Last t))
  -- = iso (\(FillTexture a) -> a) FillTexture . mapping _Wrapped
  -- -- once we depend on monoid-extras-0.4

type instance V (FillTexture n) = V2
type instance N (FillTexture n) = n

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Floating n => Transformable (FillTexture n) where
  transform = over (_FillTexture . _recommend) . transform

instance Default (FillTexture n) where
  def = mkFillTexture $ _AC ## transparent

getFillTexture :: FillTexture n -> Texture n
getFillTexture (FillTexture tx) = getLast . getRecommend $ tx

fillTexture :: (InSpace V2 n a, Typeable n, Floating n, HasStyle a) => Texture n -> a -> a
fillTexture = applyTAttr . mkFillTexture

mkFillTexture :: Texture n -> FillTexture n
mkFillTexture = FillTexture . Commit . Last

-- | Lens onto the 'Recommend' of a fill texture in a style.
_fillTextureR :: (Typeable n, Floating n) => Lens' (Style V2 n) (Recommend (Texture n))
_fillTextureR = atTAttr . anon def isDef . _FillTexture
  where
    isDef = anyOf (_FillTexture . _Recommend . _AC) (== transparent)

-- | Commit a fill texture in a style. This is /not/ a valid setter
--   because it doesn't abide the functor law (see 'committed').
_fillTexture :: (Typeable n, Floating n) => Lens' (Style V2 n) (Texture n)
_fillTexture = _fillTextureR . committed

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (InSpace V2 n a, Color c, Typeable n, Floating n, HasStyle a) => c -> a -> a
fillColor = fillTexture . SC . SomeColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
--   See comment after 'fillColor' about backends.
recommendFillColor :: (InSpace V2 n a, Color c, Typeable n, Floating n, HasStyle a) => c -> a -> a
recommendFillColor =
  applyTAttr . FillTexture . Recommend . Last . SC . SomeColor

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors). See comment after 'fillColor' about backends.
fc :: (InSpace V2 n a, Floating n, Typeable n, HasStyle a) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency). See comment after 'fillColor' about backends.
fcA :: (InSpace V2 n a, Floating n, Typeable n, HasStyle a) => AlphaColour Double -> a -> a
fcA = fillColor

-- Split fills ---------------------------------------------------------

data FillTextureLoops n = FillTextureLoops

instance Typeable n => SplitAttribute (FillTextureLoops n) where
  type AttrType (FillTextureLoops n) = FillTexture n
  type PrimType (FillTextureLoops n) = Path V2 n

  primOK _ = all (isLoop . unLoc) . pathTrails

-- | Push fill attributes down until they are at the root of subtrees
--   containing only loops. This makes life much easier for backends,
--   which typically have a semantics where fill attributes are
--   applied to lines/non-closed paths as well as loops/closed paths,
--   whereas in the semantics of diagrams, fill attributes only apply
--   to loops.
splitTextureFills
  :: forall b v n a. (
                     Typeable n) => RTree b v n a -> RTree b v n a
splitTextureFills = splitAttr (FillTextureLoops :: FillTextureLoops n)

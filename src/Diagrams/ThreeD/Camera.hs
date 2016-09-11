{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Camera
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify viewpoint for 3D rendering.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Camera
       ( Camera  -- do not export constructor
        -- These are safe to construct manually
       , PerspectiveLens(..), OrthoLens(..)
       , horizontalFieldOfView, verticalFieldOfView
       , orthoWidth, orthoHeight
       , camLoc, camForward, camUp, camRight, camLens
       , facing_ZCamera, mm50Camera
       , mm50, mm50Wide, mm50Narrow
       , aspect, camAspect
       )
       where

import           qualified Control.Lens.Type
import           Data.Monoid
import           Data.Typeable

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Direction
import           Diagrams.ThreeD.Vector

import           Linear.V3

-- Parameterize Camera on the lens type, so that Backends can express which
-- lenses they handle.
data Camera l n = Camera
    { camLoc  :: Point V3 n
    , forward :: V3 n
    , up      :: V3 n
    , lens    :: l n
    }
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#else

instance forall l. Typeable1 l => Typeable1 (Camera l) where
  typeOf1 _ = mkTyConApp (mkTyCon3 "diagrams-lib" "Diagrams.ThreeD.Camera" "Camera") [] `mkAppTy`
              typeOf1 (undefined :: l n)
#endif

type instance V (Camera l n) = V3
type instance N (Camera l n) = n

#if __GLASGOW_HASKELL__ > 707
class Typeable l => CameraLens l where
#else
class Typeable1 l => CameraLens l where
#endif
  -- | The natural aspect ratio of the projection.
  aspect :: Floating n => l n -> n

-- | A perspective projection
data PerspectiveLens n = PerspectiveLens
  { _horizontalFieldOfView :: Angle n -- ^ Horizontal field of view.
  , _verticalFieldOfView   :: Angle n -- ^ Vertical field of view.
  }
  deriving Typeable

-- makeLenses ''PerspectiveLens
horizontalFieldOfView ::
  forall n_a1num.
  Control.Lens.Type.Lens' (PerspectiveLens n_a1num) (Angle n_a1num)
horizontalFieldOfView f_a1nCi (PerspectiveLens x1_a1nCj x2_a1nCk)
  = fmap
      (\ y1_a1nCl -> PerspectiveLens y1_a1nCl x2_a1nCk)
      (f_a1nCi x1_a1nCj)
{-# INLINE horizontalFieldOfView #-}
verticalFieldOfView ::
  forall n_a1num.
  Control.Lens.Type.Lens' (PerspectiveLens n_a1num) (Angle n_a1num)
verticalFieldOfView f_a1nCq (PerspectiveLens x1_a1nCr x2_a1nCs)
  = fmap
      (\ y1_a1nCt -> PerspectiveLens x1_a1nCr y1_a1nCt)
      (f_a1nCq x2_a1nCs)
{-# INLINE verticalFieldOfView #-}

type instance V (PerspectiveLens n) = V3
type instance N (PerspectiveLens n) = n

instance CameraLens PerspectiveLens where
  aspect (PerspectiveLens h v) = angleRatio h v

-- | An orthographic projection
data OrthoLens n = OrthoLens
               { _orthoWidth  :: n -- ^ Width
               , _orthoHeight :: n -- ^ Height
               }
  deriving Typeable

-- makeLenses ''OrthoLens
orthoHeight ::
  forall n_a1nDi. Control.Lens.Type.Lens' (OrthoLens n_a1nDi) n_a1nDi
orthoHeight f_a1nGw (OrthoLens x1_a1nGx x2_a1nGy)
  = fmap
      (\ y1_a1nGz -> OrthoLens x1_a1nGx y1_a1nGz) (f_a1nGw x2_a1nGy)
{-# INLINE orthoHeight #-}
orthoWidth ::
  forall n_a1nDi. Control.Lens.Type.Lens' (OrthoLens n_a1nDi) n_a1nDi
orthoWidth f_a1nGA (OrthoLens x1_a1nGB x2_a1nGC)
  = fmap
      (\ y1_a1nGD -> OrthoLens y1_a1nGD x2_a1nGC) (f_a1nGA x1_a1nGB)
{-# INLINE orthoWidth #-}

type instance V (OrthoLens n) = V3
type instance N (OrthoLens n) = n

instance CameraLens OrthoLens where
  aspect (OrthoLens h v) = h / v

instance Num n => Transformable (Camera l n) where
  transform t (Camera p f u l) =
      Camera (transform t p)
             (transform t f)
             (transform t u)
             l

instance Num n => Renderable (Camera l n) NullBackend where
  render _ _ = mempty

-- | A camera at the origin facing along the negative Z axis, with its
-- up-axis coincident with the positive Y axis.  The field of view is
-- chosen to match a 50mm camera on 35mm film. Note that Cameras take
-- up no space in the Diagram.
mm50Camera :: (Typeable n, Floating n, Ord n, Renderable (Camera PerspectiveLens n) b)
           => QDiagram b V3 n Any
mm50Camera = facing_ZCamera mm50

-- | 'facing_ZCamera l' is a camera at the origin facing along the
-- negative Z axis, with its up-axis coincident with the positive Y
-- axis, with the projection defined by l.
facing_ZCamera :: (Floating n, Ord n, Typeable n, CameraLens l, Renderable (Camera l n) b) =>
                  l n -> QDiagram b V3 n Any
facing_ZCamera l = mkQD (Prim $ Camera origin unit_Z unitY l)
        mempty mempty mempty (Query . const . Any $ False)
{-# ANN facing_ZCamera ("HLint: ignore Use camelCase" :: String) #-}

mm50, mm50Wide, mm50Narrow :: Floating n => PerspectiveLens n

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg)

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide = PerspectiveLens (43.2 @@ deg)  (27 @@ deg)

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resolutions.
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg)

camForward :: Camera l n -> Direction V3 n
camForward = direction . forward

camUp :: Camera l n -> Direction V3 n
camUp = direction . up

camRight :: Fractional n => Camera l n -> Direction V3 n
camRight c = direction right where
  right = cross (forward c) (up c)

camLens :: Camera l n -> l n
camLens = lens

camAspect :: (Floating n, CameraLens l) => Camera l n -> n
camAspect = aspect . camLens

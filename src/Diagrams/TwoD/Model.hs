{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for visualizing diagrams' internal model: local origins,
-- envelopes, traces, /etc./
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Model
       ( -- * Showing the local origin
         showOrigin
       , showOrigin'
       , OriginOpts(..), oColor, oScale, oMinSize

         -- * Showing an approximation of the envelope
       , showEnvelope
       , showEnvelope'
       , EnvelopeOpts(..), eColor, eLineWidth, ePoints

         -- * Showing an approximation of the trace
       , showTrace
       , showTrace'
       , TraceOpts(..), tColor, tScale, tMinSize, tPoints

         -- * Showing labels of all named subdiagrams
       , showLabels
       ) where

import           Control.Arrow            (second)
import           Control.Lens             ({- makeLenses, -} (^.))
import qualified Control.Lens.Type
import           Data.Colour              (Colour)
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List                (intercalate)
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes)
import           Data.Semigroup

import           Diagrams.Attributes
import           Diagrams.Combinators     (atPoints)
import           Diagrams.Core
import           Diagrams.Core.Names
import           Diagrams.CubicSpline
import           Diagrams.Path
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Transform  (rotateBy)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (unitX)
import           Diagrams.Util

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

data OriginOpts n = OriginOpts
  { _oColor   :: Colour Double
  , _oScale   :: n
  , _oMinSize :: n
  }

-- makeLenses ''OriginOpts
oColor ::
  forall n_aj1h.
  Control.Lens.Type.Lens' (OriginOpts n_aj1h) (Colour Double)
oColor f_ajsO (OriginOpts x1_ajsP x2_ajsQ x3_ajsR)
  = fmap
      (\ y1_ajsS -> OriginOpts y1_ajsS x2_ajsQ x3_ajsR) (f_ajsO x1_ajsP)
{-# INLINE oColor #-}
oMinSize ::
  forall n_aj1h. Control.Lens.Type.Lens' (OriginOpts n_aj1h) n_aj1h
oMinSize f_ajsT (OriginOpts x1_ajsU x2_ajsV x3_ajsW)
  = fmap
      (\ y1_ajsX -> OriginOpts x1_ajsU x2_ajsV y1_ajsX) (f_ajsT x3_ajsW)
{-# INLINE oMinSize #-}
oScale ::
  forall n_aj1h. Control.Lens.Type.Lens' (OriginOpts n_aj1h) n_aj1h
oScale f_ajsY (OriginOpts x1_ajsZ x2_ajt0 x3_ajt1)
  = fmap
      (\ y1_ajt2 -> OriginOpts x1_ajsZ y1_ajt2 x3_ajt1) (f_ajsY x2_ajt0)
{-# INLINE oScale #-}

instance Fractional n => Default (OriginOpts n) where
  def = OriginOpts red (1/50) 0.001

data EnvelopeOpts n = EnvelopeOpts
  { _eColor     :: Colour Double
  , _eLineWidth :: Measure n
  , _ePoints    :: Int
  }

-- makeLenses ''EnvelopeOpts
eColor ::
  forall n_ajtb.
  Control.Lens.Type.Lens' (EnvelopeOpts n_ajtb) (Colour Double)
eColor f_amgE (EnvelopeOpts x1_amgF x2_amgG x3_amgH)
  = fmap
      (\ y1_amgI -> EnvelopeOpts y1_amgI x2_amgG x3_amgH)
      (f_amgE x1_amgF)
{-# INLINE eColor #-}
eLineWidth ::
  forall n_ajtb n_amgD.
  Control.Lens.Type.Lens (EnvelopeOpts n_ajtb) (EnvelopeOpts n_amgD) (Measure n_ajtb) (Measure n_amgD)
eLineWidth f_amgJ (EnvelopeOpts x1_amgK x2_amgL x3_amgM)
  = fmap
      (\ y1_amgN -> EnvelopeOpts x1_amgK y1_amgN x3_amgM)
      (f_amgJ x2_amgL)
{-# INLINE eLineWidth #-}
ePoints ::
  forall n_ajtb. Control.Lens.Type.Lens' (EnvelopeOpts n_ajtb) Int
ePoints f_amgO (EnvelopeOpts x1_amgP x2_amgQ x3_amgR)
  = fmap
      (\ y1_amgS -> EnvelopeOpts x1_amgP x2_amgQ y1_amgS)
      (f_amgO x3_amgR)
{-# INLINE ePoints #-}

instance OrderedField n => Default (EnvelopeOpts n) where
  def = EnvelopeOpts red medium 32

data TraceOpts n = TraceOpts
  { _tColor   :: Colour Double
  , _tScale   :: n
  , _tMinSize :: n
  , _tPoints  :: Int
  }

-- makeLenses ''TraceOpts
tColor ::
  forall n_amh2.
  Control.Lens.Type.Lens' (TraceOpts n_amh2) (Colour Double)
tColor f_amj8 (TraceOpts x1_amj9 x2_amja x3_amjb x4_amjc)
  = fmap
      (\ y1_amjd -> TraceOpts y1_amjd x2_amja x3_amjb x4_amjc)
      (f_amj8 x1_amj9)
{-# INLINE tColor #-}
tMinSize ::
  forall n_amh2. Control.Lens.Type.Lens' (TraceOpts n_amh2) n_amh2
tMinSize f_amje (TraceOpts x1_amjf x2_amjg x3_amjh x4_amji)
  = fmap
      (\ y1_amjj -> TraceOpts x1_amjf x2_amjg y1_amjj x4_amji)
      (f_amje x3_amjh)
{-# INLINE tMinSize #-}
tPoints ::
  forall n_amh2. Control.Lens.Type.Lens' (TraceOpts n_amh2) Int
tPoints f_amjk (TraceOpts x1_amjl x2_amjm x3_amjn x4_amjo)
  = fmap
      (\ y1_amjp -> TraceOpts x1_amjl x2_amjm x3_amjn y1_amjp)
      (f_amjk x4_amjo)
{-# INLINE tPoints #-}
tScale ::
  forall n_amh2. Control.Lens.Type.Lens' (TraceOpts n_amh2) n_amh2
tScale f_amjq (TraceOpts x1_amjr x2_amjs x3_amjt x4_amju)
  = fmap
      (\ y1_amjv -> TraceOpts x1_amjr y1_amjv x3_amjt x4_amju)
      (f_amjq x2_amjs)
{-# INLINE tScale #-}

instance Floating n => Default (TraceOpts n) where
  def = TraceOpts red (1/100) 0.001 64

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
           => QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
           => OriginOpts n -> QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin' oo d = o <> d
  where o      = strokeP (circle sz)
                   # fc (oo^.oColor)
                   # lw none
                   # fmap (const mempty)
        V2 w h = oo^.oScale *^ size d
        sz     = maximum [w, h, oo^.oMinSize]

-- | Mark the envelope with an approximating cubic spline with control
--   over the color, line width and number of points.
showEnvelope' :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
              => EnvelopeOpts n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
showEnvelope' opts d = cubicSpline True pts # lc (opts^.eColor)
                                            # lw w <> d
  where
    pts = catMaybes [envelopePMay v d | v <- map (`rotateBy` unitX) [0,inc..top]]
    w   = opts ^. eLineWidth
    inc = 1 / fromIntegral (opts^.ePoints)
    top = 1 - inc


-- | Mark the envelope with an approximating cubic spline
--   using 32 points, medium line width and red line color.
showEnvelope :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
             => QDiagram b V2 n Any -> QDiagram b V2 n Any
showEnvelope = showEnvelope' def

-- | Mark the trace of a diagram, with control over colour and scale
-- of marker dot and the number of points on the trace.
showTrace' :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
          => TraceOpts n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
showTrace' opts d =  atPoints ps (repeat pt) <> d
  where
    ps = concatMap p ts
    ts = zip rs vs
    p (r, v) = [origin .+^ (s *^ v) | s <- r]
    vs = map (`rotateBy` unitX) [0, inc..top]
    rs = [getSortedList $ (appTrace . getTrace) d origin v | v <- vs]
    pt = circle sz # fc (opts^.tColor) # lw none
    V2 w h = opts^.tScale *^ size d
    sz     = maximum [w, h, opts^.tMinSize]
    inc = 1 / fromIntegral (opts^.tPoints)
    top = 1 - inc

-- | Mark the trace of a diagram by placing 64 red dots 1/100th its size
--   along the trace.
showTrace :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
          => QDiagram b V2 n Any -> QDiagram b V2 n Any
showTrace = showTrace' def

------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (TypeableFloat n, Renderable (Text n) b, Semigroup m)
           => QDiagram b V2 n m -> QDiagram b V2 n Any
showLabels d =
             ( mconcat
             . map (\(n,p) -> text (simpleName n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) location
             . M.assocs
             $ m
             ) <>
             fmap (const (Any False)) d
  where
    SubMap m = d^.subMap
    simpleName (Name ns) = intercalate " .> " $ map simpleAName ns
    simpleAName (AName n) = show n

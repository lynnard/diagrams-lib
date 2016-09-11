{-# LANGUAGE FlexibleContexts     #-}
-- {-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Parametric.Adjust
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for adjusting the length of parametric objects such as
-- segments and trails.
--
-----------------------------------------------------------------------------
module Diagrams.Parametric.Adjust
    ( adjust
    , AdjustOpts(_adjMethod, _adjSide, _adjEps)
    , adjMethod, adjSide, adjEps
    , AdjustMethod(..), AdjustSide(..)

    ) where

import           Control.Lens        (Lens', {- generateSignatures, -} {- lensRules, makeLensesWith, -} -- (&),
                                      {- (.~), -} (^.))
import           Data.Proxy

import           Data.Default.Class

import           Diagrams.Core.V
import           Diagrams.Parametric

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod n = ByParam n     -- ^ Extend by the given parameter value
                                    --   (use a negative parameter to shrink)
                    | ByAbsolute n  -- ^ Extend by the given arc length
                                    --   (use a negative length to shrink)
                    | ToAbsolute n  -- ^ Extend or shrink to the given
                                    --   arc length

-- | Which side of a segment, trail, or path should be adjusted?
data AdjustSide = Start  -- ^ Adjust only the beginning
                | End    -- ^ Adjust only the end
                | Both   -- ^ Adjust both sides equally
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | How should a segment, trail, or path be adjusted?
data AdjustOpts n = AO { _adjMethod    :: AdjustMethod n
                       , _adjSide      :: AdjustSide
                       , _adjEps       :: n
                       , adjOptsvProxy :: Proxy n
                       }

-- See Diagrams.Combinators for reasoning behind 'Proxy'.

-- makeLensesWith (lensRules & generateSignatures .~ False) ''AdjustOpts
adjEps f_aQYg (AO x1_aQYh x2_aQYi x3_aQYj x4_aQYk)
  = fmap
      (\ y1_aQYl -> AO x1_aQYh x2_aQYi y1_aQYl x4_aQYk) (f_aQYg x3_aQYj)
{-# INLINE adjEps #-}
adjMethod f_aQYm (AO x1_aQYn x2_aQYo x3_aQYp x4_aQYq)
  = fmap
      (\ y1_aQYr -> AO y1_aQYr x2_aQYo x3_aQYp x4_aQYq) (f_aQYm x1_aQYn)
{-# INLINE adjMethod #-}
adjSide f_aQYs (AO x1_aQYt x2_aQYu x3_aQYv x4_aQYw)
  = fmap
      (\ y1_aQYy -> AO x1_aQYt y1_aQYy x3_aQYv x4_aQYw) (f_aQYs x2_aQYu)
{-# INLINE adjSide #-}

-- | Which method should be used for adjusting?
adjMethod :: Lens' (AdjustOpts n) (AdjustMethod n)

-- | Which end(s) of the object should be adjusted?
adjSide :: Lens' (AdjustOpts n) AdjustSide

-- | Tolerance to use when doing adjustment.
adjEps :: Lens' (AdjustOpts n) n

instance Fractional n => Default (AdjustMethod n) where
  def = ByParam 0.2

instance Default AdjustSide where
  def = Both

instance Fractional n => Default (AdjustOpts n) where
  def = AO { _adjMethod    = def
           , _adjSide      = def
           , _adjEps       = stdTolerance
           , adjOptsvProxy = Proxy
           }

-- | Adjust the length of a parametric object such as a segment or
--   trail.  The second parameter is an option record which controls how
--   the adjustment should be performed; see 'AdjustOpts'.
adjust :: (N t ~ n, Sectionable t, HasArcLength t, Fractional n)
       => t -> AdjustOpts n -> t
adjust s opts = section s
  (if opts^.adjSide == End   then domainLower s else getParam s)
  (if opts^.adjSide == Start then domainUpper s else domainUpper s - getParam (reverseDomain s))
 where
  getParam seg = case opts^.adjMethod of
    ByParam p -> -p * bothCoef
    ByAbsolute len -> param (-len * bothCoef)
    ToAbsolute len -> param (absDelta len * bothCoef)
   where
    param        = arcLengthToParam eps seg
    absDelta len = arcLength eps s - len
  bothCoef = if opts^.adjSide == Both then 0.5 else 1
  eps = opts^.adjEps

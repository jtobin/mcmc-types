{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Sampling.Types (
    Transition
  , Chain(..)
  , Target(..)
  ) where

import Control.Monad.Trans.State.Strict (StateT)
import System.Random.MWC.Probability (Prob)

-- | A transition operator.
type Transition m a = StateT a (Prob m) ()

-- | The @Chain@ type specifies the state of a Markov chain at any given
--   iteration.
data Chain a b = Chain {
    chainTarget   :: Target a
  , chainScore    :: !Double
  , chainPosition :: a
  , chainTunables :: Maybe b
  }

instance Show a => Show (Chain a b) where
  show Chain {..} = filter (`notElem` "fromList []") (show chainPosition)

-- | A @Target@ consists of a function from parameter space to the reals, as
--   well as possibly a gradient.
data Target a = Target {
    lTarget  :: a -> Double
  , glTarget :: Maybe (a -> a)
  }


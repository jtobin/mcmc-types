
module Data.Sampling.Types (
    Parameter(..)
  , continuous
  , discrete
  , continuousVector
  , discreteVector
  , Target
  , Parameters
  , Observations
  , createTargetWithGradient
  , createTargetWithoutGradient
  , handleGradient
  , lTarget
  , glTarget
  ) where

import Data.Map (Map)

data Parameter =
    Continuous Double
  | Discrete Int
  | ContinuousVector [Double]
  | DiscreteVector [Int]
  deriving (Eq, Show)

discrete :: Int -> Parameter
discrete = Discrete

continuous :: Double -> Parameter
continuous = Continuous

continuousVector :: [Double] -> Parameter
continuousVector = ContinuousVector

discreteVector :: [Int] -> Parameter
discreteVector = DiscreteVector

instance Num Parameter where
  (Continuous a) + (Continuous b) = Continuous (a + b)
  (Discrete a) + (Discrete b)     = Discrete (a + b)
  _ + _ = noInstanceError

  (Continuous a) - (Continuous b) = Continuous (a - b)
  (Discrete a) - (Discrete b)     = Discrete (a - b)
  _ - _ = noInstanceError

  (Continuous a) * (Continuous b) = Continuous (a * b)
  (Discrete a) * (Discrete b)     = Discrete (a * b)
  _ * _ = noInstanceError

  abs (Continuous a) = Continuous (abs a)
  abs (Discrete a)   = Discrete (abs a)
  abs _ = noInstanceError

  signum (Continuous a) = Continuous (signum a)
  signum (Discrete a)   = Discrete (signum a)
  signum _ = noInstanceError

  fromInteger = Continuous . fromInteger

noInstanceError :: t
noInstanceError = error
  "only continuous and discrete parameters support ring operations"

-- | A @Target@ consists of a function from parameter space to the reals, as
--   well as possibly a gradient.
data Target = Target {
    lTarget  :: Parameters -> Double
  , glTarget :: Maybe (Parameters -> Parameters)
  }

createTargetWithGradient
  :: (Parameters -> Double) -> (Parameters -> Parameters) -> Target
createTargetWithGradient f g = Target f (Just g)

createTargetWithoutGradient :: (Parameters -> Double) -> Target
createTargetWithoutGradient f = Target f Nothing

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

type Environment a = Map String a
type Parameters    = Environment Parameter
type Observations  = Environment Parameter


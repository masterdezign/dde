{-# LANGUAGE BangPatterns #-}
module R1 where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           System.IO.Unsafe ( unsafePerformIO )


model :: Double -> Int -> V.Vector R -> V.Vector R
model hStep delaySamples ttrace0 = ttrace1
  where
    sample0 = Sample 0.0 0.0
    hist0 = V.replicate delaySamples 0.0

    iterator = heun2 (hStep / 2.0) (bandpassRhs (tau, delta) $ (\x -> if x < 0 then -x else 0))

    totalIters = V.length ttrace0

    -- Integrator consumes one additional input
    ttrace = ttrace0 V.++ V.singleton (V.last ttrace0)

    (_, ttrace1) = integrator iterator
                                  delaySamples
                                  totalIters
                                  (sample0, hist0, ttrace)

    (tau, delta) = (2.15625, -3.0)

type R = Double

-- Sample is a vector of any length (x, y, z, ...)
data Sample = Sample {-# UNPACK #-} !R {-# UNPACK #-} !R

getX :: Sample -> R
getX (Sample x _) = x
{-# INLINE getX #-}

-- Pair is used to define exactly a pair of values
data Pair = Pair {-# UNPACK #-} !R {-# UNPACK #-} !R

infixl 6 ..+..
(..+..) :: Sample -> Sample -> Sample
(..+..) (Sample x1 y1) (Sample x2 y2) = Sample (x1 + x2) (y1 + y2)
{-# INLINE (..+..) #-}

infixl 7 .*..
(.*..) :: R -> Sample -> Sample
(.*..) c (Sample x2 y2) = Sample (c * x2) (c * y2)
{-# INLINE (.*..) #-}

heun2
  :: R -> ((Sample, R) -> Sample) -> Sample -> Pair -> Sample
heun2 hOver2 f !x !(Pair x_h x_h2) = x_1
  where
    ! f1 = f (x, x_h)
    ! x_1' = x ..+.. 2 * hOver2 .*.. f1
    ! f2 = f (x_1', x_h2)
    ! x_1 = x ..+.. hOver2 .*.. (f1 ..+.. f2)

bandpassRhs
  :: (R, R) -> (R -> R) -> ((Sample, R) -> Sample)
bandpassRhs !(!ε, !δ) f (!(Sample x y), !x_h) = Sample x' y'
  where
    ! x' = (recip ε) * (-x - δ * y + (f x_h))
    ! y' = x

integrator
  :: (Sample -> Pair -> Sample)
    -> Int
    -> Int
    -> (Sample, V.Vector R, V.Vector R)
    -- ^ Dynamical state, delay history, external forcing
    ->  (Sample, V.Vector R)
integrator iterate1 len total (!xy0, !history0, !input) = unsafePerformIO $ do
    ! v <- VM.new total
    xy <- go v 0 xy0
    history <- V.unsafeFreeze v
    return (xy, history)
  where
    addInput !val !i = val + inputVal
      where ! inputVal = input V.! i
    {-# INLINE addInput #-}
    h i = addInput (history0 `V.unsafeIndex` i) i
    {-# INLINE h #-}
    go !v !i !xy
      | i < len - 1 = do
        let !r = iterate1 xy (Pair (h i) (h $ i + 1))
        VM.unsafeWrite v i (getX r)
        go v (i + 1) r
      | i == total = do
        return xy
      -- Iterations after the initial history has been exhausted
      | otherwise = do
        ! newX0 <- if i == len - 1
                      then return (getX xy0)
                      else VM.unsafeRead v (i - len - 1)
        ! newX <- VM.unsafeRead v (i - len)
        let !r = iterate1 xy (Pair (addInput newX0 i) (addInput newX (i + 1)))
        VM.unsafeWrite v i (getX r)
        go v (i + 1) r
{-# INLINE integrator #-}


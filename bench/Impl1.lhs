% How to compile a pdf:
% $ cabal exec lhs2TeX -- -o MackeyGlass.tex MackeyGlass.lhs && pdflatex \
%   MackeyGlass.tex

\documentclass{article}
%include polycode.fmt

\title{Mackey-Glass DDE}
\author{Bogdan Penkovsky}

\begin{document}

\maketitle

This document describes a numeric model of the Mackey-Glass DDE
optimized for speed. The fourth-order Runge-Kutta integration method is employed.

> {-# LANGUAGE BangPatterns #-}
> module Impl1 ( mgModel ) where

> import qualified Data.Vector.Storable as V
> import qualified Data.Vector.Storable.Mutable as VM
> import System.IO.Unsafe ( unsafePerformIO )
> import Control.Monad
> import Control.Lens hiding ((<.>))


Sample can be a vector of any length (x, y, z, ...).
We import V1 for single-component vector (scalar) Samples.

> import Linear.V1 as V1

> type Sample = V1.V1
> type Delay = V.Vector
> type Input = V.Vector

Autonomous system integrator

iterate1 is the function describing DDE system;
len1 is the number of delay elements in a delay;
krecord is the number of last samples to record;
total is the total number of iterations;

> integrator'
>   :: (VM.Storable a, Floating a) =>
>     (Sample a -> (a, a) -> Sample a)
>     -> Parameters a
>     -> Int
>     -> Int
>     -> Int
>     -> (Sample a, Delay a, Input a)
>     -> (Sample a, V.Vector a)
> integrator' iterate1 _ len1 krecord total (!xy0, !hist0, _) = a
>   where
>     a = unsafePerformIO $ do
>       ! v <- VM.new (len1 + total)  -- Delay history
>       -- Copy the initial history values
>       copyHist v hist0
> 
>       -- Calculate the rest of the vector
>       xy' <- go v len1 xy0
> 
>       trace <- V.unsafeFreeze v
>       return (xy', V.slice (len1 + total - krecord) krecord trace)
> 
>     -- Copy initial conditions
>     copyHist !v !hist =
>       mapM_ (\i -> VM.unsafeWrite v i (hist V.! i)) [0..V.length hist - 1]
> 
>     go !v !i !xy
>       | i == len1 + total =
>           return xy
>       | otherwise = do
>         x_tau1 <- VM.unsafeRead v (i - len1)  -- Delayed element
>         x_tau1' <- VM.unsafeRead v (i - len1 + 1)  -- The next one
>         let !xy' = iterate1 xy (x_tau1, x_tau1')
>             !x' = xy' ^._x
>         VM.unsafeWrite v i x'
>         go v (i + 1) xy'

%if False

> {-# SPECIALISE integrator' ::
>   (Sample Float -> (Float, Float) -> Sample Float)
>     -> Parameters Float
>     -> Int
>     -> Int
>     -> Int
>     -> (Sample Float, Delay Float, Input Float)
>     -> (Sample Float, V.Vector Float) #-}
> {-# SPECIALISE integrator' ::
>   (Sample Double -> (Double, Double) -> Sample Double)
>     -> Parameters Double
>     -> Int
>     -> Int
>     -> Int
>     -> (Sample Double, Delay Double, Input Double)
>     -> (Sample Double, V.Vector Double) #-}

%endif

> data Parameters a = Parameters {
>                              pBeta :: a
>                              , pGamma :: a
>                              } deriving Show

> param1 :: Parameters Double
> param1 = Parameters {
>                      pBeta = 0.2
>                      , pGamma = 0.1
>                      }

The Mackey-Glass model.

> mackeyGlass
>   :: Floating a => Parameters a -> ((Sample a, a) -> Sample a)
> mackeyGlass p (V1.V1 !x, !x_tau1) = V1.V1 x'
>   where
>     ! x' = beta * x_tau1 / (1 + x_tau1^10) - gamma * x
>     beta = pBeta p
>     gamma = pGamma p

%if False

> {-# SPECIALISE mackeyGlass ::
>   Parameters Float -> (Sample Float, Float) -> Sample Float #-}
> {-# SPECIALISE mackeyGlass ::
>   Parameters Double -> (Sample Double, Double) -> Sample Double #-}

%endif

Fourth-order Runge-Kutta for a 1D system with a single delay $\tau_1$.

> rk4 :: Double
>   -> ((Sample Double, Double) -> Sample Double)
>   -> Sample Double -> (Double, Double) -> Sample Double
> rk4 hStep sys !xy (!x_tau1, !x_tau1') = xy_next
>   where
>     xy_next = xy + over6 * (a + x2 * b + x2 * c + d)
>     over6 = V1.V1 (recip 6)
>     over2 = V1.V1 (recip 2)
>     x2 = V1.V1 2
>     h = V1.V1 hStep
>     ! a = h * sys (xy, x_tau1)
>     ! b = h * sys (xy + over2 * a, x_tau1_b)
>     ! c = h * sys (xy + over2 * b, x_tau1_c)
>     ! d = h * sys (xy + c, x_tau1')
>     ! x_tau1_b = (x_tau1 + x_tau1') / 2
>     ! x_tau1_c = x_tau1_b

Returns the last delay

> fastIntegrRk4 :: Double -> Int -> Int -> Int -> (V.Vector Double, Double)
>            -> Parameters Double -> (V.Vector Double, Double)
> fastIntegrRk4 hStep len1 len2 totalIters (hist0, x0) p = (data1, x1)
>   where sample0 = V1.V1 x0
>         -- Iterator implements Runge-Kutta schema
>         iterator = rk4 hStep (mackeyGlass p)
>         -- Record only the last long delay
>         (V1.V1 x1, data1) = integrator' iterator p len1 len1 totalIters (sample0, hist0, V.fromList [])

Records the whole time trace $x(t)$

> fastIntegrRk4' :: Double -> Int -> Int -> (V.Vector Double, Double)
>            -> Parameters Double -> (V.Vector Double, Double)
> fastIntegrRk4' hStep len1 totalIters (hist0, x0) p = (data1, x1)
>   where sample0 = V1.V1 x0
>         -- Iterator implements Runge-Kutta schema
>         iterator = rk4 hStep (mackeyGlass p)
>         -- Record all the time trace
>         (V1.V1 x1, data1) = integrator' iterator p len1 totalIters totalIters (sample0, hist0, V.fromList [])

Constant initial conditions

> initCondConst :: Int -> Double -> [Double]
> initCondConst = replicate

Define a Mackey-Glass simulation, the final state is the result

> mgModel :: Double -> Int -> Double
> mgModel hStep total = s
>   where
>     len1 = 17 * round (recip hStep)  -- tauD = 17, delay time
>
>     icond0 = V.fromList $ initCondConst len1 0.2
>
>     -- Integrate an autonomous system (no external input)
>     (_, s) = fastIntegrRk4' hStep len1 total (icond0, 0.2) param1

\end{document}

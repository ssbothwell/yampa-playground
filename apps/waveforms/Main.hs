{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Main where

import Data.Ratio
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import FRP.Yampa

arrowApply :: (Functor (a b), Arrow a) => ((c, c) -> d) -> a b c -> a b c -> a b d
arrowApply f x y = fmap f $ x &&& y

addS :: SF a Double -> SF a Double -> SF a Double
addS = arrowApply (uncurry (+))

zero :: SF a Double
zero = constant 0

one :: SF a Double
one = constant 1

rampUp :: SF a Double
rampUp = one >>> integral

rampUpFrom :: Double -> SF a Double
rampUpFrom n = rampUp >>^ (+ n)

rampUpBy2 :: SF a Double
rampUpBy2 = addS rampUp rampUp

rampDown :: SF a Double
rampDown = negate <$> rampUp

rampDownFrom :: Double -> SF a Double
rampDownFrom n = rampDown >>^ (+ n)

sawTooth :: SF a Double
sawTooth = switch sf cont
  where
    sf :: SF a (Double, Event ())
    sf = proc _ -> do
      x <- rampUp -< ()
      e <- edge -< x >= 10
      returnA -< (x, e)
    cont _ = sawTooth

triangle :: SF a Double
triangle = switch up cont
  where
    up :: SF a (Double, Event Bool)
    up = proc _ -> do
      x <- rampUpFrom 0 -< ()
      e <- edge -< x >= 10
      returnA -< (x, e `tag` True)

    down :: SF a (Double, Event Bool)
    down = proc _ -> do
      x <- rampDownFrom 10 -< ()
      e <- edge -< x < 1
      returnA -< (x, e `tag` False)

    cont True = switch down cont
    cont False = switch up cont

square :: Time -> SF a Double
square t = switch on cont
  where
    pulseWidth :: SF a (Event ())
    pulseWidth = repeatedly t ()

    on :: SF a (Double, Event Bool)
    on = proc _ -> do
      e <- pulseWidth -< ()
      returnA -< (1, e `tag` False)

    off :: SF a (Double, Event Bool)
    off = proc _ -> do
      e <- pulseWidth -< ()
      returnA -< (0, e `tag` True)

    cont True = switch on cont
    cont False = switch off cont

------------
--- Main ---
------------

main :: IO ()
main = reactimate (return NoEvent) produceInput handleOutput pipeline
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event ()))
      produceInput _ = do
        threadDelay 30000
        return (1, Just $ Event ())
      handleOutput :: Bool -> Double -> IO Bool
      handleOutput _ y = print y >> pure False
      pipeline :: SF a Double
      pipeline = square 10


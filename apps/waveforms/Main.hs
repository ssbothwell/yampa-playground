{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Main where

import Data.Bifunctor hiding (second)
import Data.Ratio
import Data.Monoid
import Data.Vector.Storable (Vector(..), fromList, toList)
import Control.Applicative
import Control.Concurrent
import FRP.Yampa
import FRP.Yampa.EventS (sampleWindow)

import qualified SDL
import SDL (Window, Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))

import Foreign.C.Types

zero :: SF a Double
zero = constant 0

one :: SF a Double
one = constant 1

sinewave :: SF a Double
sinewave = proc input -> do
  let freq = 1
  phi <- integral -< 2 * pi * freq
  returnA -< sin phi

squarewave :: SF a Double
squarewave = sinewave >>^ signum

sawtoothwave :: SF a Double
sawtoothwave = proc _ -> do
  t <- time -< ()
  returnA -< t - fromIntegral (floor t)

trianglewave :: SF a Double
trianglewave = proc _ -> do
  phi <- integral -< 2 * pi
  returnA -< asin $ sin phi

scope :: SF (DTime, Double) (Event (Vector (Point V2 CInt)))
scope = bias >>> modulo >>> scaleX *** scaleY >>> sampleWindow samples interval >>> toFloor >>> toPoints >>> toVector
  where
    samples :: Int
    samples = 10

    interval = 1 / fromIntegral samples

    bias :: SF (Double, Double) (Double, Double)
    bias = second $ arr (+1)

    modulo :: SF (Double, Double) (Double, Double)
    modulo = FRP.Yampa.first $ arr $ \t -> t - fromIntegral (floor (t / fromIntegral samples)) * fromIntegral samples

    scaleX :: SF Double Double
    scaleX = arr (* (windowW / fromIntegral samples))

    scaleY :: SF Double Double
    scaleY = arr (* (windowH / 2))

    toFloor :: SF (Event [(DTime, Double)]) (Event [(CInt, CInt)])
    toFloor = arr $ (fmap . fmap) (bimap floor floor)

    toPoints :: SF (Event [(CInt, CInt)]) (Event [Point V2 CInt])
    toPoints = arr $ (fmap . fmap) (\(x, y) -> P $ V2 x y)

    toVector :: SF (Event [Point V2 CInt]) (Event (Vector (Point V2 CInt)))
    toVector = arr $ fmap fromList

arrowApply :: (Functor (a b), Arrow a) => ((c, c) -> d) -> a b c -> a b c -> a b d
arrowApply f x y = fmap f $ x &&& y

addS :: SF a Double -> SF a Double -> SF a Double
addS = arrowApply (uncurry (+))

-------------------
--- Interaction ---
-------------------

keypress :: SF AppInput (Event SDL.Scancode)
keypress = inpKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keypress >>^ filterE (code ==) >>^ tagWith ()

data AppInput = AppInput
  { inpKeyPressed :: Maybe SDL.Scancode
  , inpQuit       :: Bool
  }

initAppInput :: AppInput
initAppInput = AppInput Nothing False

parseSDLInput :: SF (Event SDL.EventPayload) AppInput
parseSDLInput = accumHoldBy nextAppInput initAppInput

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp (SDL.KeyboardEvent e) =
  case SDL.keyboardEventKeyMotion e of
    SDL.Pressed  -> inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym e }
    SDL.Released -> inp { inpKeyPressed = Nothing}
nextAppInput inp _ = inp

shouldExit :: SF AppInput Bool
shouldExit = quitTrigger >>^ isEvent

quitTrigger :: SF AppInput (Event ())
quitTrigger = proc input -> do
  qButtonTap <- keyPressed SDL.ScancodeQ -< input
  returnA -< qButtonTap

-----------------
--- Rendering ---
-----------------

-----------
--- SDL ---
-----------

data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown | Black
  deriving Show

windowW :: Num a => a
windowW = 1000

windowH :: Num a => a
windowH = 500

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 windowW windowH }

renderObject :: Renderer -> (SDL.Rectangle CInt, Color) -> IO ()
renderObject r (rect, color) = do
  setDrawColor r color
  SDL.fillRect r (Just rect)

clearFrame :: Renderer -> IO ()
clearFrame renderer = do
  setDrawColor renderer Black
  SDL.clear renderer

setDrawColor :: Renderer -> Color -> IO ()
setDrawColor renderer color =
  case color of
    Red      -> SDL.rendererDrawColor renderer $= V4 255 0 0 0
    Blue     -> SDL.rendererDrawColor renderer $= V4 0 0 255 0
    BabyBlue -> SDL.rendererDrawColor renderer $= V4 0 235 255 0
    Green    -> SDL.rendererDrawColor renderer $= V4 120 200 15 0
    White    -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
    Black    -> SDL.rendererDrawColor renderer $= V4 0 0 0 0
    Brown    -> SDL.rendererDrawColor renderer $= V4 150 90 25 0
    Yellow   -> SDL.rendererDrawColor renderer $= V4 255 200 50 0

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = setDrawColor renderer color >> SDL.clear renderer

draw :: Renderer -> Vector (Point V2 CInt) -> IO ()
draw renderer points = do
  clearFrame renderer
  setDrawColor renderer Red
  SDL.drawLines renderer points
  SDL.present renderer

initSDL :: IO (Renderer, Window)
initSDL = do
  SDL.initializeAll
  window' <- SDL.createWindow "My Window" window
  renderer <- SDL.createRenderer window' 0 SDL.defaultRenderer
  return (renderer, window')

------------
--- Main ---
------------

main :: IO ()
main = do
  (renderer, window') <- initSDL
  reactimate (return NoEvent) produceInput (handleOutput renderer) pipeline
  SDL.destroyRenderer renderer
  SDL.destroyWindow window'
  SDL.quit
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
      produceInput _ = do
        let sampleRate = 0.1
        threadDelay 30000
        mevent <- SDL.pollEvent
        case mevent of
          Just e -> return (sampleRate, Just . Event $ SDL.eventPayload e)
          Nothing -> return (sampleRate, Nothing)

      handleOutput :: Renderer -> Bool -> (Event (Vector (Point V2 CInt)), Bool) -> IO Bool
      handleOutput r _ (e, exitBool) =
        case e of
          Event pts -> draw r pts >> print (toList pts) >> pure exitBool
          NoEvent -> pure exitBool

      pipeline :: SF (Event SDL.EventPayload) (Event (Vector (Point V2 CInt)), Bool)
      pipeline = parseSDLInput >>> ((time &&& sinewave) >>> scope) &&& shouldExit


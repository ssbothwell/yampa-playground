{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Main where

import Data.Bifunctor
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

square :: DTime -> SF a Double
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
      returnA -< (-1, e `tag` True)

    cont True = switch on cont
    cont False = switch off cont

squareWave :: SF a Double
squareWave = sinewave >>^ signum

sawtoothwave :: SF a Double
sawtoothwave = proc _ -> do
  t <- time -< ()
  returnA -< t - fromIntegral (floor t)

trianglewave :: SF a Double
trianglewave = proc _ -> do
  phi <- integral -< 2 * pi
  returnA -< asin $ sin phi

sinewave :: SF a Double
sinewave = proc input -> do
  let freq = 2
  phi <- integral -< 2 * pi * freq
  returnA -< sin phi

scope :: SF (DTime, Double) (Event (Vector (Point V2 CInt)))
scope = sampleWindow 10 1 >>> bias >>> scale >>> toFloor >>> toModulo >>> toPoints >>> toVector
  where
    bias :: SF (Event [(DTime, Double)]) (Event [(DTime, Double)])
    bias = arr $ (fmap . fmap) (bimap (+ 1) (+ 1))

    scale :: SF (Event [(DTime, Double)]) (Event [(DTime, Double)])
    scale = arr $ (fmap . fmap) (bimap (* windowW) (* windowH))

    toFloor :: SF (Event [(DTime, Double)]) (Event [(CInt, CInt)])
    toFloor = arr $ (fmap . fmap) (bimap floor floor)

    toModulo :: SF (Event [(CInt, CInt)]) (Event [(CInt, CInt)])
    toModulo = arr $ (fmap . fmap) (Data.Bifunctor.first (`mod` 10))

    toPoints :: SF (Event [(CInt, CInt)]) (Event [Point V2 CInt])
    toPoints = arr $ (fmap . fmap) (\(x, y) -> P $ V2 x y)

    toVector :: SF (Event [Point V2 CInt]) (Event (Vector (Point V2 CInt)))
    toVector = arr $ fmap fromList

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

data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown
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
  setDrawColor renderer White
  SDL.clear renderer

setDrawColor :: Renderer -> Color -> IO ()
setDrawColor renderer color =
  case color of
    Red      -> SDL.rendererDrawColor renderer $= V4 255 0 0 0
    Blue     -> SDL.rendererDrawColor renderer $= V4 0 0 255 0
    BabyBlue -> SDL.rendererDrawColor renderer $= V4 0 235 255 0
    Green    -> SDL.rendererDrawColor renderer $= V4 120 200 15 0
    White    -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
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
        threadDelay 1000
        mevent <- SDL.pollEvent
        case mevent of
          Just e -> return (0.01, Just . Event $ SDL.eventPayload e)
          Nothing -> return (0.01, Nothing)

      handleOutput :: Renderer -> Bool -> (Event (Vector (Point V2 CInt)), Bool) -> IO Bool
      handleOutput r _ (e, exitBool) =
        case e of
          Event pts -> draw r pts >> print (toList pts) >> pure exitBool
          NoEvent -> print () >> pure exitBool

      pipeline :: SF (Event SDL.EventPayload) (Event (Vector (Point V2 CInt)), Bool)
      pipeline = parseSDLInput >>> ((time &&& zero) >>> scope) &&& shouldExit


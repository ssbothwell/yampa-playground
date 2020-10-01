{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Main where

import Control.Monad
import Data.Bifunctor hiding (second)
import Data.Ratio
import Data.List (unfoldr)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector.Storable (Vector(..), fromList, toList)
import Control.Applicative
import Control.Concurrent
import FRP.Yampa
import FRP.Yampa.EventS (sampleWindow, sample)

import qualified SDL
import SDL (Window, Renderer, Texture, Point(..), V4(..), V2(..), WindowConfig(..), ($=))
import SDL.Video.Renderer (Rectangle(..))
import qualified SDL.Font
import SDL.Raw.Video (renderCopy)

import Foreign.C.Types

zero :: SF a Double
zero = constant 0

one :: SF a Double
one = constant 1

sinewave :: SF a Double
sinewave = proc input -> do
  let freq = 1
      phase = 0
      amplitude = 1
  phi <- integral -< 2 * pi * freq
  returnA -< amplitude * sin (phi + phase)

squarewave :: SF a Double
squarewave = sinewave >>^ signum

--trianglewave' :: SF a Double
--trianglewave' = sinewave >>^ asin

sawtoothwave :: SF a Double
sawtoothwave = proc _ -> do
  let freq = 2
      phase = 0
      amplitude = 0.5
  t <- time -< ()
  returnA -< ((t * freq) - fromIntegral (floor (t * freq))) * amplitude

trianglewave :: SF a Double
trianglewave = proc _ -> do
  let freq = 1
      phase = 0
      amplitude = 0.63
  phi <- integral -< 2 * pi * freq
  returnA -< amplitude * asin (sin $ phi + phase)

scope :: Scope -> SF (DTime, Double) (Scope, Event [Point V2 CInt])
scope s@Scope{ amplitudeBase, timeBase } =
  constant s &&&
  (scaleX *** scaleY >>> invert >>> bias >>> modulo >>> sampleWindow samples interval >>> toFloor >>> toPoints)
  where
    samples :: Int
    samples = 1000

    interval = fromIntegral timeBase / fromIntegral samples

    bias :: SF (Double, Double) (Double, Double)
    bias = second $ arr (+ (windowH / 2))

    modulo :: SF (Double, Double) (Double, Double)
    modulo = FRP.Yampa.first $ arr $ \t -> t - fromIntegral (floor (t / windowW)) * windowW

    scaleX :: SF Double Double
    scaleX = arr (* (windowW / fromIntegral timeBase))

    scaleY :: SF Double Double
    scaleY = arr (* ((windowH / (fromIntegral amplitudeBase / 2)) / 2))

    toFloor :: SF (Event [(DTime, Double)]) (Event [(CInt, CInt)])
    toFloor = arr $ (fmap . fmap) (bimap floor floor)

    toPoints :: SF (Event [(CInt, CInt)]) (Event [Point V2 CInt])
    toPoints = arr $ (fmap . fmap) (\(x, y) -> P $ V2 x y)

    invert :: SF (Double, Double) (Double, Double)
    invert = second $ arr $ \x -> if x == 0 then x else -x

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

-----------
--- SDL ---
-----------

data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown | Black | Grey
  deriving Show

windowW :: Num a => a
windowW = 400 * 2

windowH :: Num a => a
windowH = 300 * 2

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

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0

type Size = Int
type Position = (Int, Int)
drawText :: Renderer -> Size -> Position -> Text -> IO ()
drawText r size (xpos, ypos) t = do
  font <- SDL.Font.load "Anonymous.ttf" 70
  text <- SDL.Font.blended font red t
  SDL.Font.free font
  texture <- SDL.createTextureFromSurface r text
  SDL.freeSurface text
  let width = fromIntegral $ size * T.length t
      height = fromIntegral $ size * 2
      xpos' = fromIntegral xpos
      ypos' = fromIntegral ypos
  SDL.copy r texture Nothing (Just $ Rectangle (P $ V2 xpos' ypos') (V2 width height))

setDrawColor :: Renderer -> Color -> IO ()
setDrawColor renderer color =
  case color of
    Red      -> SDL.rendererDrawColor renderer $= V4 255 0 0 0
    Blue     -> SDL.rendererDrawColor renderer $= V4 0 0 255 0
    BabyBlue -> SDL.rendererDrawColor renderer $= V4 0 235 255 0
    Green    -> SDL.rendererDrawColor renderer $= V4 120 200 15 0
    Grey     -> SDL.rendererDrawColor renderer $= V4 191 191 191 191
    White    -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
    Black    -> SDL.rendererDrawColor renderer $= V4 0 0 0 0
    Brown    -> SDL.rendererDrawColor renderer $= V4 150 90 25 0
    Yellow   -> SDL.rendererDrawColor renderer $= V4 255 200 50 0

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = setDrawColor renderer color >> SDL.clear renderer

pair :: [a] -> [(a, a)]
pair = unfoldr f
  where
    f :: [a] -> Maybe ((a, a), [a])
    f (x:y:xs) = Just ((x, y), xs)
    f _ = Nothing

-- 1cps 10divs 0.1s/div
drawTimeDivisons :: Renderer -> TimeBase -> TimeInterval -> IO ()
drawTimeDivisons r tb ti =
  let numDivisions = fromIntegral tb / ti
      divisions = [(P $ V2 t 0, P $ V2 t windowH) | t <- floor . (* windowW) <$> [ti, ti * 2 .. numDivisions]]

  in setDrawColor r Grey >>
     mapM_ (uncurry $ SDL.drawLine r) divisions

drawAmplitudeDivisions :: Renderer -> AmpBase -> AmpInterval -> IO ()
drawAmplitudeDivisions r ab ai =
  let numDivisions = fromIntegral ab / ai
      divisions = [(P $ V2 0 a, P $ V2 windowW a) | a <- floor . (* windowH) <$> [ai, ai * 2 .. numDivisions]]

  in setDrawColor r Grey >>
     mapM_ (uncurry $ SDL.drawLine r) divisions

drawSignal :: Renderer -> [Point V2 CInt] -> IO ()
drawSignal r pts =
  let f :: (Point V2 CInt, Point V2 CInt) -> IO ()
      f (a@(P (V2 x _)), b@(P (V2 x' _))) =
        if abs (x - x') >= floor (windowH / 2)
        then pure ()
        else SDL.drawLine r a b
  in mapM_ f $ pair pts

drawLines :: (Window, Renderer) -> Scope -> [Point V2 CInt] -> IO ()
drawLines (w, r) Scope{ amplitudeBase, amplitudeInterval, timeBase, timeInterval } points = do
  clearFrame r
  drawTimeDivisons r timeBase timeInterval
  drawAmplitudeDivisions r amplitudeBase amplitudeInterval
  setDrawColor r Red
  drawText r 20 (0, 520) $ T.concat ["amp  base: ", (T.pack . show) amplitudeBase, "v"]
  drawText r 20 (0, 560) $ T.concat ["time base: ", (T.pack . show) timeBase, "s"]
  drawSignal r points
  SDL.present r

initSDL :: IO (Renderer, Window)
initSDL = do
  SDL.initializeAll
  SDL.Font.initialize
  window' <- SDL.createWindow "My Window" window
  renderer <- SDL.createRenderer window' 0 SDL.defaultRenderer
  return (renderer, window')

------------
--- Main ---
------------

type AmpBase      = CInt
type AmpInterval  = Double
type TimeBase     = CInt
type TimeInterval = Double

data Scope = Scope
  { amplitudeBase     :: AmpBase
  , amplitudeInterval :: AmpInterval
  , timeBase          :: TimeBase
  , timeInterval      :: TimeInterval
  , positivePeak      :: Maybe Double
  , negativePeak      :: Maybe Double
  }

scopeConfig :: Scope
scopeConfig = Scope
  { amplitudeBase = 4
  , amplitudeInterval = 0.1
  , timeBase = 4
  , timeInterval = 0.1
  , positivePeak = Nothing
  , negativePeak = Nothing
  }


main :: IO ()
main = do
  (r, w) <- initSDL
  reactimate (return NoEvent) produceInput (handleOutput (w, r)) pipeline
  SDL.destroyRenderer r
  SDL.destroyWindow w
  SDL.quit
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
      produceInput _ = do
        let sampleRate = 0.0001
        mevent <- SDL.pollEvent
        case mevent of
          Just e -> return (sampleRate, Just . Event $ SDL.eventPayload e)
          Nothing -> return (sampleRate, Nothing)

      handleOutput :: (Window, Renderer) -> Bool -> ((Scope, Event [Point V2 CInt]), Bool) -> IO Bool
      handleOutput (w, r) _ ((s, e), exitBool) =
        case e of
          Event pts -> drawLines (w, r) s pts >> pure exitBool
          NoEvent -> pure exitBool

      pipeline :: SF (Event SDL.EventPayload) ((Scope, Event [Point V2 CInt]), Bool)
      pipeline = parseSDLInput >>> ((time &&& sinewave) >>> scope scopeConfig) &&& shouldExit

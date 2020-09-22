{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Main where

import Data.Ratio
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import FRP.Yampa
import qualified SDL
import SDL (Window, Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))

import Foreign.C.Types

data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown
  deriving Show

data Cube = Cube { _x :: Double, _y :: Double, _xv :: Double, _yv :: Double }
  deriving Show

windowW :: Num a => a
windowW = 400

windowH :: Num a => a
windowH = 800

initCube :: Double -> Double -> Cube
initCube = Cube 200 800


---------------------
--- SDL Rendering ---
---------------------

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 windowW windowH }

renderObject :: Renderer -> (SDL.Rectangle CInt, Color) -> IO ()
renderObject r (rect, color) = do
  setDrawColor r color
  SDL.fillRect r (Just rect)

mkSdlRect :: (Integral a, Num a) => Cube -> (SDL.Rectangle a, Color)
mkSdlRect (Cube x y _ _) = (SDL.Rectangle (P (V2 (round x) (round (windowH - y)))) (V2 20 20), Red)

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

draw :: Renderer -> Cube -> IO ()
draw renderer rect = do
  clearFrame renderer
  renderObject renderer (mkSdlRect rect)
  SDL.present renderer

initSDL :: IO (Renderer, Window)
initSDL = do
  SDL.initializeAll
  window' <- SDL.createWindow "Yampy Cube Clone" window
  renderer <- SDL.createRenderer window' 0 SDL.defaultRenderer
  return (renderer, window')

----------------
-- USER INPUT --
----------------

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

------------------------
--- Signal Functions ---
------------------------

shouldExit :: SF AppInput Bool
shouldExit = quitTrigger >>^ isEvent

quitTrigger :: SF AppInput (Event ())
quitTrigger = proc input -> do
  qButtonTap <- keyPressed SDL.ScancodeQ -< input
  returnA -< qButtonTap

downVect :: SF a (Double, Double)
downVect = constant (0, -50)

upVect :: SF a (Double, Double)
upVect = constant (0, 50)

leftVect :: SF a (Double, Double)
leftVect = constant (-50, 0)

rightVect :: SF a (Double, Double)
rightVect = constant (50, 0)

gravity :: SF a (Double, Double)
gravity = const 0 &&& const (-9.81) ^>> integral

arrowApply :: (Functor (a b), Arrow a) => ((c, c) -> d) -> a b c -> a b c -> a b d
arrowApply f x y = fmap f $ x &&& y

addVect :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVect (x1, y1) (x2, y2) = (x1+x2, y1+y2)

infixr 2 +>>
(+>>) :: SF a (Double, Double) -> SF a (Double, Double) -> SF a (Double, Double)
(+>>) a b = (arrowApply $ uncurry addVect) a b

infixr 2 <<+
(<<+) :: SF a (Double, Double) -> SF a (Double, Double) -> SF a (Double, Double)
(<<+) b a = (arrowApply $ uncurry addVect) a b

interpolate :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
interpolate ratio (x1, y1) (x2, y2) =
  let (x1', y1') = (x1 * ratio, y1 * ratio)
      (x2', y2') = (x2 * ratio, y2 * (1 - ratio))
  in (x1' + x2', y1' + y2')

move :: Cube -> SF a Cube
move (Cube x0 y0 _ _) = proc _ -> do
  (xv0, yv0) <- leftVect <<+ downVect -< ()
  y <- (y0 +) ^<< integral -< yv0
  x <- (x0 +) ^<< integral -< xv0
  returnA -< Cube x y xv0 yv0

fallingCube :: Cube -> SF a Cube
fallingCube (Cube x y xv yv0) = proc _ -> do
  v <- (yv0 +) ^<< integral -< -9.81
  y' <- (y +) ^<< integral -< v
  returnA -< Cube x y' xv v


x = repeatedly 10 ()

leftRightCube' :: Cube -> SF a Cube
leftRightCube' cube = switch (sf cube) cont
  where
    sf :: Cube -> SF a (Cube, Event Cube)
    sf (Cube x y xv yv) = proc input -> do
      x1 <- (x +) ^<< integral -< xv
      e <- repeatedly 2 () -< ()
      returnA -< (Cube x1 y xv yv, e `tag` Cube x1 y xv yv)
    cont :: Cube -> SF a Cube
    cont (Cube x y xv yv) = leftRightCube' (Cube x y (-xv) yv)

downCube :: SF Cube Cube
downCube = proc input -> do
  Cube x y xv yv  <- identity -< input
  yv' <- integral -< yv
  returnA -< Cube x y xv yv'

leftRightCube :: Cube -> SF a Cube
leftRightCube cube = switch (sf cube) cont
  where
    sf :: Cube -> SF a (Cube, Event Cube)
    sf (Cube x y xv yv) = proc input -> do
      x1 <- (x +) ^<< integral -< xv
      e1 <- edge -< x1 <= 0
      e2 <- edge -< x1 >= 380
      returnA -< (Cube x1 y xv yv, (e1 <|> e2) `tag` Cube x1 y xv yv)
    cont :: Cube -> SF a Cube
    cont (Cube x y xv yv) = leftRightCube (Cube x y (-xv) yv)

bouncingCube :: Cube -> SF a Cube
bouncingCube cube = switch (sf cube) cont
  where
    sf :: Cube -> SF a (Cube, Event Cube)
    sf cube' = proc input -> do
      Cube x y xv yv <- fallingCube cube' -< input
      e <- edge -< y <= 20
      returnA -< (Cube x y xv yv, e `tag` Cube x y xv yv)
    cont :: Cube -> SF a Cube
    cont (Cube x y xv yv) = bouncingCube (Cube x y xv (-yv * 0.7))

game :: SF AppInput Cube
game = proc input -> do
  cube <- downCube <<< leftRightCube' (initCube (-20) 0) -< input
  returnA -< cube


------------
--- Main ---
------------

mainLoop :: IO ()
mainLoop = do
  (renderer, window') <- initSDL
  reactimate (return NoEvent) produceInput (handleOutput renderer) pipeline
  SDL.destroyRenderer renderer
  SDL.destroyWindow window'
  SDL.quit
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
      produceInput _ = do
        threadDelay 30000
        mevent <- SDL.pollEvent
        case mevent of
          Just e -> return (0.1, Just . Event $ SDL.eventPayload e)
          Nothing -> return (0.1, Nothing)
      handleOutput :: Renderer -> Bool -> (Cube, Bool) -> IO Bool
      handleOutput r _ (gameState, exitBool) =
        draw r gameState >> return exitBool
      pipeline :: SF (Event SDL.EventPayload) (Cube, Bool)
      pipeline = parseSDLInput >>> (game &&& shouldExit)

main :: IO ()
main = mainLoop

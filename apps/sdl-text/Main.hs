{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad
import Data.Bifunctor hiding (second)
import Data.Ratio
import Data.List (unfoldr)
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Data.Text

import qualified SDL
import SDL (Window, Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))
import SDL.Video.Renderer (Rectangle(..))
import qualified SDL.Font

windowW :: Num a => a
windowW = 400 * 2

windowH :: Num a => a
windowH = 300 * 2

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 windowW windowH }

clearFrame :: Renderer -> IO ()
clearFrame renderer = do
  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.clear renderer

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0

drawText :: SDL.Window -> IO ()
drawText window = do
  SDL.showWindow window
  font <- SDL.Font.load "/home/solomon/Development/haskell/yampa-playground/apps/sdl-text/Gidole-Regular.ttf" 70
  text <- SDL.Font.solid font red "Solid!"
  SDL.Font.free font
  screen <- SDL.getWindowSurface window
  SDL.surfaceBlit text Nothing screen Nothing
  SDL.freeSurface text
  SDL.updateWindowSurface window

initSDL :: IO (Renderer, Window)
initSDL = do
  SDL.initializeAll
  SDL.Font.initialize
  window' <- SDL.createWindow "My Window" window
  renderer <- SDL.createRenderer window' 0 SDL.defaultRenderer
  return (renderer, window')

eventLoop :: Renderer -> Window -> IO ()
eventLoop r w = do
  mevent <- fmap SDL.eventPayload <$> SDL.pollEvent
  case mevent of
    Just (SDL.KeyboardEvent e) ->
      case SDL.keyboardEventKeyMotion e of
        SDL.Pressed -> do
          SDL.destroyRenderer r
          SDL.destroyWindow w
          SDL.Font.quit
          SDL.quit
    _ -> do
      --clearFrame r
      drawText w
      SDL.present r
      eventLoop r w

main :: IO ()
main = initSDL >>= uncurry eventLoop

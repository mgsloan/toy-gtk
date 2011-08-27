-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFramework.Core
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Core toy framework - a simplifying interface over Gtk and Cairo.

module Graphics.ToyFramework.Core
  ( Toy(..), KeyTable, KeyInfo, IRect, IPnt
  , dummyToy, runToy, quit
  ) where 

import Control.Arrow
import Control.Monad (when)
import Data.IORef
import qualified Data.Map as M
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as E
import qualified Graphics.Rendering.Cairo as C
import System.IO.Unsafe

type KeyInfo = (Bool, G.TimeStamp, [G.Modifier])
type KeyTable = M.Map String KeyInfo

type IPnt = (Int, Int)
type IRect = (IPnt, IPnt)

data Toy a = Toy
  { initialState :: a

  -- Given the current keyboard state, perform a single 30ms periodic execution.
  , tick    :: KeyTable                              -> a -> IO a

  -- Display using cairo, based on the canvas size and dirty region.
  , display :: IPnt -> IRect                         -> a -> C.Render a

  -- Handle mouse presses (first parameter is (pressed?, which)) and motion.
  , mouse   :: Maybe (Bool, Int) -> (Double, Double) -> a -> IO a

  -- Handle key-presses, first parameter is "pressed?", second is (Left string)
  -- to give the names of non-character keys, and (Right char) for the rest.
  , key     :: Bool -> Either String Char            -> a -> IO a
  }

dummyToy x = Toy
  { initialState = x
  , tick    = const $ return . id
  , display = const $ const $ return . id
  , mouse   = const $ const $ return . id
  , key     = const $ const $ return . id
  }

updateState :: IORef (a, b) -> (b -> IO b) -> (a -> a) -> IO ()
updateState r f g = updateRef r (\r -> secondIO f r >>= return . first g)

updateFirst  r f = updateRef r $ \s -> f s >>= (\a' -> return (a', snd s))
updateSecond r f = updateRef r $ \s -> f s >>= (\b' -> return (fst s, b'))

updateRef r f = readIORef r >>= f >>= writeIORef r

firstIO :: (a -> IO a') -> (a, b) -> IO (a', b)
firstIO f (a, b) = f a >>= (\a' -> return (a', b))
secondIO :: (b -> IO b') -> (a, b) -> IO (a, b')
secondIO f (a, b) = f b >>= (\b' -> return (a, b'))

runToy ::Toy a -> IO ()
runToy toy = do
  G.initGUI

  window <- G.windowNew
  canvas <- G.drawingAreaNew
  state <- newIORef (M.empty, initialState toy)

  G.onKeyPress   window $ handleKey state $ key toy
  G.onKeyRelease window $ handleKey state $ key toy

  G.onMotionNotify  window True $ handleMotion state $ mouse toy
  G.onButtonPress   window      $ handleButton state $ mouse toy
  G.onButtonRelease window      $ handleButton state $ mouse toy

  G.onExposeRect canvas $ \(G.Rectangle x y w h) -> do 
    let r = ((x, y), (x + w, y + h))
    dw <- G.widgetGetDrawWindow canvas
    sz <- G.widgetGetSize canvas
    updateRef state $ secondIO $ G.renderWithDrawable dw . display toy sz r

  G.set window $ [G.containerChild G.:= canvas]
  G.widgetShowAll window

  G.timeoutAddFull (do
    updateSecond state $ uncurry $ tick toy
    G.widgetQueueDraw canvas
    return True)
      G.priorityDefaultIdle 30

  G.mainGUI

quit = G.mainQuit


handleKey :: IORef (KeyTable, a) -> (Bool -> Either String Char -> a -> IO a) ->
  E.Event -> IO Bool
handleKey st f ev = do
  updateState st (f pres $ maybe (Left name) Right char)
                (M.insert name (pres, time, mods))
  return True
 where name = E.eventKeyName ev
       char = E.eventKeyChar ev
       time = E.eventTime ev
       mods = E.eventModifier ev
       pres = not $ E.eventRelease ev

handleMotion :: IORef (KeyTable, a) -> (Maybe (Bool, Int) -> (Double, Double) -> a -> IO a) -> 
  E.Event -> IO Bool
handleMotion st f ev = do
  updateRef st $ secondIO $ f Nothing pos
  return True
 where pos = (E.eventX ev, E.eventY ev)

handleButton :: IORef (KeyTable, a) -> (Maybe (Bool, Int) -> (Double, Double) -> a -> IO a) -> 
  E.Event -> IO Bool
handleButton st f ev = do
  when (click == E.SingleClick || click == E.ReleaseClick)
    $ updateState st (f (Just (pressed, but)) pos)
                     (M.insert ("Mouse" ++ show but) (pressed, time, mods))
  return True
 where pos = (E.eventX ev, E.eventY ev)
       time = E.eventTime ev
       mods = E.eventModifier ev
       click = E.eventClick ev
       pressed = click /= E.ReleaseClick
       but = case E.eventButton ev of
               E.LeftButton -> 0
               E.RightButton -> 1
               E.MiddleButton -> 2
               E.OtherButton ix -> ix --TODO: guaranteed to not be 0,1,2?

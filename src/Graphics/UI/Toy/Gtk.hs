{-# LANGUAGE ConstraintKinds,
             EmptyDataDecls,
             FlexibleContexts,
             TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Gtk
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- The Gtk Toy Framework is a wrapper over Gtk for conveniently creating 
-- applications which draw things and use the mouse or keyboard.
--
-- It handles the minutiae of setting up the Gtk window and canvas, and
-- processes mouse and keyboard inputs into more palatable data structures.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Gtk
  ( module Graphics.UI.Toy

  , Toy(..), newToy
  , runToy, quitToy

  -- * Display
  , Gtk
  , GtkInteractive
  , GtkDisplay(..), simpleDisplay

  -- * Key Utilities
  , eitherHeld, escapeKeyHandler

  ) where

import Control.Monad (when)
import Data.IORef
import qualified Data.Map as M
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as E

import Graphics.UI.Toy

data Gtk

type instance MousePos Gtk = (Double, Double)
type instance KeyModifier Gtk = E.Modifier

type GtkInteractive a = (Interactive Gtk a, GtkDisplay a)

class GtkDisplay a where
  -- | @display@ is called when the rendering needs to be refreshed.
  display  :: G.DrawWindow -> InputState Gtk -> a -> IO a
  display _ _ = return

-- | Postfixes \"_L\" and \"_R\" on the key name, and returns true if either of
--   those keys are being held.
eitherHeld :: String -> InputState b -> Bool
eitherHeld key inp = (keyHeld (key ++ "_L") inp || keyHeld (key ++ "_R") inp)

-- | Converts a diagram projection to a function for Interactive 'display'.
simpleDisplay :: (G.DrawWindow -> a -> a)
              -> G.DrawWindow -> InputState Gtk -> a -> IO a
simpleDisplay f dw _ = return . f dw

-- | Like it says on the can.  This is a synonym for 'Graphics.UI.Gtk.mainQuit'
quitToy :: IO ()
quitToy = G.mainQuit

-- | Calls 'quitToy' when the escape key is pressed.
escapeKeyHandler :: KeyHandler Gtk a
escapeKeyHandler
  = keyHandler
  $ \ke x -> when (ke == (True, Left "Escape")) quitToy 
          >> return x

-- | Main program entry-point. This is how you turn an instance of 'Interactive'
--   into an application.
runToy :: GtkInteractive a => a -> IO ()
runToy toy = do
  G.initGUI

  window <- G.windowNew
  G.windowSetDefaultSize window 640 480

  canvas <- newToy toy

  G.set window $ [G.containerChild G.:= toyWindow canvas]
  G.widgetShowAll window

--TODO: tell the toy that we're going dowm
--TODO: or better would catch onDelete events and ask toy what to do
  G.onDestroy window quitToy

  G.mainGUI

-- | Subroutine data.
data Toy a = Toy
  { -- | This root widget catches interactive events.  Pack this into your GUI.
    toyWindow :: G.EventBox
  , -- | This child widget does the drawing.
    toyCanvas :: G.DrawingArea
  , -- | This contains our world, exposed so that your other worlds can interfere.
    toyState  :: IORef (InputState Gtk, a)
  }

-- | Subroutine entrypoint. This is how you turn an instance of Interactive
--   into a widget-like thing.
newToy :: GtkInteractive a => a -> IO (Toy a)
newToy toy = do
  window <- G.eventBoxNew
  canvas <- G.drawingAreaNew
  state <- newIORef (InputState (0, 0) M.empty, toy)

  let doRedraw = G.widgetQueueDraw canvas >> return True

  G.onKeyPress   window $ (>> doRedraw) . handleKey state
  G.onKeyRelease window $ (>> doRedraw) . handleKey state

  G.onMotionNotify  window True $ (>> doRedraw) . handleMotion state
  G.onButtonPress   window      $ (>> doRedraw) . handleButton state
  G.onButtonRelease window      $ (>> doRedraw) . handleButton state

  G.onExposeRect canvas $ \(G.Rectangle x y w h) -> do
    let r = ((x, y), (x + w, y + h))
    dw <- G.widgetGetDrawWindow canvas
    sz <- G.widgetGetSize canvas
    (inp, x) <- readIORef state
    x' <- display dw inp x
    writeIORef state (inp, x')

  G.set window $ [G.containerChild G.:= canvas]

  let tickHandler = do
        st@(inp, _) <- readIORef state
        (state', redraw) <- uncurry tick st
        when redraw (doRedraw >> return ())
        writeIORef state (inp, state')
        return True
  
--TODO: how does this timer behave when hiding / reshowing windows?
--TODO: do we want timer to run only when window is visible?
--TODO: definitely want timer to stop when widgets are permanently gone
  timer <- G.timeoutAddFull tickHandler G.priorityHighIdle 30
  G.onUnrealize window $ G.timeoutRemove timer

  return $ Toy window canvas state

handleKey :: Interactive Gtk a => IORef (InputState Gtk, a) -> E.Event -> IO ()
handleKey st ev = do
  (InputState p m, x) <- readIORef st
  let inp' = InputState p (M.insert name (pres, time, mods) m)
  x' <- keyboard (pres, maybe (Left name) Right char) inp' x
  writeIORef st (inp', x')
 where
  name = E.eventKeyName ev
  char = E.eventKeyChar ev
  time = fromIntegral $ E.eventTime ev
  mods = E.eventModifier ev
  pres = not $ E.eventRelease ev

handleMotion :: Interactive Gtk a => IORef (InputState Gtk, a) -> E.Event -> IO ()
handleMotion st ev = do
  (InputState p m, x) <- readIORef st
  let inp' = InputState pos m
  x' <- mouse Nothing inp' x
  writeIORef st (inp', x')
 where
  pos = (E.eventX ev, E.eventY ev)

handleButton :: Interactive Gtk a => IORef (InputState Gtk, a) -> E.Event -> IO ()
handleButton st ev = do
  when (click == E.SingleClick || click == E.ReleaseClick) $ do
    (InputState p m, x) <- readIORef st
    let m' = M.insert ("Mouse" ++ show but) (pressed, time, mods) m
        inp' = InputState pos m'
    x' <- mouse (Just (pressed, but)) inp' x
    writeIORef st (inp', x')
 where
  pos = (E.eventX ev, E.eventY ev)
  time = fromIntegral $ E.eventTime ev
  mods = E.eventModifier ev
  click = E.eventClick ev
  pressed = click /= E.ReleaseClick
  but = case E.eventButton ev of
    E.LeftButton -> 0
    E.RightButton -> 1
    E.MiddleButton -> 2
--TODO: guaranteed to not be 0,1,2?
    E.OtherButton ix -> ix

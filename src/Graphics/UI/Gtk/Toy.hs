{-# LANGUAGE TupleSections, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy
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
module Graphics.UI.Gtk.Toy
  ( KeyInfo, KeyTable, MouseEvent, KeyEvent, InputState(..)
  , Interactive(..), GtkInteractive(..)
  , runToy, quitToy

  -- * InputState Accessors
  , keyInfo, keyHeld, mouseHeld

  -- * Utilities
  -- | Functions to allow for writing simpler, pure implementations of the
  --   different members of Interactive.
  , simpleTick, simpleDisplay, simpleMouse, simpleKeyboard
  , quitKeyboard

  ) where

import Control.Monad (when)
import Data.IORef
import qualified Data.Map as M
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as E

-- | Information about the most recent key-state transition.
--   The tuple contains whether the button was pressed,
--   at what time in msec, and with which GTK modifiers.
type KeyInfo = (Bool, Int, [G.Modifier])

-- | A map of GTK keynames to last-received event regarding each respective
--   key.  This can be interpreted as the current keyboard state - a key is
--   down if it was last seen being pressed.
type KeyTable = M.Map String KeyInfo

data InputState = InputState 
  { mousePos :: (Double, Double) -- ^ The most recent mouse position
  , keyTable :: KeyTable         -- ^ Map from key-name to most recent event
  }

-- | A @KeyEvent@ tuple specifies whether the key was pressed or not, and
--   which key was pressed.  @Right Char@ is yielded for keys which would
--   normally correspond to character insertions, while @Left String@ provides
--   GTK-convention names for the rest.
type KeyEvent = (Bool, Either String Char)

-- | A @MouseEvent@ is @Nothing@ if it's a mouse motion event, and otherwise
--   provides mouse press information.
type MouseEvent = Maybe (Bool, Int)

-- | A class for things which change within an interactive context.  The default
--   method implementations do nothing.
class Interactive a where
  -- | @tick@ is (ideally) called every 30ms.  The bool result indicates if the
  --   graphics need to be refreshed.
  tick                     :: InputState -> a -> IO (a, Bool)

  -- | @mouse@ is called when the mouse moves or presses occur.
  mouse    :: MouseEvent   -> InputState -> a -> IO a

  -- | @keyboard@ is called on key-presses.  
  keyboard :: KeyEvent     -> InputState -> a -> IO a

  -- No-op defaults.
  tick _ = return . (, False)
  mouse    _ _ = return
  keyboard _ _ = return

class Interactive a => GtkInteractive a where
  -- | @display@ is called when the rendering needs to be refreshed.
  display  :: G.DrawWindow -> InputState -> a -> IO a
  display _ _ = return

-- InputState Queries.

-- |  the information for the most recent key event of the named key.  
keyInfo :: String -> InputState -> Maybe KeyInfo
keyInfo name = M.lookup name . keyTable

-- | Gets whether the named key is held down.
keyHeld :: String -> InputState -> Bool
keyHeld name (keyInfo name -> Just (True, _, _)) = True
keyHeld _ _ = False

-- | Postfixes "_L" and "_R" on the key name, and returns true if either of
--   those keys are being held.
eitherHeld :: String -> InputState -> Bool
eitherHeld key inp = (keyHeld (key ++ "_L") inp || keyHeld (key ++ "_R") inp)

-- | Whether the indicated mouse button is considered pressed in the InputState.
mouseHeld :: Int -> InputState -> Bool
mouseHeld ix = keyHeld ("Mouse" ++ show ix)

-- | Converts a pure state transform to a function for Interactive 'tick'.
simpleTick :: (a -> a)
           -> InputState -> a -> IO (a, Bool)
simpleTick f _ = return . (, True) . f

-- | Converts a diagram projection to a function for Interactive 'display'.
simpleDisplay :: (G.DrawWindow -> a -> a)
              -> G.DrawWindow -> InputState -> a -> IO a
simpleDisplay f dw _ = return . f dw

-- | Converts a function which responds to mouse-presses, and transforms state
--   accordingly to a function for Interactive 'mouse'.
simpleMouse :: (MouseEvent -> (Double, Double) -> a -> a)
            -> (MouseEvent -> InputState -> a -> IO a)
simpleMouse f c inp = return . f c (mousePos inp)

-- | Converts a function which responds to mouse-presses, and transforms state
--   accordingly to a function for Interactive 'mouse'.
simpleMouseClick :: ((Bool, Int) -> (Double, Double) -> a -> a)
                 -> (MouseEvent -> InputState -> a -> IO a)
simpleMouseClick f (Just c) inp = return . f c (mousePos inp)
simpleMouseClick _ _ _ = return

simpleMousePos :: ((Double, Double) -> a -> a)
               -> (MouseEvent -> InputState -> a -> IO a)
simpleMousePos f _ inp = return . f (mousePos inp)

-- | Converts a function which responds to key-presses, and transforms state
--   accordingly to a function for Interactive 'keyboard'.
simpleKeyboard :: (KeyEvent -> a -> a)
               -> (KeyEvent -> InputState -> a -> IO a)
simpleKeyboard f e _ = return . f e

-- | A definition for the keyboard handler that just calls "quitToy" when
--   Escape is pressed.
quitKeyboard :: KeyEvent -> InputState -> a -> IO a
quitKeyboard (True, (Left "Escape")) _ x = quitToy >> return x
quitKeyboard _ _ x = return x

-- | Like it says on the can.  This is a synonym for 'Graphics.UI.Gtk.mainQuit'
quitToy :: IO ()
quitToy = G.mainQuit

-- | Main program entrypoint. This is how you turn an instance of Interactive
--   into an application.
runToy :: GtkInteractive a => a -> IO ()
runToy toy = do
  G.initGUI

  window <- G.windowNew
  canvas <- G.drawingAreaNew
  state <- newIORef (InputState (0, 0) M.empty, toy)

  let doRedraw = G.widgetQueueDraw canvas >> return True
  G.windowSetDefaultSize window 640 480

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
  G.widgetShowAll window

  let tickHandler = do
        st@(inp, _) <- readIORef state
        (state', redraw) <- uncurry tick st
        when redraw (doRedraw >> return ())
        writeIORef state (inp, state')
        return True
  
  G.timeoutAddFull tickHandler G.priorityDefaultIdle 30

  G.mainGUI
 where

handleKey :: Interactive a => IORef (InputState, a) -> E.Event -> IO ()
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

handleMotion :: Interactive a => IORef (InputState, a) -> E.Event -> IO ()
handleMotion st ev = do
  (InputState p m, x) <- readIORef st
  let inp' = InputState pos m
  x' <- mouse Nothing inp' x
  writeIORef st (inp', x')
 where
  pos = (E.eventX ev, E.eventY ev)

handleButton :: Interactive a => IORef (InputState, a) -> E.Event -> IO ()
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

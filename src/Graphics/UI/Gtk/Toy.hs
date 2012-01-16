{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- The Gtk Toy Framework is a simplifying interface over Gtk for creating
-- applications which draw things and interact with the mouse and keyboard.  It
-- handles the minutae of setting up the Gtk window and canvas, and processes
-- the input events into more palatable data structures.
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy
  (
    KeyInfo, KeyTable, InputState(..), Interactive(..)
  , runToy, quitToy

  -- * InputState Accessors.
  , keyInfo, keyHeld, mousePos, mouseHeld

  -- * Utilities for writing Interactive instances.

  -- | Functions to allow for writing simpler, pure implementations of the
  --   different members of Interactive.
  , simpleTick, simpleDisplay, simpleMouse, simpleKeyboard
  , quitKeyboard

  ) where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Char (toLower)
import Data.IORef
import qualified Data.Map as M
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as E

type KeyInfo = (Bool, Int, [G.Modifier])
type KeyTable = M.Map String KeyInfo

data InputState = InputState (Double, Double) KeyTable

-- | A class for things which can be drawn and change within an interactive
--   context.  The default method implementations do nothing.
class Interactive a where

  -- | @tick@ is (ideally) called every 30ms.  The bool result indicates if the
  --   graphics need to be refreshed.
  tick     :: InputState -> a -> IO (a, Bool)

  -- | @display@ is called when the rendering needs to be refreshed.
  display  :: G.DrawWindow 
           -> InputState -> a -> IO a

  -- | @mouse@ is called when the mouse moves or presses occur.
  mouse    :: Maybe (Bool, Int)
           -> InputState -> a -> IO a

  -- | @keyboard@ is called on key-presses, first parameter is "pressed?", and 
  --   second is (Right Char) for string-representeable names, and (Left String)
  --   for the rest.
  keyboard :: Bool -> Either String Char 
           -> InputState -> a -> IO a

  -- No-op defaults.
  tick _ = return . (, False)
  mouse    = simpleMouse    (const $ const id)
  keyboard = simpleKeyboard (const $ const id)

-- InputState Queries.

-- | Gets the information for the most recent key press / release of the named
-- key.  If the key is representeable as a Char, then the lowercase variant is
-- its name.  Otherwise, it's string form name is the identifier.
keyInfo :: String -> InputState -> Maybe KeyInfo
keyInfo name (InputState _ kbd)
  | length name == 1 = M.lookup (map toLower name) kbd
  | otherwise = M.lookup name kbd

-- | Gets whether the named key is currently thought to be held down.
keyHeld :: String -> InputState -> Bool
keyHeld name inp = case keyInfo name inp of
  (Just (True, _, _)) -> True
  _ -> False

-- | Postfixes "_L" and "_R" on the key name, and returns true if either of
--   those keys are being held. 
eitherHeld :: String -> InputState -> Bool
eitherHeld key inp = (keyHeld (key ++ "_L") inp || keyHeld (key ++ "_R") inp)

-- | Accesses the most recently reported mouse position.
mousePos :: InputState -> (Double, Double)
mousePos (InputState p _) = p

-- | Queries if a particular mouse button was reported as held.
mouseHeld :: Int -> InputState -> Bool
mouseHeld ix = keyHeld ("Mouse" ++ show ix)

-- Convenience functions to use pure functions that don't need the InputState.

-- | Converts a pure state transform to a function for Interactive 'tick'.
simpleTick :: (a -> a)
           -> InputState -> a -> IO (a, Bool)
simpleTick f _ = return . (, True) . f

-- | Converts a diagram projection to a function for Interactive 'display'.
simpleDisplay :: (G.DrawWindow -> a -> a)
              -> G.DrawWindow -> InputState -> a -> IO a
simpleDisplay f dw _ = return . f dw

-- | Converts a function which responds to mouse-presses, and transforms state
-- accordingly to a function for Interactive 'mouse'.
simpleMouse :: ((Double, Double) -> Maybe (Bool, Int) -> a -> a)
            -> Maybe (Bool, Int)
            -> InputState -> a -> IO a
simpleMouse f c inp = return . f (mousePos inp) c

-- | Converts a function which responds to mouse-presses, and transforms state
-- accordingly to a function for Interactive 'mouse'.
simpleMouseClick :: ((Double, Double) -> (Bool, Int) -> a -> a)
                 -> Maybe (Bool, Int)
                 -> InputState -> a -> IO a
simpleMouseClick f (Just c) inp = return . f (mousePos inp) c
simpleMouseClick _ _ _ = return

simpleMousePos :: ((Double, Double) -> a -> a)
               -> Maybe (Bool, Int)
               -> InputState -> a -> IO a
simpleMousePos f _ inp = return . f (mousePos inp)

-- | Converts a function which responds to key-presses, and transforms state
--   accordingly to a function for Interactive 'keyboard'.
simpleKeyboard :: (Bool -> Either String Char -> a -> a)
               -> (Bool -> Either String Char -> InputState -> a -> IO a)
simpleKeyboard f p k _ = return . f p k

-- | A definition for the keyboard handler that just calls "quitToy" when
--   Escape is pressed.
quitKeyboard :: Bool -> Either String Char -> InputState -> a -> IO a
quitKeyboard True (Left "Escape") _ x = quitToy >> return x
quitKeyboard _ _ _ x = return x

-- | Like it says on the can.  This is a synonym for 'Graphics.UI.Gtk.mainQuit'
quitToy :: IO ()
quitToy = G.mainQuit

-- | Main program entrypoint. This is how you turn an instance of Interactive
--   into an application.
runToy :: Interactive a => a -> IO ()
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
  x' <- keyboard pres (maybe (Left name) Right char) inp' x
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

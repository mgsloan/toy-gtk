{-# LANGUAGE
    ConstraintKinds
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies
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

import Control.Monad        ( when )
import Control.Monad.Reader ( lift )
import Data.IORef
import qualified Data.Map as M
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Graphics.UI.Toy

data Gtk

type instance MousePos Gtk = (Double, Double)
type instance KeyModifier Gtk = Modifier

deriving instance Eq   (InputState Gtk)
deriving instance Show (InputState Gtk)

type GtkInteractive a = (Interactive Gtk a, GtkDisplay a)

class GtkDisplay a where
  -- | @display@ is called when the rendering needs to be refreshed.
  display  :: DrawWindow -> InputState Gtk -> a -> IO a
  display _ _ = return

-- | Postfixes \"_L\" and \"_R\" on the key name, and returns true if either of
--   those keys are being held.
eitherHeld :: String -> InputState b -> Bool
eitherHeld key inp = (keyHeld (key ++ "_L") inp || keyHeld (key ++ "_R") inp)

-- | Converts a diagram projection to a function for Interactive 'display'.
simpleDisplay :: (DrawWindow -> a -> a)
              -> DrawWindow -> InputState Gtk -> a -> IO a
simpleDisplay f dw _ = return . f dw

-- | Like it says on the can.  This is a synonym for 'Graphics.UI.Gtk.mainQuit'
quitToy :: IO ()
quitToy = mainQuit

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
  initGUI

  window <- windowNew
  windowSetDefaultSize window 640 480

  canvas <- newToy toy

  set window $ [containerChild := toyWindow canvas]
  widgetShowAll window

--TODO: tell the toy that we're going dowm
--TODO: or better would catch onDelete events and ask toy what to do
  onDestroy window quitToy

  mainGUI

-- | Subroutine data.
data Toy a = Toy
  { -- | This root widget catches interactive events.  Pack this into your GUI.
    toyWindow :: EventBox
  , -- | This child widget does the drawin
    toyCanvas :: DrawingArea
  , -- | This contains our world, exposed so that your other worlds can interfer
    toyState  :: IORef (InputState Gtk, a)
  }

-- | Subroutine entrypoint. This is how you turn an instance of Interactive
--   into a widget-like thin
newToy :: forall a. GtkInteractive a => a -> IO (Toy a)
newToy toy = do
  window <- eventBoxNew
  canvas <- drawingAreaNew
  state <- newIORef (InputState (0, 0) M.empty, toy)
  requested <- newIORef True

  let windowEv :: Signal EventBox (EventM e Bool) 
               -> (IORef (InputState Gtk, a) -> EventM e ())
               -> IO (ConnectId EventBox)
      windowEv e f = on window e $ f state >> lift redraw

      redraw :: IO Bool
      redraw = do
        r <- readIORef requested
        when (not r) $ do
          widgetQueueDraw canvas
          writeIORef requested True
        return True

  windowEv keyPressEvent      (handleKey True)
  windowEv keyReleaseEvent    (handleKey False)
  windowEv motionNotifyEvent  handleMotion
  windowEv buttonPressEvent   handleButton
  windowEv buttonReleaseEvent handleButton

  widgetAddEvents window [PointerMotionMask, PointerMotionHintMask, ButtonMotionMask]

  widgetSetCanFocus window True
  widgetGrabFocus window

  onExposeRect canvas $ \(Rectangle x y w h) -> do
    let r = ((x, y), (x + w, y + h))
    dw <- widgetGetDrawWindow canvas
    sz <- widgetGetSize canvas
    (inp, x) <- readIORef state
    x' <- display dw inp x
    writeIORef state (inp, x')
    writeIORef requested False

  set window $ [containerChild := canvas]

  let tickHandler = do
        st@(inp, _) <- readIORef state
        (state', dodraw) <- uncurry tick st
        when dodraw (redraw >> return ())
        writeIORef state (inp, state')
        return True
  
--TODO: how does this timer behave when hiding / reshowing windows?
--TODO: do we want timer to run only when window is visible?
--TODO: definitely want timer to stop when widgets are permanently gone
  timer <- timeoutAddFull tickHandler priorityHighIdle 60
  onUnrealize window $ timeoutRemove timer

  return $ Toy window canvas state

handleKey :: Interactive Gtk a 
          => Bool -> IORef (InputState Gtk, a) -> EventM EKey ()
handleKey press st = do
  ev   <- eventKeyVal
  name <- eventKeyName
  time <- eventTime
  mods <- eventModifier

  let kv = maybe (Left name) Right $ keyToChar ev

  lift $ do
    (InputState p m, x) <- readIORef st
    let inp' = InputState p (M.insert name (press, fromIntegral time, mods) m)
    x' <- keyboard (press, kv) inp' x
    writeIORef st (inp', x')

handleMotion :: Interactive Gtk a
             => IORef (InputState Gtk, a) -> EventM EMotion ()
handleMotion st = do
  pos <- eventCoordinates
  lift $ do
    (InputState p m, a) <- readIORef st
    let inp' = InputState pos m
    a' <- mouse Nothing inp' a
    writeIORef st (inp', a')

handleButton :: Interactive Gtk a
             => IORef (InputState Gtk, a) -> EventM EButton ()
handleButton st = do
  time  <- eventTime
  mods  <- eventModifier
  click <- eventClick
  bIx   <- eventButton
  pos   <- eventCoordinates
  let pres = click /= ReleaseClick
      buttonIx = case bIx of
        LeftButton -> 0
        RightButton -> 1
        MiddleButton -> 2
    --TODO: guaranteed to not be 0,1,2?
        OtherButton ix -> ix

  when (click == SingleClick || click == ReleaseClick) . lift $ do
    (InputState p m, a) <- readIORef st
    let m' = M.insert ("Mouse" ++ show buttonIx) 
                      (pres, fromIntegral time, mods) m
        inp' = InputState pos m'
    a' <- mouse (Just (pres, buttonIx)) inp' a
    writeIORef st (inp', a')

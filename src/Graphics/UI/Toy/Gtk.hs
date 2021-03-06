{-# LANGUAGE
    ConstraintKinds
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies
  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Gtk
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Implements the toy-interface for gtk.  This provides a simple interface for
-- creating applications which draw things and interact with the mouse and
-- keyboard.  It handles the minutiae of setting up the gtk window and canvas,
-- and processes the input events.
--
-- The name \"toy\" comes from the \"toy framework\", a part of the lib2geom
-- library (<http://lib2geom.sourceforge.net/>).  It's used in building \"toys\"
-- demonstrating the features of the library.  This is a different variety of
-- \"TDD\"- but instead of tests, it's toys! We found that building little demos
--  to be a nice way to drive initial design / development.
--
--------------------------------------------------------------------------------
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

import Graphics.UI.Toy

-- | A constructor-less type used to indicate usage of the 'Gtk' backend.
data Gtk

type instance MousePos Gtk = (Double, Double)
type instance KeyModifier Gtk = Modifier

deriving instance Eq   (InputState Gtk)
deriving instance Show (InputState Gtk)

-- | This is a constraint synonym for the typeclasses that need to be
--   instantiated in order for 'runToy' to be able to run your toy in a Gtk
--   window.
type GtkInteractive a = (Interactive Gtk a, GtkDisplay a)

-- | The typeclass that applications should implement in order to update the
--   canvas.
class GtkDisplay a where
  -- | @display@ is called when the rendering needs to be refreshed.  It's
  --   called with the 'DrawWindow' that should be drawn to, and the current
  --   input state / application state.
  display  :: DrawWindow -> InputState Gtk -> a -> IO a
  display _ _ = return

-- | Postfixes \"_L\" and \"_R\" on the key name, and returns true if either of
--   those keys are being held.  This is useful for \"Ctrl\", \"Shift\", and
--   \"Alt\".
eitherHeld :: String -> InputState b -> Bool
eitherHeld key inp = (keyHeld (key ++ "_L") inp || keyHeld (key ++ "_R") inp)

-- | Converts a diagram projection to a function for Interactive 'display'.
simpleDisplay :: (DrawWindow -> a -> a)
              -> DrawWindow -> InputState Gtk -> a -> IO a
simpleDisplay f dw _ = return . f dw

-- | Like it says on the can. This is a synonym for 'Graphics.UI.Gtk.mainQuit'.
quitToy :: IO ()
quitToy = mainQuit

-- | Calls 'quitToy' when the escape key is pressed.
escapeKeyHandler :: KeyHandler Gtk a
escapeKeyHandler =
  keyHandler $ \k x -> do
    when (k == (True, Left "Escape")) quitToy
    return x

-- | Main program entry-point. This is how you turn an instance of 'Interactive'
--   into an application.
runToy :: GtkInteractive a => a -> IO ()
runToy toy = do
  _ <- initGUI

  window <- windowNew
  windowSetDefaultSize window 640 480

  canvas <- newToy toy

  set window $ [containerChild := toyWindow canvas]
  widgetShowAll window

--TODO: tell the toy that we're going down
--TODO: or better would catch onDelete events and ask toy what to do
  _ <- onDestroy window quitToy

  mainGUI

-- | Subroutine data.
data Toy a = Toy
  { toyWindow :: EventBox
    -- ^ This root widget catches interactive events.  Pack this into your GUI.
  , toyCanvas :: DrawingArea
    -- ^ This child widget does the drawin
  , toyState  :: IORef (InputState Gtk, a)
    -- ^ This contains our world, exposed so that your other worlds can interfer
  }

-- | Subroutine entrypoint. This is how you turn an instance of 'GtkInteractive'
--   into a widget-like thing.
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

  _ <- windowEv keyPressEvent      (handleKey True)
  _ <- windowEv keyReleaseEvent    (handleKey False)
  _ <- windowEv motionNotifyEvent  handleMotion
  _ <- windowEv buttonPressEvent   handleButton
  _ <- windowEv buttonReleaseEvent handleButton

  widgetAddEvents window [PointerMotionMask, PointerMotionHintMask, ButtonMotionMask]

  widgetSetCanFocus window True
  widgetGrabFocus window

  _ <- onExposeRect canvas $ \_ -> do
    dw <- widgetGetDrawWindow canvas
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
  _ <- onUnrealize window $ timeoutRemove timer

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
    (InputState _ m, a) <- readIORef st
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
    (InputState _ m, a) <- readIORef st
    let m' = M.insert ("Mouse" ++ show buttonIx)
                      (pres, fromIntegral time, mods) m
        inp' = InputState pos m'
    a' <- mouse (Just (pres, buttonIx)) inp' a
    writeIORef st (inp', a')

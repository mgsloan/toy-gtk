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
  ( Toy(..), KeyTable, KeyInfo
  , dummyToy, runToy, quit
  ) where 

import Control.Monad (when)
import Data.IORef
import qualified Data.HashTable as HT
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as E
import qualified Graphics.Rendering.Cairo as C
import System.IO.Unsafe

type KeyInfo = (Bool, G.TimeStamp, [G.Modifier])
type KeyTable = HT.HashTable String KeyInfo

data Toy a = Toy
  { initialState :: a
  , tick    :: (String -> Maybe KeyInfo)             -> a -> IO a
  , display :: (Int, Int)                            -> a -> C.Render a
  , mouse   :: Maybe (Bool, Int) -> (Double, Double) -> a -> IO a
  , key     :: Either String Char -> Bool            -> a -> IO a
  }

dummyToy x = Toy
  { initialState = x
  , tick    = const $ return . id
  , display = const $ return . id
  , mouse   = const $ const $ return . id
  , key     = const $ const $ return . id
  }

runToy ::Toy a -> IO ()
runToy toy = do
  G.initGUI

  window <- G.windowNew
  canvas <- G.drawingAreaNew
  state <- newIORef (initialState toy)

  keyTable <- HT.new (==) HT.hashString

  G.onKeyPress   window $ handleKey state keyTable $ key toy
  G.onKeyRelease window $ handleKey state keyTable $ key toy

  G.onMotionNotify  window True $ handleMotion state          $ mouse toy
  G.onButtonPress   window      $ handleButton state keyTable $ mouse toy
  G.onButtonRelease window      $ handleButton state keyTable $ mouse toy

  --TODO: partial update logic
  G.onExpose canvas $ const $ do 
    dw <- G.widgetGetDrawWindow canvas
    sz <- G.widgetGetSize canvas
    updateRef state (G.renderWithDrawable dw . display toy sz)
    return True

  G.set window $ [G.containerChild G.:= canvas]
  G.widgetShowAll window

  G.timeoutAddFull (do
    updateRef state $ tick toy (unsafePerformIO . HT.lookup keyTable)
    G.widgetQueueDraw canvas
    return True)
      G.priorityDefaultIdle 30

  G.mainGUI

quit = G.mainQuit

updateRef r f = readIORef r >>= f >>= writeIORef r

handleKey :: IORef a -> KeyTable -> (Either String Char -> Bool -> a -> IO a) ->
  E.Event -> IO Bool
handleKey st ht f ev = do
  HT.update ht name (pres, time, mods)
  updateRef st $ f (maybe (Left name) Right char) pres
  return True
 where name = E.eventKeyName ev
       char = E.eventKeyChar ev
       time = E.eventTime ev
       mods = E.eventModifier ev
       pres = E.eventPressed ev

handleMotion :: IORef a -> (Maybe (Bool, Int) -> (Double, Double) -> a -> IO a) -> 
  E.Event -> IO Bool
handleMotion st f ev = do
  updateRef st $ f Nothing pos
  return True
 where pos = (E.eventX ev, E.eventY ev)

handleButton :: IORef a -> KeyTable -> (Maybe (Bool, Int) -> (Double, Double) -> a -> IO a) -> 
  E.Event -> IO Bool
handleButton st ht f ev = do
  when (click == E.SingleClick || click == E.ReleaseClick) $ do
      HT.update ht ("Mouse" ++ show but) (pressed, time, mods)
      updateRef st $ f (Just (pressed, but)) pos
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



-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFramework
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Toy Framework - Simplifies the creation of simple Cairo / GTK applications.

{-# LANGUAGE TemplateHaskell, TypeOperators, TypeFamilies,
  ExistentialQuantification, FlexibleInstances, StandaloneDeriving,
  GeneralizedNewtypeDeriving #-}

module Graphics.ToyFramework
  ( DPoint, DColor, DLine
  , Positionable(..)
  , Handle(..)
  , Slider(..), mkToggle, mkSlider, sliderValue, sliderPos, sliderHandle
  , Focusable(..), changeFocus, focus, focusToList, clickElement, displayFocusable, moveFocusable
  , apipe
  , move, relMove, line, pathBounds, textSize, textRect, relText, drawArrow
  , Draw(..), Drawable(..), Color(..)
  , module Graphics.ToyFramework.Core
  ) where

import Graphics.ToyFramework.Core

import Control.Applicative (Applicative, liftA2)
import Control.Arrow (second, (***))
import Control.Monad (when)

import Data.AffineSpace
import Data.Curve
import qualified Data.Curve.Interval as I
import Data.Curve.Util
import Data.IORef
import Data.List ((\\))
import Data.Maybe
import Data.Label

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Internal as CI

type DPoint = (Double, Double)
type DColor = (Double, Double, Double)
type DLine = (Linear Double, Linear Double)

--TODO: convenience function like 'trace' except taking a C.render or
-- drawable

-----------------------------------------------------------------------------
-- Handles / light gui library
-----------------------------------------------------------------------------

data Handle = Handle
  { _handleText :: String
  , _handlePosition :: DPoint 
  , _handleColor :: DColor
  } deriving (Eq)

data Slider a = Slider 
  { _sliderMetric :: Bijection (->) Double a
  , sliderValue_ :: a
  , _sliderLine :: DLine 
  , _sliderColor :: DColor
  }

$(mkLabels [''Handle, ''Slider])

instance Eq a => Eq (Slider a) where
  x == y = sliderValue_ x == sliderValue_ y
        && _sliderColor x == _sliderColor y
        && _sliderLine x == _sliderLine y

class Positionable a where
  pos :: a :-> DPoint

instance Positionable Handle     where pos = handlePosition
instance Positionable (Slider a) where pos = sliderPos

instance Draw Handle where
  draw (Handle txt (x, y) col) = do
    setColor col
    C.newPath
    C.arc x y 5 0 (2 * pi)
    C.stroke
    C.moveTo x y
    C.showText txt

instance Show a => Draw (Slider a) where
  draw s@(Slider _  val line col) = do
    draw $ sliderHandle s
    setColor col
    draw line 
    C.stroke

instance Predicate Handle where
    type Element Handle = DPoint
    element p = (<10) . distance p . _handlePosition

instance (Show a) => Predicate (Slider a) where
    type Element (Slider a) = DPoint
    element p = element p . sliderHandle

-- | Creates an isomorphism between (0, 1) and some other interval
ivlIso ivl = Bij (I.mapping I.unit ivl) (I.mapping ivl I.unit)

-- | Not a real isomorphism.
boolIso = Bij (> 0.5) (fromIntegral . fromEnum)

mkToggle (x, y) v = Slider boolIso v (Linear x x, Linear (y - 5) (y + 5))
mkSlider ivl = Slider (ivlIso ivl)

sliderT sl val = I.clamp I.unit $ bw (_sliderMetric sl) val
sliderCurT sl = sliderT sl $ sliderValue_ sl

sliderValue = lens sliderValue_
  (\x sl -> sl { sliderValue_ = fw (_sliderMetric sl) $ sliderT sl x })

sliderPos :: Slider a :-> DPoint
sliderPos = lens (\sl -> (_sliderLine sl `at`) $ sliderCurT sl)
  (\x sl -> flip (set sliderValue) sl .
    fw (_sliderMetric sl) . fst $ (toSBasis $ _sliderLine sl) `nearest` x)

sliderHandle :: (Show a) => Slider a -> Handle
sliderHandle s@(Slider _ val _ col) = Handle (show val) (get sliderPos s) col


-----------------------------------------------------------------------------
-- Focusable
-----------------------------------------------------------------------------

type Focusable a = ([a], Maybe a)

clickElement :: (Eq a, Predicate a, Element a ~ DPoint) => 
  Maybe a -> DPoint -> Focusable a -> Focusable a
clickElement noMatch cur x@(xs, _) = 
  maybe (xs, noMatch) (flip (set focus) x . Just) $ 
  listToMaybe (filter (element cur) xs)

changeFocus y' (x, y) = (maybeToList y ++ (x \\ maybeToList y'), y')

focus :: (Eq a) => Focusable a :-> Maybe a
focus = lens snd (\y' (x, y) -> (x, y'))

focusToList :: Focusable a -> [a]
focusToList (xs, Just x) = x : xs
focusToList (xs, Nothing) = xs

displayFocusable :: (Draw a) => IPnt -> IRect -> Focusable a -> C.Render (Focusable a)
displayFocusable = const $ const $ apipe (mapM_ draw . focusToList)

moveFocusable :: (Eq a, Positionable a) => (DPoint -> Focusable a -> Maybe a) ->
  DPoint -> Focusable a -> Focusable a
moveFocusable f p x = modify focus (maybe (f p x) (Just . set pos p)) x


-----------------------------------------------------------------------------
-- Cairo Utilities
-----------------------------------------------------------------------------

move = uncurry C.moveTo
relMove = uncurry C.relMoveTo

line (x1, y1) (x2, y2) = (Linear x1 x2, Linear y1 y2)

pathBounds :: C.Render (I.Interval Double, I.Interval Double)
pathBounds = do (x1, y1, x2, y2) <- C.fillExtents
                return $ fromCorners (x1, y1) (x2, y2)

textSize :: String -> C.Render DPoint
textSize "" = return (0, 0)
textSize txt = do
  (C.TextExtents _ _ _ h w _) <- C.textExtents txt
  return (w, h)

deriving instance Applicative C.Render

textRect :: String -> Int -> Int -> C.Render DRect
textRect txt f t = liftA2 (\(x, _) (w, h) -> fromCorners (x, -h) (x + w, 0))
                          (textSize pre) (textSize (take (t - f) post))
  where (pre, post) = splitAt f txt

relText :: DPoint -> DPoint -> String -> C.Render ()
relText (x, y) pos txt = do
    sz <- textSize txt
    let r = rect (0, 0) sz
    move $ pos ^-^ ((fst $ rside 0 r) `at` x, negate $ (snd $ rside 3 r) `at` y)
    C.showText txt

drawArrow :: (Integrable a, Codomain a ~ DPoint) =>
  Double -> Domain a -> Domain a -> a -> C.Render ()
drawArrow w ht tt c = draw (line hp $ tp + norm)
                   >> draw (line hp $ tp - norm)
          where hp = c `at` ht
                tp = c `at` tt
                norm = (w*^) . rotCW . normalized $ derivative c `at` tt

data Drawable = forall a. Draw a => Drawable a

class Draw a where
    draw :: a -> C.Render ()

--TODO: Arc?

instance Draw Drawable where
    draw (Drawable x) = draw x

instance Draw (Linear Double, Linear Double) where
    draw (Linear x1 x2, Linear y1 y2) = C.moveTo x1 y1 >> C.lineTo x2 y2

instance Draw (Bezier Double, Bezier Double) where
    draw (Bezier [x1, x2], Bezier [y1, y2])         = C.moveTo x1 y1 >> C.lineTo x2 y2
    draw (Bezier [x1, x2, x3], Bezier [y1, y2, y3]) = C.moveTo x1 y1 >>
        C.curveTo ((x22 + x1) / 3) ((y22 + y1) / 3) ((x22 + x3) / 3) ((y22 + y3) / 3) x3 y3
      where x22 = 2 * x2
            y22 = 2 * y2
    draw (Bezier [x1, x2, x3, x4], Bezier [y1, y2, y3, y4]) =
        C.moveTo x1 y1 >> C.curveTo x2 y2 x3 y3 x4 y4
    draw _ = undefined

-- instance (ToBezier a, Drawable (BezierType a, BezierType a)) => Drawable (a, a) where
--    draw (x, y) = draw (toBezier x, toBezier y)

instance Draw (I.Interval Double, I.Interval Double) where
    draw (x, y) = C.rectangle (I.inf x) (I.inf y) (I.extent x) (I.extent y)

class Color a where
    setColor :: a -> C.Render ()

instance Color (Double, Double, Double) where
    setColor = uncurry3 C.setSourceRGB

instance Color (Double, Double, Double, Double) where
    setColor = uncurry4 C.setSourceRGBA

instance Color (Int, Int, Int) where
    setColor = uncurry3 C.setSourceRGB . mapT ((/255.0) . fromIntegral)

instance Color (Int, Int, Int, Int) where
    setColor = uncurry4 C.setSourceRGBA . mapT ((/255.0) . fromIntegral)

{-
instance (Drawable (a, a), Portionable a) => Drawable (Pw a, Pw a) where
    draw (x, y) = map draw $ pwSegs $ zipPw (,) x y

instance (Drawable (a, a)) => Drawable (Pw (a, a)) where
    draw = mapM_ draw . pwSegs
-}


-----------------------------------------------------------------------------
-- Example code follows
-----------------------------------------------------------------------------

mouse1 :: Maybe (Bool, Int) -> (Double, Double) ->
  Focusable Handle -> IO (Focusable Handle)
mouse1 (Just (True, 0))  p = return . clickElement (Just $ Handle "hi" p (1.0,0,0)) p
mouse1 (Just (False, 0)) p = return . changeFocus Nothing
mouse1 Nothing           p = return . moveFocusable (const $ const Nothing) p

key1 (Left "Escape") _ s = quit >> return s
key1 _ _ s = return s

--IDEA: once we have fclabels, make a function called 'override' which,
-- when given a label, sets that label to be equivalent to calling a new
-- function, falling back to the old function in the case of failure.

apipe f s = f s >> return s

demo1 = (dummyToy ([], Nothing))
  { mouse = mouse1, display = displayFocusable, key = key1 }

type DSlider = Slider Double

dline :: (Double, Double) -> (Double, Double) -> DLine
dline (x1, y1) (x2, y2) = (Linear x1 x2, Linear y1 y2)

mouse2 :: Maybe (Bool, Int) -> (Double, Double) ->
  Focusable (Slider Double) -> IO (Focusable (Slider Double))
mouse2 (Just (True, 0))  p = return . clickElement
  (Just $ mkSlider (0 I.... 100) 0 (dline p (p ^+^ (0,100))) (1.0,0,0)) p
mouse2 (Just (False, 0)) _ = return . changeFocus Nothing
mouse2 Nothing           p = return . moveFocusable (const $ const Nothing) p

demo2 = (dummyToy ([], Nothing))
  { mouse = mouse2, display = displayFocusable, key = key1 }

{-
demo1 = startToy [Handle "yo" (100, 100) (1.0, 0.0, 0.0)] upd dr 
  where upd = return . id 
        dr = const (\s -> mapM_ draw s >> return s)
-}

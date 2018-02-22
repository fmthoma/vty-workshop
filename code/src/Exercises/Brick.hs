{-# LANGUAGE LambdaCase #-}
module Exercises.Brick (

    -- * Exercises

    -- ** Exercise 5
      brickListApp
    , brickListAppState

    -- ** Exercise 6
    , dialogApp
    , yesNoDialog

    -- ** Exercise 7
    , renderPongPaddle
    , pongPaddleApp

    -- * Utilities

    , simpleMain
    , SimpleApp (..)
    , runSimpleBrickApp

    -- * Library Functions

    -- ** 'List' Widget
    , List
    , list, renderList, handleListEvent

    -- ** 'Dialog' Widget
    , Dialog
    , dialog, renderDialog, handleDialogEvent

    -- ** Basic Widgets
    , str, center, hCenter, padAll

    -- ** 'AttrMap's
    , attrMap
    , listAttr, listSelectedAttr
    , dialogAttr, buttonAttr, buttonSelectedAttr

    -- ** Rendering
    , Result, emptyResult
    , translateX, translateY, charFill

    -- ** 'Event's & handling
    , BrickEvent (..)
    , Event (..)
    , Key (..)
    , Next, continue, halt

    ) where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Dialog
import           Brick.Widgets.List
import qualified Data.Vector          as Vector
import           Graphics.Vty         as Vty


-- | A simplified version of Brick's 'App' type.
--
-- @'SimpleApp' s e n@ is an application with state @s@ and internal names of
-- type @n@.
data SimpleApp s n = SimpleApp
    { simpleAppDraw        :: s -> [Widget n]
    -- ^ The drawing function. Widgets are stacked, the first item in the list
    -- appears on top.
    --
    -- @
    -- simpleAppDraw :: 'String' -> ['Widget' n]
    -- simpleAppDraw = \s -> ['str' s]
    -- @

    , simpleAppHandleEvent :: s -> BrickEvent n () -> EventM n (Next s)
    -- ^ Handle a Brick event. The 'EventM' type allows access to Brick's
    -- internal state (e.g. @'getVtyHandle' :: 'EventM' n 'Vty'@). To determine
    -- the next action, use one of:
    --
    -- * @'continue' :: s -> 'EventM' n ('Next' s)@ to resume with an updated
    --   application state
    -- * @'halt' :: s -> 'EventM' n ('Next' s)@ to end the app with a given
    --   final state
    -- * @'suspendAndResume' :: 'IO' s -> 'EventM' n ('Next' s)@ to suspend the
    --   application, run the given 'IO' action, and resume with the new
    --   application state.
    --
    -- @
    -- simpleAppHandleEvent :: 'Int' -> 'BrickEvent' n () -> 'EventM' n ('Next' s)
    -- simpleAppHandleEvent = \n e -> case e of
    --     'VtyEvent' ('EvKey' 'KUp' [])   -> 'continue' (n + 1)
    --     'VtyEvent' ('EvKey' 'KDown' []) -> 'continue' (n - 1)
    --     'VtyEvent' ('EvKey' 'KEsc' [])  -> 'halt' n
    --     _otherwise                -> 'continue' n
    -- @

    , simpleAppAttrMap     :: s -> AttrMap
    -- ^ Define a state-dependent 'AttrMap'. An 'AttrMap' is constructed using
    -- @'attrMap' :: 'Attr' -> [('AttrName', 'Attr')] -> 'AttrMap'@, where the
    -- first argument is the default attribute (usually 'defAttr'), and the
    -- second argument are overrides for special attributes. For 'AttrName's,
    -- see documentation of the individual 'Widget's.
    }

-- | Runs a 'SimpleApp' full-screen and prints the final state after the
-- application is ended.
--
-- @
-- let app :: 'SimpleApp' () ()
--     app = 'SimpleApp'
--         { simpleAppDraw = \_ -> ['str' "Hello World"]
--         , simpleAppHandleEvent = \_ _ -> 'halt' ()
--         , simpleAppAttrMap = \_ -> 'attrMap' 'defAttr' [] }
-- in 'runSimpleBrickApp' app ()
-- @
runSimpleBrickApp :: Ord n => SimpleApp s n -> s -> IO s
runSimpleBrickApp simpleApp initialState = defaultMain app initialState
  where
    app = App
        { appDraw = simpleAppDraw simpleApp
        , appChooseCursor = neverShowCursor
        , appHandleEvent = simpleAppHandleEvent simpleApp
        , appStartEvent = pure
        , appAttrMap = simpleAppAttrMap simpleApp }

-- | Similar to 'VtyTest.list2', but now we use Brick. The selected item should
-- be highlighted, the arrow keys should change the selection. The @Enter@ key
-- should end the application.
--
-- Brick already supplies us with a 'List' 'Widget', so have a look at functions
-- like 'renderList' and 'handleListEvent'.
--
-- Keep in mind that everything will be renderd in default style unless you
-- define an `attrMap`!
--
-- Bonus: 'center' the list horizontally and vertically on the screen.
--
-- @
-- 'runSimpleBrickApp' 'brickListApp' 'brickListAppState' >>= 'print' . 'listSelectedElement'
-- @
brickListApp :: (Ord n, Show n) => SimpleApp (List n String) n
brickListApp = SimpleApp
    { simpleAppDraw = \state -> undefined
    , simpleAppHandleEvent = \state event -> undefined
    , simpleAppAttrMap = \state -> undefined
    }

brickListAppState :: List String String
brickListAppState = list "myList" (fmap (("Item " ++) . show) (Vector.fromList [1..10 :: Int])) 0

-- | A 'Dialog' asking the user to answer »Yes« or »No«.
--
-- Brick supplies us with a 'Dialog' Widget, have a look at 'renderDialog' and
-- 'handleDialogEvent'.
--
-- The dialog should be rendered in reverse video, with the active button in
-- red.
--
-- @
-- 'runSimpleBrickApp' 'dialogApp' 'yesNoDialog' >>= 'print' . 'dialogSelection'
-- @
dialogApp :: SimpleApp (Dialog Bool) ()
dialogApp = SimpleApp
    { simpleAppDraw = \state -> undefined
    , simpleAppHandleEvent = \state event -> undefined
    , simpleAppAttrMap = \state -> undefined
    }

yesNoDialog :: Dialog Bool
yesNoDialog = undefined

-- | Define your own 'Widget'.
--
-- Create a widget that renders a Pong paddle. The function takes as parameters
-- the offset of the top edge of the paddle, and the height of the paddle. The
-- widget should be unbounded ('Greedy') in height, and have a width of exactly
-- 1. For the render function, use 'emptyResult' and add an image using the Vty
-- render functions in "Graphics.Vty.Image".
--
-- @
-- 'simpleMain' ('renderPongPaddle' (10, 10))
-- @
renderPongPaddle :: (Int, Int) -> Widget n
renderPongPaddle (paddleTop, paddleHeight) = Widget
    { hSize = undefined
    , vSize = undefined
    , render = pure emptyResult
        { image = undefined } }

-- | Integrate the pong paddle widget into an app.
--
-- Arrow keys should move the paddle up and down, Esc should end the
-- application.
--
-- @
-- 'runSimpleBrickApp' 'pongPaddleApp' (10, 10)
-- @
pongPaddleApp :: SimpleApp (Int, Int) ()
pongPaddleApp = SimpleApp
    { simpleAppDraw = \state -> undefined
    , simpleAppHandleEvent = \(paddleTop, paddleHeight) -> undefined
    , simpleAppAttrMap = \state -> attrMap defAttr [] }

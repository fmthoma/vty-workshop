module Exercises.Vty (

    -- * Exercises

    -- ** Exercise 1
      plainHelloWorld

    -- ** Exercise 2
    , colorfulHelloWorld

    -- ** Exercise 3
    , staticVtyList

    -- ** Exercise 4
    , vtyListApp
    , vtyListAppState

    -- * Utilities

    , renderVtyImage
    , MiniApp (..)
    , runVtyApp

    -- * Library functions

    -- ** Creating and combining 'Image's
    , string
    , horizCat, vertCat, (<|>), (<->)

    -- ** 'Attr'ibutes
    , defAttr
    , withForeColor, withBackColor, red, blue
    , withStyle, standout, bold

    -- ** 'Event's
    , Event(..)
    , Key(..)

    ) where

import           Control.Exception (bracket)
import           Control.Monad     (void)
import           Graphics.Vty

-- | Renders the given 'Image' full-screen, and exits on the first event:
--
-- @
-- 'renderVtyImage' ('string' 'defAttr' "Hello World")
-- @
renderVtyImage :: Image -> IO ()
renderVtyImage image = bracket startVty shutdown drawImage
  where
    startVty = standardIOConfig >>= mkVty
    drawImage vty = do
        update vty (picForImage image)
        void (nextEvent vty)

-- | Print "Hello World" in terminal default style.
--
-- @
-- 'renderVtyImage' 'plainHelloWorld'
-- @
plainHelloWorld :: Image
plainHelloWorld = undefined

-- | Print "Hello World" in red on blue
--
-- @
-- 'renderVtyImage' 'colorfulHelloWorld'
-- @
colorfulHelloWorld :: Image
colorfulHelloWorld = undefined

-- | Print a list with "Item 1", "Item 2", ..., "Item 10", where the first item
-- is in 'standout' (reverse video) style.
--
-- @
-- 'renderVtyImage' 'staticVtyList'
-- @
staticVtyList :: Image
staticVtyList = undefined
  where
    first : rest = fmap (("Item " ++) . show) [1..10 :: Int]


-- A minimalistic Vty application has a state and two functions: One that
-- renders the state as an 'Image', and one that changes the state when an
-- 'Event' occurs.
data MiniApp s = MiniApp
    { appRender :: s -> Image
    -- ^ How to render the image for a given state
    , appHandle :: Event -> s -> Maybe s
    -- ^ State transitions for a given event. Return 'Nothing' to end the
    -- application.
    }

-- | Runs a 'MiniApp' given an initial state
runVtyApp :: MiniApp s -> s -> IO ()
runVtyApp app initialState = bracket startVty shutdown (eventLoop initialState)
  where
    startVty = standardIOConfig >>= mkVty
    eventLoop state vty = do
        update vty (picForImage (appRender app state))
        e <- nextEvent vty
        case appHandle app e state of
            Just state' -> eventLoop state' vty
            Nothing     -> pure ()

-- | This list is similar to 'list1', but should react to arrow keys by moving
-- the index of the selected item. The internal state @(['String'], 'Int')@ is
-- the list of items to be displayed, and the index of the currently selected
-- item.
--
-- @
-- 'runVtyApp' 'vtyListApp' 'vtyListAppState'
-- @
vtyListApp :: MiniApp ([String], Int)
vtyListApp = MiniApp
    { appRender = undefined
    , appHandle = undefined
    }
  where
    split3 :: Int -> [a] -> ([a], a, [a])
    split3 n items
        | n < 0 = ([], head items, tail items)
        | n >= length items = (init items, last items, [])
        | otherwise =
            let (pre, post') = splitAt n items
                ([sel], post) = splitAt 1 post'
            in  (pre, sel, post)

vtyListAppState :: ([String], Int)
vtyListAppState = (fmap (("Item " ++) . show) [1..10 :: Int], 0)

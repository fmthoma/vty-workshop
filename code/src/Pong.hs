module Pong (pong) where

import           Brick
import           Brick.BChan
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Dialog
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Graphics.Vty             as Vty
import           Lens.Micro
import           Lens.Micro.Extras

import           Pong.Motions
import           Pong.Opponent
import           Pong.Physics
import           Pong.Types

pong :: IO ()
pong = do
    events <- newBChan 1000
    eventThread (writeBChan events PhysicsTick >> threadDelay physicsDelay)
    eventThread (writeBChan events OpponentTick >> threadDelay opponentDelay)
    void (customMain (Vty.mkVty Vty.defaultConfig) (Just events) app (Running newGame))
  where
    physicsDelay, opponentDelay :: Int
    physicsDelay = microseconds (1 / 100)
    opponentDelay = microseconds (1 / 10)

    microseconds :: Double -> Int
    microseconds = round . (* 1000000)

    eventThread = void . async . forever

data Tick = PhysicsTick | OpponentTick

app :: App AppState Tick ()
app = App
    { appDraw = \state -> case state of
        Running gameState  -> [renderGame gameState]
        _                  -> []
    , appChooseCursor = const (const Nothing)
    , appHandleEvent = appEvent
    , appStartEvent = pure
    , appAttrMap = const (attrMap defAttr [(buttonSelectedAttr, defAttr `withStyle` standout)]) }

data AppState
    = Running GameState
    | Paused GameState (Dialog Bool)
    | Ready

newGame :: GameState
newGame = GameState
    { _lPlayer = Player { _pTop = 10, _pHeight = 10, _score = 0 }
    , _rPlayer = Player { _pTop = 00, _pHeight = 30, _score = 0 }
    , _field   = Field {_fHeight = 30, _fWidth = 116 }
    , _ball = Ball
        { _position = Cartesian { _x = 60, _y = 15 }
        , _velocity = Polar { _r = 0.5, _phi = -0.13 } } }

appEvent :: AppState -> BrickEvent n Tick -> EventM n (Next AppState)
appEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt state
appEvent state _ = continue state

renderGame :: GameState -> Widget n
renderGame gameState = str "Hello World!"

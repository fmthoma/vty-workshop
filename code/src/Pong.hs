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
    void (customMain (Vty.mkVty Vty.defaultConfig) (Just events) app Ready)
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
        Paused gameState d -> [suspended d, renderGame gameState]
        Ready              -> [areYouReady]
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
    , _rPlayer = Player { _pTop = 10, _pHeight = 10, _score = 0 }
    , _field   = Field {_fHeight = 30, _fWidth = 116 }
    , _ball = Ball
        { _position = Cartesian { _x = 60, _y = 15 }
        , _velocity = Polar { _r = 0.5, _phi = -0.13 } } }

appEvent :: AppState -> BrickEvent n Tick -> EventM n (Next AppState)
appEvent (Running gameState) event = case event of
    VtyEvent e -> case e of
        Vty.EvKey Vty.KEsc  [] -> continue (Paused gameState yesNoDialog)
        Vty.EvKey Vty.KUp   [] -> continue (Running (mvLPlayer (-1) gameState))
        Vty.EvKey Vty.KDown [] -> continue (Running (mvLPlayer 1    gameState))
        _                      -> continue (Running gameState)
    AppEvent e -> case e of
        PhysicsTick -> case collisions (inertialMovement gameState) of
            WithinBounds gameState' -> continue (Running gameState')
            LeftOutOfBounds         -> continue (Running (over (rPlayer . score) (+1) (resetGame gameState newGame)))
            RightOutOfBounds        -> continue (Running (over (lPlayer . score) (+1) (resetGame gameState newGame)))
        OpponentTick -> continue (Running (mvOpponent gameState))
    _ -> continue (Running gameState)
appEvent (Paused gameState d) event = case event of
    VtyEvent (EvKey KEnter []) -> case dialogSelection d of
        Nothing    -> continue (Paused gameState d)
        Just True  -> continue (Running gameState)
        Just False -> halt (Running gameState)
    VtyEvent e -> handleDialogEvent e d >>= continue . Paused gameState
    _ -> continue (Paused gameState d)
appEvent Ready event = case event of
    VtyEvent (EvKey KEnter []) -> continue (Running newGame)
    _ -> continue Ready

renderPaddle :: Player -> Widget n
renderPaddle playerState = Widget
    { hSize = Fixed
    , vSize = Fixed
    , render = pure emptyResult { image = paddleImg } }
  where
    paddleImg = Vty.translateY paddleTop (Vty.charFill Vty.defAttr '#' 1 paddleHeight)
    paddleTop = view pTop playerState
    paddleHeight = view pHeight playerState

renderField :: Field -> Ball -> Widget n
renderField f b = Widget
    { hSize = Fixed
    , vSize = Fixed
    , render = pure emptyResult { image = fieldImage } }
  where
    fieldImage = Vty.resize fieldWidth fieldHeight
        ( Vty.translate ballX ballY
            ( Vty.char Vty.defAttr 'o' ) )
    ballX = view (position . x . to round) b
    ballY = view (position . y . to round) b
    fieldWidth = view fWidth f
    fieldHeight = view fHeight f

renderGame :: GameState -> Widget n
renderGame gameState = center $ borderWithLabel scoreWidget fieldWidget
  where
    scoreWidget = padLeftRight 1 $ hBox
        [ view (lPlayer . score . to show . to str) gameState
        , str " : "
        , view (rPlayer . score . to show . to str) gameState ]
    fieldWidget = vLimit fieldHeight $ hBox
        [ renderPaddle (view lPlayer gameState)
        , renderField (view field gameState) (view ball gameState)
        , renderPaddle (view rPlayer gameState) ]
    fieldHeight = view (field . fHeight) gameState

readyDialog :: Dialog ()
readyDialog = dialog (Just "Pong") (Just (0, [("Go!", ())])) 40

yesNoDialog :: Dialog Bool
yesNoDialog = dialog (Just "Pong") (Just (0, [("Yes", True), ("No", False)])) 40

areYouReady :: Widget n
areYouReady = renderDialog readyDialog (hCenter (padAll 2 (str "Are you ready?")))

suspended :: Dialog Bool -> Widget n
suspended d = renderDialog d (hCenter (padAll 2 (str "Game paused." <=> str "Do you want to continue?")))

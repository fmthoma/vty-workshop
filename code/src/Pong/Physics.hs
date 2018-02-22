module Pong.Physics (inertialMovement, collisions, BoundsCheck (..)) where

import           Lens.Micro
import           Lens.Micro.Extras

import           Pong.Motions
import           Pong.Types


-- | Move the ball inertially, i.e. along the velocity vector.
inertialMovement :: GameState -> GameState
inertialMovement gameState = mvBall deltaX deltaY gameState
  where
    deltaX = view (ball . velocity . r) gameState * cos (view (ball . velocity . phi) gameState)
    deltaY = view (ball . velocity . r) gameState * sin (view (ball . velocity . phi) gameState)

-- | Check for collisions, and bounce the ball from paddles and/or bounds if
-- necessary.
collisions :: GameState -> BoundsCheck
collisions gameState
    | ballReflectedByLeftPaddle
        = WithinBounds (addSpin (deltaPhi lPlayer) (reflectBallLeft gameState))
    | ballReflectedByRightPaddle
        = WithinBounds (addSpin (-deltaPhi rPlayer) (reflectBallRight gameState))
    | ballX < 0
        = LeftOutOfBounds
    | ballX > fieldWidth
        = RightOutOfBounds
    | ballReflectedByField
        = WithinBounds (reflectBallVertically gameState)
    | otherwise
        = WithinBounds gameState
  where
    ballReflectedByField = ballY < 0 || ballY > fieldHeight
    ballReflectedByLeftPaddle = ballX < 0 && ballY > playerTop lPlayer && ballY < playerBottom lPlayer
    ballReflectedByRightPaddle = ballX > fieldWidth && ballY > playerTop rPlayer && ballY < playerBottom rPlayer

    ballX = view (ball . position . x) gameState
    ballY = view (ball . position . y) gameState
    fieldWidth = view (field . fWidth . to fromIntegral) gameState
    fieldHeight = view (field . fHeight . to fromIntegral) gameState
    playerTop player = view (player . pTop . to fromIntegral) gameState
    playerHeight player = view (player . pHeight . to fromIntegral) gameState
    playerBottom player = playerTop player + playerHeight player
    deltaPhi player = (ballY - (playerTop player + playerBottom player) / 2) / playerHeight player / pi

data BoundsCheck
    = LeftOutOfBounds
    -- ^ Ball left the field on the left side (i.e. right player wins)
    | RightOutOfBounds
    -- ^ Ball left the field on the right side (i.e. left player wins)
    | WithinBounds GameState
    -- ^ Ball is inside the field (i.e. game continues)

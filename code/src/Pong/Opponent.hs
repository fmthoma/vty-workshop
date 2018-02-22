module Pong.Opponent where

import           Lens.Micro
import           Lens.Micro.Extras

import           Pong.Motions
import           Pong.Types

-- | Simple computer opponent.
--
-- This opponent will always try to keep the paddle level with the ball, but is
-- limited in its motion speed.
mvOpponent :: GameState -> GameState
mvOpponent gameState
    | ballPos < paddleCenter - paddleHeight / 6 = mvRPlayer (- 1) gameState
    | ballPos > paddleCenter + paddleHeight / 6 = mvRPlayer 1     gameState
    | otherwise                                 = gameState
  where
    ballPos = view (ball . position . y) gameState
    paddleTop = view (rPlayer . pTop . to fromIntegral) gameState
    paddleHeight = view (rPlayer . pHeight . to fromIntegral) gameState
    paddleCenter = paddleTop + paddleHeight / 2

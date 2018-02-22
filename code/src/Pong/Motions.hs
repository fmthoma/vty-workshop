module Pong.Motions (
      mvBall
    , mvLPlayer, mvRPlayer
    , reflectBallLeft, reflectBallRight, reflectBallVertically
    , addSpin
    , resetGame
    ) where

import Lens.Micro
import Lens.Micro.Extras

import Pong.Types

-- | Moves the ball @deltaX@ units right and @deltaY@ units down.
--
-- @
-- mvBall deltaX deltaY
-- @
mvBall :: Double -> Double -> GameState -> GameState
mvBall deltaX deltaY = over (ball . position . x) (+ deltaX)
                     . over (ball . position . y) (+ deltaY)

-- | Moves the left player paddle @deltaY@ units down.
--
-- @
-- mvLPlayer deltaY
-- @
mvLPlayer :: Int -> GameState -> GameState
mvLPlayer deltaY = over (lPlayer . pTop) (+ deltaY)

-- | Moves the right player paddle @deltaY@ units down.
--
-- @
-- mvRPlayer deltaY
-- @
mvRPlayer :: Int -> GameState -> GameState
mvRPlayer deltaY = over (rPlayer . pTop) (+ deltaY)

reflectBallHorizontally :: GameState -> GameState
reflectBallHorizontally = over (ball . velocity . phi) (pi -)

-- | Reflects the ball at the left paddle
reflectBallLeft :: GameState -> GameState
reflectBallLeft = reflectBallHorizontally . set (ball . position . x) 0

-- | Reflects the ball at the right paddle
reflectBallRight :: GameState -> GameState
reflectBallRight = do
    x' <- view (field . fWidth . to fromIntegral)
    reflectBallHorizontally . set (ball . position . x) x'

-- | Reflects the ball at the top or bottom bound
reflectBallVertically :: GameState -> GameState
reflectBallVertically = over (ball . velocity . phi) negate

-- | Changes the direction of motion by @deltaPhi@.
--
-- @
-- addSpin deltaPhi
-- @
addSpin :: Double -> GameState -> GameState
addSpin deltaPhi = over (ball . velocity . phi) (+ deltaPhi)

-- | Returns @newGame@ with the score from @oldGame@
--
-- @
-- resetGame oldGame newGame
-- @
resetGame :: GameState -> GameState -> GameState
resetGame oldGame
    = set (lPlayer . score) (view (lPlayer . score) oldGame)
    . set (rPlayer . score) (view (rPlayer . score) oldGame)

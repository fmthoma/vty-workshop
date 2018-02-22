{-# LANGUAGE TemplateHaskell #-}
module Pong.Types where

import Lens.Micro.TH

data GameState = GameState
    { _lPlayer :: Player
    , _rPlayer :: Player
    , _field   :: Field
    , _ball    :: Ball
    } deriving (Show)


data Player = Player
    { _pTop    :: Int
    , _pHeight :: Int
    , _score   :: Int
    } deriving (Show)

data Field = Field
    { _fHeight :: Int
    , _fWidth  :: Int
    } deriving (Show)

data Ball = Ball
    { _position :: Cartesian
    , _velocity :: Polar
    } deriving (Show)

data Cartesian = Cartesian
    { _x :: Double
    , _y :: Double
    } deriving (Show)

data Polar = Polar
    { _r   :: Double
    , _phi :: Double
    } deriving (Show)

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Field
makeLenses ''Ball
makeLenses ''Cartesian
makeLenses ''Polar

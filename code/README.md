Pong
================================================================================

Implementation suggestion:

* Implement static image rendering (`renderGame`)
    * hint: [`view`] through lenses provided in `Pong.Types`, e.g.
      `view (ball . position . x) gameState`
* Implement inertial movement (`appEvent` for `AppEvent PhysicsTick`)
    * see `Pong.Physics.inertialMovement`
* Implement player movement (`appEvent` for `VtyEvent _`)
    * see `Pong.Player`
* Implement collisions and win/lose situations
    * see `Pong.Physics.collisions`
* Add computer opponent (`appEvent` for `AppEvent OpponentTick`)
    * see `Pong.Opponent`
    * decrease opponent paddle size in `newGame`
* Add dialogs, e.g.:
    * »Are you ready?« dialog on startup, or
    * »Start another game?« when winning/losing, or
    * »Do you really want to quit?« when pressing `Esc`.
* Add score keeping

[`view`]: http://hackage.haskell.org/package/microlens-0.4.8.0/docs/Lens-Micro-Extras.html#v:view

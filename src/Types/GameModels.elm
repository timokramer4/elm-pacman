module Types.GameModels exposing (Direction(..), Game, Ghost, Msg(..), State(..))

import Types.Point exposing (Point)


type alias Game =
    { pPosition : Point
    , pRotation : Int
    , state : State
    , nextDir : Direction
    , score : Int
    , items : List Point
    , pills : List Point
    , itemCounter : Int
    , secondCounter : Int
    , fruitAvailable : Bool
    , redGhost : Ghost
    , pinkGhost : Ghost
    , blueGhost : Ghost
    , yellowGhost : Ghost
    }


type alias Ghost =
    { position : Point
    , dir : Direction
    , lastDir : Direction
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction
    | Fruit
    | GhostMove


type State
    = Running Direction
    | Stopped Direction


type Direction
    = Up
    | Down
    | Left
    | Right
    | None

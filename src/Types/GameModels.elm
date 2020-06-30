module Types.GameModels exposing (Direction(..), Game, Msg(..), State(..))

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
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction


type State
    = Running Direction
    | Stopped Direction


type Direction
    = Up
    | Down
    | Left
    | Right
    | None

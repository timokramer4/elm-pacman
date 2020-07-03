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
    , secondCounter : Int
    , fruitAvailable : Bool
    , redGhoastPosition : Point
    , pinkGhoastPosition : Point
    , blueGhoastPosition : Point
    , yellowGhoastPosition : Point
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction
    | Fruit
    | Ghoast


type State
    = Running Direction
    | Stopped Direction


type Direction
    = Up
    | Down
    | Left
    | Right
    | None

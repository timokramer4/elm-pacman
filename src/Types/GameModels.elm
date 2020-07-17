module Types.GameModels exposing (Direction(..), Game, Ghost, GhostColors(..), Msg(..), SoundModel(..), SoundState(..), State(..))

import Audio
import Time
import Types.Point exposing (Point)


type alias Game =
    { pPosition : Point
    , pRotation : Int
    , lifes : Int
    , state : State
    , nextDir : Direction
    , score : Int
    , message : String
    , items : List Point
    , pills : List Point
    , itemCounter : Int
    , fruitSecondCounter : Int
    , fruitAvailable : Bool
    , redGhost : Ghost
    , pinkGhost : Ghost
    , blueGhost : Ghost
    , yellowGhost : Ghost
    , pillActive : Bool
    , pillSecondCounter : Int
    , sound : SoundModel
    }


type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    }


type SoundState
    = NotPlaying
    | Playing Time.Posix
    | FadingOut Time.Posix Time.Posix


type SoundModel
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel


type alias Ghost =
    { ghostColor : GhostColors
    , position : Point
    , dir : Direction
    , active : Bool
    , offset : Int
    }


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction
    | Fruit
    | Pill
    | GhostMove
    | ResetGame
    | StartGame
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | GetCurrentTime Audio.Source Time.Posix


type State
    = Running Direction
    | Stopped Direction
    | Waiting


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type GhostColors
    = Red
    | Pink
    | Blue
    | Yellow

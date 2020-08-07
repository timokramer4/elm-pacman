module Types.GameModels exposing (Direction(..), Game, Ghost, GhostColors(..), GhostName(..), Msg(..), SoundModel(..), SoundState(..), StartMode(..), State(..), ScoreMessage)

import Audio
import Time
import Types.Point exposing (Point)


type alias Game =
    { pPosition : Point
    , pRotation : Int
    , pacmanSrc : String
    , mouthMovement : Bool
    , lifes : Int
    , state : State
    , nextDir : Direction
    , score : Int
    , message : String
    , items : List Point
    , totalItemCount : Int
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
    , eatenGhostsCounter : Int
    , level : Int
    , sound : SoundModel
    , eatItem : Bool
    , eatItemSecondCounter : Int
    , nextGhostCanGoOut : Bool
    , scoreMessage : ScoreMessage
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
    { color : GhostColors
    , position : Point
    , dir : Direction
    , active : Bool
    , offset : Int
    , src : String
    , goBackInPrison : Bool
    , name : GhostName
    , running : Bool
    }


type alias ScoreMessage =
    {
        point : Point
        , msg : String
    }

type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction
    | Fruit
    | Pill
    | GhostMove
    | ChangeColor
    | ChangePacmanSrc
    | GhostGoBackInPrison
    | ResetGame StartMode
    | StartGame
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | GetCurrentTime Audio.Source Time.Posix
    | EatWaiter
    | ClearScoreMsg


type StartMode
    = NewLevel
    | NormalReset
    | Init


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
    | Hunted
    | White
    | GoBackInPrison


type GhostName
    = Blinky
    | Pinky
    | Inky
    | Clyde

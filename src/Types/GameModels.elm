module Types.GameModels exposing (Direction(..), Game, Ghost, GhostColors(..), GhostName(..), Msg(..), ScoreMessage, SoundModel(..), SoundState(..), StartMode(..), State(..))

import Audio
import Time
import Types.Point exposing (Point)



-- Global game model


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
    , showScoreMessage : Bool
    }



-- Loaded sound file/state


type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    }



-- Sound states


type SoundState
    = NotPlaying
    | Playing Time.Posix
    | FadingOut Time.Posix Time.Posix



-- Different states when loading a sound file


type SoundModel
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel



-- Ghost model


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



-- Score message


type alias ScoreMessage =
    { point : Point
    , msg : String
    }



-- Global message struct


type Msg
    = MoveDirection Direction
    | Nothing
    | NoMoving
    | ChangeDirection Direction
    | Fruit
    | Pill
    | GhostMove GhostName
    | ChangeColor
    | ChangePacmanSrc
    | ResetGame StartMode
    | StartGame
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | GetCurrentTime Audio.Source Time.Posix
    | EatWaiter
    | ClearScoreMsg



-- Start modes


type StartMode
    = NewLevel
    | NormalReset
    | Init



-- Game states


type State
    = Running Direction
    | Stopped Direction
    | Waiting



-- Available move directions


type Direction
    = Up
    | Down
    | Left
    | Right
    | None



-- Ghost colors


type GhostColors
    = Red
    | Pink
    | Blue
    | Yellow
    | Hunted
    | White
    | GoBackInPrison



-- Ghost names


type GhostName
    = Blinky
    | Pinky
    | Inky
    | Clyde

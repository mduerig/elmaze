module Actor exposing (..)

import Ease

type alias Animation =
    { dX : Float -> Float
    , dY : Float -> Float
    , dPhi : Float -> Float
    }

type alias ActorData =
    { x : Int
    , y : Int
    , phi : Direction
    , animation : Animation
    }

type Move
    = Forward
    | TurnLeft
    | TurnRight
    | Nop

type Direction
    = Right
    | Up
    | Left
    | Down

move : Direction -> ActorData -> ActorData
move direction hero =
    case direction of
        Right -> { hero | x = hero.x + 1 }
        Left  -> { hero | x = hero.x - 1 }
        Up    -> { hero | y = hero.y + 1 }
        Down  -> { hero | y = hero.y - 1 }

turn : Direction -> ActorData -> ActorData
turn direction hero =
    { hero | phi =
        case direction of
            Right -> rightOfDirection hero.phi
            Left  -> leftOfDirection hero.phi
            _     -> hero.phi
    }

animate : Animation -> ActorData -> ActorData
animate animation hero =
    { hero | animation = animation }

directionToMove : Direction -> Move
directionToMove direction =
    case direction of
        Up    -> Forward
        Left  -> TurnLeft
        Right -> TurnRight
        _      -> Nop

oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up    -> Down
        Down  -> Up
        Left  -> Right
        Right -> Left

rightOfDirection : Direction -> Direction
rightOfDirection direction =
    case direction of
        Right -> Down
        Up    -> Right
        Left  -> Up
        Down  -> Left

leftOfDirection : Direction -> Direction
leftOfDirection direction =
    case direction of
        Right -> Up
        Up    -> Left
        Left  -> Down
        Down  -> Right

moveToString : Move -> String
moveToString m =
    case m of
        Forward   -> "forward\n"
        TurnLeft  -> "left\n"
        TurnRight -> "right\n"
        _         -> ""

noAnimation : Animation
noAnimation =
    { dY = always 0
    , dX = always 0
    , dPhi = always 0
    }

moveAnimation : Direction -> Animation
moveAnimation direction =
    case direction of
        Left  -> { noAnimation | dX = Ease.inOutBack >> \t -> 1 - t }
        Right -> { noAnimation | dX = Ease.inOutBack >> \t -> t - 1 }
        Up    -> { noAnimation | dY = Ease.inOutBack >> \t -> t - 1 }
        Down  -> { noAnimation | dY = Ease.inOutBack >> \t -> 1 - t }

turnAnimation : Direction -> Animation
turnAnimation direction =
    case direction of
        Left  -> { noAnimation | dPhi = Ease.inOutBack >> \t -> pi/2 * (t - 1) }
        Right -> { noAnimation | dPhi = Ease.inOutBack >> \t -> pi/2 * (1 - t) }
        _     ->   noAnimation

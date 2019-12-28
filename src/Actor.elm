module Actor exposing
    ( Actor
    , Move ( .. )
    , Direction ( .. )
    , IsFreePredicate
    , IsGoalPredicate
    , hero
    , friend
    , viewActor
    , oppositeDirection
    , setActorDirection
    , mapActors
    , canHeroMove
    , isHeroAtGoal
    , canFriendMove
    , moveActorAhead
    , turnActor
    , playLoseAnimation
    , playWinAnimation
    , clearActorAnimation
    , directionToMove
    , moveToString
    )

import Ease
import Collage as C exposing ( Collage )
import Collage.Text as Text

type Actor
    = Hero ActorData
    | Friend ActorData

type alias ActorData =
    { x : Int
    , y : Int
    , phi : Direction
    , animation : Animation
    , avatar : String
    }

type alias Animation =
    { dX : Float -> Float
    , dY : Float -> Float
    , dPhi : Float -> Float
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

type alias IsFreePredicate
    = ( Int, Int ) -> Direction -> Bool

type alias IsGoalPredicate
    = ( Int, Int ) -> Bool

hero : ( Int, Int ) -> Direction -> String -> Actor
hero pos phi avatar = Hero ( actorData pos phi avatar )

friend : ( Int, Int ) -> Direction -> String -> Actor
friend pos phi avatar = Friend ( actorData pos phi avatar )

actorData : ( Int, Int ) -> Direction -> String -> ActorData
actorData pos phi avatar =
    { x = Tuple.first pos
    , y = Tuple.second pos
    , phi = phi
    , animation = noAnimation
    , avatar = avatar
    }

isHeroAtGoal : List Actor -> IsGoalPredicate -> Bool
isHeroAtGoal actors isGoal =
    actors
        |> mapHero (\data -> isGoal ( data.x, data.x ))
        |> headOrElse False

canHeroMove : List Actor -> IsFreePredicate -> Bool
canHeroMove actors isFree =
    actors
        |> mapHero (\data -> isFree (data.x, data.y) data.phi )
        |> headOrElse False

canFriendMove : List Actor -> IsFreePredicate -> Bool
canFriendMove actors isFree =
    actors
        |> mapFriend (\data -> isFree (data.x, data.y) data.phi )
        |> headOrElse False

mapHero : ( ActorData -> a ) -> List Actor -> List a
mapHero f actors =
    actors
        |> List.concatMap
            ( \actor -> case actor of
                Hero data    -> [ f data ]
                Friend _     -> [ ]
            )

mapFriend : ( ActorData -> a ) -> List Actor -> List a
mapFriend f actors =
    actors
        |> List.concatMap
            ( \actor -> case actor of
                Hero _       -> [ ]
                Friend data  -> [ f data ]
            )

headOrElse : a -> List a -> a
headOrElse default xs =
    xs
        |> List.head
        |> Maybe.withDefault default

turnActor : Direction -> Actor -> Actor
turnActor direction actor =
    let
        turn data = data
            |> turnActorData direction
            |> animateActor ( turnAnimation direction )
    in
        mapActor turn actor

turnActorData : Direction -> ActorData -> ActorData
turnActorData direction actor =
    { actor | phi =
        case direction of
            Right -> rightOfDirection actor.phi
            Left  -> leftOfDirection actor.phi
            _     -> actor.phi
    }

animateActor : Animation -> ActorData -> ActorData
animateActor animation actor =
    { actor | animation = animation }

mapActors : ( Actor -> a ) -> ( Actor -> a ) -> List Actor -> List a
mapActors fHero fFriend actors =
    actors
        |> List.map
        ( \actor -> case actor of
            Hero _    -> fHero actor
            Friend _  -> fFriend actor
        )

moveActorAhead : Actor -> Actor
moveActorAhead actor =
    let
        move a = a
            |> moveActor a.phi
            |> animateActor ( moveAnimation a.phi )
    in
        mapActor move actor

moveActor : Direction -> ActorData -> ActorData
moveActor direction actor =
    case direction of
        Right -> { actor | x = actor.x + 1 }
        Left  -> { actor | x = actor.x - 1 }
        Up    -> { actor | y = actor.y + 1 }
        Down  -> { actor | y = actor.y - 1 }

setActorDirection : Direction -> Actor -> Actor
setActorDirection direction actor =
    actor
        |> mapActor ( \data -> { data | phi = direction } )

playLoseAnimation : Actor -> Actor
playLoseAnimation actor =
    actor
        |> mapActor ( \data -> { data | animation = loseAnimation } )

playWinAnimation : Actor -> Actor
playWinAnimation actor =
    actor
        |> mapActor ( \data -> { data | animation = winAnimation } )

clearActorAnimation : Actor -> Actor
clearActorAnimation actor =
    actor
        |> mapActor ( \data -> { data | animation = noAnimation } )

mapActor : ( ActorData -> ActorData ) -> Actor -> Actor
mapActor update actor =
    case actor of
        Hero data   -> Hero ( update data )
        Friend data -> Friend ( update data )

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
        Forward   -> "forward"
        TurnLeft  -> "left"
        TurnRight -> "right"
        _         -> ""

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

winAnimation : Animation
winAnimation =
    { noAnimation
    | dY = Ease.outBack >> \t -> t
    , dPhi = \t -> 4 * pi * t
    }

loseAnimation : Animation
loseAnimation =
    { noAnimation
    | dY = Ease.inBack >> \t -> -10 * t
    , dPhi = \t -> 4 * pi * t
    }

noAnimation : Animation
noAnimation =
    { dY = always 0
    , dX = always 0
    , dPhi = always 0
    }

viewActor : Float -> Float -> Actor -> Collage msg
viewActor t cellSize actor =
    let
        { x, y, phi, animation, avatar } = case actor of
            Hero data    -> data
            Friend data  -> data

        angle = case phi of
            Left  -> pi/2
            Up    -> 0
            Right -> -pi/2
            Down  -> pi

        dPhi = animation.dPhi t
        dX = animation.dX t
        dY = animation.dY t
    in
        [ Text.fromString avatar
            |> Text.size (round (cellSize/5*3))
            |> C.rendered
            |> C.rotate ( angle + dPhi)
        , C.circle (cellSize/5*2)
            |> C.filled C.transparent
        ]
        |> C.group
        |> C.shiftX ( cellSize * ( toFloat x + dX ) )
        |> C.shiftY ( cellSize * ( toFloat y + dY ) )

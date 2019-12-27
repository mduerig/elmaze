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
    , setActor
    , mapActors
    , canHeroMove
    , isHeroAtGoal
    , canFriendMove
    , moveActorAhead
    , turnHero
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
hero pos phi avatar = Hero
    { x = Tuple.first pos
    , y = Tuple.second pos
    , phi = phi
    , animation = noAnimation
    , avatar = avatar
    }

friend : ( Int, Int ) -> Direction -> String -> Actor
friend pos phi avatar = Friend
    { x = Tuple.first pos
    , y = Tuple.second pos
    , phi = phi
    , animation = noAnimation
    , avatar = avatar
    }

queryActor : ( Actor -> Maybe a ) -> List ( Actor ) -> Maybe a
queryActor get actors =
    actors
        |> List.filterMap get
        |> List.head

isHeroAtGoal : List Actor -> ( ( Int, Int ) -> Bool ) -> Bool
isHeroAtGoal actors isGoal =
    let
        atGoal actor =
            case actor of
               Hero data -> Just ( isGoal ( data.x, data.y )  )
               _ -> Nothing
    in
        actors
            |> queryActor atGoal
            |> Maybe.withDefault False

canHeroMove : List Actor -> ( ( Int, Int ) -> Direction -> Bool ) -> Bool
canHeroMove actors isFree =
    let
        canMove actor =
            case actor of
               Hero data -> Just ( isFree ( data.x, data.y ) data.phi )
               _ -> Nothing
    in
        actors
            |> queryActor canMove
            |> Maybe.withDefault False

canFriendMove : List Actor -> ( ( Int, Int ) -> Direction -> Bool ) -> Bool
canFriendMove actors isFree =
    let
        canMove actor =
            case actor of
               Friend data -> Just ( isFree ( data.x, data.y ) data.phi )
               _ -> Nothing
    in
        actors
            |> queryActor canMove
            |> Maybe.withDefault False

setActor : Actor -> List Actor -> List Actor
setActor actor actors =
    actors
        |> List.map
        ( \currentActor ->
            case ( currentActor, actor ) of
                ( Hero _, Hero _ ) -> actor
                ( Friend _, Friend _ ) -> actor
                ( _, _ ) -> currentActor
        )

moveActor : Direction -> ActorData -> ActorData
moveActor direction actor =
    case direction of
        Right -> { actor | x = actor.x + 1 }
        Left  -> { actor | x = actor.x - 1 }
        Up    -> { actor | y = actor.y + 1 }
        Down  -> { actor | y = actor.y - 1 }

turnActor : Direction -> ActorData -> ActorData
turnActor direction actor =
    { actor | phi =
        case direction of
            Right -> rightOfDirection actor.phi
            Left  -> leftOfDirection actor.phi
            _     -> actor.phi
    }

setActorDirection : Direction -> Actor -> Actor
setActorDirection direction actor =
    actor
        |> mapActor ( \data -> { data | phi = direction } )

mapActor : ( ActorData -> ActorData ) -> Actor -> Actor
mapActor update actor =
    case actor of
        Hero data   -> Hero ( update data )
        Friend data -> Friend ( update data )

animateActor : Animation -> ActorData -> ActorData
animateActor animation actor =
    { actor | animation = animation }

moveActorAhead : Actor -> Actor
moveActorAhead actor =
    let
        move a = a
            |> moveActor a.phi
            |> animateActor ( moveAnimation a.phi )
    in
        mapActor move actor

turnHero : Direction -> Actor -> Actor
turnHero direction actor =
    case actor of
        Hero data -> data
            |> turnActor direction
            |> animateActor ( turnAnimation direction )
            |> Hero

        Friend _ -> actor

mapActors : ( Actor -> a ) -> ( Actor -> a ) -> List Actor -> List a
mapActors updateHero updateFriend actors =
    actors
        |> List.map
        ( \actor -> case actor of
            Hero _    -> updateHero actor
            Friend _  -> updateFriend actor
        )

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

viewActor : Float -> Float -> Actor -> Collage msg
viewActor t cellSize actor =
    let
        { x, y, phi, animation, avatar } = case actor of
            Hero actorData    -> actorData
            Friend actorData  -> actorData

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

module Interpreter exposing
    ( init
    , update
    , Interpreter
    )

import Dict exposing ( Dict )

import Parse as P
import Game as G

type Interpreter =
    Interpreter P.Program Bindings

type alias Bindings
    = Dict String P.Move

init : P.Program -> Interpreter
init program = Interpreter program Dict.empty

update : G.Board s -> Interpreter -> ( Interpreter, G.Move )
update board ( Interpreter ( program ) bindings ) =
    let
        hero = G.getHero board.actors
            |> Maybe.withDefault ( G.HeroData 50 50 G.Up G.noHeroAnimation [] )

        isAtGoal tile = tile.tileType == G.Goal

        queryHero : (G.Tile -> Bool) -> Bool
        queryHero = G.queryTile (hero.x, hero.y) board

        evalCondition : P.Condition -> Bool
        evalCondition condition = case condition of
            P.Not notCondition
                -> not <| evalCondition notCondition

            P.Free
                -> queryHero ( G.hasBoundary hero.phi G.Path )

            P.Blocked
                -> queryHero ( G.hasBoundary hero.phi G.Wall )

            P.Goal
                -> queryHero isAtGoal

    in
        case program of
            [] ->
                ( Interpreter [] bindings, G.Nop )

            (P.Command move) :: stmts ->
                case move of
                    P.Do [] ->
                        update board ( Interpreter stmts bindings )

                    P.Do ( P.Move moveId :: moves) ->
                        let
                            stmt : List P.Statement
                            stmt = Dict.get moveId bindings
                                |> Maybe.map (P.Command >> List.singleton)
                                |> Maybe.withDefault []
                        in
                            update board
                                ( Interpreter (stmt ++ ( P.Command <| P.Do moves ) :: stmts) bindings )

                    P.Do ( atomicMove :: moves) ->
                        ( Interpreter ( ( P.Command <| P.Do <| moves ) :: stmts ) bindings
                        , case atomicMove of
                            P.Left     -> G.TurnLeft
                            P.Right    -> G.TurnRight
                            P.Forward  -> G.Forward
                            P.Move _   -> G.Nop
                        )

                    P.If condition trueMoves falseMoves ->
                        let
                            nextMoves : List P.Statement
                            nextMoves =
                                ( if evalCondition condition
                                    then Just trueMoves
                                    else falseMoves
                                )
                                |> Maybe.map ( P.Do >> P.Command >> List.singleton )
                                |> Maybe.withDefault []
                        in
                            update board ( Interpreter ( nextMoves ++ stmts ) bindings )

                    P.While condition moves ->
                        let
                            nextMoves : List P.Statement
                            nextMoves =
                                if evalCondition condition
                                    then
                                        [ P.Command <| P.Do moves
                                        , P.Command move
                                        ]
                                    else
                                        []
                        in
                            update board ( Interpreter ( nextMoves ++ stmts ) bindings )

                    P.Repeat count moves ->
                        let
                            nextMoves : List P.Statement
                            nextMoves =
                                if count > 0
                                    then
                                        [ P.Command <| P.Do moves
                                        , P.Command <| P.Repeat ( count - 1 ) moves
                                        ]
                                    else
                                        []
                        in
                            update board ( Interpreter ( nextMoves ++ stmts ) bindings )

            (P.Binding (P.Let moveId move)) :: stmts ->
                update board
                    ( Interpreter stmts ( Dict.insert moveId move bindings ) )

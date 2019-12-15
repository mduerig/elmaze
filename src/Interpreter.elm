module Interpreter exposing
    ( void
    , init
    , update
    , Interpreter
    )

import Dict exposing ( Dict )

import Parse as P
import Actor as A

type Interpreter =
    Interpreter P.Program Bindings

type alias Bindings
    = Dict String P.Move

void : Interpreter
void = init []

init : P.Program -> Interpreter
init program = Interpreter program Dict.empty

update : ( P.Condition -> Bool ) -> Interpreter -> ( Interpreter, A.Move )
update isMet ( Interpreter ( program ) bindings ) =
    let
        update0 int = update isMet int
    in
        case program of
            [] ->
                ( Interpreter [] bindings, A.Nop )

            (P.Command move) :: stmts ->
                case move of
                    P.Do [] ->
                        update0 ( Interpreter stmts bindings )

                    P.Do ( P.Move moveId :: moves) ->
                        let
                            stmt : List P.Statement
                            stmt = Dict.get moveId bindings
                                |> Maybe.map (P.Command >> List.singleton)
                                |> Maybe.withDefault []
                        in
                            update0 ( Interpreter (stmt ++ ( P.Command <| P.Do moves ) :: stmts) bindings )

                    P.Do ( atomicMove :: moves) ->
                        ( Interpreter ( ( P.Command <| P.Do <| moves ) :: stmts ) bindings
                        , case atomicMove of
                            P.Left     -> A.TurnLeft
                            P.Right    -> A.TurnRight
                            P.Forward  -> A.Forward
                            P.Move _   -> A.Nop
                        )

                    P.If condition trueMoves falseMoves ->
                        let
                            nextMoves : List P.Statement
                            nextMoves =
                                ( if isMet condition
                                    then Just trueMoves
                                    else falseMoves
                                )
                                |> Maybe.map ( P.Do >> P.Command >> List.singleton )
                                |> Maybe.withDefault []
                        in
                            update0 ( Interpreter ( nextMoves ++ stmts ) bindings )

                    P.While condition moves ->
                        let
                            nextMoves : List P.Statement
                            nextMoves =
                                if isMet condition
                                    then
                                        [ P.Command <| P.Do moves
                                        , P.Command move
                                        ]
                                    else
                                        []
                        in
                            update0 ( Interpreter ( nextMoves ++ stmts ) bindings )

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
                            update0 ( Interpreter ( nextMoves ++ stmts ) bindings )

            (P.Binding (P.Let moveId move)) :: stmts ->
                update0 ( Interpreter stmts ( Dict.insert moveId move bindings ) )

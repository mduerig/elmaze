module Main exposing ( main )

import Game exposing (Game, Msg(..), Direction(..), Move(..) )

updateGame : List Msg -> ( List Msg, Move )
updateGame commands =
    let
        newCommands =
            List.tail commands
                |> Maybe.withDefault []

        move =
            case List.head commands of
                Just (KeyArrow Up)    -> Forward
                Just (KeyArrow Right) -> TurnRight
                Just (KeyArrow Left)  -> TurnLeft
                _                     -> Nop
    in
        ( newCommands, move )


main : Program () (Game ( List Msg ) ) Msg
main =
    Game.play
        { init = .commands
        , update = updateGame
        }


-- program
--   = command*
--     WHERE
--     definition*
--
-- definition
--   = commandDefinition
--   | conditionDefinition
--
-- commandDefinition
--   = COMMAND_IDENTIFIER IDENTIFIER* ASSIGN ...
--
-- conditionDefinition
--   = CONDITION_IDENTIFIER IDENTIFIER* ASSIGN ...
--
-- command
--   = COMMAND_IDENTIFIER parameter*
--   | CONDITION_IDENTIFIER parameter* command
--
-- parameter
--   = NUM
--   | BOOL
--
-- COMMAND_IDENTIFIER = IDENTIFIER
-- CONDITION_IDENTIFIER = IDENTIFIER"?"
-- IDENTIFIER = [a-zA-Z][a-zA-Z0-9]*
-- ASSIGN = "="
-- NUM = [+-]?0*[0-9]
-- BOOL = "true"|"false"

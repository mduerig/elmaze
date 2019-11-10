module Parse exposing (..)

import Set exposing ( Set )
import Parser exposing ( .. )

reserved : Set String
reserved = Set.fromList [ "let", "if", "then", "else", "while", "repeat" ]

-- atomic_move
--   = LEFT
--   | RIGHT
--   | FORWARD
--   | move_id

type AtomicMove
    = Left
    | Right
    | Forward
    | Move String

-- move_id
--   = ID

moveId : Parser String
moveId = variable
    { start = Char.isAlpha
    , inner = Char.isAlphaNum
    , reserved = reserved
    }

atomicMove : Parser AtomicMove
atomicMove =
    oneOf
        [ succeed Left
            |. keyword "left"
        , succeed Right
            |. keyword "right"
        , succeed Forward
            |. keyword "forward"
        , succeed Move
            |. keyword "move"
            |. spaces
            |= moveId
        ]


-- moves
--   = atomic_move
--   | ( atomic_move* )

type alias Moves
    = List AtomicMove

moves : Parser Moves
moves =
    oneOf
        [ succeed List.singleton
            |= atomicMove
        , sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = atomicMove
            , trailing = Forbidden
            }
        ]


-- condition
--   = FREE
--   | BLOCKED
--   | GOAL
--   | NOT condition

type Condition
    = Free
    | Blocked
    | Goal
    | Not Condition

condition : Parser Condition
condition =
    oneOf
        [ succeed Free
            |. keyword "free"
        , succeed Blocked
            |. keyword "blocked"
        , succeed Goal
            |. keyword "goal"
        , succeed Not
            |. keyword "not"
            |. spaces
            |= lazy (\_ -> condition)
        ]


-- move
--   = moves
--   | IF condition THEN moves [ ELSE moves ]
--   | WHILE condition moves
--   | REPEAT int moves

type Move
    = Do Moves
    | If Condition Moves ( Maybe Moves )
    | While Condition Moves
    | Repeat Int Moves

move : Parser Move
move =
    oneOf
        [ succeed Do
            |= moves
        , succeed If
            |. keyword "if"
            |. spaces
            |= condition
            |. spaces
            |. keyword "then"
            |. spaces
            |= moves
            |. spaces
            |= maybeMoves
        , succeed While
            |. keyword "while"
            |. spaces
            |= condition
            |. spaces
            |= moves
        , succeed Repeat
            |. keyword "repeat"
            |. spaces
            |= int
            |. spaces
            |= moves
        ]

maybeMoves : Parser ( Maybe Moves )
maybeMoves =
    oneOf
        [ succeed Just
            |. keyword "else"
            |. spaces
            |= moves
        , succeed Nothing
        ]


-- binding
--   = LET move_id = move

type Binding =
    Let String Move

binding : Parser Binding
binding =
    succeed Let
        |. keyword "let"
        |. spaces
        |= moveId
        |. spaces
        |. symbol "="
        |. spaces
        |= move


-- program
--   = ( move | binding )*

type Statement
    = Command Move
    | Binding Binding

statement : Parser Statement
statement =
    oneOf
        [ succeed Command
            |= move
        , succeed Binding
            |= binding
        ]

type alias Program
    = List Statement

program : Parser Program
program =
    let
        ws : Parser ()
        ws = chompWhile (\c -> c == ' ' || c == '\t')

        eol : Parser ()
        eol = ws |. oneOf [ symbol "\n", symbol "\r" ]

        line : Parser ( Maybe Statement )
        line = succeed identity
            |. ws
            |= oneOf
                [ map Just statement
                , succeed Nothing |. token ""
                ]
            |. eol

        lines : Program -> Parser ( Step Program Program )
        lines statements = oneOf
            [ succeed (\stmt -> case stmt of
                            Just s  -> Loop (s :: statements)
                            _       -> Loop  statements)
                |= line
            , succeed ()
                |. end
                |> map (\_ -> Done ( List.reverse statements ) )
            ]
    in
        loop [] lines


{-
Examples:
.let rightIfFreeForwardOtherwise = if free then right else [right, forward, move rightIfFreeForwardOtherwise]
.let forwardUntilLeftFree = [ left, move rightIfFreeForwardOtherwise ]

.let leftIfFreeForwardOtherwise = if free then left else [left, forward, move forwardUntilRightFree]
.let forwardUntilRightFree = [ right, move leftIfFreeForwardOtherwise ]

.let forwardUntilBlocked = while free forward
.let forwardUntilGoal = while not goal forward

Solution:
( forwardUntilRightFree right forwardUntilLeftFree forwardUntilGoal )
-}
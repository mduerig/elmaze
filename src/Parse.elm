module Parse exposing
    ( parse
    , Program
    , Statement (..)
    , Binding (..)
    , Move (..)
    , Moves
    , AtomicMove (..)
    , Condition (..)
    )

import Set
import Parser exposing (..)


type alias Program
    = List Statement

type Statement
    = Binding Binding
    | Command Move

type Binding =
    Let String Move

type Move
    = Do Moves
    | If Condition Moves ( Maybe Moves )
    | While Condition Moves
    | Repeat Int Moves

type alias Moves
    = List AtomicMove

type AtomicMove
    = Left
    | Right
    | Forward
    | Move String

type Condition
    = Free
    | Blocked
    | Goal
    | Not Condition


parse : String -> Result (List DeadEnd) Program
parse programText = run program programText

program : Parser Program
program =
    let
        line : Parser ( Maybe Statement )
        line = succeed identity
            |. hspaces
            |= oneOf
                [ map Just statement
                , succeed Nothing |. token ""
                ]
            |. hspaces
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

statement : Parser Statement
statement =
    oneOf
        [ succeed Command
            |= move
        , succeed Binding
            |= binding
        ]

binding : Parser Binding
binding =
    succeed Let
        |. keyword "let"
        |. hspaces
        |= moveId
        |. hspaces
        |. symbol "="
        |. hspaces
        |= move

move : Parser Move
move =
    oneOf
        [ succeed Do
            |= moves
        , succeed If
            |. keyword "if"
            |. hspaces
            |= condition
            |. hspaces
            |. keyword "then"
            |. hspaces
            |= moves
            |= maybeMoves
        , succeed While
            |. keyword "while"
            |. hspaces
            |= condition
            |. hspaces
            |= moves
        , succeed Repeat
            |. keyword "repeat"
            |. hspaces
            |= int
            |. hspaces
            |= moves
        ]

maybeMoves : Parser ( Maybe Moves )
maybeMoves =
    oneOf
        [ succeed Just
            |. hspaces
            |. keyword "else"
            |. hspaces
            |= moves
        , succeed Nothing
        ]

moves : Parser Moves
moves =
    oneOf
        [ succeed List.singleton
            |= atomicMove
        , sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = hspaces
            , item = atomicMove
            , trailing = Forbidden
            }
        ]

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
            |= moveId
        ]

moveId : Parser String
moveId = variable
    { start = Char.isAlpha
    , inner = Char.isAlphaNum
    , reserved = Set.fromList
        [ "let", "if", "then", "else", "while", "repeat", "left", "right", "forward" ]
    }

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
            |. hspaces
            |= lazy (\_ -> condition)
        ]

hspaces : Parser ()
hspaces = chompWhile (\c -> c == ' ' || c == '\t')

eol : Parser ()
eol = oneOf [ symbol "\n", symbol "\r" ]

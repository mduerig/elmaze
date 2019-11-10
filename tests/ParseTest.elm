module ParseTest exposing (..)

import Expect exposing ( Expectation )
import Test exposing (..)

import Parse exposing ( program )
import Parser

expectOk : Result (List Parser.DeadEnd) a -> Expectation
expectOk s =
    case s of
        Ok _       -> Expect.pass
        Err error  -> Expect.fail <| Debug.toString error

testProgram : String -> () -> Expectation
testProgram text =
    \_ -> Parser.run program text
            |> expectOk

suite : Test
suite = describe "Parse tests"
    [ test "empty program" <|
        testProgram
            ""

    , test "atomic move" <|
        testProgram
            "left\n"

    , test "custom move" <|
        testProgram
            "move foobar\n"

    , test "list of atomic moves" <|
        testProgram
            "[left, right,move  foobar,forward]\n"

    , test "empty list of moves" <|
        testProgram
            "[]\n"

    , test "if condition" <|
        testProgram
            "if free then left\n"

    , test "if-else condition" <|
        testProgram
            "if goal then [left, right] else []\n"

    , test "repeat loop" <|
        testProgram
            "repeat 42 move foobar\n"

    , test "while loop" <|
        testProgram
            "while not blocked [left, right]\n"

    , test "let binding" <|
        testProgram
            "let q = while not blocked [left, right]\n"

    , test "leading spaces" <|
        testProgram
            "    left\n"

    , test "trailing spaces" <|
        testProgram
            "left   \n"

    , test "multiple lines" <|
        testProgram
            "left\nleft\nleft\n"

    , test "empty lines" <|
        testProgram
            "left\n\nleft\n"

    , test "program" <|
        testProgram  <|
            """
            let rightIfFreeForwardOtherwise = if free then right else [right, forward, move rightIfFreeForwardOtherwise]
            let forwardUntilLeftFree = [ left, move rightIfFreeForwardOtherwise ]

            let leftIfFreeForwardOtherwise = if free then left else [left, forward, move forwardUntilRightFree]
            let forwardUntilRightFree = [ right, move leftIfFreeForwardOtherwise ]

            let forwardUntilBlocked = while free forward
            let forwardUntilGoal = while not goal forward

            [ move forwardUntilRightFree, right, move forwardUntilLeftFree, move forwardUntilGoal ]
            """ ++ "\n"
    ]


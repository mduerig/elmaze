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

expectErr : Result a value -> Expectation
expectErr s =
    case s of
        Ok value  -> Expect.fail <| "Expected parsing to fail. Got " ++ Debug.toString value
        Err _     -> Expect.pass

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
            "foobar\n"

    , test "list of atomic moves" <|
        testProgram
            "[left, right,  foobar,forward]\n"

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
            "repeat 42  foobar\n"

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
            let rightIfFreeForwardOtherwise = if free then right else [right, forward, rightIfFreeForwardOtherwise]
            let forwardUntilLeftFree = [ left, rightIfFreeForwardOtherwise ]

            let leftIfFreeForwardOtherwise = if free then left else [left, forward, forwardUntilRightFree]
            let forwardUntilRightFree = [ right, leftIfFreeForwardOtherwise ]

            let forwardUntilBlocked = while free forward
            let forwardUntilGoal = while not goal forward

            [ forwardUntilRightFree, right, forwardUntilLeftFree, forwardUntilGoal ]
            """ ++ "\n"

    , test "fail on reserved word in move" <|
        \_ -> Parser.run program "[then]\n"
            |> expectErr

    , test "fail on reserved word in binding" <|
        \_ -> Parser.run program "let then = [left]\n"
            |> expectErr

    ]


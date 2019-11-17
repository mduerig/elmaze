module ParseTest exposing ( suite )

import Expect exposing ( Expectation )
import Test exposing (..)

import Parse exposing ( parse )

expectOk : Result (List a) b -> Expectation
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
    \_ -> parse text
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
            let rightIfBlocked = if blocked then [right, forward, forwardUntilLeftFree]
            let forwardUntilLeftFree = [left, rightIfBlocked]

            let leftIfBlocked = if blocked then [left, forward, forwardUntilRightFree]
            let forwardUntilRightFree = [right, leftIfBlocked]

            let forwardUntilGoal = while not goal forward

            forwardUntilRightFree
            forward
            forwardUntilLeftFree
            forward
            forwardUntilLeftFree
            forwardUntilGoal
            """ ++ "\n"

    , test "fail on reserved word in move" <|
        \_ -> parse "[then]\n"
            |> expectErr

    , test "fail on reserved word in binding" <|
        \_ -> parse "let then = [left]\n"
            |> expectErr

    ]


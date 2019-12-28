module Controller exposing
    ( Controller
    , nopController
    , keyboardController
    , programController
    , updateKeyboardController
    , updateProgramController
    , resetKeyboardController
    , resetProgramController
    , isProgram
    , getInput
    )

import Actor as A
import Interpreter exposing ( Interpreter )
import Parse as P

type Controller
    = Keyboard A.Move
    | Program ( Interpreter, A.Move )
    | Nop

keyboardController : Controller
keyboardController = Keyboard A.Nop

programController : Interpreter -> Controller
programController interpreter = Program ( interpreter, A.Nop )

nopController : Controller
nopController = Nop

getInput : Controller -> A.Move
getInput controller =
    case controller of
        Keyboard move        -> move
        Program ( _ , move ) -> move
        Nop                  -> A.Nop

isProgram : Controller -> Bool
isProgram controller =
    case controller of
        Program _ -> True
        _ -> False

updateKeyboardController : A.Direction -> Controller -> Controller
updateKeyboardController direction controller =
    case controller of
        Keyboard _  -> Keyboard ( A.directionToMove direction )
        _           -> controller

resetKeyboardController : Controller -> Controller
resetKeyboardController controller =
    case controller of
        Keyboard _  -> keyboardController
        _           -> controller

updateProgramController : ( P.Condition -> Bool ) -> Controller -> Controller
updateProgramController isTrue controller =
    case controller of
        Program ( interpreter, _ )  -> Program ( Interpreter.update isTrue interpreter )
        _                           -> controller

resetProgramController : Controller -> Controller
resetProgramController controller =
    case controller of
        Program ( interpreter, _ )  -> programController interpreter
        _                           -> controller

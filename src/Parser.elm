module Parser exposing (..)

parse = ""


-- atomar_move
--   = LEFT
--   | RIGHT
--   | FORWARD
--   | move_id
--
-- simple_move
--   = atomar_move
--   | ( atomar_move* )
--
-- move
--   = simple_move
--   | IF condition THEN simple_move [ ELSE simple_move ]
--   | WHILE condition simple_move
--   | REPEAT num simple_move
--
-- condition
--   = FREE
--   | BLOCKED
--   | GOAL
--   | !condition
--
-- num
--   = nat
--
-- binding
--   = LET move_id = move
--
-- move_id
--   = ID
--
-- program
--   = ( move | binding )*
--
--
-- Examples:
-- rightIfFreeForwardOtherwise = if free then right else (right forward forwardUntilLeftFree)
-- forwardUntilLeftFree = ( left rightIfFreeForwardOtherwise )
--
-- leftIfFreeForwardOtherwise = if free then left else (left forward forwardUntilRightFree)
-- forwardUntilRightFree = ( right leftIfFreeForwardOtherwise )
--
-- forwardUntilBlocked = while free forward
-- forwardUntilGoal = while !goal forward
--
-- Solution:
-- ( forwardUntilRightFree right forwardUntilLeftFree forwardUntilGoal )

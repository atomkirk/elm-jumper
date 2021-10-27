module Example exposing (suite)

import DateTime exposing (..)
import Expect
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Change"
        [ test "You can change a day" <|
            \_ ->
                DateTime.change [ day 4, month 1 ] (new 2021 10 25 8 31 0)
                    |> Expect.equal (new 2021 1 4 8 31 0)
        , test "You can move forward" <|
            \_ ->
                DateTime.move [ day 4, minute 3 ] (new 1986 7 11 0 0 0)
                    |> Expect.equal (new 1986 7 15 0 3 0)
        , test "You can move backward" <|
            \_ ->
                DateTime.move [ day -4 ] (new 1986 7 11 0 0 0)
                    |> Expect.equal (new 1986 7 7 0 0 0)
        ]

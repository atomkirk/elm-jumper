module DateTime exposing (day, hour, minute, month, second, year, change, new, move)

import Time
import Array exposing (Array)


type Component
    = Year Int
    | Month Int
    | Day Int
    | Hour Int
    | Minute Int
    | Second Int


type alias Components =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


posixToComponents : Time.Posix -> Components
posixToComponents posix =
    { year = Time.toYear Time.utc posix
    , month = Time.toMonth Time.utc posix |> monthToInt
    , day = Time.toDay Time.utc posix
    , hour = Time.toHour Time.utc posix
    , minute = Time.toMinute Time.utc posix
    , second = Time.toSecond Time.utc posix
    }



-- TODO

new : Int -> Int -> Int -> Int -> Int -> Int -> Time.Posix
new y m d h min s =
  componentsToPosix {year = y, month = m, day = d, hour = h, minute = min, second = s}

componentsToPosix : Components -> Time.Posix
componentsToPosix components =
    Time.millisToPosix (toMillis components)


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12

year : Int -> Component
year n =
    Year n


month : Int -> Component
month n =
    Month n


day : Int -> Component
day n =
    Day n


hour : Int -> Component
hour n =
    Hour n


minute : Int -> Component
minute n =
    Minute n


second : Int -> Component
second n =
    Second n


change : List Component -> Time.Posix -> Time.Posix
change compList posix =
    let
        components =
            posixToComponents posix
    in
    (List.foldr update components compList) |> componentsToPosix

move : List Component -> Time.Posix -> Time.Posix
move compList posix =
    let
        components =
            posixToComponents posix
    in
    (List.foldr add components compList) |> componentsToPosix


update : Component -> Components -> Components
update component components =
    case component of
        Year int ->
            { components | year = int }

        Month int ->
            { components | month = int }

        Day int ->
            { components | day = int }

        Hour int ->
            { components | hour = int }

        Minute int ->
            { components | minute = int }

        Second int ->
            { components | second = int }

add : Component -> Components -> Components
add component components =
    case component of
        Year int ->
            { components | year = components.year + int }

        Month int ->
            { components | month = components.month + int }

        Day int ->
            { components | day = components.day + int }

        Hour int ->
            { components | hour = components.hour + int }

        Minute int ->
            { components | minute = components.minute + int }

        Second int ->
            { components | second = components.second + int }














-- PRIVATE ----------------------------------------------------------------------

toMillis : Components -> Int
toMillis components =
    millisSinceEpoch components.year
        + millisSinceStartOfTheYear components.year components.month
        + millisSinceStartOfTheMonth components.day

millisSinceEpoch : Int -> Int
millisSinceEpoch y =
    let
        epochYear =
            1970

        getTotalMillis =
            List.sum << List.map millisInYear
    in
    if y >= 1970 then
        -- We chose (year - 1) here because we want the milliseconds
        -- in the start of the target year in order to add
        -- the months + days + hours + minutes + secs + millis if we want to.
        getTotalMillis (List.range epochYear (y - 1))

    else
        -- We chose (epochYear - 1) here because we want to
        -- get the total milliseconds of all the previous years,
        -- including the target year which we'll then add
        -- the months + days + hours + minutes + secs + millis in millis
        -- in order to get the desired outcome.
        -- Example: Target date = 26 Aug 1950.
        -- totalMillis from 1/1/1950 - 1/1/1969 = -631152000000
        -- 26 Aug date millis = 20476800000
        -- Resulting millis will be = -631152000000 + 20476800000 == -610675200000 == 26 Aug 1950
        Basics.negate <| getTotalMillis (List.range y (epochYear - 1))

millisSinceStartOfTheYear : Int -> Int -> Int
millisSinceStartOfTheYear y m =
    List.foldl
        (\mo res ->
            res + (millisInADay * (lastDayOf y mo))
        )
        0
        (getPrecedingMonths m)

getPrecedingMonths : Int -> List Int
getPrecedingMonths m =
    Array.toList <|
        Array.slice 0 (m - 1) months

months : Array Int
months =
    Array.fromList
        [ 1
        , 2
        , 3
        , 4
        , 5
        , 6
        , 7
        , 8
        , 9
        , 10
        , 11
        , 12
        ]

{-| Returns the milliseconds in a year.
-}
millisInYear : Int -> Int
millisInYear y =
    if isLeapYear y then
        millisInADay * 366

    else
        millisInADay * 365

{-| Returns the milliseconds in a day.
-}
millisInADay : Int
millisInADay =
    1000 * 60 * 60 * 24

{-| Checks if the `Year` part of the given [Date](Calendar#Date) is a leap year.
    -- date  == 25 Dec 2019
    isLeapYear (getYear date) -- False
    -- date2 == 25 Dec 2020
    isLeapYear (getYear date2) -- True
-}
isLeapYear : Int -> Bool
isLeapYear int =
    (modBy 4 int == 0) && ((modBy 400 int == 0) || not (modBy 100 int == 0))

millisSinceStartOfTheMonth : Int -> Int
millisSinceStartOfTheMonth d =
    -- -1 on the day because we are currently on that day and it hasn't passed yet.
    -- We also need time in order to construct the full posix.
    millisInADay * (d - 1)

lastDayOf : Int -> Int -> Int
lastDayOf y m =
    case m of
        1 ->
            31

        2 ->
            if isLeapYear y then
                29

            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        _ ->
            31
module Entry.Parser exposing (parse)

import Main exposing (Date, Entry)
import Parser exposing (..)


checkRange : Int -> String -> Parser String
checkRange max string =
    let
        int =
            string |> String.toInt |> Maybe.withDefault 0
    in
    if int > max then
        problem ("Value cannot be bigger than " ++ String.fromInt max)

    else
        succeed string


checkDigitsLength : Int -> String -> Parser String
checkDigitsLength length string =
    if String.length string == length then
        succeed string

    else
        problem ("This segment must have " ++ String.fromInt length ++ " digits.")


maybeToIntParser : Maybe Int -> Parser Int
maybeToIntParser maybe =
    case maybe of
        Just i ->
            succeed i

        Nothing ->
            problem "Couldn't parse value to integer."


digitsParser : Parser String
digitsParser =
    getChompedString (chompWhile Char.isDigit)


segmentParser : Int -> Parser String
segmentParser length =
    digitsParser
        |> andThen (checkDigitsLength length)


dayParser : Parser String
dayParser =
    segmentParser 2 |> andThen (checkRange 31)


monthParser : Parser String
monthParser =
    segmentParser 2 |> andThen (checkRange 12)


yearParser : Parser String
yearParser =
    segmentParser 4 |> andThen (checkRange 9999)


dateParser : Parser Date
dateParser =
    (succeed (\y m d -> ( y, m, d ))
        |= yearParser
        |. token "-"
        |= monthParser
        |. token "-"
        |= dayParser
    )
        |> andThen
            (\( y, m, d ) ->
                (y ++ m ++ d)
                    |> String.toInt
                    |> maybeToIntParser
            )


amountParser : Parser Int
amountParser =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= numberParser
        , numberParser
        ]


numberParser : Parser Int
numberParser =
    number
        { int = Just identity
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just <| (*) 100 >> round
        }


simpleParser : Parser Entry
simpleParser =
    succeed Entry
        |= dateParser
        |. spaces
        |= amountParser
        |. spaces
        |= succeed []
        |= (getChompedString (chompUntilEndOr "\n") |> andThen (String.trim >> succeed))


fullParser : Parser Entry
fullParser =
    succeed Entry
        |= dateParser
        |. spaces
        |= amountParser
        |. spaces
        |= (getChompedString (chompUntil "|") |> andThen (prepareTags >> succeed))
        |. token "|"
        |= (getChompedString (chompUntilEndOr "\n") |> andThen (String.trim >> succeed))


parse : Parser Entry
parse =
    oneOf [ backtrackable fullParser, simpleParser ]


prepareTags : String -> List String
prepareTags =
    String.split "," >> List.map String.trim >> List.filter (String.isEmpty >> not)


module PlayWhe exposing
    ( spirit
    , formatDate, formatPeriod
    , monthAbbr
    )

import Dict exposing (Dict)

spirit : Int -> String
spirit n =
    -- 1 <= n <= 36
    Maybe.withDefault "unknown" (Dict.get n spirits)

formatDate : String -> String
formatDate date =
    -- Assumes date has the format "yyyy-mm-dd"
    let
        parts = String.split "-" date

        day dd =
            if String.startsWith "0" dd then
                String.dropLeft 1 dd
            else
                dd

        ordinalIndicator dd =
            case dd of
                "01" -> "st"
                "21" -> "st"
                "31" -> "st"
                "02" -> "nd"
                "22" -> "nd"
                "03" -> "rd"
                "23" -> "rd"
                _ -> "th"
    in
        case parts of
            [yyyy, mm, dd] -> String.join " "
                [ monthAbbr (Result.withDefault 0 (String.toInt mm))
                , day dd ++ ordinalIndicator dd ++ ","
                , yyyy
                ]

            _ -> "Unknown"

formatPeriod : String -> String
formatPeriod period =
    case period of
        "EM" -> "10:30am"
        "AM" -> "1:00pm"
        "AN" -> "4:00pm"
        "PM" -> "6:30pm"
        _ -> "--:--"

monthAbbr : Int -> String
monthAbbr n =
    -- 1 <= n <= 12
    Maybe.withDefault "Unk" (Dict.get n monthAbbrs)

spirits : Dict Int String
spirits = Dict.fromList
    [ (1, "centipede")
    , (2, "old lady")
    , (3, "carriage")
    , (4, "dead man")
    , (5, "parson man")
    , (6, "belly")
    , (7, "hog")
    , (8, "tiger")
    , (9, "cattle")
    , (10, "monkey")
    , (11, "corbeau")
    , (12, "king")
    , (13, "crapaud")
    , (14, "money")
    , (15, "sick woman")
    , (16, "jamette")
    , (17, "pigeon")
    , (18, "water boat")
    , (19, "horse")
    , (20, "dog")
    , (21, "mouth")
    , (22, "rat")
    , (23, "house")
    , (24, "queen")
    , (25, "morocoy")
    , (26, "fowl")
    , (27, "little snake")
    , (28, "red fish")
    , (29, "opium man")
    , (30, "house cat")
    , (31, "parson wife")
    , (32, "shrimps")
    , (33, "spider")
    , (34, "blind man")
    , (35, "big snake")
    , (36, "donkey")
    ]

monthAbbrs : Dict Int String
monthAbbrs = Dict.fromList
    [ (1, "Jan")
    , (2, "Feb")
    , (3, "Mar")
    , (4, "Apr")
    , (5, "May")
    , (6, "Jun")
    , (7, "Jul")
    , (8, "Aug")
    , (9, "Sep")
    , (10, "Oct")
    , (11, "Nov")
    , (12, "Dec")
    ]

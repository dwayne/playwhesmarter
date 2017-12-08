module Search exposing (main)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Task

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

-- MODEL

type alias Flags =
    { api : String }

type alias Model =
    { searchForm : SearchForm
    , api : String
    , currentDate : Maybe Date
    , response : Maybe PlayWheResponse
    }

type alias SearchForm =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    , draw : Maybe Int
    , time : Maybe Period
    , mark : Maybe Int
    , limit : Int
    , order : Ordering
    }

type Period = EM | AM | AN | PM

periodToString : Period -> String
periodToString time =
    case time of
        EM -> "EM"
        AM -> "AM"
        AN -> "AN"
        PM -> "PM"

type Ordering = ASC | DESC

orderingToString : Ordering -> String
orderingToString order =
    case order of
        ASC  -> "ASC"
        DESC -> "DESC"

type alias PlayWheResponse =
    { results : List PlayWheResult
    , totalResults : Int
    , page: Int
    , totalPages : Int
    , next : Maybe String
    , prev : Maybe String
    }

type alias PlayWheResult =
    { draw : Int
    , date : String
    , period : String
    , number : Int
    }

type Msg
    = SetYear String
    | SetMonth String
    | SetDay String
    | SetDraw String
    | SetTime String
    | SetMark String
    | SetLimit String
    | SetOrder String
    | SetDate (Maybe Date)
    | GetResults
    | GetPrevResults String
    | GetNextResults String
    | NewResults (Result Http.Error PlayWheResponse)

init : Flags -> (Model, Cmd Msg)
init { api } =
    let
        defaultForm =
            { year = Nothing
            , month = Nothing
            , day = Nothing
            , draw = Nothing
            , time = Nothing
            , mark = Nothing
            , limit = defaultLimit
            , order = defaultOrder
            }

        now = Task.perform (Just >> SetDate) Date.now
    in
        ( { searchForm = defaultForm
          , api = api
          , currentDate = Nothing
          , response = Nothing
          }
        , now
        )

defaultLimit : Int
defaultLimit = 12

defaultOrder : Ordering
defaultOrder = DESC

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetDate d ->
            ({ model | currentDate = d }, Cmd.none)

        SetYear s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | year = normalizeYear s }}, Cmd.none)

        SetMonth s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | month = normalizeMonth s }}, Cmd.none)

        SetDay s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | day = normalizeDay s }}, Cmd.none)

        SetDraw s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | draw = normalizeDraw s }}, Cmd.none)

        SetTime s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | time = normalizeTime s }}, Cmd.none)

        SetMark s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | mark = normalizeMark s }}, Cmd.none)

        SetLimit s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | limit = normalizeLimit s }}, Cmd.none)

        SetOrder s ->
            let
                oldSearchForm = model.searchForm
            in
                ({ model | searchForm = { oldSearchForm | order = normalizeOrder s }}, Cmd.none)

        GetResults ->
            (model, getResults (requestFromForm model.api model.searchForm))

        GetPrevResults p ->
            (model, getResults (requestFromPath model.api p))

        GetNextResults p ->
            (model, getResults (requestFromPath model.api p))

        NewResults (Ok response) ->
            ({ model | response = Just response }, Cmd.none)

        NewResults (Err e) ->
            Debug.log (toString e) (model, Cmd.none)

normalizeYear : String -> Maybe Int
normalizeYear year =
    let
        n = Result.withDefault 0 (String.toInt year)
    in
        if n >= startYear then
            Just n
        else
            Nothing

normalizeMonth : String -> Maybe Int
normalizeMonth month =
    let
        n = Result.withDefault 0 (String.toInt month)
    in
        if Dict.member n months then
            Just n
        else
            Nothing

normalizeDay : String -> Maybe Int
normalizeDay day =
    let
        n = Result.withDefault 0 (String.toInt day)
    in
        if List.member n days then
            Just n
        else
            Nothing

normalizeDraw : String -> Maybe Int
normalizeDraw draw =
    let
        n = Result.withDefault 0 (String.toInt draw)
    in
        if n >= 1 then
            Just n
        else
            Nothing

normalizeTime : String -> Maybe Period
normalizeTime time =
    case String.toUpper time of
        "EM" -> Just EM
        "AM" -> Just AM
        "AN" -> Just AN
        "PM" -> Just PM
        _    -> Nothing

normalizeMark : String -> Maybe Int
normalizeMark mark =
    let
        n = Result.withDefault 0 (String.toInt mark)
    in
        if Dict.member n marks then
            Just n
        else
            Nothing

normalizeLimit : String -> Int
normalizeLimit limit =
    let
        n = Result.withDefault defaultLimit (String.toInt limit)
    in
        if List.member n limits then
            n
        else
            defaultLimit

normalizeOrder : String -> Ordering
normalizeOrder order =
    case String.toUpper order of
        "ASC"  -> ASC
        "DESC" -> DESC
        _      -> defaultOrder

startYear : Int
startYear = 1994

months : Dict Int String
months = Dict.fromList
    [ (1, "January")
    , (2, "February")
    , (3, "March")
    , (4, "April")
    , (5, "May")
    , (6, "June")
    , (7, "July")
    , (8, "August")
    , (9, "September")
    , (10, "October")
    , (11, "November")
    , (12, "December")
    ]

days : List Int
days = List.range 1 31

marks : Dict Int String
marks = Dict.fromList
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

limits : List Int
limits = [4, 8, defaultLimit, 24, 48]

decodePlayWheResult : Decoder PlayWheResult
decodePlayWheResult =
    Decode.map4 PlayWheResult
        (Decode.field "draw" Decode.int)
        (Decode.field "date" Decode.string)
        (Decode.field "period" Decode.string)
        (Decode.field "number" Decode.int)

decodePlayWheResponse : Decoder PlayWheResponse
decodePlayWheResponse =
    Decode.map6 PlayWheResponse
        (Decode.field "results" (Decode.list decodePlayWheResult))
        (Decode.field "total_results" Decode.int)
        (Decode.field "page" Decode.int)
        (Decode.field "total_pages" Decode.int)
        (Decode.maybe (Decode.field "next" Decode.string))
        (Decode.maybe (Decode.field "prev" Decode.string))

getResults : Http.Request PlayWheResponse -> Cmd Msg
getResults request =
    Http.send NewResults request

requestFromForm : String -> SearchForm -> Http.Request PlayWheResponse
requestFromForm api f =
    let
        endpoint = api ++ "/results?" ++ toQuery f
    in
        Http.get endpoint decodePlayWheResponse

toQuery : SearchForm -> String
toQuery { year, month, day, draw, time, mark, limit, order } =
    let
        toTerm x f k =
            let
                v = f x
            in
                if v == "" then v else k ++ "=" ++ v
    in
        [ toTerm year yearToString "year"
        , toTerm month monthToString "month"
        , toTerm day dayToString "day"
        , toTerm draw drawToString "draw"
        , toTerm time timeToString "period"
        , toTerm mark markToString "number"
        , toTerm limit limitToString "limit"
        , toTerm order orderToString "order"
        ]
            |> List.filter ((/=) "")
            |> String.join "&"

yearToString : Maybe Int -> String
yearToString = maybeToString

monthToString : Maybe Int -> String
monthToString = maybeToString

dayToString : Maybe Int -> String
dayToString = maybeToString

drawToString : Maybe Int -> String
drawToString = maybeToString

timeToString : Maybe Period -> String
timeToString = Maybe.map periodToString >> Maybe.withDefault ""

markToString : Maybe Int -> String
markToString = maybeToString

limitToString : Int -> String
limitToString = toString

orderToString : Ordering -> String
orderToString = orderingToString

requestFromPath : String -> String -> Http.Request PlayWheResponse
requestFromPath api p =
    let
        endpoint = api ++ p
    in
        Http.get endpoint decodePlayWheResponse

-- VIEW

view : Model -> Html Msg
view { searchForm, currentDate, response } =
    case response of
        Just response ->
            div []
                [ viewForm searchForm currentDate
                , viewResponse response
                ]

        Nothing ->
            viewForm searchForm currentDate

viewForm : SearchForm -> Maybe Date -> Html Msg
viewForm { year, month, day, draw, time, mark, limit, order } currentDate =
    Html.form []
        [ viewSearch currentDate year month day
        , viewFilter draw time mark
        , viewOptions limit order
        , button
            [ type_ "button", class "btn btn-primary", onClick GetResults ]
            [ text "Show results" ]
        ]

viewSearch : Maybe Date -> Maybe Int -> Maybe Int -> Maybe Int -> Html Msg
viewSearch currentDate year month day =
    fieldset []
        [ legend [] [ text "Search" ]
        , row
            [ div [ class "col-md-4" ] [ byYear currentDate year ]
            , div [ class "col-md-4" ] [ byMonth month ]
            , div [ class "col-md-4" ] [ byDay day ]
            ]
        ]

byYear : Maybe Date -> Maybe Int -> Html Msg
byYear currentDate year =
    let
        years endYear = List.range startYear endYear

        y = yearToString year

        toOption s = option [ value s, selected (s == y) ] [ text s ]
    in
        formGroup
            [ label [] [ text "By year" ]
            , case currentDate of
                Nothing ->
                    div [] [ text "Loading..." ]

                Just d ->
                    select [ class "form-control", onInput SetYear ]
                        <| List.append [ option [ selected ("" == y) ] [] ]
                        <| List.reverse
                        <| List.map (toString >> toOption) (years (Date.year d))
            ]

byMonth : Maybe Int -> Html Msg
byMonth month =
    let
        m = monthToString month

        toOption n name =
            let
                s = toString n
            in
                option [ value s, selected (s == m)] [ text name ]

        options = months
            |> Dict.map toOption
            |> Dict.values
            |> List.append [ option [ selected ("" == m) ] [] ]
    in
        formGroup
            [ label [] [ text "By month" ]
            , select [ class "form-control", onInput SetMonth ] options
            ]

byDay : Maybe Int -> Html Msg
byDay day =
    let
        d = dayToString day

        toOption s = option [ value s, selected (s == d) ] [ text s ]
    in
        formGroup
            [ label [] [ text "By day" ]
            , select [ class "form-control", onInput SetDay ]
                <| List.append [ option [ selected ("" == d) ] [] ]
                <| List.map (toString >> toOption) days
            ]

viewFilter : Maybe Int -> Maybe Period -> Maybe Int -> Html Msg
viewFilter draw time mark =
    fieldset []
        [ legend [] [ text "Filter" ]
        , row
            [ div [ class "col-md-4" ] [ byDraw draw ]
            , div [ class "col-md-4" ] [ byTime time ]
            , div [ class "col-md-4" ] [ byMark mark ]
            ]
        ]

byDraw : Maybe Int -> Html Msg
byDraw draw =
    formGroup
        [ label [] [ text "By draw" ]
        , input
            [ type_ "number"
            , value (drawToString draw)
            , class "form-control"
            , placeholder "For e.g. 15324"
            , onInput SetDraw
            ]
            []
        ]

byTime : Maybe Period -> Html Msg
byTime time =
    let
        t = timeToString time
    in
        formGroup
            [ label [] [ text "By time of day" ]
            , select [ class "form-control", onInput SetTime ]
                [ option [ selected ("" == t) ] []
                , option [ value "em", selected ("em" == t) ] [ text "Morning (10:30am)" ]
                , option [ value "am", selected ("am" == t) ] [ text "Midday (1:00pm)" ]
                , option [ value "an", selected ("an" == t) ] [ text "Afternoon (4:00pm)" ]
                , option [ value "pm", selected ("pm" == t) ] [ text "Evening (6:30pm)" ]
                ]
            ]

byMark : Maybe Int -> Html Msg
byMark mark =
    let
        m = markToString mark

        toOption n spirit =
            let
                s = toString n
            in
                option [ value s, selected (s == m) ] [ text (s ++ " (" ++ spirit ++ ")") ]

        options = marks
            |> Dict.map toOption
            |> Dict.values
            |> List.append [ option [ selected ("" == m) ] [] ]
    in
        formGroup
            [ label [] [ text "By mark" ]
            , select [ class "form-control", onInput SetMark ] options
            ]

viewOptions : Int -> Ordering -> Html Msg
viewOptions limit order =
    fieldset []
        [ legend [] [ text "Options" ]
        , p []
            [ text "Show "
            , limitOptions limit
            , text " results per page."
            ]
        , p []
            [ text "Sort in "
            , orderOptions order
            , text " order of the draw number."
            ]
        ]

limitOptions : Int -> Html Msg
limitOptions limit =
    let
        l = limitToString limit

        toOption s = option [ value s, selected (s == l) ] [ text s ]
    in
        select [ class "form-control d-inline w-auto", onInput SetLimit ]
            <| List.map (toString >> toOption) limits

orderOptions : Ordering -> Html Msg
orderOptions order =
    let
        o = orderToString order
    in
        select [ class "form-control d-inline w-auto", onInput SetOrder ]
            [ option [ value "desc", selected ("desc" == o) ] [ text "descending" ]
            , option [ value "asc", selected ("asc" == o) ] [ text "ascending" ]
            ]

viewResponse : PlayWheResponse -> Html Msg
viewResponse response =
    div [ class "mt-5" ] [ viewResults response ]

viewResults : PlayWheResponse -> Html Msg
viewResults { results, totalResults, page, totalPages, prev, next } =
    let
        message =
            [ i [ class "far fa-smile" ] []
            , text " Great! We found "
            , strong [] [ text (toString totalResults) ]
            , text " "
            , text (if totalResults == 1 then "result" else "results")
            , text " for you."
            ]

        rows = List.map viewResult results
    in
        if List.isEmpty results then
            div [ class "alert alert-danger" ]
                [ i [ class "far fa-frown" ] []
                , text " Oops! We couldn't find any results for you. Please try a different search."
                ]
        else
            div []
                [ div [ class "alert alert-success" ] message
                , table [ class "table table-responsive-md table-bordered table-striped" ]
                    [ thead [ class "thead-dark" ]
                        [ tr []
                            [ th [] [ text "Draw" ]
                            , th [] [ text "Date" ]
                            , th [] [ text "Time" ]
                            , th [] [ text "Mark" ]
                            , th [] [ text "Spirit" ]
                            ]
                        ]
                    , tbody [] (List.append rows [ viewPager page totalPages prev next ])
                    ]
                ]

viewResult : PlayWheResult -> Html Msg
viewResult { draw, date, period, number } =
    let
        spirit = Maybe.withDefault "unknown" (Dict.get number marks)
    in
        tr []
            [ td [] [ text (toString draw) ]
            , td [] [ text (formatDate date) ]
            , td [] [ text (formatPeriod period) ]
            , td [] [ text (toString number) ]
            , td [] [ text spirit ]
            ]

formatDate : String -> String
formatDate date =
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

        month mm =
            let
                n = Result.withDefault 0 (String.toInt mm)
            in
                Maybe.withDefault "Unknown" (Dict.get n months)

        monthAbbr mm = String.left 3 (month mm)
    in
        case parts of
            [yyyy, mm, dd] -> String.join " "
                [ monthAbbr mm
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
        _ -> "Unknown"

viewPager : Int -> Int -> Maybe String -> Maybe String -> Html Msg
viewPager page totalPages prev next =
    tr []
        [ td [ colspan 5, style [ ("backgroundColor", "white") ] ]
            [ div [ class "d-flex flex-row justify-content-center" ]
                [ nav []
                    [ ul [ class "mb-0 pagination" ]
                        [ viewPrev prev
                        , viewPage page totalPages
                        , viewNext next
                        ]
                    ]
                ]
            ]
        ]

viewPrev : Maybe String -> Html Msg
viewPrev prev =
    case prev of
        Nothing ->
            li [ class "page-item disabled" ]
                [ span [ class "page-link" ]
                    [ i [ class "fas fa-caret-left" ] [] ]
                ]

        Just p ->
            li [ class "page-item" ]
                [ span [ class "page-link", onClick (GetPrevResults p) ]
                    [ i [ class "fas fa-caret-left" ] [] ]
                ]

viewPage : Int -> Int -> Html Msg
viewPage page totalPages =
    let
        message = (toString page) ++ " of " ++ (toString totalPages)
    in
        li [ class "page-item disabled" ]
            [ span [ class "page-link" ] [ text message ] ]


viewNext : Maybe String -> Html Msg
viewNext next =
    case next of
        Nothing ->
            li [ class "page-item disabled" ]
                [ span [ class "page-link" ]
                    [ i [ class "fas fa-caret-right" ] [] ]
                ]

        Just p ->
            li [ class "page-item" ]
                [ span [ class "page-link", onClick (GetNextResults p) ]
                    [ i [ class "fas fa-caret-right" ] [] ]
                ]

-- Helpers

maybeToString : Maybe a -> String
maybeToString = Maybe.map toString >> Maybe.withDefault ""

formGroup : List (Html msg) -> Html msg
formGroup children = div [ class "form-group" ] children

row : List (Html msg) -> Html msg
row children = div [ class "row" ] children

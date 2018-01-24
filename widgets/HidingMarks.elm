module HidingMarks exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import PlayWhe

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
    { endpoint : String }

type alias Model =
    { endpoint : String
    , status : Status
    }

type Status
    = Loading
    | Empty
    | Loaded Results

type alias Results = List Draw

type alias Draw =
    { date : String
    , period : String
    , number : Int
    }

type Msg
    = NewResults (Result Http.Error Results)

init : Flags -> (Model, Cmd Msg)
init { endpoint } =
    ( { endpoint = endpoint, status = Loading }
    , getLastDraw (lastDrawRequest endpoint)
    )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewResults (Ok results) ->
            ( { model | status = Loaded results }, Cmd.none )

        NewResults (Err e) ->
            Debug.log (toString e) ( { model | status = Empty }, Cmd.none )

-- DECODERS

decodeResults : Decoder Results
decodeResults =
    Decode.field "results" (Decode.list decodeDraw)

decodeDraw : Decoder Draw
decodeDraw =
    Decode.map3 Draw
        (Decode.field "date" Decode.string)
        (Decode.field "period" Decode.string)
        (Decode.field "number" Decode.int)

-- HTTP

getLastDraw : Http.Request Results -> Cmd Msg
getLastDraw request =
    Http.send NewResults request

lastDrawRequest : String -> Http.Request Results
lastDrawRequest endpoint =
    Http.get endpoint decodeResults

-- VIEW

view : Model -> Html msg
view { status } =
    case status of
        Loading ->
            div [ class "mb-5x" ] [ text "Loading..." ]

        Empty ->
            div [ class "mb-5x" ]
                [ div [ class "alert alert-danger" ]
                    [ strong [] [ text "Oops! " ]
                    , text "Something went wrong. Please check back in a few minutes."
                    ]
                ]

        Loaded results ->
            div [ class "mb-5x" ]
                [ div [ class "mb-3" ]
                    [ h2 [] [ text "Top 5 Hiding Marks" ]
                    , viewResults (List.take 5 results)
                    ]
                , div [ class "mb-3" ]
                    [ h2 [] [ text "Next 5 Hiding Marks" ]
                    , viewResults (List.take 5 (List.drop 5 results))
                    ]
                , div []
                    [ h2 [] [ text "The Rest" ]
                    , viewResults (List.drop 10 results)
                    ]
                ]

viewResults : Results -> Html msg
viewResults results =
    div [ class "table-responsive" ]
        [ table [ class "table table-bordered table-striped" ]
            [ thead [ class "thead-dark" ]
                [ tr []
                    [ th [ scope "col" ] [ text "Mark" ]
                    , th [ scope "col" ] [ text "Spirit" ]
                    , th [ scope "col" ] [ text "Date last played" ]
                    , th [ scope "col" ] [ text "Time last played" ]
                    ]
                ]
            , tbody [] (List.map viewDraw results)
            ]
        ]

viewDraw : Draw -> Html msg
viewDraw { date, period, number } =
    tr []
        [ td [] [ text (toString number) ]
        , td [] [ text (PlayWhe.spirit number) ]
        , td [] [ text (PlayWhe.formatDate date) ]
        , td [] [ text (PlayWhe.formatPeriod period) ]
        ]

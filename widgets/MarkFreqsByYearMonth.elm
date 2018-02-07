module MarkFreqsByYearMonth exposing (main)

import Date exposing (Date)
import Dict
import Erl.Query as Query
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Navigation
import Task

import PlayWhe

main : Program Flags Model Msg
main =
  Navigation.programWithFlags SetLocation
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }

-- MODEL

type alias Flags =
  { api : String }

type alias Model =
  { api : String
  , location : Navigation.Location
  , currentYear : Maybe Int
  , formData : FormData
  , data : RemoteData Rows
  }

type alias FormData =
  { year : Maybe Int
  , month : Maybe Int
  }

type RemoteData a
  = Loading
  | Loaded a
  | Error

type alias Rows =
  List Row

type alias Row =
  { mark : Int
  , em : Int
  , am : Int
  , an : Int
  , pm : Int
  , total : Int
  }

type Msg
  = NewRows (Result Http.Error Rows)
  | Now Date
  | SetLocation Navigation.Location
  | SetYear String
  | SetMonth String

init : Flags -> Navigation.Location -> (Model, Cmd Msg)
init { api } location =
  ( { api = api
    , location = location
    , currentYear = Nothing
    , formData = toFormData location
    , data = Loading
    }
  , Task.perform Now Date.now
  )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewRows (Ok rows) ->
      ( { model | data = Loaded rows }, Cmd.none )

    NewRows (Err e) ->
      Debug.log (toString e)
        ( { model | data = Error }, Cmd.none )

    Now date ->
      let
        year =
          Just (Date.year date)

        formData =
          case model.formData.year of
            Nothing ->
              setYear year model.formData

            _ ->
              model.formData
      in
        ( { model | currentYear = year, formData = formData, data = Loading }
        , Http.send NewRows (getRows model.api formData)
        )

    SetLocation location ->
      let
        formData =
          toFormData location
      in
        ( { model
          | location = location
          , formData =
              case formData.year of
                Nothing ->
                  { formData | year = model.currentYear }

                _ ->
                  formData
          , data = Loading
          }
        , Http.send NewRows (getRows model.api formData)
        )

    SetYear s ->
      let
        year =
          normalizeYear s

        formData =
          setYear year model.formData
      in
        ( model, Navigation.newUrl (toSearch formData) )

    SetMonth s ->
      let
        month =
          normalizeMonth s

        formData =
          setMonth month model.formData
      in
        ( model, Navigation.newUrl (toSearch formData) )

normalizeYear : String -> Maybe Int
normalizeYear s =
  String.toInt s
    |> Result.toMaybe
    |> Maybe.andThen
      (\year ->
        if year >= PlayWhe.startYear then
          Just year
        else
          Nothing
      )

normalizeMonth : String -> Maybe Int
normalizeMonth s =
  String.toInt s
    |> Result.toMaybe
    |> Maybe.andThen
      (\m ->
        if m >= 1 && m <= 12 then
          Just m
        else
          Nothing
      )

setYear : Maybe Int -> FormData -> FormData
setYear year formData =
  { formData | year = year }

setMonth : Maybe Int -> FormData -> FormData
setMonth month formData =
  { formData | month = month }

-- DECODERS

decodeRows : Decoder Rows
decodeRows = Decode.field "counts" (Decode.list decodeRow)

decodeRow : Decoder Row
decodeRow =
  Decode.map6 Row
    (Decode.field "number" Decode.int)
    (decodeByPeriod 0)
    (decodeByPeriod 1)
    (decodeByPeriod 2)
    (decodeByPeriod 3)
    (Decode.field "count" Decode.int)

decodeByPeriod : Int -> Decoder Int
decodeByPeriod index =
  Decode.field "by_period"
    <| Decode.index index (Decode.field "count" Decode.int)

-- HTTP

getRows : String -> FormData -> Http.Request Rows
getRows api { year, month } =
  let
    endpoint =
      api
      ++ "/stats/mark-counts-for-date?by_period=true"
      ++ param "year" year
      ++ param "month" month
  in
    Http.get endpoint decodeRows

param : String -> Maybe a -> String
param key =
  Maybe.map (toString >> ((++) ("&" ++ key ++ "="))) >> Maybe.withDefault ""

-- VIEW

view : Model -> Html Msg
view { currentYear, formData, data } =
  case currentYear of
    Nothing ->
      viewLoading

    Just currentYear ->
      div []
        [ h2 [] [ text "Frequencies" ]
        , viewForm currentYear formData
        , viewData data
        ]

viewForm : Int -> FormData -> Html Msg
viewForm currentYear { year, month } =
  p []
    [ text "Show me how many times each mark has played in "
    , year
      |> Maybe.map (viewYear currentYear)
      |> Maybe.withDefault (text "-")
    , text " for "
    , viewMonth month
    , text "."
    ]

viewYear : Int -> Int -> Html Msg
viewYear currentYear year =
  let
    options =
      currentYear
        |> PlayWhe.years
        |> List.map toOption
        |> List.reverse

    toOption y =
      option
        [ value (toString y), selected (y == year) ]
        [ text (toString y) ]
  in
    select
      [ class "form-control d-inline w-auto"
      , onInput SetYear
      ]
      options

viewMonth : Maybe Int -> Html Msg
viewMonth month =
  let
    defaultOption =
      option [ value "all", selected (month == Nothing) ] [ text "All months" ]

    toOption n name =
      option [ value (toString n), selected (Just n == month)] [ text name ]

    options = PlayWhe.months
        |> Dict.map toOption
        |> Dict.values
  in
    select [ class "form-control d-inline w-auto", onInput SetMonth ]
      <| defaultOption :: options

viewData : RemoteData Rows -> Html msg
viewData data =
  case data of
    Loading ->
      viewLoading

    Loaded rows ->
      viewRows rows

    Error ->
      div [ class "alert alert-danger" ]
        [ strong [] [ text "Oops! " ]
        , text "Something went wrong. Please check back in a few minutes."
        ]

viewLoading : Html msg
viewLoading = text "Loading"

viewRows : Rows -> Html msg
viewRows rows =
  div [ class "table-responsive" ]
    [ table [ class "table table-sm table-bordered table-striped" ]
      [ viewHead
      , viewBody rows
      ]
    ]

viewHead : Html msg
viewHead =
  thead [ class "thead-dark" ]
    [ tr []
      [ th [ rowspan 2 ] [ text "Mark" ]
      , th [ rowspan 2 ] [ text "Spirit" ]
      , th [ colspan 5, class "text-center border-bottom-0" ] [ text "Times played" ]
      ]
    , tr []
      [ th [ title "Morning" ] [ text "10:30am" ]
      , th [ title "Midday" ] [ text "1:00pm" ]
      , th [ title "Afternoon" ] [ text "4:00pm" ]
      , th [ title "Evening" ] [ text "6:30pm" ]
      , th [] [ text "Total" ]
      ]
    ]

viewBody : Rows -> Html msg
viewBody rows = tbody [] (List.map viewRow rows)

viewRow : Row -> Html msg
viewRow { mark, em, am, an, pm, total } =
  tr []
    [ td [] [ text (toString mark) ]
    , td [] [ text (PlayWhe.spirit mark) ]
    , td [] [ text (toString em) ]
    , td [] [ text (toString am) ]
    , td [] [ text (toString an) ]
    , td [] [ text (toString pm) ]
    , td [] [ text (toString total) ]
    ]

-- MISC

toFormData : Navigation.Location -> FormData
toFormData { search } =
  let
    query =
      Query.parse search

    value key =
      case Query.getValuesForKey key query of
        [] ->
          Nothing

        v::_ ->
          Just v
  in
    { year = value "year" |> Maybe.andThen normalizeYear
    , month = value "month" |> Maybe.andThen normalizeMonth
    }

toSearch : FormData -> String
toSearch { year, month } =
  let
    addMaybe key value query =
      case value of
        Nothing ->
          query

        Just value ->
          Query.add key (toString value) query

    query =
      Query.parse ""
        |> addMaybe "year" year
        |> addMaybe "month" month
  in
    Query.toString query

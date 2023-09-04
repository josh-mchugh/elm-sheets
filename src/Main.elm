module Main exposing (main)

{-| Elm Sheets application
-}


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


-- MAIN

main : Program Model Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- MODEL


type alias Model =
    {}


init : Model -> ( Model , Cmd Msg )
init model =
    ( model, Cmd.none )


-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ div [ class "sheet"]
              ( List.map viewSimpleDiv (List.range 0 100)  )
        ]

viewSimpleDiv : Int -> Html Msg
viewSimpleDiv value =
    let
        displayValue =
            "div #" ++ String.fromInt value
    in
    div [] [ text displayValue ]




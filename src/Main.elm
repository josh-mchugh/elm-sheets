module Main exposing (main)

{-| Elm Sheets application
-}

import Browser
import Browser.Dom exposing (Element, Error, Viewport)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick, onMouseOver)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { sheets : List Int
    , items : List Int
    , viewport : Maybe Viewport
    , element : Maybe Element
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = []
      , items = []
      , viewport = Nothing
      , element = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddSheet
    | AddItem
    | GetViewportClicked
    | GotViewport (Result Error Viewport)
    | OnMouseOverItem Int
    | GotElement (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSheet ->
            let
                nextSheetId =
                    List.length model.sheets + 1
            in
            ( { model | sheets = model.sheets ++ [ nextSheetId ] }
            , Task.attempt GotViewport (Browser.Dom.getViewportOf ("sheet" ++ String.fromInt nextSheetId))
            )

        AddItem ->
            ( { model | items = model.items ++ [ List.length model.items + 1 ] }
            , Cmd.none
            )

        GetViewportClicked ->
            ( model, Task.attempt GotViewport (Browser.Dom.getViewportOf "sheet1") )

        GotViewport result ->
            case result of
                Ok viewport ->
                    ( { model | viewport = Just viewport }, Cmd.none )

                Err error ->
                    ( { model | viewport = Nothing }, Cmd.none )

        OnMouseOverItem value ->
            ( model, Task.attempt GotElement (Browser.Dom.getElement ("item" ++ String.fromInt value)) )

        GotElement result ->
            case result of
                Ok element ->
                    ( { model | element = Just element }, Cmd.none )

                Err error ->
                    ( { model | element = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "sidebar" ]
            [ button [ onClick AddSheet ]
                [ text "Add Sheet" ]
            , button [ onClick GetViewportClicked ]
                [ text "Sheet Viewport" ]
            , viewViewportInfo model.viewport
            , button [ onClick AddItem ]
                [ text "Add Item" ]
            , viewElementInfo model.element
            ]
        , div [ class "content" ]
            (List.map (viewSheet model.items) model.sheets)
        ]


viewSheet : List Int -> Int -> Html Msg
viewSheet items sheet =
    div [ id ("sheet" ++ String.fromInt sheet), class "sheet" ]
        (List.map viewSimpleDiv items)


viewViewportInfo : Maybe Viewport -> Html Msg
viewViewportInfo maybeViewport =
    let
        displayValue =
            case maybeViewport of
                Just viewport ->
                    "x: "
                        ++ String.fromFloat viewport.viewport.x
                        ++ ", "
                        ++ "y: "
                        ++ String.fromFloat viewport.viewport.y
                        ++ ", "
                        ++ "width: "
                        ++ String.fromFloat viewport.viewport.width
                        ++ ", "
                        ++ "height: "
                        ++ String.fromFloat viewport.viewport.height

                Nothing ->
                    ""
    in
    div []
        [ text displayValue ]


viewElementInfo : Maybe Element -> Html Msg
viewElementInfo maybeElement =
    let
        displayValue =
            case maybeElement of
                Just element ->
                    "x: "
                        ++ String.fromFloat element.element.x
                        ++ ", "
                        ++ "y: "
                        ++ String.fromFloat element.element.y
                        ++ ", "
                        ++ "width: "
                        ++ String.fromFloat element.element.width
                        ++ ", "
                        ++ "height: "
                        ++ String.fromFloat element.element.height

                Nothing ->
                    ""
    in
    div []
        [ text displayValue ]


viewSimpleDiv : Int -> Html Msg
viewSimpleDiv value =
    let
        displayValue =
            "div #" ++ String.fromInt value

        itemId =
            "item" ++ String.fromInt value
    in
    div [ id itemId, class "item", onMouseOver (OnMouseOverItem value) ]
        [ text displayValue ]

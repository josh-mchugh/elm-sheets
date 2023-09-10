module Main exposing (main)

{-| Elm Sheets application
-}

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Element, Error)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
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
    { sheets : Array Sheet
     , rows : Array Row
    }


type alias Sheet =
    { bounds : Maybe Bounds }


type alias Bounds =
    { top: Float
    , left: Float
    , bottom: Float
    , right: Float
    }


type alias Row =
    { bounds : Maybe Bounds
    , columns : Array Column
    }


type alias Column =
    { bounds : Maybe Bounds }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = Array.fromList [ emptySheet ]
      , rows = Array.fromList []
      }
    , Task.attempt (UpdateSheetBounds 0)  (Browser.Dom.getElement "sheet0")
    )


emptySheet : Sheet
emptySheet =
    { bounds = Nothing }


emptyRow : Row
emptyRow =
    { bounds = Nothing
    , columns = Array.fromList []
    }


emptyColumn : Column
emptyColumn =
    { bounds = Nothing }


-- Update


type Msg
    = AddRow
    | AddColumn Int
    | UpdateSheetBounds Int (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            ( { model | rows = Array.push emptyRow model.rows }
            , Cmd.none
            )

        AddColumn index ->
            case (Array.get index model.rows) of
                Just row ->
                    ( { model | rows = Array.set index { row | columns =  Array.push emptyColumn row.columns } model.rows }
                    , Cmd.none
                    )
                Nothing ->
                    ( model, Cmd.none )

        UpdateSheetBounds index result ->
            case result of
                Ok element ->
                    case (Array.get index model.sheets) of
                        Just sheet ->
                            ( { model | sheets = Array.set index { sheet | bounds = Just (createBounds element) } model.sheets }
                            , Cmd.none
                            )
                        Nothing ->
                            ( model, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


createBounds : Element -> Bounds
createBounds element =
    { top = element.element.y
    , left = element.element.x
    , bottom = element.element.y + element.element.height
    , right = element.element.x + element.element.width
    }


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewSidebar model
        , viewContent model
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "sidebar" ]
        [ div [ class "actions" ]
              [ button [ onClick AddRow ]
                    [ text "Add Row" ]
              ]
        , div [ class "cards" ]
              (List.map viewSidebarRow (Array.toIndexedList model.rows))
        ]


viewSidebarRow : (Int, Row) -> Html Msg
viewSidebarRow rowTuple =
    let
        index =
            Tuple.first rowTuple

        row =
            Tuple.second rowTuple
    in
    div [ class "card" ]
        [ div [ class "header" ]
              [ div [] [ text ("Row #" ++ String.fromInt index)]
              , div []
                  [ button [ onClick (AddColumn index) ]
                        [ text "Add Column" ]
                  ]
              ]
        , div []
            [ div [ ]
                  (List.map viewSidebarColumn (Array.toIndexedList row.columns))
            ]
        ]


viewSidebarColumn : (Int, Column) -> Html Msg
viewSidebarColumn columnTuple =
    let
        index =
            Tuple.first columnTuple
    in
    div [ class "column" ]
        [ div [ class "header" ]
              [ div []
                    [ text ("Column #" ++ String.fromInt index) ]
              , div []
                  [ button []
                        [ text "Add Section" ]
                  ]
              ]
        ]

viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map (viewSheet model.rows) (Array.toIndexedList model.sheets))


viewSheet : Array Row -> (Int, Sheet) -> Html Msg
viewSheet rows sheetTuple =
    let
        index =
            Tuple.first sheetTuple
    in
    div [ id ("sheet" ++ String.fromInt index), class "sheet" ]
        [ div [ id "sheetContainer", style "height" "auto" ]
              (List.map viewRow (Array.toList rows))
        ]


viewRow : Row -> Html Msg
viewRow row =
    div [] [ text "" ]

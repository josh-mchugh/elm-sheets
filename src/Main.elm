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
import Platform.Cmd as Cmd



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
    { bounds : Maybe Bounds
    , sections : Array Section
    }


type alias Section =
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
    { bounds = Nothing
    , sections = Array.fromList []
    }


emptySection : Section
emptySection =
    { bounds = Nothing }


-- Update


type Msg
    = AddRow
    | AddColumn Int
    | AddSection Int Int
    | UpdateSectionBounds Int Int Int (Result Error Element)
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

        AddSection rowIndex columnIndex ->
            let
                updateColumn column =
                    { column | sections = Array.push emptySection column.sections }
            in
            case (Array.get rowIndex model.rows) of
                Just row ->
                    case (Array.get columnIndex row.columns) of
                        Just column ->
                            ( { model | rows = Array.set rowIndex { row | columns = Array.set columnIndex (updateColumn column) row.columns } model.rows }
                            , Task.attempt (UpdateSectionBounds rowIndex columnIndex (Array.length column.sections)) (Browser.Dom.getElement ("section" ++ String.fromInt (Array.length column.sections)))
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSectionBounds rowIndex columnIndex sectionIndex result ->
            let
                updateRow ( index, row ) =
                    if (index == rowIndex) then
                        { row | columns = Array.fromList (List.map updateColumn (Array.toIndexedList row.columns)) }
                    else
                        row

                updateColumn ( index, column ) =
                    if (index == columnIndex) then
                        { column | sections = Array.fromList (List.map updateSection (Array.toIndexedList column.sections)) }
                    else
                        column

                updateSection ( index, section ) =
                    if (index == sectionIndex) then
                        { section | bounds = maybeBounds result }
                    else
                        section
            in
            ( { model | rows = Array.fromList (List.map updateRow (Array.toIndexedList model.rows)) }
            , Cmd.none
            )

        UpdateSheetBounds sheetIndex result ->
            let
                updateSheet ( index, sheet ) =
                    if (index == sheetIndex) then
                        { sheet | bounds = maybeBounds result }
                    else
                        sheet
            in
            ( { model | sheets = Array.fromList (List.map updateSheet (Array.toIndexedList model.sheets)) }
            , Cmd.none
            )


maybeBounds : (Result Error Element) -> Maybe Bounds
maybeBounds result =
    case result of
        Ok element ->
            Just <| createBounds element
        Err error ->
            Nothing


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
                  (List.map (viewSidebarColumn index) (Array.toIndexedList row.columns))
            ]
        ]


viewSidebarColumn : Int -> (Int, Column) -> Html Msg
viewSidebarColumn rowIndex columnTuple =
    let
        index =
            Tuple.first columnTuple

        column =
            Tuple.second columnTuple
    in
    div [ class "column" ]
        [ div [ class "header" ]
              [ div []
                    [ text ("Column #" ++ String.fromInt index) ]
              , div []
                  [ button [ onClick (AddSection rowIndex index) ]
                        [ text "Add Section" ]
                  ]
              ]
        , div []
            (List.map (viewSidebarSection rowIndex index) (Array.toIndexedList column.sections))
        ]


viewSidebarSection : Int -> Int -> (Int, Section) -> Html Msg
viewSidebarSection rowIndex columnIndex sectionTuple =
    let
        index =
            Tuple.first sectionTuple
    in
    div []
        [ text ("Section #" ++ String.fromInt index)]


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
    div [ class "row" ]
        (List.map viewColumn (Array.toList row.columns))


viewColumn : Column -> Html Msg
viewColumn column =
    div []
        (List.map viewSection (Array.toIndexedList column.sections))


viewSection : (Int, Section) -> Html Msg
viewSection sectionTuple =
    let
        index =
            Tuple.first sectionTuple
    in
    div [ id ("section" ++ String.fromInt index)]
        [ text ("Section #" ++ String.fromInt index) ]

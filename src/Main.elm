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
import Html exposing (section)



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
    , outOfBounds : Bool
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
    , contentBounds: Maybe Bounds
    , sections : Array Section
    }


type alias Section =
    { bounds : Maybe Bounds }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = Array.fromList [ emptySheet ]
      , rows = Array.fromList []
      , outOfBounds = False
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
    , contentBounds = Nothing
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
    | UpdateSheetContainerBounds Int (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            ( { model | rows = Array.push emptyRow model.rows }
            , Cmd.none
            )

        AddColumn rowIndex ->
            let
                updateRow (index, row) =
                    if (index == rowIndex) then
                        { row | columns = Array.push emptyColumn row.columns }
                    else
                        row
            in
            ( { model | rows = Array.fromList (List.map updateRow (Array.toIndexedList model.rows)) }
            , Cmd.none
            )

        AddSection rowIndex columnIndex ->
            let
                updateRow (index, row) =
                    if (index == rowIndex) then
                        { row | columns = Array.fromList (List.map updateColumn (Array.toIndexedList row.columns)) }
                    else
                        row

                updateColumn (index, column) =
                    if (index == columnIndex) then
                        { column | sections = Array.push emptySection column.sections }
                    else
                        column

                cmd =
                    Array.get rowIndex model.rows
                        |> Maybe.andThen (\row -> Array.get columnIndex row.columns)
                        |> Maybe.map (\column -> Array.length column.sections)
                        |> Maybe.map (\length -> Task.attempt (UpdateSectionBounds rowIndex columnIndex length) (Browser.Dom.getElement ("section" ++ String.fromInt length)))
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | rows = Array.fromList (List.map updateRow (Array.toIndexedList model.rows)) }
            , cmd
            )

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
            , Task.attempt (UpdateSheetContainerBounds 0) (Browser.Dom.getElement "sheetContainer")
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

        UpdateSheetContainerBounds sheetIndex result ->
            let
                sheetBounds =
                    Array.get sheetIndex model.sheets
                        |> Maybe.andThen (\sheet -> sheet.bounds)
                        |> Maybe.map (\bounds -> bounds.bottom)
                        |> Maybe.withDefault 0

                containerBounds =
                    maybeBounds result
                        |> Maybe.map (\bounds -> bounds.bottom)
                        |> Maybe.withDefault 0
            in
            ( { model | outOfBounds = containerBounds > sheetBounds }, Cmd.none )


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
    let
        displayText =
            if (model.outOfBounds) then
                text "Elements exceed sheet bounds."
            else
                text "Everything is looking good."
    in
    div [ class "sidebar" ]
        [ div [ class "actions" ]
              [ button [ onClick AddRow ]
                    [ text "Add Row" ]
              ]
        , div []
              [ displayText ]
        , div [ class "cards" ]
              (List.map viewSidebarRow (Array.toIndexedList model.rows))
        ]


viewSidebarRow : (Int, Row) -> Html Msg
viewSidebarRow ( index, row ) =
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
viewSidebarColumn rowIndex ( index, column ) =
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
viewSidebarSection rowIndex columnIndex ( index, section ) =
    div []
        [ text ("Section #" ++ String.fromInt index)]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map (viewSheet model.rows) (Array.toIndexedList model.sheets))


viewSheet : Array Row -> (Int, Sheet) -> Html Msg
viewSheet rows ( index, sheet ) =
    div [ id ("sheet" ++ String.fromInt index), class "sheet" ]
        [ div [ id "sheetContainer", style "height" "auto" ]
              (List.map viewRow (Array.toIndexedList rows))
        ]


viewRow : (Int, Row) -> Html Msg
viewRow ( index, row ) =
    div [ class "row" ]
        (List.map viewColumn (Array.toIndexedList row.columns))


viewColumn : (Int, Column) -> Html Msg
viewColumn ( index, column ) =
    div [ class "column" ]
        [ div [ id ("columnContent#" ++ (String.fromInt index)), class "column__content" ]
              (List.map viewSection (Array.toIndexedList column.sections))
        ]


viewSection : (Int, Section) -> Html Msg
viewSection ( index, section ) =
    div [ id ("section" ++ String.fromInt index)]
        [ text ("Section #" ++ String.fromInt index) ]

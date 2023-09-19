module Main exposing (main)

{-| Elm Sheets application
-}

import Browser
import Browser.Dom exposing (Element, Error)
import Html exposing (section)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed, step)
import Task
import Platform.Cmd as Cmd
import Uuid



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { currentSeed : Seed
    , sheets : List Sheet
    , exceedsHeight : Bool
    }


type alias Sheet =
    { id : String
    , dimension : Maybe Dimension
    , container : SheetContainer
    , rows : List Row
    }


type alias SheetContainer =
    { id : String
    , dimension : Maybe Dimension
    }


type alias Row =
    { id : String
    , columns : List Column
    }


type alias Column =
    { id : String
    , sections : List Section
    }


type alias Section =
    { id : String
    , dimension : Maybe Dimension
    }


type alias Dimension =
    { height : Float
    , width : Float
    }


init : Int -> ( Model, Cmd Msg )
init externalRandom =
    let
        ( sheetUuid, sheetSeed ) =
            createUuid (initialSeed externalRandom)

        ( containerUuid, containerSeed ) =
            createUuid (sheetSeed)
    in
    ( { currentSeed = containerSeed
      , sheets = [ (initSheet sheetUuid containerUuid) ]
      , exceedsHeight = False
      }
    , Task.attempt (UpdateSheetDimension sheetUuid)  (Browser.Dom.getElement sheetUuid)
    )


initSheet : String -> String -> Sheet
initSheet uuid containerUuid =
    { id = uuid
    , dimension = Nothing
    , container = (initSheetContainer containerUuid)
    , rows = [ initRow ]
    }


initSheetContainer : String -> SheetContainer
initSheetContainer uuid =
    { id = uuid
    , dimension = Nothing
    }


initRow :  Row
initRow =
    { id = "row"
    , columns = [ initLeftColumn, initRightColumn ]
    }


initLeftColumn : Column
initLeftColumn =
    { id = "leftColumn"
    , sections =
          [ (initSection "Name")
          , (initSection "Header-Summary")
          , (initSection "Summary")
          , (initSection "Header-Contact")
          , (initSection "Contact")
          , (initSection "Header-Social")
          , (initSection "Social")
          ]
    }


initRightColumn : Column
initRightColumn =
    { id = "rightColumn"
    , sections =
          [ (initSection "Header-Work-Experiences")
          , (initSection "Experiences")
          , (initSection "Header-Professional-Skills")
          , (initSection "Skills")
          , (initSection "Header-Certifications")
          , (initSection "Certifications")
          ]
    }


initSection : String -> Section
initSection id =
    { id = id
    , dimension = Nothing
    }


-- Update


type Msg
    = UpdateSheetDimension String (Result Error Element)
    | UpdateSheetContainerDimension String (Result Error Element)
    | AddSection String String String
    | UpdateSectionDimension String String String String (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSheetDimension sheetId result ->
            let
                updateSheet sheet =
                    if (sheetId == sheet.id) then
                        { sheet | dimension = maybeDimension result }
                    else
                        sheet
            in
            ( { model | sheets = List.map updateSheet model.sheets }
            , Cmd.none
            )

        UpdateSheetContainerDimension sheetId result ->
            let
                sheetHeight =
                    List.filter (\sheet -> sheetId == sheet.id) model.sheets
                        |> List.head
                        |> Maybe.andThen (\sheet -> sheet.dimension)
                        |> Maybe.map (\dimension -> dimension.height)
                        |> Maybe.withDefault 0

                containerHeight =
                    maybeDimension result
                        |> Maybe.map (\dimension -> dimension.height)
                        |> Maybe.withDefault 0
            in
            ( { model | exceedsHeight = containerHeight > sheetHeight }
            , Cmd.none
            )

        AddSection sheetId rowId columnId ->
            let
                sheetContainerId : String
                sheetContainerId =
                    List.filter (\sheet -> sheet.id == sheetId) model.sheets
                        |> List.head
                        |> Maybe.map (\sheet -> sheet.container.id)
                        |> Maybe.withDefault ""

                ( uuid, newSeed ) =
                    createUuid model.currentSeed

                createSection : Section
                createSection =
                    Section uuid Nothing

                updateColumn : Column -> Column
                updateColumn column =
                    if (column.id == columnId) then
                        { column | sections = column.sections ++ [ createSection ] }
                    else
                        column

                updateRow : Row -> Row
                updateRow row =
                    if (row.id == rowId) then
                        { row | columns = List.map updateColumn row.columns }
                    else
                        row

                updateSheet : Sheet -> Sheet
                updateSheet sheet =
                    if (sheet.id == sheetId) then
                        { sheet | rows = List.map updateRow sheet.rows }
                    else
                        sheet
            in
            ( { model | sheets = List.map updateSheet model.sheets,
                    currentSeed = newSeed
              }
            , Task.attempt (UpdateSectionDimension sheetId rowId columnId uuid) (Browser.Dom.getElement uuid)
            )

        UpdateSectionDimension sheetId rowId columnId sectionId result ->
            let
                sheetContainerId : String
                sheetContainerId =
                    List.filter (\sheet -> sheet.id == sheetId) model.sheets
                        |> List.head
                        |> Maybe.map (\sheet -> sheet.container.id)
                        |> Maybe.withDefault ""

                updateSection : Section -> Section
                updateSection section =
                    if (section.id == sectionId) then
                        { section | dimension = maybeDimension result }
                    else
                        section

                updateColumn : Column -> Column
                updateColumn column =
                    if (column.id == columnId) then
                        { column | sections = List.map updateSection column.sections }
                    else 
                        column
                updateRow : Row -> Row
                updateRow row =
                    if (row.id == rowId) then
                        { row | columns = List.map updateColumn row.columns }
                    else
                        row

                updateSheet : Sheet -> Sheet
                updateSheet sheet =
                    if (sheet.id == sheetId) then
                        { sheet | rows = List.map updateRow sheet.rows }
                    else
                        sheet
            in
            ( { model | sheets = List.map updateSheet model.sheets }
            , Task.attempt (UpdateSheetContainerDimension sheetId) (Browser.Dom.getElement sheetContainerId)
            )

createUuid : Seed -> ( String, Seed )           
createUuid seed =
    step Uuid.uuidGenerator seed
        |> Tuple.mapFirst Uuid.toString


maybeDimension : (Result Error Element) -> Maybe Dimension
maybeDimension result =
    case result of
        Ok element ->
            Just <| Dimension element.element.height element.element.width
        Err error ->
            Nothing


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
            if (model.exceedsHeight) then
                text "Elements exceed sheet height."
            else
                text "Everything is looking good."

        sheetIdAndRowsTuple : Sheet -> ( String, List Row )
        sheetIdAndRowsTuple sheet =
            ( sheet.id, sheet.rows )

        sheetIdAndRowsExpand : ( String, List Row ) -> List ( String, Row )
        sheetIdAndRowsExpand ( sheetId, rows ) =
            List.map (\row -> ( sheetId, row ) ) rows

        sheetIdAndRowToView : ( String, Row ) -> Html Msg
        sheetIdAndRowToView ( sheetId, row ) =
            viewSidebarRow sheetId row
    in
    div [ class "sidebar" ]
        [ div [ class "actions" ]
              []
        , div []
              [ displayText ]
        , div [ class "cards" ]
              (List.map sheetIdAndRowsTuple model.sheets
                   |> List.map sheetIdAndRowsExpand
                   |> List.concat
                   |> List.map sheetIdAndRowToView
              )
        ]


viewSidebarRow : String -> Row -> Html Msg
viewSidebarRow sheetId row =
    div [ class "card" ]
        [ div [ class "header" ]
              [ div [] [ text ("Row - " ++ row.id)] ]
        , div []
            [ div []
                  (List.map (viewSidebarColumn sheetId row.id) row.columns)
            ]
        ]


viewSidebarColumn : String -> String -> Column -> Html Msg
viewSidebarColumn sheetId rowId column =
    div [ class "column" ]
        [ div [ class "header" ]
              [ div []
                    [ text ("Column - " ++ column.id) ]
              , div []
                  [ div []
                        [ button [ onClick (AddSection sheetId rowId column.id) ]
                              [ text "+ Section" ]
                        ]
                  ]
              ]
        , div []
            (List.map viewSidebarSection column.sections)
        ]


viewSidebarSection : Section -> Html Msg
viewSidebarSection section =
    div []
        [ text ("Section - " ++ section.id)]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map viewSheet model.sheets)


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    div [ id sheet.id, class "sheet" ]
        [ div [ id sheet.container.id, style "height" "auto" ]
              (List.map viewRow sheet.rows)
        ]


viewRow : Row -> Html Msg
viewRow row =
    div [ id row.id, class "row" ]
        (List.map viewColumn row.columns)


viewColumn : Column -> Html Msg
viewColumn column =
    div [ class "column" ]
        [ div [ id column.id, class "column__content" ]
              (List.map viewSection column.sections)
        ]


viewSection : Section -> Html Msg
viewSection section =
    div [ id section.id ]
        [ text ("Section - " ++ section.id) ]

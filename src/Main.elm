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
    , rows = []
    }


initSheetContainer : String -> SheetContainer
initSheetContainer uuid =
    { id = uuid
    , dimension = Nothing
    }


-- Update


type Msg
    = AddRow
    | AddColumn String
    | AddSection String
    | UpdateSectionDimension String (Result Error Element)
    | UpdateSheetDimension String (Result Error Element)
    | UpdateSheetContainerDimension String (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            let
                ( uuid, newSeed ) =
                    createUuid model.currentSeed

                addRowToLastSheet =
                    List.reverse model.sheets
                        |> List.head
                        |> Maybe.map (\sheet -> { sheet | rows = sheet.rows ++ [ (createRow uuid)  ] })
                        |> Maybe.map (\sheet -> (List.drop 1 model.sheets) ++ [ sheet ])
                        |> Maybe.withDefault model.sheets
                        
            in
            ( { model | sheets = addRowToLastSheet, currentSeed = newSeed }
            , Cmd.none
            )

        AddColumn rowId ->
            let
                ( uuid, newSeed ) =
                    createUuid model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Cmd.none
            )

        AddSection columnId ->
            let
                ( uuid, newSeed ) =
                    createUuid model.currentSeed
            in
            ( { model | currentSeed = newSeed }
            , Task.attempt (UpdateSectionDimension uuid) (Browser.Dom.getElement uuid)
            )

        UpdateSectionDimension sectionId result ->
            let
               updateSection section =
                   if (sectionId == section.id) then
                       { section | dimension = maybeDimension result }
                   else
                       section
            in
            ( model
            , Task.attempt (UpdateSheetContainerDimension "") (Browser.Dom.getElement "sheetContainer")
            )

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


createUuid : Seed -> ( String, Seed )
createUuid currentSeed =
    step Uuid.uuidGenerator currentSeed
        |> Tuple.mapFirst Uuid.toString


createRow : String -> Row
createRow uuid =
    Row uuid []


createColumn : String -> Column
createColumn uuid =
    Column uuid []


createSection : String -> Section
createSection uuid =
    Section uuid Nothing


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
    in
    div [ class "sidebar" ]
        [ div [ class "actions" ]
              [ button [ onClick AddRow ]
                    [ text "Add Row" ]
              ]
        , div []
              [ displayText ]
        , div [ class "cards" ]
              (List.map (\sheet -> sheet.rows) model.sheets
                   |> List.concat
                   |> List.map viewSidebarRow
              )
        ]


viewSidebarRow : Row -> Html Msg
viewSidebarRow row =
    div [ class "card" ]
        [ div [ class "header" ]
              [ div [] [ text ("Row - " ++ row.id)]
              , div []
                  [ button [ onClick (AddColumn row.id) ]
                        [ text "Add Column" ]
                  ]
              ]
        , div []
            [ div [ ]
                  (List.map viewSidebarColumn row.columns)
            ]
        ]


viewSidebarColumn : Column -> Html Msg
viewSidebarColumn column =
    div [ class "column" ]
        [ div [ class "header" ]
              [ div []
                    [ text ("Column - " ++ column.id) ]
              , div []
                  [ button [ onClick (AddSection column.id) ]
                        [ text "Add Section" ]
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

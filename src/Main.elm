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
    , rows : List Row
    , columns: List Column
    , sections: List Section
    , exceedsHeight : Bool
    }


type alias Sheet =
    { id : String
    , order : Int
    , dimension : Maybe Dimension
    }


type alias Row =
    { id : String
    , sheetId: String
    , order : Int
    }


type alias Column =
    { id : String
    , rowId: String
    , order: Int
    }


type alias Section =
    { id : String
    , columnId: String
    , order : Int
    , dimension : Maybe Dimension
    }


type alias Dimension =
    { height : Float
    , width : Float
    }


init : Int -> ( Model, Cmd Msg )
init seed =
    ( { currentSeed = initialSeed seed
      ,sheets = [ initSheet ]
      , rows = []
      , columns = []
      , sections = []
      , exceedsHeight = False
      }
    , Task.attempt (UpdateSheetDimension "sheet0")  (Browser.Dom.getElement "sheet0")
    )


initSheet : Sheet
initSheet =
    { id = "sheet0"
    , order = 0
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
            ( { model | rows = model.rows ++ [(createRow "sheet0" (List.length model.rows))] }
            , Cmd.none
            )

        AddColumn rowId ->
            ( { model | columns = model.columns ++ [(createColumn rowId (List.length model.columns))] }
            , Cmd.none
            )

        AddSection columnId ->
            let
                ( uuid, newSeed ) =
                    createUuid model.currentSeed
            in
            ( { model | sections = model.sections ++ [(createSection columnId (List.length model.sections) uuid)]
              , currentSeed = newSeed }
            , Task.attempt (UpdateSectionDimension (createId "section" (List.length model.sections) )) (Browser.Dom.getElement (Uuid.toString uuid))
            )

        UpdateSectionDimension sectionId result ->
            let
               updateSection section =
                   if (sectionId == section.id) then
                       { section | dimension = maybeDimension result }
                   else
                       section
            in
            ( { model | sections = List.map updateSection model.sections }
            , Task.attempt (UpdateSheetContainerDimension "sheet0") (Browser.Dom.getElement "sheetContainer")
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
            ( { model | exceedsHeight = containerHeight > sheetHeight }, Cmd.none )


createUuid : Seed -> ( Uuid.Uuid, Seed )
createUuid currentSeed =
    step Uuid.uuidGenerator currentSeed

createId : String -> Int -> String
createId prefix value =
    prefix ++ String.fromInt value


createRow : String -> Int -> Row
createRow sheetId order=
    Row (createId "row" order) sheetId order


createColumn : String -> Int -> Column
createColumn rowId order =
    Column (createId "column" order) rowId order


createSection : String  -> Int -> Uuid.Uuid-> Section
createSection columnId order uuid =
    Section (Uuid.toString uuid) columnId order Nothing


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
              (List.map (viewSidebarRow model) model.rows)
        ]


viewSidebarRow : Model -> Row -> Html Msg
viewSidebarRow model row =
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
                  (List.map (viewSidebarColumn model) (List.filter (\column -> column.rowId == row.id) model.columns))
            ]
        ]


viewSidebarColumn : Model -> Column -> Html Msg
viewSidebarColumn model column =
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
            (List.map viewSidebarSection (List.filter (\section -> section.columnId == column.id) model.sections))
        ]


viewSidebarSection : Section -> Html Msg
viewSidebarSection section =
    div []
        [ text ("Section - " ++ section.id)]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map (viewSheet model) model.sheets)


viewSheet : Model -> Sheet -> Html Msg
viewSheet model sheet =
    div [ id sheet.id, class "sheet" ]
        [ div [ id "sheetContainer", style "height" "auto" ]
              (List.map (viewRow model) (List.filter (\row -> row.sheetId == sheet.id) model.rows))
        ]


viewRow : Model -> Row -> Html Msg
viewRow model row =
    div [ id row.id, class "row" ]
        (List.map (viewColumn model) (List.filter (\column -> column.rowId == row.id) model.columns))


viewColumn : Model -> Column -> Html Msg
viewColumn model column =
    div [ class "column" ]
        [ div [ id column.id, class "column__content" ]
              (List.map viewSection (List.filter (\section -> section.columnId == column.id) model.sections))
        ]


viewSection : Section -> Html Msg
viewSection section =
    div [ id section.id ]
        [ text ("Section - " ++ section.id) ]

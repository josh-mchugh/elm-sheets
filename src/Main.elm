module Main exposing (main)

{-| Elm Sheets application
-}

import Browser
import Browser.Dom exposing (Element, Error)
import Handlebars
import Html exposing (section)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Html.Parser
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Random exposing (Seed, initialSeed, step)
import Task
import Platform.Cmd as Cmd
import Uuid
import Html.Parser.Util



-- MAIN


main : Program Flags Model Msg
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
    , layout : Maybe Layout
    , resume : Maybe Resume
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
    , dimension: Maybe Dimension
    , columns : List Column
    }


type alias Column =
    { id : String
    , dimension : Maybe Dimension
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


type alias Layout =
    { class : String
    , rows : List LayoutRow
    }


type alias LayoutRow =
    { columns : List LayoutColumn }


type alias LayoutColumn =
    { class : String
    , sections : List LayoutSection
    }


type alias LayoutSection =
    { name : String
    , type_ : SectionType
    , template : String
    }


type SectionType
    = SectionName
    | SectionSummary
    | SectionContact
    | SectionSocial
    | SectionExperience
    | SectionSkill
    | SectionCertification


type alias Flags =
    { externalRandom : Int
    , layout : Decode.Value
    , resume : Decode.Value
    }


type alias Resume =
    { name : String
    , title : String
    , summary : String
    , phone : String
    , email : String
    , location : String
    , socials : List Social
    , experiences : List Experience
    , skills : List Skill
    , certifications : List Certification
    }


type alias Social =
    { name : String
    , icon : String
    , url : String
    }


type alias Experience =
    { organization : String
    , title : String
    , duration : String
    , location : String
    , descriptions : List String
    , skills : List String
    }


type alias Skill =
    { name : String
    , proficiency : Int
    }


type alias Certification =
    { title : String
    , organization : String
    , location : String
    , year : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( sheetUuid, sheetSeed ) =
            createUuid (initialSeed flags.externalRandom)

        ( containerUuid, containerSeed ) =
            createUuid (sheetSeed)

        parseLayout =
            case Decode.decodeValue layoutDecoder flags.layout of
                Ok layout ->
                    Just layout
                Err err ->
                    Nothing


        parseResume =
            case Decode.decodeValue resumeDecoder flags.resume of
                Ok resume ->
                    Just resume
                Err err ->
                    Nothing
    in
    ( { currentSeed = containerSeed
      , sheets = [ (initSheet sheetUuid containerUuid) ]
      , exceedsHeight = False
      , layout = parseLayout
      , resume = parseResume
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
    , dimension = Nothing
    , columns = [ initLeftColumn, initRightColumn ]
    }


initLeftColumn : Column
initLeftColumn =
    { id = "leftColumn"
    , dimension = Nothing
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
    , dimension = Nothing
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


emptyResume : Resume
emptyResume =
    { name = ""
    , title = ""
    , summary = ""
    , phone = ""
    , email = ""
    , location = ""
    , socials = []
    , experiences = []
    , skills = []
    , certifications = []
    }


-- Update


type Msg
    = UpdateSheetDimension String (Result Error Element)
    | UpdateSheetContainerDimension String (Result Error Element)
    | UpdateRowDimension String String (Result Error Element)
    | UpdateColumnDimension String String String (Result Error Element)
    | UpdateSectionDimension String String String String (Result Error Element)
    | AddSection String String String


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

        UpdateRowDimension sheetId rowId result ->
            let
                updateRow : Row -> Row
                updateRow row =
                    if (row.id == rowId) then
                        { row | dimension = maybeDimension result }
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
            , Cmd.none
            )

        UpdateColumnDimension sheetId rowId columnId result ->
            let
                updateColumn : Column -> Column
                updateColumn column =
                    if (column.id == columnId) then
                        { column | dimension = maybeDimension result }
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
            , Cmd.none
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

                -- Calculate what can be removed
                dimensionHeight : Maybe Dimension -> Float
                dimensionHeight maybeDimension1 =
                    Maybe.map (\dimension -> dimension.height) maybeDimension1
                        |> Maybe.withDefault 0

                foldSectionsHeight : List Section -> Float
                foldSectionsHeight sections =
                    List.foldl (\section accum -> accum + (dimensionHeight section.dimension))  0 sections

                calculateColumnHeights : List ( String, String, List Section ) -> List ( String, String, Float )
                calculateColumnHeights tupleWithSections =
                    List.map (\( rowId, columnId, sections ) -> ( rowId, columnId, (foldSectionsHeight sections) ) ) tupleWithSections

                tupleOfRowIdColumnIdAndSections : List ( String, List Column ) -> List ( String, String, List Section)
                tupleOfRowIdColumnIdAndSections rowIdColumns =
                    List.map (\( rowId, columns ) -> List.map (\column -> ( rowId, column.id, column.sections )) columns ) rowIdColumns
                        |> List.concat

                tupleOfRowIdAndColumns : List Row -> List ( String, List Column )
                tupleOfRowIdAndColumns rows =
                    List.map (\row -> ( row.id, row.columns ) ) rows

                listOfRows : Maybe Sheet -> List Row
                listOfRows maybeSheet =
                    Maybe.map (\sheet -> sheet.rows) maybeSheet
                        |> Maybe.withDefault []

                sheetById : Maybe Sheet
                sheetById =
                    List.filter (\sheet -> sheet.id == sheetId) model.sheets
                        |> List.head

                thatThingIAmDoing : List ( String, String, Float )
                thatThingIAmDoing =
                    sheetById
                        |> listOfRows
                        |> tupleOfRowIdAndColumns
                        |> tupleOfRowIdColumnIdAndSections
                        |> calculateColumnHeights
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
            , Cmd.batch (updateDimensionCmds model) 
            )


updateDimensionCmds : Model -> List (Cmd Msg)
updateDimensionCmds model =
    let

        createSheetCmd : String -> Cmd Msg
        createSheetCmd sheetId =
            Task.attempt (UpdateSheetDimension sheetId) (Browser.Dom.getElement sheetId) 


        createSheetCmds : List Sheet -> List (Cmd Msg)
        createSheetCmds sheets =
            List.map (\sheet -> (createSheetCmd sheet.id) ) sheets


        createSheetContainerCmd : String -> String -> Cmd Msg
        createSheetContainerCmd sheetId sheetContainerId =
            Task.attempt (UpdateSheetContainerDimension sheetId) (Browser.Dom.getElement sheetContainerId)


        createSheetContainerCmds : List Sheet -> List (Cmd Msg)
        createSheetContainerCmds sheets =
            List.map (\sheet -> (createSheetContainerCmd sheet.id sheet.container.id) ) sheets


        createRowCmd : ( String, String ) -> Cmd Msg
        createRowCmd ( sheetId, rowId ) =
            Task.attempt (UpdateRowDimension sheetId rowId) (Browser.Dom.getElement rowId)


        createRowCmds : List Sheet -> List (Cmd Msg)
        createRowCmds sheets =
            List.map (\sheet -> ( sheet.id, sheet.rows) ) sheets
                |> List.map (\( sheetId, rows ) -> List.map (\row -> ( sheetId, row.id )) rows)
                |> List.concat
                |> List.map createRowCmd


        createColumnCmd : ( String, String, String ) -> Cmd Msg
        createColumnCmd ( sheetId, rowId, columnId ) =
            Task.attempt (UpdateColumnDimension sheetId rowId columnId) (Browser.Dom.getElement columnId)


        createColumnCmds : List Sheet -> List (Cmd Msg)
        createColumnCmds sheets =
            List.map (\sheet -> ( sheet.id, sheet.rows ) ) sheets
                |> List.map (\( sheetId, rows ) -> List.map (\row -> ( sheetId, row.id, row.columns ) ) rows)
                |> List.concat
                |> List.map (\( sheetId, rowId, columns ) -> List.map (\column -> ( sheetId, rowId, column.id ) ) columns)
                |> List.concat
                |> List.map createColumnCmd


        createSectionCmd : ( String, (Result Error Element) ->  Msg ) -> Cmd Msg
        createSectionCmd ( sectionId, msg ) =
            Task.attempt msg (Browser.Dom.getElement sectionId)


        createSectionCmds : List Sheet -> List (Cmd Msg)
        createSectionCmds sheets =
            List.map (\sheet -> ( (UpdateSectionDimension sheet.id), sheet.rows ) ) sheets
                |> List.map (\( msg, rows ) -> List.map (\row -> ( (msg row.id), row.columns) ) rows)
                |> List.concat
                |> List.map (\( msg , columns ) -> List.map (\column -> ( (msg column.id), column.sections) ) columns)
                |> List.concat
                |> List.map (\( msg, sections ) -> List.map (\section -> ( section.id, (msg section.id) ) ) sections)
                |> List.concat
                |> List.map createSectionCmd
    in
    List.concat [ (createSheetCmds model.sheets)
                , (createSheetContainerCmds model.sheets)
                , (createRowCmds model.sheets)
                , (createColumnCmds model.sheets)
                , (createSectionCmds model.sheets)
                ]
   

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
        --, viewContent model
        , viewLayoutContent model
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


viewLayoutContent : Model -> Html Msg
viewLayoutContent model =
    div [ class "content" ]
        [ (viewLayoutSheet model) ]


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    div [ id sheet.id, class "sheet" ]
        [ div [ id sheet.container.id, style "height" "auto" ]
              (List.map viewRow sheet.rows)
        ]


viewLayoutSheet : Model -> Html Msg
viewLayoutSheet model =
    let
        layoutClass =
            case model.layout of
                Just layout ->
                    layout.class
                Nothing ->
                    ""

        layoutSheet =
            case model.layout of
                Just layout ->
                    (List.map (viewLayoutRow resumeData) layout.rows)
                Nothing ->
                    []

        resumeData =
            case model.resume of
                Just resume ->
                    resume
                Nothing ->
                    emptyResume
    in
    div [ id "sheet1", class "sheet" ]
        [ div [ class layoutClass ]
              [
               div [ id "sheet1Container"
                   , style "height" "auto"
                   , style "width" "100%"
                   ]
                   (layoutSheet)
              ]
        ]


viewRow : Row -> Html Msg
viewRow row =
    div [ id row.id, class "row" ]
        (List.map viewColumn row.columns)


viewLayoutRow : Resume -> LayoutRow -> Html Msg
viewLayoutRow resume row =
    div [ id "row?", class "row" ]
        (List.map (viewLayoutColumn resume) row.columns)


viewColumn : Column -> Html Msg
viewColumn column =
    div [ class "column" ]
        [ div [ id column.id, class "column__content" ]
              (List.map viewSection column.sections)
        ]


viewLayoutColumn : Resume -> LayoutColumn -> Html Msg
viewLayoutColumn resume column =
    div [ class "column", class column.class ]
        [ div [ id "column?", class "column__content" ]
              (List.map (viewLayoutSection resume) column.sections)
        ]


viewSection : Section -> Html Msg
viewSection section =
    div [ id section.id ]
        [ text ("Section - " ++ section.id) ]


viewLayoutSection : Resume -> LayoutSection -> Html Msg
viewLayoutSection resume section =
    let
        handlebarValues : Encode.Value
        handlebarValues =
            case section.type_ of
                SectionName ->
                    Encode.object
                        [ ( "name", Encode.string resume.name )
                        , ( "title", Encode.string resume.title )
                        ]

                SectionSummary ->
                    Encode.object
                        [ ( "summary", Encode.string resume.summary ) ]

                SectionContact ->
                    Encode.object
                        [ ( "phone", Encode.string resume.phone )
                        , ( "email", Encode.string resume.email )
                        , ( "location", Encode.string resume.location )
                        ]

                SectionSocial ->
                    Encode.object
                        [ ( "socials", (Encode.list encodeSocial resume.socials) ) ]

                SectionExperience ->
                    Encode.object []

                SectionSkill ->
                    Encode.object []

                SectionCertification ->
                    Encode.object []


        handleCompile value=
            Handlebars.compile Handlebars.defaultConfig section.template value


        parseTemplate =
            case (handleCompile handlebarValues) of
                    Ok template ->
                        case Html.Parser.run template of
                            Ok result ->
                                Html.Parser.Util.toVirtualDom result
                            Err err ->
                                [ text template  ]
                    Err err ->
                        [ text (Handlebars.errorToString err) ]

    in
    div [ id "section?" ]
        (parseTemplate)


-- JSON ENCODE


encodeSocial : Social -> Encode.Value
encodeSocial social =
    Encode.object
        [ ( "name", Encode.string social.name )
        , ( "icon", Encode.string social.icon )
        , ( "url", Encode.string social.url )
        ]


-- JSON DECODE


layoutDecoder : Decoder Layout
layoutDecoder =
    Decode.succeed Layout
        |> required "class" Decode.string
        |> required "rows" (Decode.list layoutRowDecoder)


layoutRowDecoder : Decoder LayoutRow
layoutRowDecoder =
    Decode.succeed LayoutRow
        |> required "columns" (Decode.list layoutColumnDecoder)


layoutColumnDecoder : Decoder LayoutColumn
layoutColumnDecoder =
    Decode.succeed LayoutColumn
        |> required "class" Decode.string
        |> required "sections" (Decode.list layoutSectionDecoder)


layoutSectionDecoder : Decoder LayoutSection
layoutSectionDecoder =
    Decode.succeed LayoutSection
        |> required "name" Decode.string
        |> required "type" sectionTypeDecoder
        |> optional "template" Decode.string ""


sectionTypeDecoder : Decoder SectionType
sectionTypeDecoder =
    let
        transformString: String -> Decoder SectionType
        transformString type_ =
            case type_ of
                "NAME" ->
                    Decode.succeed SectionName
                "SUMMARY" ->
                    Decode.succeed SectionSummary
                "CONTACT" ->
                    Decode.succeed SectionContact
                "SOCIAL" ->
                    Decode.succeed SectionSocial
                "EXPERIENCE" ->
                    Decode.succeed SectionExperience
                "SKILL" ->
                    Decode.succeed SectionSkill
                "CERTIFICATION" ->
                    Decode.succeed SectionCertification
                _ ->
                    Decode.fail("Unknown Section Type: " ++ type_)
    in
    Decode.string
        |> Decode.andThen transformString


resumeDecoder : Decoder Resume
resumeDecoder =
    Decode.succeed Resume
        |> optional "name" Decode.string ""
        |> optional "title" Decode.string ""
        |> optional "summary" Decode.string ""
        |> optional "phone" Decode.string ""
        |> optional "email" Decode.string ""
        |> optional "location" Decode.string ""
        |> optional "socials" (Decode.list resumeSocialDecoder) []
        |> optional "experiences" (Decode.list resumeExperienceDecoder) []
        |> optional "skills" (Decode.list resumeSkillDecoder) []
        |> optional "certifications" (Decode.list resumeCertificationDecoder) []


resumeSocialDecoder : Decoder Social
resumeSocialDecoder =
    Decode.succeed Social
        |> optional "name" Decode.string ""
        |> optional "icon" Decode.string ""
        |> optional "url" Decode.string ""


resumeExperienceDecoder : Decoder Experience
resumeExperienceDecoder =
    Decode.succeed Experience
        |> optional "organization" Decode.string ""
        |> optional "title" Decode.string ""
        |> optional "duration" Decode.string ""
        |> optional "location" Decode.string ""
        |> optional "descriptions" (Decode.list Decode.string) []
        |> optional "skills" (Decode.list Decode.string) []


resumeSkillDecoder : Decoder Skill
resumeSkillDecoder =
    Decode.succeed Skill
        |> optional "name" Decode.string ""
        |> optional "proficiency" Decode.int 0


resumeCertificationDecoder : Decoder Certification
resumeCertificationDecoder =
    Decode.succeed Certification
        |> optional "title" Decode.string ""
        |> optional "organization" Decode.string ""
        |> optional "location" Decode.string ""
        |> optional "year" Decode.string ""

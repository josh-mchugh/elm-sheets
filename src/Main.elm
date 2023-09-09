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
    { sheets : List Sheet
    , rows : Array Row
    , exceedsBounds : Bool
    }


type alias Row =
    { bounds : Maybe Bounds }


type alias Bounds =
    { top: Float
    , left: Float
    , bottom: Float
    , right: Float
    }


type alias Sheet =
    { id : Int
    , bounds : Maybe Bounds
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = [ Sheet 1 Nothing ]
      , rows = Array.fromList []
      , exceedsBounds = False
      }
    , Task.attempt (UpdateSheetBounds 1)  (Browser.Dom.getElement "sheet1")
    )



-- Update


type Msg
    = AddRow
    | UpdateSheetBounds Int (Result Error Element)
    | CheckSheetBounds (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            ( { model | rows = Array.push (Row Nothing) model.rows }
            , Cmd.none
            )

        UpdateSheetBounds id result ->
            case result of
                Ok element ->
                    let
                        updateSheet sheet =
                            if sheet.id == id then
                                { sheet | bounds = Just
                                      <| createBounds element
                                }
                            else
                                sheet
                    in
                    ( { model | sheets = List.map updateSheet model.sheets }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        CheckSheetBounds result ->
            case result of
                Ok element ->
                    let
                        maybeSheet =
                            List.head model.sheets

                        bottom =
                            element.element.y + element.element.height

                        exceedsBottom =
                            case maybeSheet of
                                Just sheet ->
                                    case sheet.bounds of
                                        Just bounds ->
                                            bottom > bounds.bottom
                                        Nothing ->
                                            False
                                Nothing ->
                                    False
                            
                    in
                    ( { model | exceedsBounds = exceedsBottom }
                    , Cmd.none
                    )
                    
                Err error ->
                    ( model, Cmd.none)


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
        ]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map (viewSheet model.rows) model.sheets)


viewSheet : Array Row -> Sheet -> Html Msg
viewSheet rows sheet =
    div [ id ("sheet" ++ String.fromInt sheet.id), class "sheet" ]
        [ div [ id "sheetContainer", style "height" "auto" ]
              (List.map viewRow (Array.toList rows))
        ]

viewRow : Row -> Html Msg
viewRow row =
    div [] [ text "" ]

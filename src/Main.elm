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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = Array.fromList [ Sheet Nothing ]
      , rows = Array.fromList []
      }
    , Task.attempt (UpdateSheetBounds 0)  (Browser.Dom.getElement "sheet0")
    )



-- Update


type Msg
    = AddRow
    | UpdateSheetBounds Int (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            ( { model | rows = Array.push (Row Nothing) model.rows }
            , Cmd.none
            )

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

        sheet =
            Tuple.second sheetTuple
    in
    div [ id ("sheet" ++ String.fromInt index), class "sheet" ]
        [ div [ id "sheetContainer", style "height" "auto" ]
              (List.map viewRow (Array.toList rows))
        ]


viewRow : Row -> Html Msg
viewRow row =
    div [] [ text "" ]







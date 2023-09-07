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
    { sheets : List Sheet
    , items : List Item
    }


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


type alias Item =
    { id : Int
    , bounds: Maybe Bounds
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheets = []
      , items = []
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddSheet
    | UpdateSheetBounds Int (Result Error Element)
    | AddItem
    | UpdateItemBounds Int (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSheet ->
            let
                nextSheetId =
                    List.length model.sheets + 1

                nextSheet =
                    { id = nextSheetId, bounds = Nothing }

                elementId =
                    "sheet" ++ String.fromInt nextSheetId
            in
            ( { model |
                    sheets = model.sheets ++ [ nextSheet ]
              }
            , Task.attempt (UpdateSheetBounds nextSheetId) (Browser.Dom.getElement elementId)
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

        AddItem ->
            let
                nextItemId =
                    List.length model.items + 1

                nextItem =
                    { id = nextItemId, bounds = Nothing }

                elementId =
                    ("item" ++ String.fromInt nextItemId)
            in
            ( { model | items = model.items ++ [ nextItem ] }
            , Task.attempt (UpdateItemBounds nextItemId)  (Browser.Dom.getElement elementId)
            )

        UpdateItemBounds id result ->
            case result of
                Ok element ->
                    let
                        updateItem item =
                            if item.id == id then
                                { item | bounds = Just
                                      <| createBounds element
                                }
                            else
                                item
                    in
                    ( { model | items = List.map updateItem model.items }
                    , Cmd.none
                    )

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
              [ button [ onClick AddSheet ]
                    [ text "Add Sheet" ]
              , button [ onClick AddItem ]
                  [ text "Add Item" ]
              ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        (List.map (viewSheet model.items) model.sheets)


viewSheet : List Item -> Sheet -> Html Msg
viewSheet items sheet =
    div [ id ("sheet" ++ String.fromInt sheet.id), class "sheet" ]
        (List.map viewSimpleDiv items)


viewSimpleDiv : Item -> Html Msg
viewSimpleDiv item =
    let
        displayValue =
            "item #" ++ String.fromInt item.id

        itemId =
            "item" ++ String.fromInt item.id
    in
    div [ id itemId, class "item" ]
        [ text displayValue ]

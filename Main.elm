module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (on)
import Svg
import Svg.Attributes as SA


-- import Html.Events exposing (onClick)

import MultiTouch as MT
import SingleTouch as ST
import Touch as T
import Mouse exposing (Position)
import Json.Decode as Decoder


type alias Location =
    ( Float, Float )


type alias Model =
    { currStEvent : Maybe String
    , currMultiTouchEvent : Maybe String
    , circle : Maybe Location
    , mousePos : Maybe Position
    , mouseDown : Bool
    }


model : Model
model =
    { currStEvent = Nothing
    , currMultiTouchEvent = Nothing
    , circle = Nothing
    , mousePos = Nothing
    , mouseDown = False
    }


type Msg
    = TouchMsg T.TouchEvent T.Touch
    | MultiTouchMsg T.TouchEvent MT.MultiTouch
    | DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        circleForPos position =
            Just <| ( toFloat position.x, toFloat position.y )
    in
        case msg of
            TouchMsg event touch ->
                let
                    tPos =
                        Just <| ( touch.clientX, touch.clientY )

                    circle =
                        case event of
                            T.TouchStart ->
                                tPos

                            T.TouchMove ->
                                tPos

                            T.TouchEnd ->
                                Nothing

                            T.TouchCancel ->
                                Nothing
                in
                    ( { model | currStEvent = Just <| (toString event) ++ (toString touch), circle = circle }
                    , Cmd.none
                    )

            MultiTouchMsg event touch ->
                ( { model | currMultiTouchEvent = Just <| (toString event) ++ (toString touch) }
                , Cmd.none
                )

            DragStart position ->
                ( { model | circle = circleForPos position, mouseDown = True }
                , Cmd.none
                )

            DragEnd position ->
                ( { model | circle = circleForPos position, mouseDown = False }
                , Cmd.none
                )

            DragAt position ->
                ( { model | circle = circleForPos position }
                , Cmd.none
                )


touchDivStyle : String -> Html.Attribute Msg
touchDivStyle borderColor =
    Html.Attributes.style [ ( "border", "2px solid " ++ borderColor ), ( "width", "500px" ), ( "height", "500px" ) ]


singleTouchAttrs : List (Html.Attribute Msg)
singleTouchAttrs =
    let
        touchInstruction touchType =
            ST.onSingleTouch touchType T.preventAndStop <| (\st -> TouchMsg st.touchType st.touch)
    in
        List.map touchInstruction
            [ T.TouchStart, T.TouchMove, T.TouchEnd ]


multiTouchAttrs : Html.Attribute Msg
multiTouchAttrs =
    MT.onMultiTouch T.TouchStart T.preventAndStop <| MultiTouchMsg T.TouchStart


singleTouchDiv : Model -> Html Msg
singleTouchDiv model =
    let
        base =
            Svg.svg <| singleTouchAttrs ++ [ touchDivStyle "black", onMouseDown ]
    in
        case model.circle of
            Nothing ->
                base []

            Just ( x, y ) ->
                base
                    [ Svg.circle [ SA.cx <| toString x, SA.cy <| toString y, SA.r "44" ] []
                    ]


multiTouchDiv : Model -> Html Msg
multiTouchDiv model =
    div [ multiTouchAttrs, touchDivStyle "red" ] [ text "multi touch tester" ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| toString model ]
        , singleTouchDiv model
        , multiTouchDiv model
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = ( model, Cmd.none )
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


onMouseDown : Html.Attribute Msg
onMouseDown =
    on "mousedown" (Decoder.map DragStart Mouse.position)

module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes

import Svg
import Svg.Attributes as SA


-- import Html.Events exposing (onClick)

import MultiTouch as MT
import SingleTouch as ST
import Touch as T

type alias Location = (Float, Float)

type alias Model =
    { currStEvent : String
    , currMultiTouchEvent : String
    , circle : Maybe Location
    }


model : Model
model =
    { currStEvent = "nothin"
    , currMultiTouchEvent = "nothing yet"
    , circle = Nothing
    }


type Msg
    = TouchMsg T.TouchEvent T.Touch
    | MultiTouchMsg T.TouchEvent MT.MultiTouch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TouchMsg event touch ->
          let
              circle =
                case event of
                  T.TouchStart ->
                    Just <| (touch.clientX, touch.clientY)
                  T.TouchMove ->
                    Just <| (touch.clientX, touch.clientY)
                  T.TouchEnd ->
                    Nothing
                  T.TouchCancel ->
                    Nothing

          in
            ( { model | currStEvent = (toString event) ++ (toString touch), circle = circle }
            , Cmd.none
            )

        MultiTouchMsg event touch ->
            ( { model | currMultiTouchEvent = (toString event) ++ (toString touch) }
            , Cmd.none
            )


touchDivStyle : String -> Html.Attribute Msg
touchDivStyle borderColor =
    Html.Attributes.style [ ( "border", "2px solid " ++ borderColor ), ( "padding", "150px" ) ]


singleTouchAttrs : List (Html.Attribute Msg)
singleTouchAttrs =
    [ ST.onSingleTouch T.TouchStart T.preventAndStop <| (\st -> TouchMsg st.touchType st.touch)
    , ST.onSingleTouch T.TouchMove T.preventAndStop <| (\st -> TouchMsg st.touchType st.touch)
    , ST.onSingleTouch T.TouchEnd T.preventAndStop <| (\st -> TouchMsg st.touchType st.touch)
    ]


multiTouchAttrs : Html.Attribute Msg
multiTouchAttrs =
    MT.onMultiTouch T.TouchStart T.preventAndStop <| MultiTouchMsg T.TouchStart


singleTouchDiv : Model -> Html Msg
singleTouchDiv model =
  let
      base = Svg.svg <| singleTouchAttrs ++ [ touchDivStyle "black" ]
  in
    case model.circle of
      Nothing ->
        base []
      Just (x, y) ->
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
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }

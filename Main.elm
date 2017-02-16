module Counter exposing (main)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Html.Attributes
import Touch as T
import SingleTouch as ST


type alias Model =
    { counter : Int
    , currStEvent : String
    }


model : Model
model =
    { counter = 1
    , currStEvent = "nothin"
    }


type Msg
    = Increment
    | Decrement
    | TouchMsg T.TouchEvent T.Touch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )

        Decrement ->
            ( { model | counter = model.counter - 1 }
            , Cmd.none
            )

        TouchMsg event touch ->
            ( { model | currStEvent = (toString event) ++ (toString touch) }
            , Cmd.none
            )


singleTouchDivStyle : Html.Attribute Msg
singleTouchDivStyle =
    Html.Attributes.style [ ( "border", "2px solid black" ), ( "padding", "15px" ) ]


stToM : ST.SingleTouch -> Msg
stToM st =
    TouchMsg st.touchType st.touch


touchAttrs : Html.Attribute Msg
touchAttrs =
    ST.onSingleTouch T.TouchStart T.preventAndStop <| stToM


singleTouchDiv : Model -> Html Msg
singleTouchDiv model =
    div [ touchAttrs, singleTouchDivStyle ] [ text "single touch tester" ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick Increment ] [ text "+" ] ]
        , div [] [ text <| toString model ]
        , div [] [ button [ onClick Decrement ] [ text "-" ] ]
        , singleTouchDiv model
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }

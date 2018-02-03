module Football exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL
-- application state


type alias Model =
    { clock : Int
    , homeScore : Int
    , awayScore : Int
    , down : Int
    , distance : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 3600 0 0 1 10, Cmd.none )


type Msg
    = Play


update msg model =
    case msg of
        Play ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "background", "#2c2c2c" )
            , ( "height", "100%" )
            , ( "color", "#fff" )
            ]
        ]
        [ dl
            []
            [ dt [] [ text "Clock:" ]
            , dd [] [ text (toString model.clock) ]
            ]
        , div []
            [ span [] [ text ("New England " ++ (toString model.awayScore) ++ "-" ++ (toString model.homeScore) ++ " Philadelphia") ]
            ]
        , span [] [ text ((toString model.down) ++ " & " ++ (toString model.distance)) ]
        ]

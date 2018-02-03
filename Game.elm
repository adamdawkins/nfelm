module Football exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


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
    , ball : Int
    , isEndOfGame : Bool
    , homeTeamHasBall : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 3600 0 0 1 10 20 False True, Cmd.none )



-- UPDATE


type Msg
    = Play
    | NewYardage Int


update msg model =
    case msg of
        Play ->
            let
                newClock =
                    model.clock - 100
            in
                if newClock <= 0 then
                    ( { model | clock = 0, isEndOfGame = True }, Cmd.none )
                else
                    ( { model | clock = newClock }, Random.generate NewYardage (Random.int 5 15) )

        NewYardage yards ->
            let
                newLOS =
                    clamp 0
                        100
                        (if model.homeTeamHasBall then
                            model.ball + yards
                         else
                            model.ball - yards
                        )
            in
                -- home team scores
                if newLOS == 100 then
                    ( { model
                        | homeScore = model.homeScore + 7
                        , ball = 80
                        , homeTeamHasBall = False
                      }
                    , Cmd.none
                    )
                -- away team scores
                else if newLOS == 0 then
                    ( { model
                        | awayScore = model.awayScore + 7
                        , ball = 20
                        , homeTeamHasBall = True
                      }
                    , Cmd.none
                    )
                -- update line of scrimmage
                else
                    ( { model | ball = newLOS }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    if model.isEndOfGame then
        div [ containerStyle ]
            [ span [] [ text "Full time!" ]
            , viewScore model
            ]
    else
        div
            [ containerStyle ]
            [ viewClock model
            , viewScore model
            , viewGameSituation model
            , div []
                [ button [ onClick Play ] [ text "Play!" ]
                ]
            ]


containerStyle =
    style
        [ ( "background", "#2c2c2c" )
        , ( "height", "100%" )
        , ( "color", "#fff" )
        ]


viewClock : Model -> Html Msg
viewClock model =
    dl
        []
        [ dt [] [ text "Clock:" ]
        , dd [] [ text (toString model.clock) ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    div [] [ text ("New England " ++ (toString model.awayScore) ++ "-" ++ (toString model.homeScore) ++ " Philadelphia") ]


viewGameSituation : Model -> Html Msg
viewGameSituation model =
    div []
        [ div [] [ text ((toString model.down) ++ " & " ++ (toString model.distance)) ]
        , div [] [ text ("LOS:" ++ (toString model.ball)) ]
        ]

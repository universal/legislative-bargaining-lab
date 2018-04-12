module BargainLab exposing (..)

import Coalitions exposing (..)
import Dict
import GAMS
import Games exposing (Game(..), gameDecoder, gameDefinition, games)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode
import Power
import Probabilities
import QOBDD exposing (BDD, QOBDD, parseMWVG, parsedMWVG, size)
import Random exposing (Generator)
import Vector exposing (toList)

import Http
import Json.Encode
import Json.Decode
-- split Model


type alias Model =
    { text : String, qobdd : Maybe QOBDD, probs : List (List Float), game: Maybe Game }


hasQOBDD : Model -> Bool
hasQOBDD model =
    case model.qobdd of
        Nothing ->
            True

        Just _ ->
            False


hasText : Model -> Bool
hasText model =
    String.isEmpty model.text



-- split Msg


type Msg
    = Parse
    | Display Game
    | Input String
    | Random
    | Probs (List (List Float))
    | Parsed QOBDD
    | PostModel
    | PostModelResponse (Result Http.Error String)


init : ( Model, Cmd Msg )
init =
    --let
    --    text = Games.gameDefinition Games.EU27
    --in
    --( { text = text, qobdd = Nothing, probs = [] }, parseMWVG text )
    ( { text = "", qobdd = Nothing, probs = [], game = Nothing }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( model, parseMWVG model.text )

        Display g ->
            ( { model | text = gameDefinition g, game = Just g }, Cmd.none )

        Input str ->
            ( { model | text = str }, Cmd.none )

        Parsed qobdd ->
            ( { model | qobdd = Just qobdd, probs = Probabilities.halvesDiag qobdd.vars }, Cmd.none )

        Random ->
            case model.qobdd of
                Just q ->
                    ( model, Random.generate Probs (Probabilities.probsDiagGen q.vars) )

                Nothing ->
                    Debug.crash ""

        Probs fs ->
            ( { model | probs = fs }, Cmd.none )
        PostModel ->
            let
                resultToString ( stmts, vs ) =
                    vs ++ "\n\n" ++ GAMS.prettyStmts stmts
                codeP = (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmt <| o) model.qobdd))
                codeAlphaWin = (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtAlphaWin <| o) model.qobdd))
                codeAlphaLose = (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtAlphaLose <| o) model.qobdd))
                codeWith = (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtWith <| o) model.qobdd))
                codeWithout = (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtWithout <| o) model.qobdd))
                game = Maybe.withDefault "no_game" (Maybe.map (\o -> Games.showGame o) model.game)
            in
            ( model, Http.send PostModelResponse (postJsonTask (codeP ++ "\n\n" ++ codeAlphaWin ++ "\n\n" ++ codeAlphaLose ++ "\n\n" ++ codeWith ++ "\n\n" ++ codeWithout) game) )
        PostModelResponse _ ->
            ( model, Cmd.none )


headerRow : Model -> Html Msg
headerRow model =
    div []
        [ text "Game:"
        , select [ on "change" (Json.Decode.map Display (Json.Decode.andThen gameDecoder targetValue)) ]
            gameOptions
        , button [ onClick Parse, disabled (hasText model) ] [ text "Load current game" ]
        ]


gameOptions : List (Html Msg)
gameOptions =
    option [ value "", disabled True, selected True ] [ text "Please Choose" ]
        :: List.map gameOption games


gameOption : Game -> Html Msg
gameOption game =
    option [ value (toString game) ] [ text (Games.showGame game) ]

viewGamsCode : Model -> Html Msg
viewGamsCode model =
    div [ id "gams-code"]
        [ viewFormula model
          , p []
              [ text "* alphawin code" ]
          , viewAlphaWin model
          , p []
              [ text "* alphalose code" ]
          , viewAlphaLose model
        ]

postGamsCode : Model -> Html Msg
postGamsCode model =
    div []
        [
          button [ onClick PostModel ] [ text "Post Model"]
        ]

jsonBody : String -> String -> Http.Body
jsonBody code game =
    Http.jsonBody <| Json.Encode.object [ ( "code", Json.Encode.string code ), ( "game", Json.Encode.string game ) ]

postJsonTask : String -> String -> Http.Request String
postJsonTask code game =
    Http.post
        ( "http://localhost:3000/gams" )
        ( jsonBody code game )
        ( tokenDecoder )
tokenDecoder : Json.Decode.Decoder String
tokenDecoder =
    Json.Decode.field "status" Json.Decode.string



--postJsonTask : String -> Task () ()
--postJsonTask str =
--    silenceTask
--        <| Http.send
--            Http.defaultSettings
--            { verb = "POST"
--            , headers =
--                [ ( "Content-Type", "application/json" )
--                , ( "Accept", "application/json" )
--                ]
--            , url = "http://localhost:5000/messages"
--            , body = Http.string (jsonBody str)
--            }

--silenceTask : Task x a -> Task () ()
--silenceTask task =
--  task
--  |> Task.map (\_-> ())
--  |> Task.mapError (\_ -> ())



view : Model -> Html Msg
view model =
    div []
        [ headerRow model
        , textarea [ class "game-input", rows 35, placeholder "Please input game", onInput Input ]
            [ text model.text ]
        , viewSize model
        , viewCoalisions model
        , h2 [] [ text "GAMS Code" ]
        , p []
            [ text "The following code can be used in "
            , a [ href "https://www.gams.com" ] [ text "GAMS" ]
            , text " to calculate the probability of a proposal to be accepted."
            ]
        , postGamsCode model
        --, viewGamsCode model
        ]


viewSize : Model -> Html Msg
viewSize model =
    div []
        [ text "QOBDD nodes: "
        , text (Maybe.withDefault "no size available" (Maybe.map (toString << QOBDD.size) model.qobdd))
        ]


viewCoalisions : Model -> Html Msg
viewCoalisions model =
    div []
        [ text "Coalisions: "
        , text (Maybe.withDefault "number of coalisions not available" (Maybe.map (toString << QOBDD.coalitions) model.qobdd))
        ]



-- viewBanzhaf : Model -> Html Msg
-- viewBanzhaf


viewFormula : Model -> Html Msg
viewFormula model =
    let
        resultToString ( stmts, vs ) =
            vs ++ "\n\n" ++ GAMS.prettyStmts stmts
    in
    div []
        [ pre [] [ text (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmt <| o) model.qobdd)) ] ]


viewAlphaWin : Model -> Html Msg
viewAlphaWin model =
    let
        resultToString ( stmts, vs ) =
            vs ++ "\n\n" ++ GAMS.prettyStmts stmts
    in
    div []
        [ pre [] [ text (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtAlphaWin <| o) model.qobdd)) ] ]

viewAlphaLose : Model -> Html Msg
viewAlphaLose model =
    let
        resultToString ( stmts, vs ) =
            vs ++ "\n\n" ++ GAMS.prettyStmts stmts
    in
    div []
        [ pre [] [ text (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtAlphaLose <| o) model.qobdd)) ] ]


viewWith : Model -> Html Msg
viewWith model =
    let
        resultToString ( stmts, vs ) =
            vs ++ "\n\n" ++ GAMS.prettyStmts stmts
    in
    div []
        [ pre [] [ text (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtWith <| o) model.qobdd)) ] ]

viewWithout : Model -> Html Msg
viewWithout model =
    let
        resultToString ( stmts, vs ) =
            vs ++ "\n\n" ++ GAMS.prettyStmts stmts
    in
    div []
        [ pre [] [ text (Maybe.withDefault "formula not available" (Maybe.map (\o -> resultToString <| GAMS.stmtWithout <| o) model.qobdd)) ] ]


viewProbs : List (List Float) -> Html Msg
viewProbs probs =
    div [] (List.indexedMap viewProbsRow probs)


viewProbsRow : Int -> List Float -> Html a
viewProbsRow i probs =
    div []
        (text ("Player " ++ toString i ++ ": ")
            :: [ text (String.concat (List.intersperse ", " (List.map viewProb probs))) ]
        )


viewProb : Float -> String
viewProb f =
    toString f


viewPowerList : Model -> Html Msg
viewPowerList model =
    Maybe.withDefault (text "no powers available")
        (Maybe.map (\q -> viewPowerListQOBDD q model.probs) model.qobdd)


viewPowerListQOBDD : QOBDD -> List (List Float) -> Html Msg
viewPowerListQOBDD qobdd probs =
    let
        probDicts =
            List.map (\ps -> Dict.fromList (List.indexedMap (\i p -> ( i, p )) ps)) probs

        ps =
            Probabilities.probs probDicts qobdd
    in
    div [ class "power-list" ] (List.indexedMap viewPower ps)


viewPower : Int -> Float -> Html Msg
viewPower player prob =
    div [] [ text ("Power of player " ++ toString player ++ ": " ++ toString prob) ]


viewResult : Maybe QOBDD -> (BDD -> a) -> Html Msg
viewResult mqobdd f =
    div [] [ text (Maybe.withDefault "no result" (Maybe.map (toString << f << .bdd) mqobdd)) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    parsedMWVG Parsed


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

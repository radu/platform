module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, method)
import Html.Events exposing (onClick)

import Json.Decode exposing (Decoder, int, string, list, nullable, field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

import Http

-- MAIN

main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, getGamesList )

gameDecoder : Decoder Game

gameDecoder =
    decode Game
    |> required "title" string
    |> required "description" string

gameListDecoder : Decoder JsonModel

gameListDecoder =
    decode JsonModel
        |> required "data" ( list gameDecoder )

-- MODEL

type alias JsonModel =
    { data: List Game }

type alias Model =
    { gamesList : List Game
    , displayGamesList : Bool
    , error: String
    }

type alias Game =
    { gameTitle: String
    , gameDescription: String
    }

emptyList = {}

gamesUrl = "/api/games"
logoutUrl = "/sessions/delete"


getGamesList : Cmd Msg
getGamesList =
    Http.send UpdateGamesList
    (Http.get gamesUrl gameListDecoder)

initialModel : Model
initialModel  =
    { gamesList = emptyGamesList , displayGamesList = False, error = ""
    }

emptyGamesList : List Game
emptyGamesList = [ ]

-- UPDATE

type Msg
    = DisplayGamesList
    | HideGamesList
    | FetchGamesList
    | UpdateGamesList (Result Http.Error JsonModel)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayGamesList ->
            ( { model | displayGamesList = True }, Cmd.none )

        HideGamesList ->
            ( { model | displayGamesList = False }, Cmd.none )

        FetchGamesList ->
            ( model, getGamesList )

        UpdateGamesList result ->
            case result of
                Ok gl ->
                    ({ model | gamesList = gl.data }, Cmd.none )

                Err error ->
                    ({ model | gamesList = emptyGamesList,  error = toString error }, Cmd.none )




-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
    div [] [
        a [ href logoutUrl , method "POST"] [text "logout"],
        div []
        [ button [ class "btn btn-success", onClick DisplayGamesList ] [ text "Display Games List" ]
        , button [ class "btn btn-danger", onClick HideGamesList  ] [ text "Hide Games List" ]
        , if model.displayGamesList then
            gamesIndex model
          else
            div [] []
        , div [] [ 
            text model.error
            ]
        ]
    ]

gamesIndex : Model -> Html msg
gamesIndex model =
    div [ class "games-index" ] [ gamesList model.gamesList ]

gamesList : List Game -> Html msg
gamesList listOfGames =
    ul [class "games-list"] (
        List.map
        gamesListItem
        listOfGames
        )

gamesListItem : Game -> Html msg
gamesListItem game =
    li [] 
    [ strong [] [ text (game.gameTitle ++ ": ") ]
    , span [] [ text game.gameDescription ]
    ]

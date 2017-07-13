module Game exposing (..)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div)
import Keyboard exposing (KeyCode, downs)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)

-- MAIN

main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type GameState = StartScreen | Playing | Success | GameOver

type alias Position = 
    { x: Int
    , y: Int
    }

type alias Size = 
    { w: Int
    , h: Int
    }

type alias ItemProp = Item ItemAttr

type alias Model =
    { gameState : GameState
    , character : ItemProp
    , items : List ItemProp
    , score : Int
    , itemsCollected : Int
    , timeRemaining : Int
    }

type  ImageType = SVG | IMG

type alias Item x =
    { image : String
    , imgType : ImageType
    , attr : x
    }

type alias ItemAttr =
    { pos  : Position
    , size : Size
    }

initialModel : Model

initialModel =
    { gameState = StartScreen
    , character = 
        { image = "/images/character.gif" 
        , imgType = IMG
        , attr = 
            { pos =
                { x = 50
                , y = 300
                }
            , size =
                { w = 50
                , h = 50
                }
            }
        }
        , items =
            [
                { image = "/images/coin.svg"
                , imgType = SVG
                , attr = 
                    { pos = 
                          { x  = 500
                          , y = 300
                          }
                    , size = 
                        { w = 20
                        , h = 20
                        }
                    }
                }
            ]
        , score = 0
        , itemsCollected = 0
        , timeRemaining = 10
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- UPDATE

type Msg 
    = KeyDown KeyCode
    | TimeUpdate Time
    | CountdownTimer Time
    | MoveItems Int
    | NoOp

type Direction
    = Up 
    | Down
    | Left
    | Right
    | Abs



itemsRequired = 10

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
      startingModel = 
        ( { model 
            | gameState = Playing
            , score = 0
            , itemsCollected = 0
            , timeRemaining = 10
            }
          , Cmd.none )
    in 
     case model.gameState of
       Playing ->
          case msg of

            KeyDown 37 ->
                ( { model | character = changeItem (changePos Left 10) model.character }, 
                Cmd.none )

            KeyDown 39 ->
                ( { model | character = changeItem (changePos Right 10) model.character }, 
                Cmd.none )

            TimeUpdate time ->
                if List.any ( characterFoundItem model) model.items then
                    ( { model 
                        | itemsCollected = model.itemsCollected + 1
                        , score = model.score + 100
                      } , Random.generate MoveItems (Random.int 50 500) )
                else if model.itemsCollected >= itemsRequired then
                    ( { model | gameState = Success }
                    , Cmd.none )
                else if model.timeRemaining <= 0 then
                    ( { model | gameState = GameOver }
                    , Cmd.none )
                else
                    ( model, Cmd.none )

            MoveItems amt ->
                ( { model | items = List.map ( changeItem (changePos Abs amt)) model.items  } ,
                  Cmd.none )

            CountdownTimer time ->
                ( { model | timeRemaining = model.timeRemaining - 1 }, Cmd.none )

            _ ->
                ( model, Cmd.none )

       _ -> 
        case msg of
            KeyDown 32 ->
                startingModel

            _ ->
                ( model, Cmd.none )

changeItem : ( ItemAttr -> ItemAttr ) -> ItemProp -> ItemProp
changeItem fun item =
    { item | attr = fun item.attr }

changePos : Direction -> Int -> ItemAttr -> ItemAttr
changePos dir amt attr =
    let 
        p = attr.pos

    in
        case dir of
            Left ->
                { attr | pos = { p | x = p.x - amt }}
            Right ->
                { attr | pos = { p | x = p.x + amt }}
            Abs ->
                { attr | pos = { p | x = amt }}
            _ ->
                attr

positionsClose : Int -> ItemProp -> ItemProp -> Bool
positionsClose bound a b =
    let
        pos = a.attr.pos
        xrange = List.range (pos.x + 20 - bound ) (pos.x + 20)
        yrange = List.range (pos.y - bound ) (pos.y + bound)

    in
        List.member b.attr.pos.x xrange &&
        List.member b.attr.pos.y yrange

characterFoundItem : Model -> ItemProp -> Bool
characterFoundItem model item =
    let
      bound = 10
    in
      positionsClose bound model.character item


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ downs KeyDown 
    , diffs TimeUpdate
    , every second CountdownTimer
    ]


-- VIEW

view : Model -> Html Msg
view model =
    div [] [ viewGame model ]


viewGameState : Model -> List (Svg Msg)
viewGameState model =
  let 
    commonItems  =
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewCharacter model
            ] ++
            ( viewItems model )
  in
    case model.gameState of
        StartScreen ->
            commonItems
            ++
            [ viewStartScreenText
            ]

        Playing ->
            commonItems
            ++
            [ viewGameScore model
            , viewItemsCollected model 
            , viewGameTime model ]

        Success ->
            commonItems
            ++
            [ viewSuccessScreenText
            ]

        GameOver ->
            commonItems
            ++
            [ viewGameOverScreenText
            ]

viewGame : Model -> Svg Msg
viewGame model =
    svg [ version "1.1", width "600", height "400" ]
        ( viewGameState model )


viewGameWindow : Svg Msg
viewGameWindow =
    rect [ width "600"
         , height "400"
         , fill "none"
         , stroke "black"
         ]
         []

viewGameSky : Svg Msg
viewGameSky =
    rect [ x "0"
         , y "0"
         , width "600"
         , height "300"
         , fill "#4b7cfb"
         ]
         []

viewGameGround : Svg Msg
viewGameGround =
    rect [ x "0"
         , y "300"
         , width "600"
         , height "100"
         , fill "green"
         ]
         []

viewCharacter : Model -> Svg Msg
viewCharacter model =
    viewItem model.character

showItemImage : String -> Position -> Size -> Svg Msg
showItemImage loc pos size =
    image [ xlinkHref loc
          , x ( toString pos.x )
          , y ( toString pos.y )
          , width ( toString size.w )
          , height ( toString size.h )
          ]
          []

viewItems : Model -> List (Svg Msg)
viewItems model =
    List.map (maybeViewItem model) model.items


maybeViewItem : Model -> ItemProp -> Svg Msg
maybeViewItem model item =
    if characterFoundItem model item then
        svg [] []
    else
        viewItem item

viewItem : ItemProp -> Svg Msg
viewItem item =
    case item.imgType of
        SVG -> showItemImage item.image item.attr.pos item.attr.size
        IMG -> showItemImage item.image item.attr.pos item.attr.size

viewGameText : Int -> Int -> String -> Svg Msg
viewGameText posX posY str =
    Svg.text_
        [ x (toString posX)
        , y (toString posY)
        , fontFamily "Courier"
        , fontWeight "bold"
        , fontSize "16"
        ]
        [ Svg.text str ]

viewGameScore : Model -> Svg Msg
viewGameScore model =
    let
      currentScore =
          model.score
          |> toString
          |> String.padLeft 5 '0'
    in
      Svg.svg []
        [ viewGameText 25 25 "SCORE"
        , viewGameText 25 40 currentScore 
        ]

viewItemsCollected : Model -> Svg Msg
viewItemsCollected model =
    let 
      currentItemCount =
          model.itemsCollected 
              |> toString
              |> String.padLeft 3 '0'

    in
      Svg.svg []
        [ image 
            [ xlinkHref "/images/coin.svg"
            , x "275"
            , y "18"
            , width "15"
            , height "15"
            ]
            []
        , viewGameText 300 30 ("x " ++ currentItemCount )
        ]

viewGameTime : Model -> Svg Msg
viewGameTime model =
    let 
      currentTime =
          model.timeRemaining 
             |> toString
             |> String.padLeft 4 '0'
    in
      Svg.svg []
         [ viewGameText 525 25 "TIME"
         , viewGameText 525 40 currentTime 
         ]

viewStartScreenText : Svg Msg
viewStartScreenText =
    Svg.svg []
        [ viewGameText 140 160 "Collect ten coins in ten seconds!"
        , viewGameText 140 180 "Press the SPACE BAR key to start."
        ]

viewSuccessScreenText : Svg Msg
viewSuccessScreenText =
    Svg.svg []
     [ viewGameText 120 160 "Success!" 
     , viewGameText 140 180 "Press the SPACE BAR key to restart."
     ]

viewGameOverScreenText : Svg Msg
viewGameOverScreenText =
    Svg.svg []
     [ viewGameText 120 160 "Game Over :("
     , viewGameText 140 180 "Press the SPACE BAR key to restart."
     ]

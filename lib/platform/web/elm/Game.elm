module Game exposing (..)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div)
import Keyboard exposing (KeyCode, downs, ups)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket

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


type alias Model =
    { gameState : GameState
    , character : Character
    , items : List Item
    , score : Int
    , phxSocket: Phoenix.Socket.Socket Msg
    , itemsCollected : Int
    , timeRemaining : Int
    }

type ImageType = SVG | IMG

type alias Image  =
    { url : String
    , imgType  : ImageType
    }

type alias ImageSingle a = 
    { a | img: Image
    }

type alias ImageLR a =
    { a | imgleft : Image
        , imgright: Image
        , facing  : FacingDirection 
    }

type alias PositionSize a =
    { a | pos  : Position
        , size : Size
    }

type alias Velocity a = 
    { a | velocity : Float
    }

type alias CharacterAttr =
    {  }

type alias ItemAttr = 
    {  }

type alias Item = PositionSize ( ImageSingle ItemAttr )
type alias Character = PositionSize ( ImageLR ( Velocity CharacterAttr ))


initialModel : Model

initialModel =
    { gameState = StartScreen
    , character = 
        { imgleft = 
            { url = "/images/character-l.gif"
            , imgType = IMG }
        , imgright =
            { url = "/images/character-r.gif"
            , imgType = IMG }
        , facing = FaceRight
        , pos =
            { x = 50
            , y = 300
            }
        , size =
            { w = 50
            , h = 50
            }
        , velocity = 0
        }
        , items =
            [
                {
                    img =
                        { url = "/images/coin.svg"
                        , imgType = SVG }
                    , pos = 
                          { x  = 500
                          , y = 300
                          }
                    , size = 
                        { w = 20
                        , h = 20
                        }
                }
            ]
        , score = 0
        , itemsCollected = 0
        , timeRemaining = 10
        , phxSocket = initPhxSocket
    }

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket = Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
                    |> Phoenix.Socket.withDebug


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- UPDATE

type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | TimeUpdate Time
    | CountdownTimer Time
    | PhoenixMsg  (Phoenix.Socket.Msg Msg)
    | MoveCharacter Time
    | MoveItems Int
    | ChangeDirection Time
    | NoOp

type Direction
    = Up
    | Down
    | Left
    | Right
    | Abs

type FacingDirection
    = FaceLeft
    | FaceRight

itemsRequired = 10

setCharacterVel v m =
    { m | character = changeVel v m.character }

timeUpdatePos : PositionSize (Velocity x) -> Float -> PositionSize (Velocity x)
timeUpdatePos p time =
    let 
        v = p.velocity
        dx = round (v * time)
    in
      changePos Right dx p

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
            KeyUp 37 ->
                ( ( model |> setCharacterVel 0)
                   , Cmd.none )

            KeyUp 39 ->
                ( ( model |> setCharacterVel 0)
                   , Cmd.none )


            KeyDown 37 ->
                ( ( model |> setCharacterVel -0.25)
                   , Cmd.none )

            KeyDown 39 ->
                ( ( model |> setCharacterVel 0.25)
                   , Cmd.none )


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
                ( { model | items = List.map ( changePos Abs amt ) model.items  } ,
                  Cmd.none )

            CountdownTimer time ->
                ( { model | timeRemaining = model.timeRemaining - 1 }, Cmd.none )

            PhoenixMsg msg ->
                let 
                    ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
                in
                    ( { model | phxSocket = phxSocket } , Cmd.map PhoenixMsg phxCmd  )


            MoveCharacter time ->
                ( { model | character = timeUpdatePos model.character time } 
                , Cmd.none )

            ChangeDirection time ->
                let 
                    m = if model.character.velocity > 0 then
                            { model | character = changeDirection FaceRight model.character }
                        else if model.character.velocity < 0 then
                            { model | character = changeDirection FaceLeft model.character }
                        else
                            model
                in
                    ( m, Cmd.none )

            _ ->
                ( model, Cmd.none )


       _ -> 
        case msg of
            KeyDown 32 ->
                startingModel

            _ ->
                ( model, Cmd.none )

changeDirection dir it =
    { it | facing = dir }

changeVel vel it =
    { it | velocity = vel }

changePos dir amt it  =
    let 
        oldpos = it.pos
        newpos = case dir of
            Left ->
                { oldpos | x = oldpos.x - amt }
            Right ->
                { oldpos | x = oldpos.x + amt }
            Abs ->
                { oldpos | x = amt }
            _ ->
                oldpos
    in
        { it | pos = newpos }

positionsClose : Int -> Character -> Item -> Bool
positionsClose bound chr it =
    let
        pos = chr.pos
        (xRangeLo , xRangeHi) = (pos.x + 20 - bound , pos.x + 20)
        (yRangeLo , yRangeHi) = (pos.y - bound , pos.y + bound)
    in
        (it.pos.x >= xRangeLo) && (it.pos.x <= xRangeHi) &&
        (it.pos.y >= yRangeLo) && (it.pos.y <= yRangeHi)

characterFoundItem : Model -> Item -> Bool
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
    , ups KeyUp
    , diffs MoveCharacter
    , diffs ChangeDirection
    , diffs TimeUpdate
    , every second CountdownTimer
    , Phoenix.Socket.listen model.phxSocket PhoenixMsg
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
            , viewCharacter model.character
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


facingImage : ImageLR c -> Image
facingImage it =
    case it.facing of
        FaceLeft -> it.imgleft
        FaceRight -> it.imgright

viewCharacter : Character -> Svg Msg
viewCharacter chr =
    let 
        img = facingImage chr
    in
        showItemImage img chr.pos chr.size


viewItem : Item -> Svg Msg
viewItem it = 
    showItemImage it.img it.pos it.size

showItemImage : Image -> Position -> Size -> Svg Msg
showItemImage img pos size =
    image [ xlinkHref img.url
          , x ( toString pos.x )
          , y ( toString pos.y )
          , width ( toString size.w )
          , height ( toString size.h )
          ]
          []

viewItems : Model -> List (Svg Msg)
viewItems model =
    List.map (maybeViewItem model) model.items


maybeViewItem : Model -> Item -> Svg Msg
maybeViewItem model item =
    if characterFoundItem model item then
        svg [] []
    else
        viewItem item


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

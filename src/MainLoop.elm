module Main exposing (Msg(..), fromCode, init, initialModel, main, matchToSide, movepc, pcState, subscriptions, update, view)

--import Signal exposing (Signal, foldp, map, map2, merge)
--import Graphics.Element exposing (Element, image)
--import Element exposing (Element, image)

import Browser
import Browser.Events
    exposing
        ( onKeyDown
        )
import Collage exposing (..)
import Collage.Render exposing (svg)
import Debug exposing (log)
import GameModel exposing (..)
import Html exposing (Html)
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)



--import Keyboard
--import Window


pcState : Character
pcState =
    { x = 0, y = 0, dir = Right }



-- tiredness strenght blabla


initialModel : Model
initialModel =
    { grid = mainGrid
    , gridSide = gridWidth
    , pc = pcState
    }



-- Msg


type Msg
    = Noop
    | KeyPress Direction



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        KeyPress dir ->
            ( model
                |> movepc dir
            , Cmd.none
            )



-- if into monster slash


movepc : Direction -> Model -> Model
movepc dir model =
    let
        checkPc default pc =
            let
                x =
                    pc.x |> Debug.log "pc x"

                y =
                    pc.y |> Debug.log "pc y"

                idx =
                    getTileIdxFromPosition ( pc.x, pc.y ) |> Debug.log "idx"

                tile =
                    getListIdx idx model.grid |> Debug.log "tile"
            in
            case tile of
                Nothing ->
                    pc

                Just tilet ->
                    if tilet == BackGround Floor then
                        pc

                    else
                        default

        updatePc pc dir_ =
            case dir_ of
                Up ->
                    { pc | y = pc.y + 1, dir = Up }

                Down ->
                    { pc | y = pc.y - 1, dir = Down }

                Left ->
                    { pc | x = pc.x - 1, dir = Left }

                Right ->
                    { pc | x = pc.x + 1, dir = Right }

                None ->
                    pc
    in
    { model | pc = checkPc model.pc (updatePc model.pc dir) }



-- on which tile it ends up
-- which other tiles become visible
-- which mosters are in range to attack
-- or be attacked
-- update monsters paths and spawn them
-- update time ticking
-- what is the center (PC)
-- VIEW


matchToSide : ( Int, Int ) -> Int -> ( Int, Int )
matchToSide frame side =
    let
        ( w, h ) =
            frame

        tW =
            w // side

        tH =
            h // side
    in
    ( log "tW" tW, log "tH" tH )


view : Model -> Html Msg
view model =
    let
        tileSide =
            64

        frame =
            ( 600, 600 )

        dir =
            case model.pc.dir of
                Left ->
                    "left"

                Right ->
                    "right"

                Up ->
                    "up"

                Down ->
                    "down"

                _ ->
                    "none"

        src =
            "img/pc/" ++ dir ++ ".png"

        -- Hardcoded
        pcImage =
            image ( tileSide, tileSide ) src

        ( tW, tH ) =
            matchToSide (log "win" frame) tileSide

        tWSide =
            tW * tileSide

        tHSide =
            tH * tileSide

        pcPos =
            ( model.pc.x * tileSide, tileSide * model.pc.y )

        {- }
           theelement =
               collage tWSide
                   tHSide
                   (displayGrid ( tW, tH ) pcPos mainGrid
                       ++ [ pcImage
                               |> toForm
                               |> Debug.log "pc"
                               |> move pcPos
                          ]
                   )
        -}
        theelement =
            group <|
                displayGrid ( tW, tH ) pcPos mainGrid
    in
    theelement
        |> svg



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    --Keyboard.presses (\code ->  (Char.fromCode code))
    Sub.batch
        [ --Keyboard.downs (\kcode -> KeyPress (fromCode kcode))
          onKeyDown (Decode.map (\kCode -> KeyPress (fromCode kCode)) keyCode)

        --, Keyboard.ups (\kcode -> KeyUpMsg (fromCode kcode))
        --Keyboard.presses (\kcode -> KeyPress (fromCode kcode))
        --, Time.every (msPerFrame model * Time.millisecond) StepNoKey
        ]


fromCode : Int -> Direction
fromCode keyCode =
    case keyCode of
        79 ->
            Left

        37 ->
            Left

        80 ->
            Right

        39 ->
            Right

        81 ->
            Up

        38 ->
            Up

        40 ->
            Down

        65 ->
            Down

        _ ->
            None



-- SIGNALS
--main : Signal Element
--main = lift2 display Window.dimensions (foldp step mario input)


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SIGNALS
--main : Signal Element
--main =
--    map2 view Window.dimensions (foldp update model input)
-- INPUTS: CONTROLS
{-
   inputDir : Signal Direction
   inputDir =
       let
           dir { x, y } =
               case ( x, y ) of
                   ( 0, 1 ) ->
                       Up

                   ( 0, -1 ) ->
                       Down

                   ( 1, 0 ) ->
                       Right

                   ( -1, 0 ) ->
                       Left

                   _ ->
                       None
       in
       merge (Signal.map dir Keyboard.arrows) (Signal.map dir Keyboard.wasd)



   -- samples arrows when fps tick


   input : Signal Direction
   input =
       map (Debug.watch "direction") inputDir


-}

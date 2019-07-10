module Main exposing (main)

import Browser
import Browser.Events
    exposing
        ( onKeyDown
        )
import Collage exposing (..)
import Collage.Render exposing (svg)
import GameModel exposing (..)
import Html exposing (Html)
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)


pcState : Character
pcState =
    { x = 7
    , y = 7
    , dir = Right
    }


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


movepc : Direction -> Model -> Model
movepc dir model =
    let
        checkPc default pc =
            let
                x =
                    pc.x

                y =
                    pc.y

                idx =
                    getTileIdxFromPosition ( pc.x, pc.y )

                tile =
                    getListIdx idx model.grid
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
                    { pc | y = pc.y - 1, dir = Up }

                Down ->
                    { pc | y = pc.y + 1, dir = Down }

                Left ->
                    { pc | x = pc.x - 1, dir = Left }

                Right ->
                    { pc | x = pc.x + 1, dir = Right }

                None ->
                    pc
    in
    { model | pc = checkPc model.pc (updatePc model.pc dir) }



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
    ( tW, tH )


getAdjustedSize : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
getAdjustedSize frame gSize =
    let
        ( w, h ) =
            frame

        ( gW, gH ) =
            gSize
    in
    ( toFloat w / toFloat gW, toFloat h / toFloat gH )


view : Model -> Html Msg
view model =
    let
        tileSide =
            64

        frame =
            ( 960, 960 )

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

        ( tW, tH ) =
            --matchToSide (log "win" frame) tileSide
            matchToSide frame tileSide

        tWSide =
            --tW * tileSide
            getAdjustedSize frame ( gridSize, gridSize )
                |> Tuple.first

        tHSide =
            --tH * tileSide
            getAdjustedSize frame ( gridSize, gridSize )
                |> Tuple.second

        xScaleFactor =
            tWSide / tileSide

        yScaleFactor =
            tHSide / tileSide

        src =
            "img/pc/" ++ dir ++ ".png"

        pcImage =
            image ( tileSide, tileSide ) src
                |> scaleX xScaleFactor
                |> scaleY yScaleFactor

        pos_x =
            (model.pc.x - toFloat (gridSize // 2))
                |> (\arg -> tWSide * arg)

        pos_y =
            (model.pc.y - toFloat (gridSize // 2))
                |> (\arg -> tHSide * arg * -1)

        pcPos =
            ( pos_x, pos_y )
    in
    --theelement =
    Collage.group
        ([ pcImage |> shift pcPos ]
            ++ displayGrid ( tWSide, tHSide ) pcPos mainGrid
        )
        |> svg



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Keyboard.downs (\kcode -> KeyPress (fromCode kcode))
          onKeyDown (Decode.map (\kCode -> KeyPress (fromCode kCode)) keyCode)
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

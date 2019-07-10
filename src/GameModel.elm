module GameModel exposing
    ( Action(..)
    , BackGroundTile(..)
    , Character
    , Condition(..)
    , Direction(..)
    , Grid
    , Model
    , Progress(..)
    , ShadowTile(..)
    , Size(..)
    , Tile(..)
    , WallJunction(..)
    , WallTile
    , checkBgImg
    , checkWallImg
    , displayGrid
    , displayTile
    , displayTileAtCoordinates
    , displayTileAtIndex
    , eefe
    , eeff
    , efee
    , efef
    , effe
    , efff
    , feee
    , feef
    , fefe
    , ffee
    , ffef
    , fffe
    , ffff
    , getListIdx
    , getTileIdxFromPosition
    , gridSize
    , gridWidth
    , mainGrid
    , tileSize
    )

import Collage exposing (..)
import List exposing (concat, drop, head, indexedMap, map)


type Direction
    = Left
    | Right
    | Up
    | Down
    | None


type Action
    = NoOp
    | Move Direction


type alias Character =
    { x : Float
    , y : Float
    , dir : Direction
    }


type BackGroundTile
    = Floor
    | WallOver WallTile
    | Wall
    | Water


type ShadowTile
    = Main


type Tile
    = Door Size
    | Chest Size
    | Skull
    | Key
    | Money
    | Box
    | Lever
    | Flag Condition
    | Column
    | BackGround BackGroundTile
    | Shadow ShadowTile


type Size
    = Small
    | Big


type Condition
    = Ruined
    | Fine


type WallJunction
    = Flat
    | Empty


type alias WallTile =
    { r : WallJunction, l : WallJunction, u : WallJunction, d : WallJunction }


type Progress
    = InProgress
    | GameOver
    | Won


type alias Grid =
    List Tile


type alias Model =
    { grid : Grid
    , gridSide : Float
    , pc : Character
    }


ffff : WallTile
ffff =
    { r = Flat, u = Flat, l = Flat, d = Flat }


efef : WallTile
efef =
    { r = Empty, u = Flat, l = Empty, d = Flat }


feef : WallTile
feef =
    { r = Flat, u = Empty, l = Empty, d = Flat }


eeff : WallTile
eeff =
    { r = Empty, u = Empty, l = Flat, d = Flat }


fefe : WallTile
fefe =
    { r = Flat, u = Empty, l = Flat, d = Empty }


ffee : WallTile
ffee =
    { r = Flat, u = Flat, l = Empty, d = Empty }


effe : WallTile
effe =
    { r = Empty, u = Flat, l = Flat, d = Empty }


feee : WallTile
feee =
    { r = Flat, u = Empty, l = Empty, d = Empty }


eefe : WallTile
eefe =
    { r = Empty, u = Empty, l = Flat, d = Empty }


efff : WallTile
efff =
    { r = Empty, u = Flat, l = Flat, d = Flat }


ffef : WallTile
ffef =
    { r = Flat, u = Flat, l = Empty, d = Flat }


efee : WallTile
efee =
    { r = Empty, u = Flat, l = Empty, d = Empty }


fffe : WallTile
fffe =
    { r = Flat, u = Flat, l = Flat, d = Empty }


mainGrid : Grid
mainGrid =
    [ --first line
      BackGround (WallOver ffee)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver effe)
    , -- second line
      BackGround (WallOver fefe)
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround (WallOver fefe)
    , -- third line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- fourth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fffe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- fifth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- sixth line
      BackGround (WallOver feee)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efee)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver eeff)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver ffef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver eefe)
    , -- seventh line
      BackGround (WallOver fefe)
    , BackGround Wall
    , BackGround Wall
    , BackGround (WallOver fefe)
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround (WallOver fefe)
    , -- eighth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- nineth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- tenth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver feef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efff)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- eleventh line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- twelfth line
      BackGround (WallOver fefe)
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround Floor
    , BackGround (WallOver fefe)
    , -- thirteenth line
      BackGround (WallOver feef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver efef)
    , BackGround (WallOver eeff)
    , -- fourteenth line
      BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , BackGround Wall
    , --last line
      BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    , BackGround Water
    ]


gridSize =
    15


tileSize =
    64


getListIdx : Int -> Grid -> Maybe Tile
getListIdx idx list =
    head (drop idx list)



{----------------------------------------------------
                    Tile Functions
-----------------------------------------------------}


checkWallImg : WallTile -> String
checkWallImg walltype =
    let
        getsrc side =
            case side of
                Flat ->
                    "flat"

                Empty ->
                    "empty"

        r =
            getsrc walltype.r

        l =
            getsrc walltype.l

        u =
            getsrc walltype.u

        d =
            getsrc walltype.d
    in
    "img/walls/" ++ r ++ "-" ++ u ++ "-" ++ l ++ "-" ++ d ++ ".png"


checkBgImg : BackGroundTile -> String
checkBgImg bgtype =
    case bgtype of
        Floor ->
            "img/floor/floor_01.png"

        Wall ->
            "img/walls/wall.png"

        Water ->
            "img/water/water_01.png"

        WallOver tile ->
            checkWallImg tile


displayTile : Tile -> Collage msg
displayTile tile =
    let
        src =
            case tile of
                BackGround tiletype ->
                    checkBgImg tiletype

                _ ->
                    ""
    in
    image ( tileSize, tileSize ) src


displayTileAtCoordinates : ( Tile, Int, Int ) -> Collage msg
displayTileAtCoordinates ( t, x, y ) =
    let
        pos_x =
            (x - (gridSize // 2))
                |> (\arg -> arg * tileSize)
                |> toFloat

        pos_y =
            (y - (gridSize // 2))
                |> (\arg -> -1 * arg * tileSize)
                |> toFloat

        position =
            ( pos_x, pos_y )
    in
    displayTile t
        |> shift position


displayTileAtIndex : Int -> Tile -> Collage msg
displayTileAtIndex index tile =
    let
        y =
            index // gridSize

        x =
            Basics.remainderBy gridSize index
    in
    displayTileAtCoordinates ( tile, x, y )



{--| the width of the entire game grid
--}


gridWidth : Float
gridWidth =
    toFloat gridSize * toFloat tileSize


getTileIdxFromPosition : ( Float, Float ) -> Int
getTileIdxFromPosition ( x, y ) =
    let
        x_tile =
            round x

        y_tile =
            round y
    in
    y_tile * gridSize + x_tile



-- display a grid


displayGrid : ( Float, Float ) -> ( Float, Float ) -> Grid -> List (Collage msg)
displayGrid adjustedSize pcCoords g =
    let
        tiles =
            indexedMap displayTileAtIndex g
    in
    tiles

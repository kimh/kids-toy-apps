port module Main exposing (..)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Time exposing (Time)
import Mouse
import AnimationFrame
import Html exposing (Html, button, div, img)
import Animation exposing (..)
import Debug exposing (log)
import Task

type alias Model =
    { x : Animation, y : Animation, clock : Time }

port play : Float -> Cmd msg

model0 =
    Model
        (animation 0 |> from 500 |> to 0 |> duration Time.second)
        (animation 0 |> from 0 |> to -500 |> duration Time.second)
        0

type Msg
    = Tick Time
    | MouseMsg Mouse.Position
    | Play

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick dt ->
            let
                clock = model.clock + dt
            in
                ({ model | clock = clock }, Cmd.none)

        MouseMsg position ->
           let
               -- We need this so that plan's center moves to the new postion
               adjustment = -150
               posx = position.x
                      |> toFloat
                      |> (+) adjustment
               posy = position.y
                      |> toFloat
                      |> (+) adjustment
                      |> (*) -1
               newX = retarget model.clock posx model.x
               newY = retarget model.clock posy model.y
           in
               ({model | x = newX, y = newY }, send Play)
        Play ->
            (model, play 1)

view : Model -> Html Msg
view { x, y, clock } =

    let
        pos =
            ( animate clock x, animate clock y )

        plane = image (500, 500) "images/airplane.svg"
        circle =
            plane |> Collage.shift pos
    in
        group [
             spacer 300 300,
             circle
             ]
            -- |> Collage.Layout.debug
            |> svg

subs : Sub Msg
subs =
    Sub.batch
        [
        AnimationFrame.diffs Tick,
        Mouse.clicks MouseMsg
        ]

main =
    Html.program
        { init = ( model0, Cmd.none )
        , update = update
        , subscriptions = always subs
        , view = view
        }

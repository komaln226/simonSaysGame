import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Html exposing (img)
import Html.Attributes exposing (src)

myShapes model =[html 1460 821 (
      Html.img 
        [Html.Attributes.src "https://cdn.discordapp.com/attachments/813197012136755216/950971082561703956/unknown.png"]
        []
      )|> scale 0.228 |> move (-100, 58), simon model, says model]

simon model =group[rect 65 75
            |> filled darkGray
            |> move (-41, -1)
        , square 60
            |> filled black
            |> move (-41, -7)
        , let
            colour = case model.state of
                    Start _ _ -> green
                    _ -> blank
            pos =
                case model.state of
                    Start _ n ->
                        case listPick (model.round - n) model.pattern of
                            1 ->(-60, 15)
                            2 ->(-40, 15)
                            3 ->(-20, 15)
                            4 ->(-63, -5)
                            5 ->(-40, -5)
                            6 ->(-20, -5)
                            7 ->(-63, -29)
                            8 ->(-41, -29)
                            9 ->(-20, -29)
                            _ ->(0, 0)
                    _ ->(0, 0)
          in
          square 17
            |> filled colour
            |> move pos
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (-22, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (-32, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (-42, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (-52, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (-62, 30)
        , List.map (\x -> circle 3 |> filled green |> move (-62 + 10 * toFloat x, 30)) (List.range 0 model.round)
            |> group]

says model =group[rect 65 75
            |> filled darkGray
            |> move (44, -1)
        , let
            colour = case model.state of
                    Waiting -> grey
                    Correct _ _ -> gray
                    Done -> green
                    Wrong _ -> red
                    _ -> charcoal
          in group[square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (63, -27)
                |> notifyTap (Click 9)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (43, -27)
                |> notifyTap (Click 8)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (23, -27)
                |> notifyTap (Click 7)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (63, -7)
                |> notifyTap (Click 6)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (43, -7)
                |> notifyTap (Click 5)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (23, -7)
                |> notifyTap (Click 4)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (63, 13)
                |> notifyTap (Click 3)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (43, 13)
                |> notifyTap (Click 2)
            , square 17
                |> filled colour
                |> addOutline (solid 0.75) black
                |> move (23, 13)
                |> notifyTap (Click 1)
            ]
        , let
            colour =
                case model.state of
                    Correct _ _ -> green
                    _ -> blank
            pos =
                case model.state of
                    Correct _ n -> case n of
                      1 ->(23, 13)
                      2 ->(43, 13)
                      3 ->(63, 13)
                      4 ->(23, -7)
                      5 ->(43, -7)
                      6 ->(63, -7)
                      7 ->(23, -27)
                      8 ->(43, -27)
                      9 ->(63, -27)
                      _ ->(0, 0)
                    _ ->(0, 0)
          in
          square 17
            |> filled colour
            |> move pos
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (22, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (32, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (42, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (52, 30)
        , circle 3
            |> filled charcoal
            |> addOutline (solid 1) black
            |> move (62, 30)
        , let
            colour = case model.state of
              Wrong _ -> red
              _ ->green
          in
          List.map (\x -> circle 3 |> filled colour |> move (22 + 10 * toFloat x, 30)) (List.range 0 (model.light - 1))
            |> group]


button str msgToSend coord col = buttonBase str
        |> scale 0.75
        |> move coord
        |> notifyTap msgToSend

buttonBase str =group[roundedRect 40 20 5
            |> filled red
        , text str
            |> centered
            |> size 8
            |> filled red
            |> move ( 0, -3 )]

listPick i lst = case (i,lst) of 
  (0, x :: _) -> x
  (1, _ :: x :: _ ) -> x
  (2, _ :: _ :: x :: _ ) -> x
  (3, _ :: _ :: _ :: x :: _ ) -> x
  (4, _ :: _ :: _ :: _ :: x :: _ ) -> x
  (_, _) -> -1

type Msg = Tick Float GetKeyState
    | Click Int

type State = Start Float Int
    | Waiting
    | Correct Float Int
    | Wrong Float
    | Done

update msg model =
    case msg of
        Tick t _ ->
            let lastTime = if model.time > 0 then model.time else t
            in case model.state of
                Start tLeft round ->
                    let
                        newModel =
                            if tLeft < t - lastTime && round == 0 then
                                {model | time = t, state = Waiting}
                            else if tLeft < t - lastTime then
                                {model | time = t, state = Start 0.5 (round - 1)}
                            else
                                {model | time = t, state = Start (tLeft - (t - lastTime)) round}
                    in newModel

                Waiting -> {model | time = t}

                Correct tLeft pick ->
                    let
                        newModel =
                            if tLeft < t - lastTime && model.round == 4 && model.counter == 4 then
                                {model | time = t, state = Done}
                            else if tLeft < t - lastTime && model.counter == model.round then
                                {model | time = t, state = Start 0.5 (model.round + 1), round = model.round + 1, counter = 0, light = 0}
                            else if tLeft < t - lastTime then
                                {model | time = t, state = Waiting, counter = model.counter + 1}
                            else
                                {model | time = t, state = Correct (tLeft - (t - lastTime)) pick}
                    in newModel

                Wrong tLeft ->
                    if tLeft < (t - lastTime) then
                        {model | time = t, state = Start 0.5 0, round = 0, counter = 0, light = 0}
                    else
                        {model | time = t, state = Wrong (tLeft - (t - lastTime))}

                Done -> model

        Click pick ->
            case model.state of
                Waiting -> if pick == listPick model.counter model.pattern then
                        {model | state = Correct 0.1 pick, light = model.light + 1}
                    else
                        {model | state = Wrong 1, light = 5}
                _ -> model

type alias Model = {state : State
    , time : Float
    , round : Int
    , counter : Int
    , pattern : List Int
    , light : Int
    }

init = {state = Start 0.5 0
    , time = 0
    , round = 0
    , counter = 0
    , pattern = [4, 9, 6, 3, 7]
    , light = 0
    }

main = gameApp Tick {model = init, view = view, update = update, title = "Password Sequence"}

view model = collage 192 128 (myShapes model)

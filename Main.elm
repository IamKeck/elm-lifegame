module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Array exposing(Array, toList, repeat, get, set, filter, length, indexedMap)
import List
import Debug
import Time

main : Program Never Model Msg
main =
    program
        { init = initial_model
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


--model
type Status = Alive | Dead
size = 50
type alias World = Array (Array Status)
type alias Model = {world: World, auto: Bool}
initial_model : (Model, Cmd Msg)
initial_model =
    ({world = repeat size Dead |> repeat size, auto = False}, Cmd.none)

type Msg = CellClicked Int Int | Next | AutoClicked

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CellClicked x y ->
            ({model | world = clickCell x y model.world}, Cmd.none)
        Next ->
            ({model | world = nextWorld model.world}, Cmd.none)
        AutoClicked ->
            ({model | auto = not model.auto,
            world = if model.auto then model.world else nextWorld model.world}, Cmd.none)

clickCell : Int -> Int -> World -> World
clickCell x y model =
    let
        row = get x model
    in
        case row of
            Nothing -> model
            Just r ->
                let
                    cell = get y r
                in
                    case cell of
                        Nothing -> model
                        Just s ->
                           let
                               new_state = if s == Alive then Dead else Alive
                               new_row = set y new_state r
                           in
                               set x new_row model


getCellState : Int -> Int -> World -> Maybe Status
getCellState x y model =
    let
        row = get x model
    in
        case row of
            Nothing -> Nothing
            Just r -> get y r


countLiving : Int -> Int -> World -> Int
countLiving x y m =
    let cells = [ (x - 1, y)
                , (x + 1, y)
                , (x, y - 1)
                , (x, y + 1)
                , (x + 1, y + 1)
                , (x - 1, y - 1)
                , (x + 1, y - 1)
                , (x - 1, y + 1) ]
     in List.filter (\(x, y) -> getCellState x y m == Just Alive) cells |> List.length

nextCellState : World -> Int -> Int -> Status -> Status
nextCellState m x y s =
    let
        living_cells = countLiving x y m
    in
        case s of
            Dead -> if living_cells == 3 then Alive else Dead
            Alive ->
                case living_cells of
                    2 -> Alive
                    3 -> Alive
                    _ -> Dead

nextRow : World -> Int -> Array Status -> Array Status
nextRow m x ss =
    indexedMap (nextCellState m x) ss

nextWorld : World -> World
nextWorld m =
    indexedMap (nextRow m) m


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.auto == True then Time.every Time.second (\_ -> Next) else Sub.none

statusToCell : Int -> Int -> Status -> Html Msg
statusToCell x y s =
    td [ classList [("living-cell", s == Alive)]
       , onClick <| CellClicked x y
     ] []

rowToHtml : Int -> Array Status -> Html Msg
rowToHtml x sx = toList sx |> List.indexedMap (statusToCell x) |> tr []

view : Model -> Html Msg
view m =
    let
        world = toList m.world |> List.indexedMap rowToHtml |>
            table [ classList [("is-moving", m.auto)]]
        next_button = button [onClick Next] [text "次世代"]
        auto_text = if m.auto then "自動停止" else "自動"
        auto_button = button [onClick AutoClicked] [text auto_text]
    in
        div [] [world, next_button, auto_button]






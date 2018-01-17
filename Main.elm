module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, toList, repeat, get, set, filter, length, indexedMap)
import Maybe exposing (andThen)
import List
import Debug
import Time
import Random


main : Program Never Model Msg
main =
    program
        { init = initial_model
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--model


type Status
    = Alive
    | Dead


size =
    50


type alias World =
    Array (Array Status)


type alias Model =
    { world : World, auto : Bool, random_mode : Bool }


initial_model : ( Model, Cmd Msg )
initial_model =
    ( { world = repeat size Dead |> repeat size, auto = False, random_mode = False }, Cmd.none )


type Msg
    = CellClicked Int Int
    | Next
    | AutoClicked
    | RandomClicked
    | GenerateRandom
    | RandomGenerated ( Int, Int )



--update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClicked x y ->
            ( { model | world = clickCell x y model.world }, Cmd.none )

        Next ->
            let
                new_world =
                    nextWorld model.world

                is_changed =
                    new_world == model.world
            in
                case is_changed of
                    True ->
                        ( { model | auto = False }, Cmd.none )

                    False ->
                        ( { model | world = new_world }, Cmd.none )

        AutoClicked ->
            ( { model
                | auto = not model.auto
                , world =
                    if model.auto then
                        model.world
                    else
                        nextWorld model.world
                , random_mode = False
              }
            , Cmd.none
            )

        RandomClicked ->
            ( { model | random_mode = not model.random_mode }, Cmd.none )

        GenerateRandom ->
            ( model, Random.generate RandomGenerated randomPointGenerator )

        RandomGenerated ( x, y ) ->
            case getCellState x y model.world of
                Nothing ->
                    ( model, Random.generate RandomGenerated randomPointGenerator )

                Just Alive ->
                    ( model, Random.generate RandomGenerated randomPointGenerator )

                Just Dead ->
                    ( { model | world = clickCell x y model.world }, Cmd.none )


randomPointGenerator : Random.Generator ( Int, Int )
randomPointGenerator =
    let
        g =
            Random.int 0 (size - 1)
    in
        Random.map2 (,) g g


invertStatus : Status -> Status
invertStatus s =
    case s of
        Alive ->
            Dead

        Dead ->
            Alive


clickCell : Int -> Int -> World -> World
clickCell x y model =
    let
        row =
            get x model

        state =
            row |> andThen (get y)
    in
        case ( row, state ) of
            ( Nothing, _ ) ->
                model

            ( _, Nothing ) ->
                model

            ( Just old_row, Just old_state ) ->
                let
                    new_row =
                        set y (invertStatus old_state) old_row
                in
                    set x new_row model


getCellState : Int -> Int -> World -> Maybe Status
getCellState x y model =
    let
        row =
            get x model
    in
        case row of
            Nothing ->
                Nothing

            Just r ->
                get y r


countLiving : Int -> Int -> World -> Int
countLiving x y m =
    let
        cells =
            [ ( x - 1, y )
            , ( x + 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            , ( x - 1, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y + 1 )
            ]
    in
        List.filter (\( x, y ) -> getCellState x y m == Just Alive) cells |> List.length


nextCellState : World -> Int -> Int -> Status -> Status
nextCellState m x y s =
    let
        living_cells =
            countLiving x y m
    in
        case s of
            Dead ->
                if living_cells == 3 then
                    Alive
                else
                    Dead

            Alive ->
                case living_cells of
                    2 ->
                        Alive

                    3 ->
                        Alive

                    _ ->
                        Dead


nextRow : World -> Int -> Array Status -> Array Status
nextRow m x ss =
    indexedMap (nextCellState m x) ss


nextWorld : World -> World
nextWorld m =
    indexedMap (nextRow m) m


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.auto == True then
        Time.every Time.second (\_ -> Next)
    else if m.random_mode == True then
        Time.every (80 * Time.millisecond) (\_ -> GenerateRandom)
    else
        Sub.none


statusToCell : Int -> Int -> Status -> Html Msg
statusToCell x y s =
    td
        [ classList [ ( "living-cell", s == Alive ) ]
        , onClick <| CellClicked x y
        ]
        []


rowToHtml : Int -> Array Status -> Html Msg
rowToHtml x sx =
    toList sx |> List.indexedMap (statusToCell x) |> tr []


view : Model -> Html Msg
view m =
    let
        world =
            toList m.world
                |> List.indexedMap rowToHtml
                |> table [ classList [ ( "is-moving", m.auto ) ] ]

        next_button =
            button [ onClick Next ] [ text "次世代" ]

        auto_text =
            if m.auto then
                "自動停止"
            else
                "自動"

        auto_button =
            button [ onClick AutoClicked ] [ text auto_text ]

        random_button =
            if m.auto then
                []
            else
                let
                    random_text =
                        if m.random_mode then
                            "ランダム配置停止"
                        else
                            "ランダム配置開始"
                in
                    [ button [ onClick RandomClicked ] [ text random_text ] ]
    in
        div [] <| [ world, next_button, auto_button ] ++ random_button

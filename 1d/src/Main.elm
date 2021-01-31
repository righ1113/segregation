module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Zipper exposing (Zipper(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace : Int
    , field : Zipper Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 (Zipper [] 0 [ 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ])
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            -- 適当個の乱数を要求する
            ( model
            , Random.generate NewFace (Random.list 100 (Random.int 1 2))
            )

        NewFace newFace ->
            -- 適当個の乱数が返ってきたら
            ( Model (model.dieFace + 1) (Zipper.extend (growNeigh newFace) model.field)
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (String.fromInt model.dieFace) ]
        , h1 [] [ text (myShow model.field) ]
        , button [ onClick Roll ] [ text "Increment" ]
        ]



-- UTILITY


myShow : Zipper Int -> String
myShow =
    let
        iToChar x =
            case x of
                0 ->
                    '□'

                1 ->
                    '●'

                2 ->
                    '←'

                4 ->
                    '→'

                _ ->
                    '？'
    in
    String.fromList << List.map iToChar << Zipper.toList


nth : List a -> Int -> Maybe a
nth xs n =
    List.head (List.drop n xs)


last : List a -> Maybe a
last xs =
    nth xs (List.length xs - 1)



-- NEITHBORHOOD


growNeigh : List Int -> Zipper Int -> Int
growNeigh rs (Zipper left a right) =
    if last left == Nothing then
        -- 左端が壁
        0

    else if List.head right == Nothing then
        -- 右端が壁
        0

    else
        let
            ( l, r ) =
                ( Maybe.withDefault 0 (last left), Maybe.withDefault 0 (List.head right) )
        in
        case ( l, a, r ) of
            ( 0, 0, 2 ) ->
                2

            ( 0, 0, _ ) ->
                0

            ( 0, 1, _ ) ->
                2 ^ Maybe.withDefault 0 (nth rs (List.length left))

            ( 0, 2, _ ) ->
                0

            ( 0, 4, 0 ) ->
                0

            ( 0, 4, 1 ) ->
                1

            ( 0, 4, 2 ) ->
                1

            ( 0, 4, 4 ) ->
                0

            ( 1, 0, 2 ) ->
                2

            ( 1, 0, _ ) ->
                0

            ( 1, 1, _ ) ->
                2 ^ Maybe.withDefault 0 (nth rs (List.length left))

            ( 1, 2, _ ) ->
                1

            ( 1, 4, 0 ) ->
                0

            ( 1, 4, 1 ) ->
                1

            ( 1, 4, 2 ) ->
                1

            ( 1, 4, 4 ) ->
                4

            ( 2, 0, 2 ) ->
                2

            ( 2, 0, _ ) ->
                0

            ( 2, 1, _ ) ->
                2 ^ Maybe.withDefault 0 (nth rs (List.length left))

            ( 2, 2, _ ) ->
                2

            ( 2, 4, 0 ) ->
                0

            ( 2, 4, 1 ) ->
                1

            ( 2, 4, 2 ) ->
                1

            ( 2, 4, 4 ) ->
                0

            ( 4, 0, 0 ) ->
                4

            ( 4, 0, 1 ) ->
                1

            ( 4, 0, 2 ) ->
                0

            ( 4, 0, 4 ) ->
                4

            ( 4, 1, _ ) ->
                2 ^ Maybe.withDefault 0 (nth rs (List.length left))

            ( 4, 2, _ ) ->
                1

            ( 4, 4, 0 ) ->
                0

            ( 4, 4, 1 ) ->
                1

            ( 4, 4, 2 ) ->
                1

            ( 4, 4, 4 ) ->
                4

            _ ->
                0

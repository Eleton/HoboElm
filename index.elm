import Html exposing (Html, div, span, text, program, select, option, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array


-- MODEL

type Node = X | O

type alias Model =
  { gameplan : Array.Array (Maybe Node)
  , turn : Node
  }

startModel : Model
startModel = Model (Array.fromList (List.repeat 9 Nothing)) X


init : ( Model, Cmd Msg )
init =
  ( startModel, Cmd.none )

-- MESSAGES

type Msg
  = Click Int
  | Restart


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Click index ->
      case Array.get index model.gameplan of
        Just Nothing ->
          let
            newTurn =
              case model.turn of
                X -> O
                O -> X
          in
            ({ model | gameplan = (Array.set index (Just model.turn) model.gameplan), turn = newTurn }, Cmd.none)
        _ ->
          (model, Cmd.none)
    Restart ->
      (startModel, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ div [] [text ("Current turn: " ++ (toString model.turn))]
  , div [ grid ] (Array.toList (Array.indexedMap (\k n -> cell k n) model.gameplan))
  , button [ onClick Restart ] [ text "Restart" ]
  ]
  

cell : Int -> Maybe Node -> Html Msg
cell k node =
  case node of
    Just X -> div [ onClick (Click k) ] [ text "X" ]
    Just O -> div [ onClick (Click k) ] [ text "O" ]
    Nothing -> div [ onClick (Click k) ] [ text "_" ]

grid =
  style
    [ ("display", "grid")
    , ("grid-template-columns", "40px 40px 40px")
    , ("grid-template-rows", "40px 40px 40px")
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
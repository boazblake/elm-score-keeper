module Main exposing (..)

import Html exposing (Html, li, ul, i, form, button, header, footer, input, p, text, div, h1, img)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


---- MODEL ----


type alias Model =
  { players: List Player
  , name: String
  , playerId: Maybe Int
  , plays: List Play
  }

type alias Player =
  { id : Int
  , name: String
  , points: Int
  }

type alias Play =
  { id: Int 
  , playerId: Int
  , name: String
  , points: Int
  }


initModel : Model
initModel =
  { players = []
  , name = ""
  , playerId = Nothing
  , plays = []
  }


---- UPDATE ----
type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Cancel
    | Save
    | DeletePlay Play

update : Msg -> Model -> Model
update msg model =
  case msg of
    
    Input name ->
      Debug.log "Input Updated Model"
        { model | name = name}
    
    Cancel ->
      model
    
    Score player points ->
      score model player points 

    Edit  player ->
      { model 
        | name = player.name
        , playerId = Just player.id
      }

    Save ->
      if ( String.isEmpty model.name ) then
        model
      else
        save model

    DeletePlay play ->
      deletePlay model play


score : Model -> Player -> Int -> Model
score model scorer points =
  let
    newPlayers =
      List.map
        (\player ->
          if player.id == scorer.id then 
            { player 
                | points = player.points + points
            }
          else 
            player 
        )
        model.players

    play =
      Play (List.length model.plays) scorer.id scorer.name points
  
  in
    { model 
      | players = newPlayers
      , plays = play :: model.plays
    }

save : Model -> Model
save model =
  case model.playerId of
    Just id ->
      edit model id
    Nothing ->
      add model


edit : Model -> Int -> Model
edit model id =
  let
    newPlayers =
      List.map
        (\player ->
          if player.id == id then
            { player | name = model.name }
          else
            player
        )
        model.players
    
    newPlays =
      List.map
      (\play ->
        if play.playerId == id then
          { play | name = model.name }
        else
          play
      )model.plays

  in
    { model
      | players = newPlayers
      , plays = newPlays
      , name = ""
      , playerId = Nothing
    }


add : Model -> Model
add model =
  let
    player =
      -- Player <id> <name> <points>
      Player (List.length model.players) model.name 0

    newPlayers =
      player :: model.players

  in
    { model 
      | players = newPlayers
      , name = "" 
    }


deletePlay : Model -> Play -> Model
deletePlay model play =
  let
      newPlays =
        List.filter(\p -> p.id /= play.id) model.plays
      
      newPlayers =
        List.map
          (\player ->
            if player.id == play.playerId then
              { player | points = player.points - 1 * play.points }
            else
              player
          )
          model.players
  in
      { model 
        | plays = newPlays
        , players = newPlayers
      }


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "scoreboard"]
        [ h1[]  [ text "Score Keeper"]
        , playerSection model
        , playerForm model
        , playSection model
        -- , p [] [ text (toString model)]
        ]


playerSection : Model -> Html Msg
playerSection model =
  div []
      [ playerListHeader
      , playerList model
      , pointTotal model
      ]

playerListHeader : Html Msg
playerListHeader =
  header []
    [ div [] [text "Name"]
    , div [] [text "Points"]
    ]

playerList : Model -> Html Msg
playerList model =
  -- ul []
  --   ( List.map player model.players )
  model.players
    |> List.sortBy .name
    |> List.map player
    |> ul []


player : Player -> Html Msg
player player =
    li []
      [ i
        [ class "edit"
        , onClick (Edit player)
        ] []
      , div []
          [ text player.name  ]
      , button
          [  type_ "button"
          , onClick (Score player 2)
          ]
          [ text "2 points"]
      , button
          [ type_ "button"
          , onClick (Score player 3)
          ]
          [ text "3 points"]
      , div [] [text (toString player.points)]
      ]


pointTotal : Model -> Html Msg
pointTotal model =
  let
    total =
      List.map .points model.plays
        |> List.sum
  
  in
    footer [] 
        [ div [] [text "Total"]
        , div [] [text (toString total)]
        ]


playerForm : Model -> Html Msg
playerForm model =
  form [ onSubmit Save ]
      [ input 
        [ type_ "text"
        , placeholder "Add/Edit player"
        , onInput Input
        , value model.name
        ] []
      , button [ type_ "submit" ] [ text "Save" ]
      , button [ type_ "button", onClick Cancel ] [ text "Cancel"]]


playSection : Model -> Html Msg
playSection model =
  div [] 
    [ playListHeader
    , playList model
    ]

playListHeader : Html Msg
playListHeader =
  header [] 
    [ div [] [text "Plays"]
    , div [] [ text "Points"]
    ]


playList: Model -> Html Msg
playList model =
  model.plays
    |> List.map play
    |> ul []

play : Play -> Html Msg
play play =
  li []
    [ i 
      [ class "remove"
      , onClick (DeletePlay play)
      ] []
    , div [] [  text play.name  ]
    , div [] [ text (toString play.points)]
    ]
---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }

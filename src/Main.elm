module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Http
import Json.Decode as D exposing (Decoder)
import WebSocket



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


wsUrl : String
wsUrl =
    "wss://programming-elm.com/"


type alias Id =
    Int


type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , newComment : String
    , comments : List String
    }


type alias Feed =
    List Photo


type alias Model =
    { feed : Maybe Feed
    , error : Maybe Http.Error
    , streamQueue : Feed
    }


photoDecoder : Decoder Photo
photoDecoder =
    D.map6
        Photo
        (D.field "id" D.int)
        (D.field "url" D.string)
        (D.field "caption" D.string)
        (D.field "liked" D.bool)
        (D.succeed "")
        (D.field "comments" <| D.list D.string)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { feed = Nothing
      , error = Nothing
      , streamQueue = []
      }
    , fetchFeed
    )


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed"
        , expect = Http.expectJson LoadFeed (D.list photoDecoder)
        }



-- Update


type Msg
    = ToggleLike Id
    | UpdateComment Id String
    | SaveComment Id
    | LoadFeed (Result Http.Error Feed)
    | LoadStreamPhoto (Result D.Error Photo)
    | FlushStreamQueue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike id ->
            ( { model | feed = updateFeed toggleLike id model.feed }
            , Cmd.none
            )

        UpdateComment id comment ->
            ( { model | feed = updateFeed (updateComment comment) id model.feed }
            , Cmd.none
            )

        SaveComment id ->
            ( { model | feed = updateFeed saveNewComment id model.feed }
            , Cmd.none
            )

        LoadFeed (Ok feed) ->
            ( { model | feed = Just feed }
            , WebSocket.listen wsUrl
            )

        LoadFeed (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        LoadStreamPhoto (Ok photo) ->
            ( { model | streamQueue = photo :: model.streamQueue }
            , Cmd.none
            )

        LoadStreamPhoto (Err _) ->
            ( model, Cmd.none )

        FlushStreamQueue ->
            ( { model
                | feed = Maybe.map ((++) model.streamQueue) model.feed
                , streamQueue = []
              }
            , Cmd.none
            )


updatePhotoById : (Photo -> Photo) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
    List.map
        (\photo ->
            if id == photo.id then
                updatePhoto photo

            else
                photo
        )
        feed


updateFeed : (Photo -> Photo) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed =
    Maybe.map (updatePhotoById updatePhoto id) maybeFeed


toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }


updateComment : String -> Photo -> Photo
updateComment comment photo =
    { photo | newComment = comment }


saveNewComment : Photo -> Photo
saveNewComment photo =
    let
        comment =
            String.trim photo.newComment
    in
    { photo
        | comments = photo.comments ++ [ comment ]
        , newComment = ""
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.receive (LoadStreamPhoto << D.decodeString photoDecoder)



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewContent model ]
        ]


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadBody _ ->
            """Sorry, we couldn't process your feed at this time.
            We're working on!"""

        _ ->
            """Sorry, we couldn't load your feed at this time.
            Please try again later."""


viewStreamNotification : Feed -> Html Msg
viewStreamNotification queue =
    case queue of
        [] ->
            text ""

        _ ->
            let
                content =
                    "View new photos: " ++ (String.fromInt << List.length <| queue)
            in
            div [ class "stream-notification", onClick FlushStreamQueue ]
                [ text content ]


viewContent : Model -> Html Msg
viewContent { feed, error, streamQueue } =
    case error of
        Just error_ ->
            div [ class "feed-error" ]
                [ text <| errorMessage error_ ]

        Nothing ->
            div []
                [ viewStreamNotification streamQueue
                , viewFeed feed
                ]


viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
    case maybeFeed of
        Just feed ->
            Html.Keyed.node "div" [] (List.map viewDetailedPhoto feed)

        Nothing ->
            div [ class "loading-feed" ] [ text "Loading Feed..." ]


viewLoveButton : Photo -> Html Msg
viewLoveButton { id, liked } =
    let
        buttonClass =
            if liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick <| ToggleLike id
            ]
            []
        ]


viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div [ class "comments" ]
                [ ul []
                    (List.map viewComment comments)
                ]


viewComments : Photo -> Html Msg
viewComments { id, comments, newComment } =
    div []
        [ viewCommentList comments
        , form [ class "new-comment", onSubmit <| SaveComment id ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value newComment
                , onInput <| UpdateComment id
                ]
                []
            , button [ disabled <| String.isEmpty <| String.trim <| newComment ] [ text "Save" ]
            ]
        ]


viewDetailedPhoto : Photo -> ( String, Html Msg )
viewDetailedPhoto photo =
    ( String.fromInt photo.id
    , div [ class "detailed-photo" ]
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton photo
            , h2 [ class "caption" ] [ text photo.caption ]
            , viewComments photo
            ]
        ]
    )

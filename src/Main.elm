module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)



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


type alias Model =
    { photo : Maybe Photo }


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
    ( { photo = Nothing }
    , fetchFeed
    )


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed/1"
        , expect = Http.expectJson LoadFeed photoDecoder
        }



-- Update


type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment
    | LoadFeed (Result Http.Error Photo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike ->
            ( { model | photo = updateFeed toggleLike model.photo }
            , Cmd.none
            )

        UpdateComment comment ->
            ( { model | photo = updateFeed (updateComment comment) model.photo }
            , Cmd.none
            )

        SaveComment ->
            ( { model | photo = updateFeed saveNewComment model.photo }
            , Cmd.none
            )

        LoadFeed (Ok photo) ->
            ( { model | photo = Just photo }, Cmd.none )

        LoadFeed (Err _) ->
            ( model, Cmd.none )


updateFeed : (Photo -> Photo) -> Maybe Photo -> Maybe Photo
updateFeed updatePhoto maybePhoto =
    Maybe.map updatePhoto maybePhoto


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
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewFeed model.photo ]
        ]


viewFeed : Maybe Photo -> Html Msg
viewFeed maybePhoto =
    case maybePhoto of
        Just photo ->
            viewDetailedPhoto photo

        Nothing ->
            div [ class "loading-feed" ] [ text "Loading Feed..." ]


viewLoveButton : Photo -> Html Msg
viewLoveButton { liked } =
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
            , onClick ToggleLike
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
viewComments { comments, newComment } =
    div []
        [ viewCommentList comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value newComment
                , onInput UpdateComment
                ]
                []
            , button [ disabled <| String.isEmpty <| String.trim <| newComment ] [ text "Save" ]
            ]
        ]


viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    div [ class "detailed-photo" ]
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton photo
            , h2 [ class "caption" ] [ text photo.caption ]
            , viewComments photo
            ]
        ]

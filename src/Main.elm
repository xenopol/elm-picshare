module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (Html, a, div, h1, i, text)
import Html.Attributes exposing (class)
import Routes
import Url exposing (Url)



-- Program


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }



-- Model


type Page
    = PublicFeed
    | Account
    | NotFound


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    }


initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { page = NotFound
    , navigationKey = navigationKey
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url navigationKey =
    setNewPage (Routes.match url) (initialModel navigationKey)



-- View


viewContent : Page -> ( String, Html Msg )
viewContent page =
    case page of
        PublicFeed ->
            ( "Picshare"
            , h1 [] [ text "Public Feed" ]
            )

        Account ->
            ( "Account"
            , h1 [] [ text "Account" ]
            )

        NotFound ->
            ( "Not Found"
            , div [ class "not-found" ]
                [ h1 [] [ text "Page Not Found" ] ]
            )


view : Model -> Document Msg
view model =
    let
        ( title, content ) =
            viewContent model.page
    in
    { title = title
    , body = [ content ]
    }



-- Update


type Msg
    = NewRoute (Maybe Routes.Route)
    | Visit UrlRequest


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            ( { model | page = PublicFeed }, Cmd.none )

        Just Routes.Account ->
            ( { model | page = Account }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRoute maybeRoute ->
            setNewPage maybeRoute model

        _ ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

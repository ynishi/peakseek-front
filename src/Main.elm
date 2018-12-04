module Main exposing (Bulma(..), Classes, Const, DataXY, DataXYId, HTTPDataXY(..), Handler, IDs(..), Model, Msg(..), apiView, bulma, bulmac, constAPI, constDecoder, constEncoder, constNInput, conv, dataXYAPI, dataXYDecoder, dataXYEncoder, decodeNs, defaultConstN, expView, formatFloat, init, initialModel, lenStr, main, makeListStr, newClasses, sampleInputInternal, stopBoth, update, view, xsInput, xysTable, ysInput, zipWithNumber)

import Array as A
import Browser
import Browser.Navigation as Nav
import FormatNumber as F
import FormatNumber.Locales exposing (Locale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, errorToString, field, float, int, list)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List as L
import List.Extra as LE
import String exposing (..)
import String.Extra exposing (..)
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)



-- Route


type Route
    = Home
    | NotFound
    | Xys DataXYId


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Xys (s "xys" </> Url.Parser.int)
        ]


toRoute : Url.Url -> Route
toRoute url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> parse routeParser
        |> Maybe.withDefault NotFound


fromRoute : Route -> String
fromRoute r =
    case r of
        Home ->
            "home"

        NotFound ->
            "not found"

        Xys i ->
            "xys: " ++ fromInt i



-- Model


type alias Model =
    { endpoint : String, consts : List Float, constErrStr : String, dataXYId : DataXYId, dataXY : HTTPDataXY, xsInputStr : String, xs : List Float, xsErrStr : String, ysInputStr : String, ys : List Float, ysErrStr : String, constN : Int, key : Nav.Key, url : Url.Url }


initialModel endpoint url key =
    { endpoint = endpoint, consts = [], constErrStr = "", dataXYId = 0, dataXY = Yet, xs = [], ys = [], xsInputStr = "", xsErrStr = "", ysInputStr = "", ysErrStr = "", constN = defaultConstN, key = key, url = url }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
    ( initialModel flag url key, Cmd.none )


defaultConstN =
    3


type HTTPDataXY
    = Loading
    | Yet
    | Success DataXY


type alias DataXYId =
    Int


type alias DataXY =
    { dataXYId : DataXYId
    , xs : List Float
    , ys : List Float
    }


dataXYDecoder : Decoder DataXY
dataXYDecoder =
    Json.Decode.succeed DataXY
        |> required "dataXYId" int
        |> required "xs" (list float)
        |> required "ys" (list float)


dataXYEncoder : DataXY -> Encode.Value
dataXYEncoder dataXY =
    Encode.object
        [ ( "dataXYId", Encode.int dataXY.dataXYId )
        , ( "xs", Encode.list Encode.float dataXY.xs )
        , ( "ys", Encode.list Encode.float dataXY.ys )
        ]


type alias Const =
    { constDataXYId : Int
    , const : List Float
    }


constDecoder : Decoder Const
constDecoder =
    Json.Decode.succeed Const
        |> required "constDataXYId" int
        |> required "const" (list float)


constEncoder : Const -> Encode.Value
constEncoder const =
    Encode.object
        [ ( "constDataXYId", Encode.int const.constDataXYId )
        , ( "const", Encode.list Encode.float const.const )
        ]


constAPI endpoint dataXYId constN =
    endpoint ++ "/data/" ++ fromInt dataXYId ++ "/const/" ++ fromInt constN


dataXYAPI endpoint dataXYId =
    let
        suffix =
            if dataXYId /= 0 then
                "/" ++ fromInt dataXYId

            else
                ""
    in
    endpoint ++ "/data" ++ suffix



-- Msg


type Msg
    = GotDataXY (Result Http.Error DataXY)
    | Xs String
    | Ys String
    | GetDataXY
    | PostDataXY
    | PutDataXY
    | PutMsg
    | GetConst
    | GotConst (Result Http.Error Const)
    | NoOpe
    | ConstN String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest



-- Update


update msg model =
    case msg of
        GotDataXY result ->
            case result of
                Ok t ->
                    let
                        xsStr =
                            t.xs |> L.map fromFloat |> join ", "

                        ysStr =
                            t.ys |> L.map fromFloat |> join ", "

                        updatedModel =
                            { model | dataXY = Success t, dataXYId = t.dataXYId, xsInputStr = xsStr, ysInputStr = ysStr }

                        ( gotModel, gotCmd ) =
                            update GetConst updatedModel
                    in
                    ( gotModel, gotCmd )

                Err _ ->
                    ( model, Cmd.none )

        Xs xsInputStr ->
            let
                ( decoded, err ) =
                    decodeNs xsInputStr
            in
            ( { model | xs = decoded, xsInputStr = xsInputStr, xsErrStr = err }, Cmd.none )

        Ys ysInputStr ->
            let
                ( decoded, err ) =
                    decodeNs ysInputStr
            in
            ( { model | ys = decoded, ysInputStr = ysInputStr, ysErrStr = err }, Cmd.none )

        GetDataXY ->
            let
                { endpoint, dataXYId } =
                    model

                cmd =
                    Http.get { url = dataXYAPI endpoint dataXYId, expect = Http.expectJson GotDataXY dataXYDecoder }
            in
            ( model, cmd )

        PostDataXY ->
            let
                { endpoint, xs, ys } =
                    model

                postData =
                    { dataXYId = 0, xs = xs, ys = ys }

                encoded =
                    dataXYEncoder postData
            in
            ( model, Http.post { body = Http.jsonBody encoded, url = dataXYAPI endpoint 0, expect = Http.expectJson GotDataXY dataXYDecoder } )

        PutDataXY ->
            let
                { dataXYId, xs, ys, endpoint } =
                    model

                putData =
                    { dataXYId = dataXYId, xs = xs, ys = ys }

                encoded =
                    dataXYEncoder putData

                putReq =
                    Http.request
                        { method = "PUT"
                        , headers =
                            [ Http.header "Access-Control-Allow-Origin" "*"
                            ]
                        , url = dataXYAPI endpoint dataXYId
                        , body = Http.jsonBody encoded
                        , expect = Http.expectWhatever (\_ -> PutMsg)
                        , timeout = Nothing
                        , tracker = Nothing
                        }
            in
            ( model, putReq )

        PutMsg ->
            let
                ( gotModel, gotCmd ) =
                    update GetConst model
            in
            ( gotModel, gotCmd )

        GetConst ->
            let
                { endpoint, dataXYId, constN } =
                    model

                cmd =
                    Http.get { url = constAPI endpoint dataXYId constN, expect = Http.expectJson GotConst constDecoder }
            in
            ( model, cmd )

        GotConst result ->
            case result of
                Ok t ->
                    ( { model | consts = t.const }, Cmd.none )

                Err e ->
                    ( { model | constErrStr = "" }, Cmd.none )

        ConstN str ->
            let
                n =
                    case toInt str of
                        Just v ->
                            if v > 0 then
                                v

                            else
                                defaultConstN

                        _ ->
                            defaultConstN
            in
            ( { model | constN = n }, Cmd.none )

        UrlChanged url ->
            let
                r =
                    toRoute url

                ( updated, cmd ) =
                    case r of
                        Xys i ->
                            update GetDataXY { model | url = url, dataXYId = i }

                        _ ->
                            ( { model | url = url, dataXYId = 0 }, Cmd.none )
            in
            ( updated, cmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        NoOpe ->
            ( model, Cmd.none )


decodeNs str =
    str
        |> (\s -> decodeString (list float) (makeListStr s))
        |> (\result ->
                case result of
                    Ok v ->
                        ( v, "" )

                    Err s ->
                        ( [], errorToString s )
           )


makeListStr orig =
    orig |> stopBoth |> (\str -> "[" ++ str ++ "]")


stopBoth orig =
    let
        trimRightList =
            ",. ]"

        trimLeftList =
            ",. ["
    in
    orig
        |> clean
        |> (\str ->
                if contains (right 1 str) trimRightList then
                    dropRight 1 str

                else
                    str
           )
        |> (\str ->
                if contains (left 1 str) trimLeftList then
                    dropLeft 1 str

                else
                    str
           )



-- view


view : Model -> Browser.Document Msg
view model =
    let
        { xs, ys, xsInputStr, ysInputStr, xsErrStr, ysErrStr, dataXY, dataXYId, consts, endpoint, constN } =
            model

        isNew =
            dataXYId /= 0

        isValid =
            L.length xs >= constN && L.length ys >= constN

        postOrPut =
            if isValid && isNew then
                PutDataXY

            else if isValid then
                PostDataXY

            else
                NoOpe

        postOrPutButtonTitle =
            if isNew then
                "Update"

            else
                "New"

        isExp =
            L.length consts > 0

        apiUrl =
            constAPI endpoint dataXYId constN

        dataXYMsg =
            case dataXY of
                Yet ->
                    div [] []

                Loading ->
                    p [] [ text "loading..." ]

                Success _ ->
                    if not isExp then
                        p [] [ text "completed." ]

                    else
                        div [] []
    in
    { title = "title1"
    , body =
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Simple Data Polynomial Regression" ]
                , p [ bulmac Content ] [ text "This is a simple tool to calculate polynomial regression. Just input list of x, and list of y, send data and expression will show. The API is provided too." ]
                , xsInput xsInputStr xs xsErrStr
                , ysInput ysInputStr ys ysErrStr
                , constNInput constN
                , xysTable xs ys
                , div [ class "field" ] [ div [ class "control" ] [ button [ bulmac <| AddB Button IsLink, disabled <| not isValid, onClick postOrPut ] [ text <| postOrPutButtonTitle ] ] ]
                , dataXYMsg
                , expView consts isExp
                , apiView apiUrl isExp
                ]
            ]
        , footer [ bulmac Footer ]
            [ div [ bulmac <| AddB Content HasTextCentered ] [ copyView ]
            ]
        ]
    }


copyView =
    p [] [ strong [] [ text "Peakseek" ], text " by (c) 2018 ", a [ href "https://github.com/ynishi" ] [ text "Yutaka Nishimura" ], text ". The source code is licensed ", a [ href "http://opensource.org/licenses/mit-license.php" ] [ text "MIT" ], text ", published in ", a [ href "https://github.com/ynishi/peakseek" ] [ text "ynishi/peakseek" ] ]


apiView apiUrl isExp =
    if isExp then
        div [ class "box" ] [ div [ class "content" ] [ text <| "API: " ++ apiUrl ] ]

    else
        div [] []


type IDs
    = XS_INPUT
    | YS_INPUT
    | ConstN_INPUT


conv : IDs -> String
conv id =
    case id of
        XS_INPUT ->
            "xs_input"

        YS_INPUT ->
            "ys_input"

        ConstN_INPUT ->
            "const_input"


constNInput : Int -> Html Msg
constNInput constN =
    let
        cid =
            conv ConstN_INPUT
    in
    div [ class "field" ]
        [ label [ class "label", for cid ] [ text "Number of Consts" ]
        , div [ class "control" ]
            [ input [ id cid, bulmac Input, type_ "number", Html.Attributes.min "1", placeholder "3", value (fromInt constN), onInput ConstN ] []
            ]
        ]


xsInput : String -> List Float -> String -> Html Msg
xsInput xs xsl err =
    sampleInputInternal XS_INPUT "Xs" Xs xs xsl err


ysInput : String -> List Float -> String -> Html Msg
ysInput ys ysl err =
    sampleInputInternal YS_INPUT "Ys" Ys ys ysl err


xysTable : List Float -> List Float -> Html Msg
xysTable xs ys =
    let
        len =
            Basics.min (L.length xs) (L.length ys)

        mkT h f =
            h [] << L.singleton << text << f

        mkTh =
            mkT th fromInt

        mkTd =
            mkT td fromFloat

        headPart =
            L.range 1 len
                |> L.map mkTh

        mkPart ns =
            ns
                |> L.take len
                |> L.map mkTd

        xsPart =
            mkPart xs

        ysPart =
            mkPart ys
    in
    table [ class "table" ]
        [ thead [] <| [ th [] [ abbr [ title <| "Elements(length: " ++ fromInt len ++ ")" ] [ text <| "Elem(" ++ fromInt len ++ ")" ] ] ] ++ headPart
        , tbody []
            [ tr [] <| [ th [] [ text "xs" ] ] ++ xsPart
            , tr [] <| [ th [] [ text "ys" ] ] ++ ysPart
            ]
        ]


type alias Handler =
    String -> Msg


lenStr l =
    "(len: " ++ fromInt (L.length l) ++ ")"


sampleInputInternal : IDs -> String -> Handler -> String -> List Float -> String -> Html Msg
sampleInputInternal i l handler v fl err =
    let
        istr =
            conv i

        flstr =
            L.map fromFloat fl |> join ", "

        isOk =
            err == ""

        msg =
            if isOk then
                lenStr fl ++ flstr

            else
                err

        isInit =
            L.length fl == 0 && isOk
    in
    div [ class "field" ]
        [ label [ class "label", for istr ] [ text l ]
        , div [ class "control" ]
            [ input [ id istr, bulmac Input, type_ "text", placeholder "1.0, 2.0 ...", value v, onInput handler ] []
            ]
        , p [ bulmac <| AddB Help <| SwitchB (WhitchB IsSuccess IsDanger isOk) (not isInit) ] [ "Result: " ++ msg |> text ]
        ]


expView consts isExp =
    if isExp then
        consts
            |> zipWithNumber
            |> L.map (\( i, x ) -> formatFloat x ++ " * x ^ " ++ fromInt i)
            |> String.join " + "
            |> (\x -> "y = " ++ x)
            |> text
            |> L.singleton
            |> div [ class "content" ]
            |> L.singleton
            |> div [ class "box" ]

    else
        div [] []



-- Helper


zipWithNumber l =
    L.map2 Tuple.pair (L.range 0 (L.length l)) l


formatFloat =
    F.format usLocale



-- Bulma helper


type Bulma
    = AddB Bulma Bulma
    | CompB Bulma
    | EmptyB
    | ListB (List Bulma)
    | SubB Bulma Bulma
    | SwitchB Bulma Bool
    | WhitchB Bulma Bulma Bool
    | Button
    | Content
    | Footer
    | HasTextCentered
    | Help
    | Input
    | IsDanger
    | IsLink
    | IsSuccess


type alias Classes =
    List ( String, Bool )


newClasses : String -> Classes
newClasses str =
    [ ( str, True ) ]


bulmac b =
    b |> bulma |> classList


bulma : Bulma -> Classes
bulma b =
    case b of
        AddB x y ->
            bulma x ++ bulma y

        SubB x y ->
            L.map
                (\( name, bool ) ->
                    if L.member name (L.map Tuple.first (bulma y)) then
                        ( name, False )

                    else
                        ( name, bool )
                )
            <|
                bulma x

        CompB x ->
            LE.uniqueBy Tuple.first <| L.filter Tuple.second <| bulma x

        EmptyB ->
            []

        ListB xs ->
            bulma <| L.foldr AddB EmptyB xs

        SwitchB x bool ->
            L.map (\( a, _ ) -> ( a, bool )) <| bulma x

        WhitchB ok ng bool ->
            if bool then
                bulma ok

            else
                bulma ng

        Button ->
            newClasses "button"

        Content ->
            newClasses "content"

        Footer ->
            newClasses "footer"

        HasTextCentered ->
            newClasses "has-text-centered"

        Help ->
            newClasses "help"

        IsDanger ->
            newClasses "is-danger"

        IsLink ->
            newClasses "is-link"

        IsSuccess ->
            newClasses "is-success"

        Input ->
            newClasses "input"



-- Main


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }

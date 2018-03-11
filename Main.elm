module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (src, title, class, id, type_, multiple)
import Ports exposing (Histogram, renderHistogram, jsMessageOutcome, FileContent, fileSelected, fileContentRead)
import Json.Decode as JD


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { id : String
    , message : String
    , histograms : List Histogram
    }


init : ( Model, Cmd Msg )
init =
    ( Model "FileInputId" "" [], Cmd.none )



-- UPDATE


type Msg
    = FileSelected
    | FileContentRead FileContent
    | Remove String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelected ->
            ( model, fileSelected model.id )

        FileContentRead fileContent ->
            let
                histograms =
                    (updatedHistograms model.histograms fileContent)
            in
                ( { model | histograms = histograms }, renderHistogram histograms )

        Remove histogramName ->
            ( { model | histograms = List.filter (\h -> h.name /= histogramName) model.histograms }, renderHistogram model.histograms )


updatedHistograms : List Histogram -> FileContent -> List Histogram
updatedHistograms histograms file =
    (toHistogram file) :: List.filter (\h -> h.name /= file.fileName) histograms


toHistogram : FileContent -> Histogram
toHistogram file =
    let
        datapoints =
            parseFile file.contents
    in
        Histogram file.fileName (List.map (\t -> Tuple.first t) datapoints) (List.map (\t -> Tuple.second t) datapoints)


parseFile : String -> List ( Float, Float )
parseFile text =
    String.lines text
        |> List.filterMap parseLine


parseLine : String -> Maybe ( Float, Float )
parseLine line =
    case (toFloats line) of
        [ latency, _, _, percentile ] ->
            Just ( latency, percentile )

        _ ->
            Nothing


toFloats : String -> List Float
toFloats line =
    String.words line
        |> List.filterMap (\word -> Result.toMaybe (String.toFloat word))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileContentRead FileContentRead
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , id model.id
            , multiple True
            , on "change"
                (JD.succeed FileSelected)
            ]
            []
        , div []
            [ text ("file content: " ++ (String.join ", " (fileNames model.histograms)))
            , div []
                (List.map (\name -> fileBox name) (fileNames model.histograms))
            ]
        ]


fileBox : String -> Html Msg
fileBox name =
    div []
        [ text name
        , button [ onClick (Remove name) ] [ text "x" ]
        ]


fileNames : List Histogram -> List String
fileNames histograms =
    histograms
        |> List.map (\histogram -> histogram.name)



-- main =
-- Html.text "Hello World"

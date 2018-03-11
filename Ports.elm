port module Ports exposing (..)


type alias Histogram =
    { name : String
    , latencies : List Float
    , percentiles : List Float
    }


port renderHistogram : List Histogram -> Cmd msg


port jsMessageOutcome : (String -> msg) -> Sub msg


type alias FileContent =
    { contents : String
    , fileName : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FileContent -> msg) -> Sub msg

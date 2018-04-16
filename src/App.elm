import Html exposing (..)
import Html.Events exposing (onClick)

-- MODEL
type alias Model =
    {goal: Int
    , description: String}

-- UPDATE
type Msg = Reset | ...

update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset -> ...
        ...

-- VIEW
view : Model -> Html Msg
view model =
    ...

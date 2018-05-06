import Html exposing (Html, div, button, text, br)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

main = Html.beginnerProgram {
    model=model,
    view=view,
    update=update
    }

type alias Model = {value: Int}

model: Model
model = {value=0}

type Msg =
    Increment

update: Msg -> Model -> Model
update msg model =
    case msg of
        Increment -> {model|value=model.value+1}

view: Model -> Html Msg
view model =
    div [style [("border", "solid 1px black")]] [
        text "This is an embedded Elm app.",
        br [] [],

        -- app
        button [onClick Increment] [text "+"],
        text " ",
        text (toString model.value),

        -- explainer text
        br [] [],
        text "• The number is the rendered view of the model",
        br [] [],
        text "• The button sends a signal to update the model",
        br [] [],
        text ("Full model: " ++ toString model)
    ]

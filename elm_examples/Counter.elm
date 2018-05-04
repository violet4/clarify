-- this is from https://guide.elm-lang.org/architecture/user_input/buttons.html
-- the purpose of this is to make it easy to understand the pieces here.
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- we're defining a variable "main" which will get used by
-- whatever it is that actually runs our program.
-- "Html.beginnerProgram" is a structure that holds the parts
-- of our program. in "model = model", "model =" is like a
-- named parameter, and "= model" is referring to the model
-- that's defined later in this program. same goes with
-- "view" and "update"
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

-- it's just saying "Model" is a new type that
-- is equal to "Int". it's kind of like a subtype
-- of Int.
type alias Model = Int

-- this is saying "model" is of type "Model". it's
-- like declaring a java variable, like "int i;"
model : Model
-- set model variable to 0
model =
  0


-- UPDATE

-- "Msg", "Increment", and "Decrement" are all arbitrary.
-- the purpose of this is just for passing around types
-- so we can keep track of what something is. it's like a
-- flag with an arbitrary number of possibilities (or like a
-- dummy variable)
type Msg =
    Increment
    | Decrement

-- "update" is a type that takes a "Msg" and a
-- "Model" and returns a "Model", where that
-- model is the state of our app. it actually
-- is what allows us to update our state
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

-- how to render the model as an html page. I
-- believe that "Html" is a generic type that
-- takes a type parameter, which has been
-- specified as "Msg". I don't completely understand
-- the type.
view : Model -> Html Msg
-- here, "div" is a function that takes two lists -
-- the first list is html attributes and the second is
-- the DOM contents of the div. "button" is similar.
view model =
  div []
    -- create a button, bind it to the "Decrement" message,
    -- and put the "-" symbol on the button
    [ button [ onClick Decrement ] [ text "-" ]
    -- finally, the user can see what the current state is.
    , div [] [ text (toString model) ]
    -- create a button, bind it to the "Increment" message,
    -- and put the "+" symbol on the button
    , button [ onClick Increment ] [ text "+" ]
    ]

import Html exposing (Html, text)
import Json.Encode

main = Html.beginnerProgram{
  model=model,
  view=view,
  update=update
  }

type alias Userid = Int
type alias Name = String
type alias Settings = List String

type Model =
  Model1 Userid
  | Model2 Userid Name
  | Model3 Userid Name Settings

updateModel model =
    case model of
        Model1 id -> updateModel (Model2 id "")
        Model2 id name -> updateModel (Model3 id name [])
        Model3 _ _ _ -> model

saveModel model =
    case model of
        Model1 id -> Json.Encode.object [
            ("id", Json.Encode.int id)
          ]
        Model2 id name -> Json.Encode.object [
            ("id", Json.Encode.int id),
            ("name", Json.Encode.string name)
          ]
        Model3 id name settings -> Json.Encode.object [
            ("id", Json.Encode.int id),
            ("name", Json.Encode.string name),
            ("settings", Json.Encode.list (List.map Json.Encode.string settings))
          ]


model = updateModel (Model1 0)

view model = text (toString model)
update model = model

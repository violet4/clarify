import Html exposing (div, text, Html, a)
import Html.Attributes exposing (href)

main = Html.beginnerProgram {
    model=model,
    update=update,
    view=view
    }

-- types:
-- LifeGoal, Priority, Task, Subtask

-- should a Task be recursive in that it can have one, two, or any number of tasks?
-- if a task can only have one subtask, then the user can't split up a task into multiple subtasks and save that for later, which sounds restrictive, and also could increase mental burden if the user can't get their ideas out of their head into the app. I think a task should have any number of subtasks, so each subtask can be recursively broken down until it meets the rule "less than 5 minutes or can't be broken down any further".

type Task = Task {
    title: String
    , tasks: List Task
    , complete: Bool
    }
type alias Today = {
    tasks: List Task
    }
type alias Priority = {
    title: String,
    tasks: List Task
    }
type alias LifeGoal = {
    title: String,
    priorities: List Priority
    }
type alias Model = {
    life_goals: List LifeGoal,
    today: Today
    }

model: Model
--model = Model [] (Today [])
model = Model [
    LifeGoal "cleanliness" [
        Priority "keep room clean" [
            Task {
                title="clean bookshelf",
                tasks=[
                    Task {
                        title="clean top shelf",
                        tasks=[],
                        complete=False
                    },
                    Task {
                        title="clean 2nd shelf",
                        tasks=[],
                        complete=False
                    }
                ],
                complete=False
            }
        ]
    ]
    ] (Today [])

-- I read https://www.reddit.com/r/elm/comments/4j2fg6/finding_the_last_list_element/d33671d/
-- and then re-wrote it from scratch myself.
last : List a -> Maybe a
last list =
    case list of
        [] -> Nothing
        [last] -> Just last
        h::t -> last t

-- we don't need this, but it's demonstrative of something
-- that we will need - keeping current state of the app
-- and/or what state user asked to navigate to
type Msg =
    Increment
    | Decrement

-- can't make any updates yet. just ignore any
-- message and return the model we're given
update msg model = model
-- but this is how we update our model with a new life goal called "cleanliness":
-- { model | life_goals = (LifeGoal "cleanliness" []) :: model.life_goals }
-- we need a "msg" that enumerates the actions we could take at this step,
-- (i.e. create a corresponding version of "type Msg = Increment |
-- Decrement")

--navigation: () -> Html msg
-- navigation should be able to switch us between the states.
navigation = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#")
        ] [text "priorities"],
        -- put some spacing between the links
        text " ",
        a [
            (href "#")
        ] [text "tasks"]
    ]

-- we need to create a state that holds the current state -
-- are we looking at life goals, priorities, tasks, or
-- today? (i.e. create a corresponding version of "type Msg
-- = Increment | Decrement") depending on the state we're
-- in, we need to show the model in a way that is useful,
-- with interactivity.
view: Model -> Html msg
view model =
    div [] [
        text (toString model),
        navigation
    ]


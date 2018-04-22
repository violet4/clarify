import Html exposing (div, text, Html, a, br, span, hr, button, input, form)
import Html.Attributes exposing (href, style, align, id, type_)
import Html.Events exposing (onClick, onSubmit, onInput)

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
    , id: Int
    }
type alias Today = {
    tasks: List Task
    }
type alias Priority = {
    title: String,
    tasks: List Task,
    id: Int
    }
type alias LifeGoal = {
    title: String,
    priorities: List Priority,
    id: Int
    }
type alias Model = {
    life_goals: List LifeGoal,
    today: Today,
    state: Msg,
    debug: String,
    lgid: Int,
    new_life_goal_title: String
    }

type Msg =
    -- simple states
    TodayState
    | CreateState
    | TaskState
    | LifeGoalState
    -- action states
    | CreateLifeGoal
    | UpdateCreateLifeGoalRegister String
    | LifeGoalsState
    | DeleteLifeGoal Int

currentView model =
    case model.state of
        -- each of these takes a model
        TodayState -> todayView model
        TaskState -> viewTasks model
        LifeGoalsState -> lifeGoalsView model
        CreateState -> createView model
        LifeGoalState -> lifeGoalsView model
        DeleteLifeGoal id -> lifeGoalsView model
        _ -> todayView model

viewTasks model = div [fullSizeStyle] [text "TaskState"]

fullSizeStyle = style [("width", "100%"), ("height", "75%")]

todayView: Model -> Html Msg
todayView model = div [fullSizeStyle] [
    if (List.length model.today.tasks) > 0
        then text (toString model.today.tasks)
        else span [] [
            text "You have no tasks for today! Go to ",
            lifeGoalsLinkButton,
            text " to create some!"
        ]
    ]

width100 = style [("width", "100%")]

lifeGoalElement: LifeGoal -> Html Msg
lifeGoalElement lifeGoal =
    div [width100] [
        text lifeGoal.title,
        text " ",
        button [(onClick (DeleteLifeGoal lifeGoal.id))] [text "Delete"]
    ]

lifeGoalsView: Model -> Html Msg
lifeGoalsView model = div [fullSizeStyle]
    (List.append
         (List.map lifeGoalElement model.life_goals)
         [form [onSubmit CreateLifeGoal] [
              input [
                  onInput UpdateCreateLifeGoalRegister
              ] [],
              button [type_ "submit"] [text "Create"]
           ]]
     )


createView model = div [fullSizeStyle] [
        text "createView",
        br [] [],
        a [
            (href "#"),
            (onClick TaskState)
        ] [text "Create Task"],
        br [] [],
        a [
            (href "#"),
            (onClick LifeGoalState)
        ] [text "Create Life Goal"]
    ]

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
                        complete=False,
                        id=2
                    },
                    Task {
                        title="clean 2nd shelf",
                        tasks=[],
                        complete=False,
                        id=3
                    }
                ],
                complete=False,
                id=1
            }
        ] 1
    ] 1
    ] (Today []) TodayState "" 2 ""

-- I read https://www.reddit.com/r/elm/comments/4j2fg6/finding_the_last_list_element/d33671d/
-- and then re-wrote it from scratch myself.
last : List a -> Maybe a
last list =
    case list of
        [] -> Nothing
        [last] -> Just last
        h::t -> last t


-- update the current state, which we use
-- to decide which view to display.
-- here we will also need to use "msg" to be able to
-- add/delete life goals/priorities/tasks
update msg model =
    case msg of
--        TodayState
--        CreateState
--        -- action states
--        LifeGoalsState
        UpdateCreateLifeGoalRegister in_text -> {
            model |
            new_life_goal_title = in_text,
            debug = toString msg
            }
        CreateLifeGoal -> {
            model |
            life_goals = (List.append model.life_goals [LifeGoal model.new_life_goal_title [] model.lgid]),
            lgid = model.lgid + 1,
            new_life_goal_title = "",
            debug = toString msg
            }
        DeleteLifeGoal id -> {
            model |
            state = msg,
            debug = toString msg,
            life_goals = List.filter (\lg -> lg.id /= id) model.life_goals
            }
        _ -> {model | state = msg, debug = toString msg}
-- but this is how we update our model with a new life goal called "cleanliness":
-- { model | life_goals = (LifeGoal "cleanliness" []) :: model.life_goals }
-- we need a "msg" that enumerates the actions we could take at this step,
-- (i.e. create a corresponding version of "type Msg = Increment |
-- Decrement")

htmlAppHeader = div [style [
        ("color", "#1D417D"),
        ("font-size", "28px"),
        ("padding-left", "40%")
    ]] [text "Clarify"]

--navigation: () -> Html msg
-- navigation should be able to switch us between the states.
htmlNavigationBar = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#"),
            (onClick TodayState)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        lifeGoalsLinkButton
    ]

lifeGoalsLinkButton =
    a [
        (href "#"),
        (onClick LifeGoalsState)
    ] [text "Life Goals"]


mainViewHtmlNavigationBar = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#"),
            (onClick TodayState)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        a [
            (href "#")
--            ,
--            (onClick LifeGoalsState)
        ] [text "Life Goals"]
    ]



-- we need to create a state that holds the current state -
-- are we looking at life goals, priorities, tasks, or
-- today? (i.e. create a corresponding version of "type Msg
-- = Increment | Decrement") depending on the state we're
-- in, we need to show the model in a way that is useful,
-- with interactivity.
view: Model -> Html Msg
view model =
    div [style [
        ("width", "100%"),
        ("height", "100%")
    ]] [
        text (toString model.debug),
        htmlAppHeader,
        htmlNavigationBar,
        hr [] [],
--        text (toString model),
        currentView model
    ]


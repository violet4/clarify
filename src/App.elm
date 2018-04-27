import Html exposing (
    div, text, Html, a, br, span, hr, button, input, form,
    select, option
    )
import Html.Attributes exposing (href, style, align, id, type_)
import Html.Events exposing (onClick, onSubmit, onInput)

main = Html.beginnerProgram {
    model=model,
    update=update,
    view=view
    }

-- types:
-- LifeGoal, Priority, Task, Subtask

type alias Task = {
    title: String
    , complete: Bool
    , estimatedMinutes: Int
    , taskID: Int
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
    tasks: List Task,
    state: Msg,
    debug: String,
    lifeGoalID: Int,
    taskID: Int,
    new_life_goal_title: String,
    new_task_title: String
    }

type Msg =
    -- simple states
    TodayState
    | CreateState
    | TaskState
    | LifeGoalState
    -- action states
    | CreateLifeGoal
    | CreateTask
    | UpdateCreateLifeGoalRegister String
    | UpdateTaskRegister String
    | LifeGoalsState
    | DeleteLifeGoal Int

currentView model =
    case model.state of
        -- each of these takes a model
        TodayState -> todayView model
        TaskState -> taskView model
        LifeGoalsState -> lifeGoalsView model
        CreateState -> createView model
        LifeGoalState -> lifeGoalsView model
        DeleteLifeGoal id -> lifeGoalsView model
        _ -> todayView model

taskLifeGoalSelector model taskID =
    select [] (
        List.map
        -- TODO need to map each life goal to its ID and give it a Msg message so we can update the task
        (\lifeGoal -> (option [] [text lifeGoal.title]))
        model.life_goals
    )

tasksToHtmlList model =
    List.map (\task -> [
        text (task.title ++ " "),
        -- ability to select a life goal for this task
        -- TODO this will need to pass in the id of the current task so we can map it to the life goal the user selects
        taskLifeGoalSelector model task.taskID,
        br [] []
    ]) model.tasks

-- tasks view shows all tasks
taskView model =
    div [fullSizeStyle]
        (List.append
            (List.concat
            (tasksToHtmlList model))
            [
                br [] [],
                --text "TaskState",
                form [onSubmit CreateTask] [
                    input [
                        onInput UpdateTaskRegister
                    ] [],
                    button [type_ "submit"] [text "Create"]
                ]
            ]
    )

fullSizeStyle = style [("width", "100%"), ("height", "75%")]

-- today view shows tasks we chose for today
todayView: Model -> Html Msg
todayView model = div [fullSizeStyle]
    (List.map (\x -> text x.title) model.today.tasks)



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


-- default model
model: Model
--model = Model [] (Today [])
model = Model
    -- life_goals
    [
        LifeGoal "cleanliness" [] 0,
        LifeGoal "education" [] 1
    ]
    -- today: Today
    (Today [])
    -- tasks
    [
        (Task "clean desk" False 0 0),
         Task
             "vacuum room" -- title
             False -- complete
             0 -- estimatedMinutes
             1 -- taskID
    ]
    TodayState -- state
    "" -- debug
    2 -- lifeGoalID
    0 -- taskID
    "" -- new_life_goal_title
    "" -- new_task_title

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
            life_goals = (List.append model.life_goals [LifeGoal model.new_life_goal_title [] model.lifeGoalID]),
            lifeGoalID = model.lifeGoalID + 1,
            new_life_goal_title = "",
            debug = toString msg
            }
        UpdateTaskRegister in_text -> {
            model |
            new_task_title = in_text,
            debug = toString msg
            }
        CreateTask -> {
            model |
            tasks = List.append model.tasks [(Task model.new_task_title False 0 model.taskID)],
            taskID = model.taskID + 1,
            new_task_title = "",
            debug = toString msg
            }
        -- TODO if user deletes all life goals, what should we do to the tasks marked as that life goal?
        DeleteLifeGoal id -> {
            model |
            state = msg,
            debug = toString msg,
            life_goals = List.filter (\lifeGoal -> lifeGoal.id /= id) model.life_goals
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

red = style [("color", "red")]
noStyle = style []

--navigation: () -> Html msg
-- navigation should be able to switch us between the states.
htmlNavigationBar model = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#"),
            (onClick TodayState),
            (if model.state == TodayState then red else noStyle)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        todayLinkButton model,
        text " ",
        lifeGoalsLinkButton model
    ]

lifeGoalsLinkButton model =
    a [
        (href "#"),
        (onClick LifeGoalsState),
        (if model.state == LifeGoalsState then red else noStyle)
    ] [text "Life Goals"]

todayLinkButton model =
    a [
        (href "#"),
        (onClick TaskState),
        (if model.state == TaskState then red else noStyle)
    ] [text "Tasks"]


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
        htmlNavigationBar model,
        hr [] [],
        text (toString model),
        hr [] [],
        currentView model
    ]


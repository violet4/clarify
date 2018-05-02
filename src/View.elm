module View exposing (view)

import Html exposing (
    div, text, Html, a, br, hr, button, input, form,
    select, option, map
    )
import Html.Attributes exposing (href, style, align, id, type_, value)
import Html.Events exposing (onClick, onSubmit, onInput)

import Msg exposing (..)
import Model exposing (..)

-- styles ######################################################################
fullSizeStyle = style [("width", "100%"), ("height", "75%"), ("padding-left", "1%"), ("padding-top", "1%"), ("padding-right", "1%")]
width100p = style [("width", "100%")]
htmlAppHeader = div [style [
        ("color", "#1D417D"),
        ("font-size", "30px"),
        ("padding-left", "40%")
    ]] [text "Clarify"]

redFont = style [("color", "red")]
noStyle = style []

currentView model =
    case model.state of
        -- each of these takes a model
        "TodayState" -> todayView model
        "TaskState" -> taskView model
        "LifeGoalsState" -> lifeGoalsView model
        "LifeGoalState" -> lifeGoalsView model
        _ -> todayView model

getLifeGoal goals goalId = 
    case List.head (List.filter (\goal -> goal.id == goalId) goals) of
        Maybe.Just goal -> goal
        Maybe.Nothing -> {title = "Nothing", priorities = [], id = -10}

lifeGoalSelector life_goals=
    -- TODO need to map each life goal to its ID and give it a Msg message so we can update the task
    select [onInput ((UpdateTaskRegister "lifeGoal"))] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id)] [text lifeGoal.title]))
        ({title = "Life Goal Selection", priorities = [], id = 0} :: life_goals)
    )

lifeGoalSelector2 life_goals task =
    -- TODO need to map each life goal to its ID and give it a Msg message so we can update the task
    select [onInput (UpdateTaskGoal task.taskID)] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id)] [text lifeGoal.title]))
        ((getLifeGoal life_goals task.lifeGoalID) :: (List.filter (\goal -> goal.id /= (getLifeGoal life_goals task.lifeGoalID).id) life_goals))
    )

estimatedMinutesSelector task =
    input [
        type_ "number",
        onInput (UpdateTaskEstimatedMinutes task.taskID),
        value (toString task.estimatedMinutes)
    ] []

addRemoveButton150width = style [("width", "150px")]
tasksToHtmlList tasksView model tasks =
    List.map (\task -> [
        text (if model.showDebug then ((toString task.taskID) ++ " ") else ""),
        if (List.member task.taskID model.todayTaskIds)
            then button [
                addRemoveButton150width ,
                onClick (RemoveToday task.taskID)
            ] [text "Remove from Today"]
            else button [
                addRemoveButton150width,
                onClick (AddToday task.taskID)
            ] [text    "Add to Today"],
        text (
            --(toString task.taskID) ++
            "   Title: " ++ task.title ++ "    "),
        -- ability to select a life goal for this task
        -- TODO this will need to pass in the id of the current task so we can map it to the life goal the user selects
        (lifeGoalSelector2 model.life_goals task),
        -- estimated minutes
        estimatedMinutesSelector task,
        br [] []
    ]) tasks


-- tasks view shows all tasks
taskView model =
    div [fullSizeStyle]
        (List.append
            -- list of current tasks
            (List.concat (tasksToHtmlList True model model.tasks))
            -- section to create a new task
            [
                br [] [],
                hr [] [],
                br [] [],

                -- form to create a new task
                form [onSubmit CreateTask] [
                    text "Create a Task",
                    br [] [],
                    --text "TaskState",
                    text "Life Goal: ",
                    lifeGoalSelector model.life_goals,
                    br [] [],
                    br [] [],
                    text "Time: ",
                    input [
                        type_ "number",
                        onInput (UpdateTaskRegister "estimatedMinutes")
                    ][],
                    text " Minutes",
                    br [] [],
                    br [] [],
                    text "Description: ",
                    input [
                        onInput (UpdateTaskRegister "description")
                    ] [],

                    br [] [],
                    br [] [],
                    button [type_ "submit" ] [text "Create"]
                ]
                -- end form to create a new task
         ]
    )

-- today view shows tasks we chose for today
todayView: Model -> Html Msg
todayView model =
    div [fullSizeStyle]
    (if ((List.length model.todayTaskIds) > 0)
             then (List.concat (tasksToHtmlList False model (List.filter (\t -> (List.member t.taskID model.todayTaskIds)) model.tasks)))
             else [
                 text "You don't have any tasks for today!",
                 br [] [],
                 text "Go to ",
                 todayLinkButton model,
                 text " to add some!"
             ])

lifeGoalElement: LifeGoal -> Html Msg
lifeGoalElement lifeGoal =
    div [width100p] [
        text lifeGoal.title,
        text " ",
        button [(onClick (DeleteLifeGoal lifeGoal.id))] [text "Delete"]
    ]

lifeGoalsView: Model -> Html Msg
lifeGoalsView model = div [fullSizeStyle]
    (List.append
         (List.map lifeGoalElement model.life_goals)
         [
            br [] [],
            hr [] [],
            br [] [],
            text "Create a Life Goal",
            br [] [],
            form [onSubmit CreateLifeGoal] [
                text "Description: ",
                input [
                  onInput UpdateCreateLifeGoalRegister
              ] [],
              br [] [],
              br [] [],
              button [type_ "submit"] [text "Create"]
           ]]
     )


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
            (if model.state == "TodayState" then redFont else noStyle)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        todayLinkButton model,
        text " ",
        lifeGoalsLinkButton model,
        text " ",
        a [href "#", onClick UpdateDebug] [text "Debug"]
    ]

lifeGoalsLinkButton model =
    a [
        (href "#"),
        (onClick LifeGoalsState),
        (if model.state == "LifeGoalsState" then redFont else noStyle)
    ] [text "Life Goals"]

todayLinkButton model =
    a [
        (href "#"),
        (onClick TaskState),
        (if model.state == "TaskState" then redFont else noStyle)
    ] [text "Tasks"]

createViewButton model =
    a [
        (href "#"),
        (onClick CreateState),
        (if model.state == "CreateState" then redFont else noStyle)
    ] [text "Create"]


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
    div [fullSizeStyle] [
        text (if model.showDebug then (toString model.debug) else ""),
        htmlAppHeader,
        htmlNavigationBar model,
        hr [] [],
        text (if model.showDebug then (toString model) else ""),
        if model.showDebug then hr [] [] else text "",
        currentView model
    ]

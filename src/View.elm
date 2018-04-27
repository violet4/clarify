module View exposing (view)

import Html exposing (
    div, text, Html, a, br, span, hr, button, input, form,
    select, option
    )
import Html.Attributes exposing (href, style, align, id, type_)
import Html.Events exposing (onClick, onSubmit, onInput)

import Msg exposing (..)
import Model exposing (..)



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


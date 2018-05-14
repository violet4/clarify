module View exposing (view)

import Regex
import Round exposing (round)
import Style exposing (..)
import Html exposing (
    div, text, Html, a, br, hr, button, input, form,
    select, option, map, table, tr, td, tbody,
    textarea, ul, li, h2, span, img
    )
import Html.Attributes exposing (
  href, style, align, id, type_, value, property, attribute, class,
  width, rowspan, src, disabled
  )
import Html.Events exposing (onClick, onSubmit, onInput)

import Msg exposing (..)
import Model exposing (..)


currentView: Model -> Html Msg
currentView model =
    case model.state of
        -- each of these takes a model
        "TodayState" -> todayView model
        "TaskState" -> taskView model False
        "LifeGoalsState" -> lifeGoalsView model
        "LifeGoalState" -> lifeGoalsView model
        "SettingsViewState" -> settingsView model
        "HelpState" -> helpView
        "CompletedState" -> completedView model
        _ -> todayView model

view: Model -> Html Msg
view model =
    div [fullSizeStyle] [
        htmlAppHeader,
        htmlNavigationBar model,
        hr [] [],
        currentView model,
        text (if List.member "Show debug info" model.settings then (toString model.debug) else ""),
        text (if List.member "Show debug info" model.settings then (toString model) else ""),
        if List.member "Show debug info" model.settings then hr [] [] else text ""
    ]

-- styles ######################################################################
fullSizeStyle = style [
    ("width", "98%"),
    ("height", "75%"),
    ("padding-left", "1%"),
    ("padding-top", "1%"),
    ("padding-right", "1%")
    ]

width100p = style [("width", "100%")]

centeredLayout : List Style
centeredLayout =
  [ display flex_
  , justifyContent center
  , alignItems center
  ]

htmlAppHeader = div [style [ display flex_
  , justifyContent center
  , alignItems center
  , color "#1D417D"
  , fontSize "30px"
  , fontFamily "sans-serif"
  , fontWeight "bold"
  ]] [text "Clarify"]


activeTabStyle = style [color "white", fontFamily "sans-serif", fontWeight "bold", fontSize "17px", backgroundColor "#4B206A"]
inactiveTabStyle = style [fontFamily "sans-serif", fontSize "15px"]

buttonStyle = style [fontWeight "600", borderRadius "10px", padding "6px", paddingLeft "10px", paddingRight "10px", backgroundColor "#1D417D", color "white"]
tabStyle = style [borderRadius "2px", padding "4px", marginLeft "-2px", marginRight "-2px", marginBottom "-5px", backgroundColor "#1D417D", borderColor "#1D417D", color "white"]

inputStyle = style[ borderRadius "6px", padding "5px", margin "2px"]


helpView = ul [] [
        li [] [text
            "Data is constantly saved instantly as you type, so you don't need to worry about losing your data. (That doesn't apply to the \"Create a task\" section - that is wiped away when you leave the \"Tasks\" tab.)"],
        li [] [text
            "In the \"View Subtasks\" button (e.g. \"View Subtasks (4/10)\"), the first number 4 is the number of direct subtasks, and the second number 10 is the total number of recursive subtasks"]
    ]

settingsButton model name helpText =
    div [] [
        button [buttonStyle, onClick (ToggleSetting name)] [
            text (if (List.member name model.settings)
                then "Turn off"
                else "Turn on"
            )
            ],
        text (" " ++ name ++ ": " ++ helpText)
    ]

settingsView: Model -> Html Msg
settingsView model =
    div [fullSizeStyle] [
        (settingsButton
            model
            "Hide today tasks from tasks page"
            "When you add a task to today from the today page, it will no longer display on the tasks page"
        ),
        (settingsButton
            model
            "Show debug info"
            "Contains all info about your tasks and life goals"
        ),
        br [] [],

        -- show model so it can be saved/loaded
        text "To load a saved model, first click \"Show model here\" (ignore the output), then click \"Load model\", then paste your model there.",
        br [] [],
        button [id "loadModelButton"] [text "Load model"],
        br [] [],
        button [onClick Noop] [text "Show model here:"],
        br [] [],
        textarea [id "model"] []
        -- ,
        --(settingsButton
        --    model
        --    "Subtask mode"
        --    "Allows tasks to be added underneath other tasks instead of just in a flat list"
        --)
    ]

getLifeGoal goals goalId = 
    case List.head (List.filter (\goal -> goal.id == goalId) goals) of
        Maybe.Just goal -> goal
        Maybe.Nothing -> {title = "Nothing", priorities = [], id = -10}

lifeGoalSelectorForCreating life_goals=
    select [inputStyle, onInput ((UpdateTaskRegister "lifeGoal"))] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id)] [text lifeGoal.title]))
        ({title = "Life Goal Selection", priorities = [], id = 0} :: life_goals)
    )

lifeGoalSelectorForEditing life_goals task completed =
    select [inputStyle, style [Style.width "99%"], (onInput (UpdateTaskGoal task.taskID)), disabled completed] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id), Html.Attributes.selected (lifeGoal.id == task.lifeGoalID)] [text lifeGoal.title]))
        life_goals
    )

-- move task below one of its siblings
-- list titles of siblings
-- include a Msg that sets parent
newParentSelector model task =
    select
        [onInput (MoveTaskDown task.taskID)]
        ((option [Html.Attributes.selected True] [text "Move down"])
        :: List.map
            (\t -> option [value (toString t.taskID)] [text ((String.slice 0 10 t.title) ++ "...")])
            -- get the siblings of this task
            (List.filter
                (\t -> t.parentTaskId == task.parentTaskId && t.taskID /= task.taskID)
                model.tasks
            ))

estimatedMinutesSelector task completed =
    input [
        inputStyle,
        style [Style.width "80%"],
        type_ "number",
        onInput (UpdateTaskEstimatedMinutes task.taskID),
        value (toString task.estimatedMinutes),
        Html.Attributes.min "0",
        Html.Attributes.max "9999",
        disabled completed
    ] []

sortSelectorButton fieldName =
    button [tabStyle, style [ marginLeft "1px", marginRight "1px"], onClick (ChangeTaskSorting fieldName)] [text fieldName]

sortBySelectorButtons model =
    div [] [
        text "Sort by: ",
        sortSelectorButton "None",
        sortSelectorButton "Eisenhower",
        sortSelectorButton "Life Goal",
        sortSelectorButton "Estimated Minutes",
        sortSelectorButton "Description"
    ]

addRemoveButton150width = style [Style.width "150px", alignItems "center"]

taskTodayMatchesViewState: Model -> Task -> Bool
taskTodayMatchesViewState model task =
    if List.member "Hide today tasks from tasks page" model.settings
        then
            case model.state of
                "TodayState" -> List.member task.taskID model.todayTaskIds
                _ -> not (List.member task.taskID model.todayTaskIds)
        else True


solidBlackBorderStyle = style [
        ("border", "1px solid black")
    ]

wide99percentStyle = style [("width", "99%")]

countDirectSubtasks tasks taskID =
    List.length (List.filter (\t -> t.parentTaskId == taskID) tasks)

countAllSubtasks tasks taskID =
    let
        directSubtasks = List.filter (\t -> t.parentTaskId == taskID) tasks
        directSubtaskIds = List.map (\t -> t.taskID) directSubtasks
        directSubtaskIdCount = List.length directSubtaskIds
    in
        if directSubtaskIdCount == 0
        then 0
        else List.foldr
            (\thisTaskId count -> count + (countAllSubtasks tasks thisTaskId))
            directSubtaskIdCount
            directSubtaskIds

height100p = style [("height", "100%")]

taskToTableRow: Model -> Task -> Bool -> Html Msg
taskToTableRow model task completed =
    let
        allTasksMode = not (List.member "Subtask Mode" model.settings)
    in
    tr [] [


        -- "view subtasks" button
        td [
            onClick (ViewSubTasks task.taskID),
            class "button",
            class "noselect"
        ] [
            button [
                style [
                    ("border", "none"),
                    ("background", "none"),
                    ("color", "white"),
                    ("white-space", "nowrap"),
                    ("padding", "0")
                    --("overflow", "hidden")
                ],
                class "verticalLeft"
            ] [text (
                "Subtasks ("
                ++ (toString (countDirectSubtasks model.tasks task.taskID))
                ++ "/"
                ++ (toString (countAllSubtasks model.tasks task.taskID))
                ++ ")")
            ]
        ],


        td [class "taskButtons"] [
            text (
                if List.member "Show debug info" model.settings
                then ((toString task.taskID) ++ " (p " ++ (toString task.parentTaskId) ++ ")")
                else ""),

            -- "add/remove from today" button
            (if completed
            then text ""
            else if (List.member task.taskID model.todayTaskIds)
                then button [buttonStyle,
                    onClick (RemoveToday task.taskID),
                    class "taskButton"
                ] [text "Remove from Today"]
                else button [
                    buttonStyle,
                    onClick (AddToday task.taskID),
                    class "taskButton"
                ] [text "Add to Today"]),

            -- "move up" button
            (if completed
            then text ""
            else button [
                buttonStyle,
                onClick (MoveTaskUp task.taskID),
                class "taskButton"
            ] [text "Move up"]),

            -- "Move down" selector.
            -- ability to move a task to be a subtask
            -- of one of its siblings
            (if completed
            then text ""
            else newParentSelector model task),

            -- important/urgent
            div [style [("width", "100%"), ("height", "100%"), ("text-align", "center")]] [
                (if task.urgent then urgentIcon else notUrgentIcon) task.taskID,
                (if task.important then importantIcon else notImportantIcon) task.taskID
            ],

            -- "View Siblings" button, if on the today page,
            -- or if showing all tasks on the tasks page
            if (allTasksMode || model.state == "TodayState") then
                button [
                    onClick (ViewSubTasks task.parentTaskId),
                    buttonStyle,
                    class "taskButton"
                ] [
                    text ("View Siblings ("
                        -- don't include itself
                        ++ (toString (-1 + (countDirectSubtasks model.tasks task.parentTaskId)))
                        ++ ")")
                ]
            else text "",

            -- ability to select a life goal for this task
            (lifeGoalSelectorForEditing model.life_goals task completed),

            -- estimated minutes
            estimatedMinutesSelector task completed,
            br [] [],
            text "(Minutes)",
            br [] [],

            -- un-complete button
            (if completed
            then button [
                    buttonStyle,
                    class "taskButton",
                    onClick (UncompleteTask task.taskID)
                ] [text "Un-complete"]
            else text ""),

            -- delete/complete button
            (if completed
            then
                button [buttonStyle,
                    onClick (DeleteTask task.taskID),
                    class "taskButton"
                ] [text "Delete"]
            else button [
                    onClick (CompleteTask task.taskID),
                    class "taskButton",
                    buttonStyle
                ] [text "Complete"])

        ],

        -- display/edit the task description
        td [wide99percentStyle, class "taskText"] [
            textarea [
                class "taskText",
                --inputStyle,
                Html.Attributes.defaultValue task.title,
                disabled completed,
                onInput (UpdateTaskDescription task.taskID)
            ] []
        ]


    ]


taskListToHtmlTable model tasks completed =
    let
        numRows = 1 + (List.length tasks)
    in
    Html.table [
        inputStyle, width100p
    ] (

        -- this whole section is for the left side button
        -- for going "Go up"
        (tr [] [
            if numRows == 1
            then text "" --button [onClick UpOneLevel] [text "Go up"]
            else td [
                onClick UpOneLevel,
                class "buttonLeft",
                class "button",
                class "noselect",
                rowspan numRows
            ] [
                button [
                    style [
                        ("border", "none"),
                        ("background", "none"),
                        ("color", "white"),
                        ("white-space", "nowrap"),
                        ("padding", "0")
                        --("overflow", "hidden")
                    ],
                    class "verticalLeft"
                ] [text "Go up"]
            ]
        ]) ::

        -- list of tasks, turned into table rows
        (List.map (\t -> taskToTableRow model t completed) tasks)
    )

eisenhowerSort: Task -> (Int, Int)
eisenhowerSort task =
    ((if task.important then (
        if task.urgent then 1
        else 2
    )
    else (
        if task.urgent then 3
        else 4
    )), task.taskID)

sortTasks: Model -> List Task -> List Task
sortTasks model tasks =
    -- life goal
    if List.member "Life Goal" model.settings
    then (List.sortBy .lifeGoalID tasks)

    -- eisenhower
    else if List.member "Eisenhower" model.settings
    then (List.sortBy eisenhowerSort tasks)

    -- estimated minutes
    else if List.member "Estimated Minutes" model.settings
    then (List.sortBy .estimatedMinutes tasks)

    -- description
    else if List.member "Description" model.settings
    then (List.sortBy .title tasks)

    -- none (taskId)
    else (List.sortBy .taskID tasks)

taskFilterTextInput =
    input [inputStyle,onInput FilterTasks] []

getFullTaskText: Model -> Task -> String
getFullTaskText model task =
    let
        lifeGoalID = task.lifeGoalID
        lifeGoalStringList = List.filter (\lg -> lg.id == lifeGoalID) model.life_goals
        lifeGoal =
            case List.head lifeGoalStringList of
                Nothing -> LifeGoal "" [] -1
                Just lg -> lg
        lifeGoalString = lifeGoal.title
    in
        lifeGoalString ++ " " ++ task.title

taskMatchesFilters: Task -> Model -> List String -> Bool
taskMatchesFilters task model filterParts =
    let
        fullTaskText = getFullTaskText model task
        fp = List.map (\p -> Regex.contains (Regex.regex p) fullTaskText) filterParts
    in
        List.all (\x -> x) fp

filterTasks: List Task -> Model -> List String -> List Task
filterTasks tasks model settings =
    let
        settingsThatStartWithFilter = List.filter (\s -> String.startsWith "filter " s) settings
        filterFilterString =
            case List.head settingsThatStartWithFilter of
                Nothing -> ""
                Just s -> s
        fullFilterText = String.dropLeft 7 filterFilterString
        filterParts = String.split " " fullFilterText
    in
        -- need to filter out tasks by the filterParts
        List.filter (\t -> taskMatchesFilters t model filterParts) tasks

completedView: Model -> Html Msg
completedView model =
    taskView model True

getParentTasks: List Task -> Int -> List Task
getParentTasks tasks parentTaskID =
    let
        parentTaskInList = List.filter (\t -> t.taskID == parentTaskID) tasks
    in
        case List.head parentTaskInList of
            Nothing -> []
            Just parentTask ->
                List.concat [getParentTasks tasks parentTask.parentTaskId, parentTaskInList]

inlineBlueH2 string =
    h2 [style [("color", "blue"), ("display", "inline")]] [text string]

-- tasks view shows all tasks
taskView: Model -> Bool -> Html Msg
taskView model completed =
    let
        subtaskMode = List.member "Subtask Mode" model.settings
        allTasksMode = not subtaskMode

        tasks = if not completed then model.tasks else model.completed_tasks
        subTasksOfThisParentIfSubtaskMode =
            -- don't filter completed tasks or in all-tasks mode
            if completed || allTasksMode
            then tasks
            else List.filter (\t -> t.parentTaskId == model.viewingParentTaskId) tasks

        -- hide tasks from tasks page if it's on the today page (if user preference is set)
        taskViewTasks = (List.filter (\t -> taskTodayMatchesViewState model t) subTasksOfThisParentIfSubtaskMode)
        filteredTaskViewTasks = filterTasks taskViewTasks model model.settings
        sortedTaskViewTasks = sortTasks model filteredTaskViewTasks

        parentTasks = getParentTasks model.tasks model.viewingParentTaskId

    in
    div [fullSizeStyle] 
        (List.append
            [
                -- estimated minutes sum
                text "Estimated minutes for displayed tasks (not including visible parent tasks): ",
                tasksEstimatedMinutesSumText sortedTaskViewTasks,
                text (" (" ++ (Round.round 2 ((toFloat (tasksEstimatedMinutesSum sortedTaskViewTasks))/60)) ++ " hours)"),
                br [] [],

                -- sorting buttons
                sortBySelectorButtons model,
                br [] [],

                -- task mode button (all tasks vs parent tasks)
                button [
                    onClick ToggleSubtaskViewMode,
                    buttonStyle
                ] [
                    text (
                        if subtaskMode then "Switch to All-Task Mode" else "Switch to Subtask Mode"
                    )
                ],
                br [] [],


                -- filter tasks (text input)
                text "Filter Tasks: ",
                taskFilterTextInput,
                br [] [],

                -- "completed page" warning about not editing tasks
                (if completed
                then span [style [("color", "red")]] [text "WARNING: Fields cannot be edited on this page! Only deleted or Un-completed!"]
                else text ""),
                br [] [],
                br [] [],

                -- "go up" buttons
                button [onClick TopLevel, buttonStyle, width100p] [text "Top Level"],
                button [onClick UpOneLevel, buttonStyle, width100p] [text "Go up"],

                -- parent tasks
                (if not completed && subtaskMode && List.length parentTasks > 0
                then div [] [
                    inlineBlueH2 "Hierarchy above current subtask view:",
                    taskListToHtmlTable model parentTasks completed,
                    inlineBlueH2 "Subtasks:"
                ]
                else text ""),

                -- list of current tasks
                (if List.length sortedTaskViewTasks > 0
                then taskListToHtmlTable model sortedTaskViewTasks completed
                else h2 [style [("color", "blue")]] [text "No tasks at this depth"])
            ]
            -- section to create a new task
            (if completed then [] else
            [
                br [] [],
                -- form to create a new task
                form [onSubmit CreateTask] [
                    h2 [style [("display", "inline")]] [text "Create a Task"],
                    br [] [],
                    --text "TaskState",
                    text "Life Goal",
                    br [] [],
                    lifeGoalSelectorForCreating model.life_goals,
                    br [] [],
                    text "Estimated Minutes",
                    br [] [],
                    input [
                        inputStyle,
                        type_ "number",
                        onInput (UpdateTaskRegister "estimatedMinutes"),
                        Html.Attributes.min "0",
                        Html.Attributes.max "9999"
                    ][],
                    br [] [],
                    text "Description",
                    br [] [],
                    input [
                        inputStyle,
                        onInput (UpdateTaskRegister "description")
                    ] [],

                    br [] [],
                    br [] [],
                    button [buttonStyle, type_ "submit" ] [text "Create"]
                ]
                -- end form to create a new task
            ])
        )

tasksEstimatedMinutesSum tasks =
    (List.foldl
         -- sum up the estimated minutes
         (\t1 t2 -> {estimatedMinutes=t1.estimatedMinutes + t2.estimatedMinutes})
         {estimatedMinutes=0}
         tasks
     ).estimatedMinutes

tasksEstimatedMinutesSumText tasks =
    text (toString (tasksEstimatedMinutesSum tasks))

-- today view shows tasks we chose for today
todayView: Model -> Html Msg
todayView model = div [fullSizeStyle] (
    let
        at_least_one_task = ((List.length model.todayTaskIds) > 0)
        todayTasks = List.filter (\t -> (List.member t.taskID model.todayTaskIds)) model.tasks
        filteredTodayTasks = filterTasks todayTasks model model.settings
        sortedTaskViewTasks = sortTasks model filteredTodayTasks

    in if at_least_one_task
        then ([
            div [] [
                text "Estimated minutes for displayed tasks: ",
                tasksEstimatedMinutesSumText sortedTaskViewTasks,
                text " (",
                text (Round.round 2 ((toFloat ((tasksEstimatedMinutesSum sortedTaskViewTasks))) / 60)),
                text " hours)"
            ],
            sortBySelectorButtons model,
            br [] [],

            -- filter text input
            text "Filter Tasks: ",
            taskFilterTextInput,
            taskListToHtmlTable model sortedTaskViewTasks False
        ])
        else [
            text "You don't have any tasks for today!",
            br [] [],
            text "Go to ",
            todayLinkA model,
            text " to add some!"
        ]
    )

lifeGoalElement: LifeGoal -> Html Msg
lifeGoalElement lifeGoal =
    div [width100p] [
        button [buttonStyle, (onClick (DeleteLifeGoal lifeGoal.id))] [text "Delete"],
        text " ",
        input [inputStyle, Html.Attributes.defaultValue lifeGoal.title, onInput (UpdateLifeGoalDescription lifeGoal.id)] []
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
            form [onInput UpdateCreateLifeGoalRegister,
                value model.new_life_goal_title] [],
            form [onSubmit CreateLifeGoal] [
                text "Description: ",
                input [
                    inputStyle,
                    value model.new_life_goal_title,
                    onInput UpdateCreateLifeGoalRegister
              ] [],
              br [] [],
              br [] [],
              button [buttonStyle, type_ "submit"] [text "Create"]
           ]]
     )


--navigation: () -> Html msg
-- navigation should be able to switch us between the states.
htmlNavigationBar model = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        
        button [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (tabStyle),
            (onClick TodayState),
            (if model.state == "TodayState" then activeTabStyle else inactiveTabStyle)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        todayLinkButton model,
        text " ",
        completedTabButton model,
        text " ",
        lifeGoalsLinkButton model,
        text " ",
        settingsLinkButton model,
        text " ",
        helpLinkButton model
        --text " ",
        --a [href "#", onClick UpdateDebug] [text "Debug"]
        
    ]

completedTabButton model =
    button [
        (tabStyle),
        (onClick CompletedState),
        (if model.state == "CompletedState" then activeTabStyle else inactiveTabStyle)
    ] [text "Completed"]

helpLinkButton model =
    button [
        (tabStyle),
        (onClick HelpState),
        (if model.state == "HelpState" then activeTabStyle else inactiveTabStyle)
    ] [text "Help"]

lifeGoalsLinkButton model =
    button [
        (tabStyle),
        (onClick LifeGoalsState),
        (if model.state == "LifeGoalsState" then activeTabStyle else inactiveTabStyle)
    ] [text "Life Goals"]

settingsLinkButton model =
    button [
        (tabStyle),
        (onClick SettingsViewState),
        (if model.state == "SettingsViewState" then activeTabStyle else inactiveTabStyle)
    ] [text "Settings"]

todayLinkA model =
    a [
        (onClick TaskState),
        (if model.state == "TaskState" then activeTabStyle else inactiveTabStyle)
    ] [text "Tasks"]

todayLinkButton model =
    button [
        (tabStyle),
        (onClick TaskState),
        (if model.state == "TaskState" then activeTabStyle else inactiveTabStyle)
    ] [text "Tasks"]

createViewButton model =
    button [
        (tabStyle),
        (onClick CreateState),
        (if model.state == "CreateState" then activeTabStyle else inactiveTabStyle)
    ] [text "Create"]


mainViewHtmlNavigationBar = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
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

makeIcon path msg = img [src path, Html.Attributes.height 25, onClick msg] []

importantIcon taskId = makeIcon "images/important.png" (ToggleImportance taskId)
notImportantIcon taskId = makeIcon "images/notImportant.png" (ToggleImportance taskId)
urgentIcon taskId = makeIcon "images/urgent.png" (ToggleUrgency taskId)
notUrgentIcon taskId = makeIcon "images/notUrgent.png" (ToggleUrgency taskId)


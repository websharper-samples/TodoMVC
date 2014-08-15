namespace PrettyTodo

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.UI.Next
open IntelliFactory.WebSharper.UI.Next.Html
open IntelliFactory.WebSharper.UI.Next.Notation

[<JavaScript>]
module Client =

    let Section = Html.Elements.Section
    let (==>) = Attr.Create
    let txt = Doc.TextNode

    // --------------------------------------------------------
    // Model
    // --------------------------------------------------------

    // Filter types
    type Filter = | All | Active | Completed

    // Editing states
    type State = | TodoActive | TodoComplete | TodoEditing | TodoCompleteEditing

    // Running IDs
    let mutable runningId = 0
    let fresh () =
        let res = runningId
        runningId <- runningId + 1
        res

    // Todo item model
    type TodoItem =
        {
            Id : int
            Text : Var<string>
            TodoState : Var<State>
        }

    // Todo item constructor
    let mkTodo text =
        {
            Id = fresh ()
            Text = Var.Create text
            TodoState = Var.Create TodoActive
        }

    // Returns true if the item is done
    let isDone st =
        match st with
        | TodoActive -> false | TodoEditing -> false
        | TodoComplete -> true | TodoCompleteEditing -> true

    // Filters a list given a predicate
    let FilterList filter xs =
        List.ofSeq xs
        |> List.filter (fun x ->
            match filter with
            | All -> true
            | Active -> not (isDone x)
            | Completed -> isDone x)
        |> Seq.ofList

    // Removes all completed items from the model
    let RemoveCompleted (model: ListModel<int, TodoItem>) =
        model.Iter (fun x -> if isDone x.TodoState.Value then model.Remove x)

    // Marks all items in the model as done
    let MarkAllDone (model: ListModel<int, TodoItem>) =
        model.Iter (fun x -> Var.Set x.TodoState TodoComplete)
        //model.Iter (fun x -> model.Add { x with TodoState = TodoComplete })

    // A view of the number of items in the list that aren't done
    let notDoneItems (model: ListModel<int, TodoItem>) =
        let toAdd st =
            if isDone st then 0 else 1

        ListModel.View model
        |> View.Bind (fun xs ->
            Seq.fold (fun acc x ->
                let viewSt = x.TodoState.View
                View.Map2 (fun acc vst -> acc + toAdd vst) acc viewSt) 
                    (View.Const 0) xs)

    // --------------------------------------------------------
    // View
    // --------------------------------------------------------

    // Renders a to-do item.
    let RenderTodo (model: ListModel<int,TodoItem>) (filterView: View<Filter>) (item: TodoItem) =

        // UpdateVar is a var connected to the "edit" input field
        let updateVar = Var.Create ""
        // SubmitFn modifies the model with the new value
        let submitFn (evt: Dom.Event) st =
            let (ke: int) = evt?keyCode
            let (wh: int) = evt?which
            let newState =
                match st with
                | TodoCompleteEditing -> TodoComplete
                | _ -> TodoActive

            if ke = 13 || wh = 13 then
                Var.Set item.Text updateVar.Value
                Var.Set item.TodoState newState
                Var.Set updateVar ""

        // The attributes for a given state
        let stateAttr st =
            match st with
            | TodoActive -> Attr.Empty
            | TodoEditing -> Attr.Class "editing"
            | TodoComplete -> Attr.Class "completed"
            | TodoCompleteEditing -> Attr.Concat [Attr.Class "editing" ; Attr.Class "completed"]

        let checkedAttr st =
            if isDone st then Attr.Create "checked" "" else Attr.Empty

        // Toggles whether an item is done
        let toggleDone st =
            let newState =
                match st with
                | TodoActive -> TodoComplete | TodoEditing -> TodoComplete
                | TodoComplete -> TodoActive | TodoCompleteEditing -> TodoActive
            Var.Set item.TodoState newState

        // Places an item into the "editing" state
        let startEditing st =
            let newState =
                if isDone st then TodoCompleteEditing else TodoEditing
            Var.Set item.TodoState newState

        let shouldShow st filter =
            match (filter, st) with
            | (All, _) -> true
            | (Active, state) -> not (isDone state)
            | (Completed, state) -> isDone state

        // Actually views an item.
        View.Map2 (fun st filter ->
            if shouldShow st filter then
                LI [stateAttr st] [
                    Div [Attr.Class "view"] [
                        Elements.Input [
                            Attr.Class "toggle";
                            "type" ==> "checkbox"
                            checkedAttr st
                            Attr.Handler "click" (fun (evt: Dom.Event) -> toggleDone st)
                        ] []
                        Elements.Label [
                            Attr.Handler "dblclick" (fun (evt: Dom.Event) -> startEditing st)
                        ] [Doc.TextView item.Text.View]
                        Doc.Button "" [Attr.Class "destroy"] (fun () -> model.Remove item)
                    ]
                    Doc.Input [
                        Attr.Handler "keypress" (fun (evt: Dom.Event) -> submitFn evt st)
                        Attr.Class "edit"
                    ] updateVar
                ]
            else Doc.Empty
        ) item.TodoState.View filterView
        |> Doc.EmbedView

    // Renders a list
    let RenderList model filter =
        ListModel.View model
        //|> View.Map2 FilterList filter
        // ConvertSeqBy allows us to detect changes in the todo items, projecting
        // these as views to the rendering function.
        |> Doc.ConvertBy (fun x -> x.Id) (RenderTodo model filter)

    // Puts everything together
    let TodoApp =
        let model = ListModel.Create (fun todo -> todo.Id) []

        // filterVar: variable containing the current filter criterion
        let filterVar = Var.Create All
        // todoVar: variable containing the value of the new todo box
        let todoVar = Var.Create ""

        // Submission function, adds the new todo item to the model
        let submitFn (evt: Dom.Event) =
            let (ke: int) = evt?keyCode
            let (wh: int) = evt?which
            if ke = 13 || wh = 13 then
                model.Add (Var.Get todoVar |> mkTodo)
                Var.Set todoVar ""

        // Rendering -- it's likely some of this could be shifted to static
        // HTML, but it's short enough to just have it all here
        Doc.Concat [
            Section ["id" ==> "todoapp"] [
                Elements.Header ["id" ==> "header"] [

                    H10 [txt "todos"]
                    Doc.Input
                        [
                            "id" ==> "new-todo"
                            "placeholder" ==> "What needs to be done?"
                            "autofocus" ==> ""
                            // Add the handler -- in UI.Next, these are treated
                            // as attributes
                            Attr.Handler "keypress" submitFn
                        ] todoVar
                ]

                Section ["id" ==> "main"] [
                    Elements.Input
                        [
                            "id" ==> "toggle-all"
                            "type" ==> "checkbox"
                            Attr.Handler "click" (fun (evt: Dom.Event) -> MarkAllDone model)
                        ] []
                    Elements.Label ["for" ==> "toggle-all"] [txt "Mark all as complete"]
                    UL ["id" ==> "todo-list"] [
                        // Renders the current model
                        RenderList model filterVar.View
                    ]
                ]

                Elements.Footer ["id" ==> "footer"] [
                    Span ["id" ==> "todo-count"] [
                        // Embed the counter of items remaining
                        notDoneItems model
                        |> View.Map (fun num ->
                            Doc.TextNode <| (string num) + " items remaining"
                        ) |> Doc.EmbedView
                    ]
                    UL ["id" ==> "filters"] [
                        // Filter functions, which set the current filter variable
                        LI0 [Doc.Link "All" [] (fun () -> Var.Set filterVar All)]
                        LI0 [Doc.Link "Active" [] (fun () -> Var.Set filterVar Active)]
                        LI0 [Doc.Link "Completed" [] (fun () -> Var.Set filterVar Completed)]
                    ]

                    // Clears the completed items
                    Doc.Button "Clear Completed" ["id" ==> "clear-completed"]
                        (fun () -> RemoveCompleted model)
                ]
            ]

            Elements.Footer ["id" ==> "info"] [
                P0 [txt "Double-click to edit a todo"]
            ]
        ]

    let Main =
        Doc.RunById "main" TodoApp
namespace PrettyTodo

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module Client =

    let (==>) = Attr.Create

    let onEnter (evt : Dom.KeyboardEvent) fn =
        if evt.KeyCode = 13 || evt.Which = 13 then
            fn ()

    let onEscape (evt : Dom.KeyboardEvent) fn =
        if evt.KeyCode = 27 || evt.Which = 27 then
            fn ()

    [<Inline "$e.focus()">]
    let focus (e: Dom.Element) = X<unit>

    // --------------------------------------------------------
    // Model
    // --------------------------------------------------------

    // Filter types
    type Filter = | All | Active | Completed

    // Editing states
    type State = | TodoActive | TodoComplete | TodoEditing | TodoCompleteEditing

    // RouteMap for hash routing
    let Pages =
        RouteMap.Create 
            (function
                | All -> []
                | Active -> ["active"]
                | Completed -> ["completed"])
            (function
                | ["active"] -> Active
                | ["completed"] -> Completed
                | _ -> All)

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

    // Checks if every item in the model is marked done
    let allDone model =
        model
        |> Seq.forall (fun el -> isDone el.TodoState.Value)

    // Removes all completed items from the model
    let RemoveCompleted (model: ListModel<int, TodoItem>) =
        model.Iter (fun x -> if isDone x.TodoState.Value then model.Remove x)

    // Marks all items in the model as done or if every item is done then marks them active
    let ToggleAllDone (model: ListModel<int, TodoItem>) =
        let notDone = 
            model.TryFind (fun e -> not <| isDone e.TodoState.Value)
            |> Option.isSome

        let newState = if notDone then TodoComplete else TodoActive
        model.Iter (fun x -> Var.Set x.TodoState newState)

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

    type TodoAppTemplate = Templating.Template<"todoapp.html">

    let ShowIfNotEmpty model doc =
        ListModel.View model
        |> View.Map (fun mdl ->
            if Seq.isEmpty mdl then Doc.Empty
            else doc
        )
        |> Doc.EmbedView

    // Renders a to-do item.
    let RenderTodo (model: ListModel<int,TodoItem>) (filterView: View<Filter>) (item: TodoItem) =

        let endEditing st te =
            if te = "" then model.Remove item
            else
                let newState =
                    match st with
                    | TodoCompleteEditing -> TodoComplete
                    | _ -> TodoActive
            
                Var.Set item.Text te
                Var.Set item.TodoState newState

        // UpdateVar is a var connected to the "edit" input field
        // SubmitFn modifies the model with the new value
        let submitFn (evt: Dom.KeyboardEvent) st initial te =
            onEnter evt <| fun () ->
                endEditing st te
            onEscape evt <| fun () ->
                endEditing st initial

        // The attributes for a given state
        let stateAttr st =
            match st with
            | TodoActive -> ""
            | TodoEditing -> "editing"
            | TodoComplete -> "completed"
            | TodoCompleteEditing -> "editing completed"

        // Toggles whether an item is done
        let toggleDone st =
            let newState =
                match st with
                | TodoActive -> TodoComplete | TodoEditing -> TodoComplete
                | TodoComplete -> TodoActive | TodoCompleteEditing -> TodoActive
            Var.Set item.TodoState newState

        let isChecked st =
            match st with
            | TodoActive | TodoEditing -> false
            | TodoComplete | TodoCompleteEditing -> true

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
            let titleEditing = Var.Create item.Text.Value
            let inp =
                Doc.Input [
                    attr.``class`` "edit"
                    on.keyDown (fun el ev ->
                        submitFn ev st item.Text.Value titleEditing.Value)
                    on.blur (fun _ _ ->
                        endEditing st item.Text.Value)
                    on.afterRender focus
                ] titleEditing
                
            if shouldShow st filter then
                TodoAppTemplate.ListItem()
                    .ItemState(stateAttr st)
                    .Title(item.Text.View)
                    .Destroy(fun _ -> model.Remove item)
                    .Edit(fun _ -> startEditing st)
                    .CheckedClicked(fun _ -> toggleDone st)
                    .Checked(Var.Create <| isChecked st)
                    .TitleEditing(inp)
                    .Doc()
            else Doc.Empty
        ) item.TodoState.View filterView
        |> Doc.EmbedView

    // Renders a list
    let RenderList (model: ListModel<int, TodoItem>) filter =
        // Doc.BindListModel allows us to detect changes in the todo items, projecting
        // these as views to the rendering function.
        model |> Doc.BindListModel (RenderTodo model filter)

    let TodoApp =
        let model = ListModel.Create (fun todo -> todo.Id) []

        // filterVar: variable containing the current filter criterion
        let filterVar = RouteMap.Install Pages
        // todoVar: variable containing the value of the new todo box
        let todoVar = Var.Create ""
        let allDoneVar = Var.Create false

        let doneItems = 
            ListModel.View model 
            |> View.Bind (Seq.map (fun e -> e.TodoState.View)
                            >> Seq.map (View.Map isDone)
                            >> View.Sequence)
        let allDone = 
            doneItems 
            |> View.Map (fun d ->
                let all = Seq.forall id d
                Var.Set allDoneVar all
                all)

        // Submission function, adds the new todo item to the model
        let submitFn (evt: Dom.KeyboardEvent) =
            onEnter evt <| fun () ->
                let text = todoVar.Value.Trim ()
                if text <> "" then
                    model.Add <| mkTodo text
                    Var.Set todoVar ""

        let notDone = notDoneItems model
        let selected e = Attr.DynamicClass "selected" filterVar.View ((=)e)

        TodoAppTemplate()
            .NewTask(todoVar)
            .TodoList(RenderList model filterVar.View)
            .Add(fun e -> submitFn e.Event)
            .ToggleAll(
                ShowIfNotEmpty model
                    (allDone
                     |> View.Map (fun ad ->
                        Doc.CheckBox [
                            "class" ==> "toggle-all"
                            "type" ==> "checkbox"
                            Attr.Handler "click" (fun el evt -> ToggleAllDone model)
                        ] allDoneVar)
                     |> Doc.EmbedView) 
            )
            .Footer(
                ShowIfNotEmpty model
                    (TodoAppTemplate.FooterTemplate()
                        .Remaining(notDone |> View.Map string)
                        .RemainingLabel(notDone |> View.Map (fun n -> if n = 1 then "item" else "items"))
                        .Filters(
                            [
                                li [] [Doc.Link "All" [ selected All ] (fun () -> Var.Set filterVar All)]
                                li [] [Doc.Link "Active" [ selected Active ] (fun () -> Var.Set filterVar Active)]
                                li [] [Doc.Link "Completed" [ selected Completed ] (fun () -> Var.Set filterVar Completed)] 
                            ])
                        .ClearCompleted(
                            doneItems
                                |> View.Map (fun s ->
                                    if s |> Seq.exists id then
                                        Doc.Button "Clear Completed" ["class" ==> "clear-completed"]
                                            (fun () -> RemoveCompleted model)
                                    else Doc.Empty)
                                |> Doc.EmbedView
                        )
                        .Doc()
                    )
            )
            .Doc()

    [<SPAEntryPoint>]
    let Main() =
        Doc.RunById "main" TodoApp
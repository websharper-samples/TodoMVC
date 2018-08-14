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
            Text : string
            IsDone : bool
            IsEditing : bool
        }

    // Todo item constructor
    let mkTodo text =
        {
            Id = fresh ()
            Text = text
            IsDone = false
            IsEditing = false
        }

    // Removes all completed items from the model
    let RemoveCompleted (model: ListModel<int, TodoItem>) =
        model.RemoveBy (fun x -> x.IsDone)

    // Marks all items in the model as done or if every item is done then marks them active
    let ToggleAllDone (model: ListModel<int, TodoItem>) =
        let notDone = 
            model.TryFind (fun e -> not e.IsDone)
            |> Option.isSome
        model.UpdateAll (fun x -> Some { x with IsDone = notDone })

    // A view of the number of items in the list that aren't done
    let notDoneItemsCount (model: ListModel<int, TodoItem>) =
        ListModel.View model
        |> View.Map (fun xs ->
            xs |> Seq.sumBy (fun x ->
                if x.IsDone then 0 else 1))

    // A view of whether any items are done
    let hasAnyDoneItems (model: ListModel<int, TodoItem>) =
        ListModel.View model
        |> View.Map (Seq.exists (fun x -> x.IsDone))


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
    let RenderTodo (model: ListModel<int,TodoItem>) (filterView: View<Filter>) (itemKey: int) (item: Var<TodoItem>) =

        let endEditing te =
            if te = "" then
                model.RemoveByKey itemKey
            else
                item.Update (fun item ->
                    { item with Text = te; IsEditing = false })

        // UpdateVar is a var connected to the "edit" input field
        // SubmitFn modifies the model with the new value
        let submitFn (evt: Dom.KeyboardEvent) initial te =
            onEnter evt <| fun () ->
                endEditing te
            onEscape evt <| fun () ->
                endEditing initial

        // The attributes for a given state
        let stateAttr st =
            String.concat " " [
                if st.IsEditing then yield "editing"
                if st.IsDone then yield "completed"
            ]

        // Toggles whether an item is done
        let toggleDone() =
            item.Update(fun x -> { x with IsDone = not x.IsDone })

        let titleEditing = Var.Create ""

        // Places an item into the "editing" state
        let startEditing() =
            item.Update(fun x ->
                Var.Set titleEditing x.Text
                { x with IsEditing = true })

        let shouldShow item filter =
            match filter with
            | All -> true
            | Active -> not item.IsDone
            | Completed -> item.IsDone

        let inp =
            Doc.Input [
                attr.``class`` "edit"
                on.keyDownView item.View (fun el ev item ->
                    submitFn ev item.Text titleEditing.Value)
                on.blurView item.View (fun _ _ item ->
                    endEditing item.Text)
                on.afterRender focus
            ] titleEditing

        let itemDisplay =
            TodoAppTemplate.ListItem()
                .ItemState(View.Map stateAttr item.View)
                .Title(item.View |> View.Map (fun item -> item.Text))
                .Destroy(fun _ -> model.RemoveByKey itemKey)
                .Edit(fun _ -> startEditing())
                .Checked(Var.Lens item (fun item -> item.IsDone) (fun item x -> { item with IsDone = x }))
                .TitleEditing(inp)
                .Doc()

        // Actually views an item.
        View.Map2 (fun item filter ->
            if shouldShow item filter then
                itemDisplay
            else
                Doc.Empty
        ) item.View filterView
        |> Doc.EmbedView

    // Renders a list
    let RenderList (model: ListModel<int, TodoItem>) filter =
        // Doc.BindListModelLens allows us to detect changes in the todo items, projecting
        // these as views to the rendering function.
        model |> Doc.BindListModelLens (RenderTodo model filter)

    let TodoApp =
        let model = ListModel.Create (fun todo -> todo.Id) []

        // filterVar: variable containing the current filter criterion
        let filterVar = RouteMap.Install Pages
        // todoVar: variable containing the value of the new todo box
        let todoVar = Var.Create ""
        let allDoneVar = Var.Create false

        let notDone = notDoneItemsCount model
        let allDone = notDone |> View.Map (fun c -> c = 0)
        let hasDone = hasAnyDoneItems model

        // Submission function, adds the new todo item to the model
        let submitFn (evt: Dom.KeyboardEvent) =
            onEnter evt <| fun () ->
                let text = todoVar.Value.Trim ()
                if text <> "" then
                    model.Add <| mkTodo text
                    Var.Set todoVar ""

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
                            hasDone
                                |> View.Map (fun s ->
                                    if s then
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
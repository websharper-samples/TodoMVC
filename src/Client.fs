namespace TodoMVC

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Templating

/// TodoMVC built with WebSharper.UI: a reactive ListModel of entries plus
/// hash routing for the All / Active / Completed filters.
[<JavaScript>]
module Client =

    type MasterTemplate = Templating.Template<"wwwroot/index.html", ClientLoad.FromDocument>

    type Filter =
        | All
        | Active
        | Completed

    let private Pages =
        RouteMap.Create
            (function
                | All -> []
                | Active -> [ "active" ]
                | Completed -> [ "completed" ])
            (function
                | [ "active" ] -> Active
                | [ "completed" ] -> Completed
                | _ -> All)

    type TodoItem =
        {
            Id: int
            Text: string
            IsDone: bool
            IsEditing: bool
            Edit: string
        }

    let private RemoveCompleted (model: ListModel<int, TodoItem>) =
        model.RemoveBy (fun item -> item.IsDone)

    let private SetAllDone (model: ListModel<int, TodoItem>) (done': bool) =
        model.UpdateAll (fun item -> Some { item with IsDone = done' })

    let private RenderTodo
        (model: ListModel<int, TodoItem>)
        (filter: View<Filter>)
        (itemKey: int)
        (item: Var<TodoItem>)
        =
        let commit () =
            item.Update (fun i -> { i with Text = i.Edit; IsEditing = false })

        let cancel () =
            item.Update (fun i -> { i with IsEditing = false })

        let hidden =
            View.Map2
                (fun (i: TodoItem) f ->
                    match f with
                    | All -> false
                    | Active -> i.IsDone
                    | Completed -> not i.IsDone)
                item.View
                filter

        MasterTemplate.Entry()
            .Label(text item.V.Text)
            .CssAttrs(
                Attr.ClassPred "completed" item.V.IsDone,
                Attr.ClassPred "editing" item.V.IsEditing,
                Attr.DynamicClassPred "hidden" hidden)
            .IsCompleted(Var.Lens item (fun i -> i.IsDone) (fun i x -> { i with IsDone = x }))
            .EditingTask(V item.V.Edit, fun s -> item.Update (fun i -> { i with Edit = s }))
            .EditKeyup(fun e ->
                match e.Event.Key with
                | "Enter" -> commit ()
                | "Escape" -> cancel ()
                | _ -> ())
            .EditBlur(fun _ -> commit ())
            .Remove(fun _ -> model.RemoveByKey itemKey)
            .StartEdit(fun _ ->
                item.Update (fun i -> { i with IsEditing = true; Edit = i.Text }))
            .Doc()

    [<SPAEntryPoint>]
    let Main () =
        let model = ListModel.Create (fun (item: TodoItem) -> item.Id) []
        let filter = RouteMap.Install Pages
        let newTask = Var.Create ""
        let nextId = ref 0

        let notDone =
            ListModel.View model
            |> View.Map (Seq.sumBy (fun i -> if i.IsDone then 0 else 1))

        let allDone =
            notDone |> View.Map (fun n -> n = 0)

        let addEntry () =
            let text = newTask.Value.Trim()
            if text <> "" then
                model.Add
                    {
                        Id = nextId.Value
                        Text = text
                        IsDone = false
                        IsEditing = false
                        Edit = ""
                    }
                nextId.Value <- nextId.Value + 1
                newTask.Value <- ""

        MasterTemplate()
            .Entries(model |> Doc.BindListModelLens (RenderTodo model filter.View))
            .Task(newTask)
            .Edit(fun e ->
                if e.Event.Key = "Enter" then
                    addEntry ()
                    e.Event.PreventDefault())
            .IsCompleted(allDone, SetAllDone model)
            .ItemsLeft(
                notDone
                |> View.Map (fun n ->
                    match n with
                    | 1 -> "1 item left"
                    | n -> string n + " items left"))
            .ClearCompleted(fun _ -> RemoveCompleted model)
            .CssFilterAll(Attr.DynamicClassPred "selected" (filter.View |> View.Map ((=) All)))
            .CssFilterActive(Attr.DynamicClassPred "selected" (filter.View |> View.Map ((=) Active)))
            .CssFilterCompleted(Attr.DynamicClassPred "selected" (filter.View |> View.Map ((=) Completed)))
            .Bind()

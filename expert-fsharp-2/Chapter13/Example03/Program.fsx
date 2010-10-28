// Expert F# 2.0
// Chapter 13 Example 03

open System
open System.Threading

open System.ComponentModel
open System.Windows.Forms

type IterativeBackgroundWorker<'T>(oneStep:('T -> 'T),
                                   initialState:'T,
                                   numIterations:int) =

    let worker =
        new BackgroundWorker(WorkerReportsProgress=true,
                             WorkerSupportsCancellation=true)

    // The constructor captures the synchronization context. This allows us to post
    // messages back to the GUI thread where the BackgroundWorker was created.
    let syncContext = SynchronizationContext.Current
    do if syncContext = null then failwith "no synchronization context found"

    let started = new Event<_>()
    let completed = new Event<_>()
    let error     = new Event<_>()
    let cancelled = new Event<_>()
    let progress  = new Event<_>()

    do worker.DoWork.Add(fun args ->
        syncContext.Post(SendOrPostCallback(fun _ -> started.Trigger(DateTime.Now)),
                         state=null)
        let rec iterate state i =
            // At the end of the computation terminate the recursive loop
            if worker.CancellationPending then
               args.Cancel <- true
            elif i < numIterations then
                // Compute the next result
                let state' = oneStep state

                // Report the percentage computation and the internal state
                let percent = int ((float (i+1)/float numIterations) * 100.0)
                do worker.ReportProgress(percent, box state);

                // Compute the next result
                iterate state' (i+1)
            else
                args.Result <- box state

        iterate initialState 0)

    do worker.RunWorkerCompleted.Add(fun args ->
        if args.Cancelled       then cancelled.Trigger()
        elif args.Error <> null then error.Trigger args.Error
        else completed.Trigger (args.Result :?> 'T))

    do worker.ProgressChanged.Add(fun args ->
        progress.Trigger (args.ProgressPercentage,(args.UserState :?> 'T)))

    member x.Started             = started.Publish
    member x.WorkerCompleted  = completed.Publish
    member x.WorkerCancelled  = cancelled.Publish
    member x.WorkerError      = error.Publish
    member x.ProgressChanged  = progress.Publish

    // Delegate the remaining members to the underlying worker
    member x.RunWorkerAsync()    = worker.RunWorkerAsync()
    member x.CancelAsync()       = worker.CancelAsync()

open System.Drawing
open System.Windows.Forms

let form = new Form(Visible=true,TopMost=true)

let panel = new FlowLayoutPanel(Visible=true,
                                Height = 20,
                                Dock=DockStyle.Bottom,
                                BorderStyle=BorderStyle.FixedSingle)

let progress = new ProgressBar(Visible=false,
                               Anchor=(AnchorStyles.Bottom ||| AnchorStyles.Top),
                               Value=0)

let text = new Label(Text="Paused",
                     Anchor=AnchorStyles.Left,
                     Height=20,
                     TextAlign= ContentAlignment.MiddleLeft)

panel.Controls.Add(progress)
panel.Controls.Add(text)
form.Controls.Add(panel)

let fibOneStep (fibPrevPrev:bigint,fibPrev) = (fibPrev, fibPrevPrev+fibPrev)

// Run the iterative algorithm 500 times before reporting intermediate results
let rec repeatMultipleTimes n f s = 
    if n <= 0 then s else repeatMultipleTimes (n-1) f (f s)

// Burn some additional cycles to make sure it runs slowly enough
let rec burnSomeCycles n f s = 
    if n <= 0 then f s else ignore (f s); burnSomeCycles (n-1) f s

let step = (repeatMultipleTimes 500 (burnSomeCycles 1000 fibOneStep))

// Create the iterative worker.
let worker = new IterativeBackgroundWorker<_>(step,(1I,1I),80)

worker.ProgressChanged.Add(fun (progressPercentage,state)->
    progress.Value <- progressPercentage)

worker.WorkerCompleted.Add(fun (_,result) ->
    progress.Visible <- false;
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Result = %A" result) |> ignore)

worker.WorkerCancelled.Add(fun () ->
    progress.Visible <- false;
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Cancelled OK!") |> ignore)

worker.WorkerError.Add(fun exn ->
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Error: %A" exn) |> ignore)

form.Menu <- new MainMenu()
let workerMenu = form.Menu.MenuItems.Add("&Worker")

workerMenu.MenuItems.Add(new MenuItem("Run",onClick=(fun _ args ->
    text.Text <- "Running";
    progress.Visible <- true;
    worker.RunWorkerAsync())))


workerMenu.MenuItems.Add(new MenuItem("Cancel",onClick=(fun _ args ->
    text.Text <- "Cancelling";
    worker.CancelAsync())))

form.Closed.Add(fun _ -> worker.CancelAsync())

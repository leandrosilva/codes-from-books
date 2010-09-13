//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Main

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.Windows.Forms
open Microsoft.Practices.ParallelGuideSamples.ImagePipeline

type ImageMode =
    | Sequential = 0
    | Pipelined = 1
    | LoadBalanced = 2

type Mode =
    | Stopped = 0
    | Running = 1
    | Stopping = 2

// This sample does not use the MVP pattern to separate concerns of the view from
// the model. This is largerly for reasons of simplicity in the sample code.
// For further discussion of MVVM and MVP patterns see Appendix A. For an example
// of MVVM is a WPF application see the Chapter 5 code.
type Program() =
    let form = new Gui.MainForm()
    let mainTask : Task ref = ref null
    let cts : CancellationTokenSource ref = ref null
    let imageMode = ref ImageMode.Pipelined
    let mode = ref Mode.Stopped
    let sw = new Stopwatch()
        
    let mutable imagesSoFar = 0
    let totalTime = [| 0; 0; 0; 0; 0; 0; 0; 0 |]

    let updateEnabledStatus() =
        form.radioButtonSequential.Enabled <- (!mode = Mode.Stopped)
        form.radioButtonPipeline.Enabled <- (!mode = Mode.Stopped)
        form.radioButtonLoadBalanced.Enabled <- (!mode = Mode.Stopped)
        form.buttonStart.Enabled <- (!mode = Mode.Stopped)
        form.buttonStop.Enabled <- (!mode = Mode.Running)
        form.quitButton.Enabled <- (!mode = Mode.Stopped)
        form.radioButtonSequential.Checked <- (!imageMode = ImageMode.Sequential)
        form.radioButtonPipeline.Checked <- (!imageMode = ImageMode.Pipelined)
        form.radioButtonLoadBalanced.Checked <- (!imageMode = ImageMode.LoadBalanced)

    let setBitmap (imageInfo:ImageInfo) =
        // initialization
        let priorImage = form.pictureBox1.Image
        form.pictureBox1.Image <- imageInfo.FilteredImage
        imageInfo.FilteredImage <- null
        imagesSoFar <- imagesSoFar + 1

        // calculate duration of each phase
        for i in 0 .. 3 do 
            let time = imageInfo.PhaseEndTick.[i] - imageInfo.PhaseStartTick.[i]
            totalTime.[i] <- totalTime.[i] + time
 
        // infer queue wait times by comparing phase(n+1) start with phase(n) finish timestamp
        totalTime.[4] <- totalTime.[4] + imageInfo.PhaseStartTick.[1] - imageInfo.PhaseEndTick.[0]
        totalTime.[5] <- totalTime.[5] + imageInfo.PhaseStartTick.[2] - imageInfo.PhaseEndTick.[1]
        totalTime.[6] <- totalTime.[6] + imageInfo.PhaseStartTick.[3] - imageInfo.PhaseEndTick.[2]

        form.textBoxPhase1AvgTime.Text <- (totalTime.[0] / imagesSoFar).ToString()
        form.textBoxPhase2AvgTime.Text <- (totalTime.[1] / imagesSoFar).ToString()
        form.textBoxPhase3AvgTime.Text <- (totalTime.[2] / imagesSoFar).ToString()
        form.textBoxPhase4AvgTime.Text <- (totalTime.[3] / imagesSoFar).ToString()

        form.textBoxQueue1AvgWait.Text <- (totalTime.[4] / imagesSoFar).ToString()
        form.textBoxQueue2AvgWait.Text <- (totalTime.[5] / imagesSoFar).ToString()
        form.textBoxQueue3AvgWait.Text <- (totalTime.[6] / imagesSoFar).ToString()

        form.textBoxQueueCount1.Text <- imageInfo.QueueCount1.ToString()
        form.textBoxQueueCount2.Text <- imageInfo.QueueCount2.ToString()
        form.textBoxQueueCount3.Text <- imageInfo.QueueCount3.ToString()

        form.textBoxFileName.Text <- imageInfo.FileName
        form.textBoxImageCount.Text <- imageInfo.ImageCount.ToString()

        let elapsedTime = sw.ElapsedMilliseconds
        form.textBoxFps.Text <- String.Format("{0: 0}", elapsedTime / int64 imageInfo.ImageCount)                 

        if priorImage <> null then priorImage.Dispose()
        if imageInfo.SequenceNumber <> imagesSoFar - 1 then
            let msg = 
              String.Format("Program error-- images are out of order. Expected {0} but received {1}",
                            imagesSoFar - 1, imageInfo.SequenceNumber)
            MessageBox.Show(msg, "Application Error", MessageBoxButtons.OK, MessageBoxIcon.Asterisk) |> ignore
            Application.Exit()

    let updateFn (bm:ImageInfo) =
        form.Invoke(new Action(fun () -> 
            setBitmap bm
            )) |> ignore

    let errorFn (ex:Exception) =
        form.Invoke(new Action(fun () -> 
              MessageBox.Show(ex.Message, "Application Error", MessageBoxButtons.OK, MessageBoxIcon.Asterisk) |> ignore
              cts := null
              mainTask := null
              mode := Mode.Stopped
              updateEnabledStatus() ), ()) |> ignore

    let startProcessing() =
        if !mainTask = null then
            mode := Mode.Running
            updateEnabledStatus()                
            cts := new CancellationTokenSource()
            let enumVal = int !imageMode
            sw.Restart()
            imagesSoFar <- 0
            for i in 0 .. totalTime.Length - 1 do totalTime.[i] <- 0
            mainTask := 
                Task.Factory.StartNew(new Action(fun () -> 
                    Pipeline.imagePipelineMainLoop updateFn (!cts).Token enumVal errorFn),
                    (!cts).Token, TaskCreationOptions.LongRunning, TaskScheduler.Default)

    let stopProcessing() =
        if !cts <> null then
            mode := Mode.Stopping
            updateEnabledStatus()
            (!cts).Cancel()
            Task.Factory.StartNew(fun () ->
                (!mainTask).Wait()
                form.Invoke(new Action(fun () ->
                    cts := null
                    mainTask := null
                    mode := Mode.Stopped
                    updateEnabledStatus()), null) |> ignore ) |> ignore

    do 
        updateEnabledStatus()
        form.buttonStart.Click.Add(fun _ -> startProcessing())
        form.buttonStop.Click.Add(fun _ -> stopProcessing())
        form.quitButton.Click.Add(fun _ -> Application.Exit())
        form.radioButtonSequential.CheckedChanged.Add(fun _ ->
            if form.radioButtonSequential.Checked then
                imageMode := ImageMode.Sequential )
        form.radioButtonPipeline.CheckedChanged.Add(fun _ ->
            if form.radioButtonPipeline.Checked then
                imageMode := ImageMode.Pipelined )
        form.radioButtonLoadBalanced.CheckedChanged.Add(fun _ ->
            if form.radioButtonLoadBalanced.Checked then
                imageMode := ImageMode.LoadBalanced )

    member x.MainForm = form

/// The main entry point for the application.
[<STAThread>]
do
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    let prog = new Program()
    Application.Run(prog.MainForm)

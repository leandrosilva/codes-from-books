//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Pipeline

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Drawing
open System.IO
open System.Linq
open System.Threading
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Extensions

let QueueBoundedCapacity = 4
let LoadBalancingDegreeOfConcurrency = 2
let MaxNumberOfImages = 500
let GaussianNoiseAmount = 50.0 

// ------------------------------------------------------------------------------
// Error handling, disposal & iteration utilities
// ------------------------------------------------------------------------------

/// Helper function that ensures that an object is disposed
/// but only when some exception ocurrs when running the body
let disposeOnException f (obj:#IDisposable) =
    try f obj
    with _ -> 
      obj.Dispose()  
      reraise()

/// Handling of exceptions that is used in every phase
/// When exception occurs, we cancel the processing and when operation
/// finishes, we call 'CompleteAdding' on the current queue
let handlePhaseExceptions (current:BlockingCollection<_>) (cts:CancellationTokenSource) f = 
    try
        try f()
        with e -> 
            cts.Cancel()
            if not (e :? OperationCanceledException) then reraise()
    finally 
        if current <> null then current.CompleteAdding()

/// Ensure that the queue contents is disposed. You could also implement this by 
/// subclassing BlockingCollection<> and providing an IDisposable implmentation.
let disposeImagesInQueue (queue:BlockingCollection<ImageInfo>) =
    if queue <> null then
        queue.CompleteAdding()
        for info in queue do (info :> IDisposable).Dispose()

module Seq =
    /// Iterates over the specified sequence and stops the 
    /// iteration when the provided body returns false
    let iterBreak f (seq:seq<_>) = 
        use en = seq.GetEnumerator()
        let mutable run = true
        while en.MoveNext() && run do
            run <- f en.Current

// ------------------------------------------------------------------------------
// Operations for individual images
// ------------------------------------------------------------------------------

[<SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope")>]
let loadImage fname sourceDir count clockOffset =
    let startTick = Environment.TickCount
    let info = 
        new Bitmap(Path.Combine(sourceDir, fname)) |> disposeOnException (fun bitmap ->
            bitmap.Tag <- fname
            let info = new ImageInfo(count, fname, bitmap, clockOffset)
            info.PhaseStartTick.[0] <- startTick - clockOffset 
            info )
    info.PhaseEndTick.[0] <- Environment.TickCount - clockOffset
    info 


let scaleImage (info:ImageInfo) =
    let startTick = Environment.TickCount
    let orig = info.OriginalImage
    info.OriginalImage <- null
    let scale = 200
    let isLandscape = (orig.Width > orig.Height)
    let newWidth = if isLandscape then scale else scale * orig.Width / orig.Height
    let newHeight = if not isLandscape then scale else scale * orig.Height / orig.Width
    let bitmap = new Bitmap(orig, newWidth, newHeight)
    try
        bitmap.AddBorder(15) |> disposeOnException (fun bitmap2 ->
            bitmap2.Tag <- orig.Tag
            info.ThumbnailImage <- bitmap2
            info.PhaseStartTick.[1] <- startTick - info.ClockOffset )
    finally
        bitmap.Dispose()
        orig.Dispose()
    info.PhaseEndTick.[1] <- Environment.TickCount - info.ClockOffset


let filterImage (info:ImageInfo) =
    let startTick = Environment.TickCount
    let sc = info.ThumbnailImage
    info.ThumbnailImage <- null
    sc.AddNoise(GaussianNoiseAmount) |> disposeOnException (fun bitmap ->
        bitmap.Tag <- sc.Tag
        info.FilteredImage <- bitmap
        info.PhaseStartTick.[2] <- startTick - info.ClockOffset )
    sc.Dispose()
    info.PhaseEndTick.[2] <- Environment.TickCount - info.ClockOffset


let displayImage (info:ImageInfo) count displayFn duration =
    let startTick = Environment.TickCount
    info.ImageCount <- count
    info.PhaseStartTick.[3] <- startTick - info.ClockOffset
    info.PhaseEndTick.[3] <- 
      if  duration > 0 then startTick - info.ClockOffset + duration
      else Environment.TickCount - info.ClockOffset
    displayFn info

// ------------------------------------------------------------------------------
// The pipeline phases
// ------------------------------------------------------------------------------

/// Image pipeline phase 1: Load images from disk and put them a queue.
let loadPipelinedImages (fileNames:seq<_>) sourceDir 
        (original:BlockingCollection<ImageInfo>) (cts:CancellationTokenSource) =
    let clockOffset = Environment.TickCount
    let token = cts.Token

    let fileEnumerator = fileNames.GetEnumerator()
    let rec loop count = 
        match fileEnumerator.MoveNext() with 
        | _ when token.IsCancellationRequested -> ()
        | false -> ()
        | true ->
            loadImage fileEnumerator.Current sourceDir count clockOffset |> disposeOnException (fun info ->
                original.Add(info, token) )
            loop (count + 1)

    // Run the loop and handle exceptions
    handlePhaseExceptions original cts 
        (fun () -> loop 0 )


/// Image pipeline phase 2: Scale to thumbnail size and render picture frame.
let scalePipelinedImages (originalImages:BlockingCollection<ImageInfo>)
        (thumbnailImages:BlockingCollection<ImageInfo>) (cts:CancellationTokenSource) =
    let token = cts.Token
    handlePhaseExceptions thumbnailImages cts (fun () ->
        originalImages.GetConsumingEnumerable() |> Seq.iterBreak (fun info ->
            if token.IsCancellationRequested then false else
            info |> disposeOnException (fun _ ->
                scaleImage info
                thumbnailImages.Add(info, token) )
            true ) )


/// Image pipeline phase 3: Filter images (give them a 
/// speckled appearance by adding Gaussian noise)
let filterPipelinedImages (thumbnailImages:BlockingCollection<ImageInfo>) 
        (filteredImages:BlockingCollection<ImageInfo>) (cts:CancellationTokenSource) =
    let token = cts.Token
    handlePhaseExceptions filteredImages cts (fun () ->
        thumbnailImages.GetConsumingEnumerable() |> Seq.iterBreak (fun info ->
            if token.IsCancellationRequested then false else
            info |> disposeOnException (fun _ ->
                filterImage info
                filteredImages.Add(info, token) )
            true ) )


/// Image pipeline phase 4: Invoke the user-provided callback 
/// function (for example, to display the result in a UI)
let displayPipelinedImages filteredImages displayFn updateStatisticsFn 
        (cts:CancellationTokenSource) =
    let count = ref 1
    let duration = ref 0
    let token = cts.Token
    handlePhaseExceptions null cts (fun () ->
        filteredImages |> Seq.iterBreak (fun info ->
            if token.IsCancellationRequested then false else
            info |> disposeOnException (fun _ ->
                let displayStart = Environment.TickCount
                updateStatisticsFn info
                displayImage info (!count) displayFn (!duration)
                duration := Environment.TickCount - displayStart
                incr count 
                true ) ) )
 

// ------------------------------------------------------------------------------
// Implementation of the pipeline (sequential, parallel, ballanced parallel)
// ------------------------------------------------------------------------------
        
/// <summary>
///   Run the image processing pipeline (sequential)
/// </summary>
/// <param name="fileNames">List of image file names in source directory</param>
/// <param name="sourceDir">Name of directory of source images</param>
/// <param name="displayFn">Display action</param>
/// <param name="cts">Cancellation token</param>
let runSequential (fileNames:seq<_>) sourceDir displayFn (cts:CancellationTokenSource) =
    let clockOffset = Environment.TickCount
    let token = cts.Token

    let fileEnumerator = fileNames.GetEnumerator()
    
    let rec loop count duration = 
        match fileEnumerator.MoveNext() with 
        | _ when token.IsCancellationRequested -> ()
        | false -> ()
        | true ->
            use info = loadImage fileEnumerator.Current sourceDir count clockOffset
            scaleImage info
            filterImage info
            let displayStart = Environment.TickCount
            displayImage info (count + 1) displayFn duration
            loop (count + 1) (Environment.TickCount - displayStart)
        | _ -> ()
    loop 0 0


/// <summary>
///   Run the image processing pipeline (parallel)
/// </summary>
/// <param name="fileNames">List of image file names in source directory</param>
/// <param name="sourceDir">Name of directory of source images</param>
/// <param name="queueLength">Length of image queue</param>
/// <param name="displayFn">Display action</param>
/// <param name="cts">Cancellation token</param>
let runPipelined fileNames sourceDir (queueLength:int) displayFn (cts:CancellationTokenSource) = 
    // Blocking collections to hold result of individual phases
    let originalImages  = new BlockingCollection<_>(queueLength)
    let thumbnailImages = new BlockingCollection<_>(queueLength)
    let filteredImages  = new BlockingCollection<_>(queueLength)

    try
        let f = new TaskFactory(TaskCreationOptions.LongRunning, TaskContinuationOptions.None)

        // Phase 1: Load images into the input collection
        let loadTask = f.StartNew(fun () ->
            loadPipelinedImages fileNames sourceDir originalImages cts)
        // Phase 2: Scale to thumbnail size
        let scaleTask = f.StartNew(fun () ->
            scalePipelinedImages originalImages thumbnailImages cts)
        // Phase 3: Add Gausian noise
        let filterTask = f.StartNew(fun () ->
            filterPipelinedImages thumbnailImages filteredImages cts)
        // Phase 4: Display the image in the GUI
        let displayTask = f.StartNew(fun () ->
            displayPipelinedImages 
                (filteredImages.GetConsumingEnumerable())
                displayFn (fun info -> 
                    info.QueueCount1 <- originalImages.Count
                    info.QueueCount2 <- thumbnailImages.Count
                    info.QueueCount3 <- filteredImages.Count ) cts)

        Task.WaitAll(loadTask, scaleTask, filterTask, displayTask)
    finally
        // In case of exception or cancellation, there 
        // might be bitmaps that need to be disposed
        disposeImagesInQueue originalImages
        disposeImagesInQueue thumbnailImages
        disposeImagesInQueue filteredImages                


/// <summary>
///   Run a variation of the pipeline that uses a user-specified 
///   number of tasks for the filter stage (ballanced parallel version)
/// </summary>
/// <param name="fileNames">List of image file names in source directory</param>
/// <param name="sourceDir">Name of directory of source images</param>
/// <param name="queueLength">Length of image queue</param>
/// <param name="displayFn">Display action</param>
/// <param name="cts">Cancellation token</param>
/// <param name="filterTaskCount">Number of filter tasks</param>
let runLoadBalancedPipeline fileNames sourceDir (queueLength:int) displayFn (cts:CancellationTokenSource) filterTaskCount =

    // Blocking collections to hold result of individual phases
    let originalImages = new BlockingCollection<ImageInfo>(queueLength)
    let thumbnailImages = new BlockingCollection<ImageInfo>(queueLength)
    let filteredImageMultiplexer = 
        new BlockingMultiplexer<ImageInfo>((fun info -> info.SequenceNumber), 0, queueLength)
    let filteredImagesCollections = Array.zeroCreate filterTaskCount
    let f = new TaskFactory( CancellationToken.None, TaskCreationOptions.LongRunning, 
                             TaskContinuationOptions.None, TaskScheduler.Default )

    try
        // Store all tasks in array, generate indices for array
        let tasks = Array.zeroCreate (filterTaskCount + 3)
        let taskId =
            let st = ref (-1)
            (fun () -> incr st; !st)

        // Phase 1: Load images into the input collection
        tasks.[taskId()] <- f.StartNew(fun () ->
            loadPipelinedImages fileNames sourceDir originalImages cts)
        // Phase 2: Scale to thumbnail size
        tasks.[taskId()] <- f.StartNew(fun () ->
            scalePipelinedImages originalImages thumbnailImages cts)
        // Phase 3: Add Gausian noise (load ballanced)
        for i in 0 .. filterTaskCount - 1 do
            filteredImagesCollections.[i] <- filteredImageMultiplexer.GetProducerQueue()
            tasks.[taskId()] <- f.StartNew(fun () -> 
                filterPipelinedImages thumbnailImages filteredImagesCollections.[i] cts)
        // Phase 4: Display the image in the GUI
        tasks.[taskId()] <- f.StartNew(fun () ->
            displayPipelinedImages 
                (filteredImageMultiplexer.GetConsumingEnumerable()) 
                displayFn (fun info -> 
                    info.QueueCount1 <- originalImages.Count
                    info.QueueCount2 <- thumbnailImages.Count
                    info.QueueCount3 <- filteredImageMultiplexer.Count ) cts )

        Task.WaitAll(tasks)
    finally
        // There might be cleanup in the case of cancellation or an exception
        disposeImagesInQueue originalImages
        disposeImagesInQueue thumbnailImages
        for filteredImages in filteredImagesCollections do
            disposeImagesInQueue filteredImages
        for info in filteredImageMultiplexer.GetCleanupEnumerable() do
            (info :> IDisposable).Dispose()
            
// ------------------------------------------------------------------------------
// Image Pipeline Top Level Loop
// ------------------------------------------------------------------------------

/// <summary>
///   Runs the image pipeline example. The program goes through the jpg images located in the SourceDir
///   directory and performs a series of steps: it resizes each image and adds a black border and then applies
///   a Gaussian noise filter operation to give the image a grainy effect. Finally, the program invokes 
///   a user-provided delegate to the image (for example, to display the image on the user interface).
/// 
///   Images are processed in sequential order. That is, the display delegate will be 
///   invoked in exactly the same order as the images appear in the file system.
/// </summary>
///
/// <param name="displayFn">
///   A function that is invoked for each image at the end of the pipeline, 
///   for example, to display the image in the user interface.
/// </param>
/// <param name="algorithmChoice">
///   The method of calculation. 0=sequential, 1=pipeline, 2=load balanced pipeline
/// </param>
/// <param name="errorFn">
///   A function that will be invoked if this method or any of its parallel 
///   subtasks observe an exception during their execution.
/// </param>
/// <param name="token">A token that can signal an external cancellation request.</param>
[<SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")>]
let imagePipelineMainLoop displayFn token algorithmChoice errorFn =
    try
        let sourceDir = Directory.GetCurrentDirectory()
        // Ensure that frames are presented in sequence before invoking the user-provided display function.
        let imagesSoFar = ref 0
        let safeDisplayFn (info:ImageInfo) =
            if info.SequenceNumber <> !imagesSoFar then
                failwithf "Images processed out of order. Saw %d, expected %d" info.SequenceNumber (!imagesSoFar)
            displayFn info
            incr imagesSoFar

        // Create a cancellation handle for inter-task signaling of exceptions. This cancellation
        // handle is also triggered by the incoming token that indicates user-requested
        // cancellation.
        use cts = CancellationTokenSource.CreateLinkedTokenSource([| token |]) 
        let fileNames = SampleUtilities.GetImageFilenames sourceDir MaxNumberOfImages
        match algorithmChoice with
        | 0 -> runSequential fileNames sourceDir safeDisplayFn cts
        | 1 -> runPipelined fileNames sourceDir QueueBoundedCapacity safeDisplayFn cts
        | 2 -> runLoadBalancedPipeline fileNames sourceDir QueueBoundedCapacity safeDisplayFn cts LoadBalancingDegreeOfConcurrency
        | _ -> failwith "invalid case"
    with 
    | :? AggregateException as ae when ae.InnerExceptions.Count = 1 ->
        errorFn (ae.InnerExceptions.[0])
    | e -> errorFn e

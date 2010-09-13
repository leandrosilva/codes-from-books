//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Implementation of the main data analysis component (contains sequential 
// version, parallel version using tasks and asynchronous workflow version)
// ------------------------------------------------------------------------------

/// Component for data analysis
/// Implements the IAnalaysisEngine interface
type AnalysisEngine(speedFactor:float) =

    let guiDispatch = Windows.Threading.Dispatcher.CurrentDispatcher
    let mutable cts : CancellationTokenSource = null

    // --------------------------------------------------------------------------
    // Various helper functions for analysis engine
    // --------------------------------------------------------------------------

    // Generate sample data for market analysis
    let generateSecurities exchange size =
      new ResizeArray<_>
        ([ for i in 0 .. size - 1 do
              yield { name = exchange + " Stock " + string i
                      priceHistory = [ 0.0; 1.0; 2.0 ] } ])

    /// Register a function as a notification to be called when task finishes
    let addNotification (task:Task<_>) f =
        task.ContinueWith
          ( new Action<Task<_>>(fun (t:Task<_>) -> f t.Result), CancellationToken.None, 
            TaskContinuationOptions.OnlyOnRanToCompletion, 
            TaskScheduler.FromCurrentSynchronizationContext()) |> ignore

    /// Trigger event on the GUI thread using a captured dispatcher
    let triggerGuiEvent (e:Event<_>) v = 
        guiDispatch.Invoke(new Action(fun () -> 
              e.Trigger(v) ), [| |]) |> ignore

    /// Constructs workflow that triggers the specified event 
    /// on the GUI thread when the wrapped async completes 
    let triggerWhenCompletes (e:Event<_>) (a:Async<_>) = async {
        let! res = a
        triggerGuiEvent e res
        return res }

    /// Utility function that creates a long running task from F# function
    let taskLong (f:unit -> 'T) = 
        Task<'T>.Factory.StartNew(new Func<'T>(f), TaskCreationOptions.LongRunning)

    // --------------------------------------------------------------------------
    // Generate sample data for analysis
    // --------------------------------------------------------------------------

    let makeNyseSecurityInfo() =
        generateSecurities "NYSE" 100

    let makeNasdaqSecurityInfo() =
        generateSecurities "NASDAQ" 100

    let makeFedSecurityInfo() =
        generateSecurities "" 100

    // Synchronous I/O intensive analysis methods

    let loadNyseData() =
        SampleUtilities.DoIoIntensiveOperation 2.5 cts.Token false |> ignore 
        if cts.Token.IsCancellationRequested then null
        else new StockDataCollection(makeNyseSecurityInfo())

    let loadNasdaqData() =
        SampleUtilities.DoIoIntensiveOperation (2.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else new StockDataCollection(makeNasdaqSecurityInfo())

    let loadFedHistoricalData() = 
        SampleUtilities.DoIoIntensiveOperation (3.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else new StockDataCollection(makeFedSecurityInfo())

    // Asynchronous I/O intensive analysis methods (F# specific)

    let asyncLoadNyseData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperation 2.5 cts.Token false 
        if cts.Token.IsCancellationRequested then return null
        else return new StockDataCollection(makeNyseSecurityInfo()) }

    let asyncLoadNasdaqData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperation (2.0 * speedFactor) cts.Token false 
        if cts.Token.IsCancellationRequested then return null
        else return new StockDataCollection(makeNasdaqSecurityInfo()) }

    let asyncLoadFedHistoricalData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperation (3.0 * speedFactor) cts.Token false 
        if cts.Token.IsCancellationRequested then return null
        else return new StockDataCollection(makeFedSecurityInfo()) }

    // --------------------------------------------------------------------------
    // CPU intensive analysis methods
    // --------------------------------------------------------------------------

    let mergeMarketData allMarketData =
        SampleUtilities.DoCpuIntensiveOperation (2.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else new StockDataCollection(new ResizeArray<_>(Seq.concat allMarketData))

    let normalizeData marketData =
        SampleUtilities.DoCpuIntensiveOperation (2.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else new StockDataCollection(marketData)

    let analyzeData data = 
        if cts.Token.IsCancellationRequested then null
        else MarketAnalyzer.Run(data)

    let runModel data =
        SampleUtilities.DoCpuIntensiveOperation (2.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else MarketModeler.Run(data)

    let compareModels models =
        SampleUtilities.DoCpuIntensiveOperation (2.0 * speedFactor) cts.Token false |> ignore
        if cts.Token.IsCancellationRequested then null
        else ModelComparer.Run(models |> Array.ofSeq)

    let createErrorHandler tasks =
        Task.Factory.ContinueWhenAll(Array.ofList tasks, fun t ->
            try Task.WaitAll(Array.ofList tasks)
            with :? AggregateException as e ->
                Console.WriteLine(e.Flatten()) ) 

    // --------------------------------------------------------------------------
    // Implementation of the analysis engine
    // --------------------------------------------------------------------------

    new() = new AnalysisEngine(1.0)

    interface IAnalysisEngine with

        /// Creates a market recommendation using a fully sequential operation
        /// Returns a MarketRecomendation value (as the result of analysis)
        member x.DoAnalysisSequential() =
            
            // Load & process NYSE and NASDAQ data
            let nyseData = loadNyseData()
            let nasdaqData = loadNasdaqData()
            let mergedMarketData = mergeMarketData [ nyseData; nasdaqData ]
            let normalizedMarketData = normalizeData mergedMarketData
            let analyzedStockData = analyzeData normalizedMarketData
            
            // Load & process FED data
            let fedHistoricalData = loadFedHistoricalData()
            let normalizedHistoricalData = normalizeData fedHistoricalData
            let analyzedHistoricalData = analyzeData normalizedHistoricalData
            
            // Run both models and compare them
            let modeledMarketData = runModel analyzedStockData
            let modeledHistoricalData = runModel analyzedHistoricalData
            let recommendation = compareModels [ modeledMarketData; modeledHistoricalData ]
            recommendation


        /// Initiates market analysis using parallel computation (Tasks).
        /// Returns an object that can be used to register notifications with 
        /// the background calculations (and can be used to get the results)
        ///
        /// Note: Compare with the DoAnalysisSequential method
        member x.DoAnalysisParallel() =
            if cts <> null then cts.Dispose()
            cts <- new CancellationTokenSource()

            // Load & process NYSE and NASDAQ data
            let loadNyseDataTask = taskLong loadNyseData
            let loadNasdaqDataTask = taskLong loadNasdaqData

            let mergeMarketDataTask = 
              Task.Factory.ContinueWhenAll
                ( [| loadNyseDataTask; loadNasdaqDataTask |],
                  fun (tasks:Task<_>[]) -> 
                      mergeMarketData [ for t in tasks -> t.Result ])

            let normalizeMarketDataTask =
                mergeMarketDataTask.ContinueWith(fun (t:Task<_>) ->
                    normalizeData(t.Result))

            let analyzeMarketDataTask =
                normalizeMarketDataTask.ContinueWith(fun (t:Task<_>) ->
                    analyzeData(t.Result))

            // Load & process FED data
            let loadFedHistoricalDataTask = taskLong loadFedHistoricalData
            let normalizeHistoricalDataTask =
                loadFedHistoricalDataTask.ContinueWith(fun (t:Task<_>) -> 
                    normalizeData(t.Result))

            let modelMarketDataTask =
                analyzeMarketDataTask.ContinueWith(fun (t:Task<_>) ->
                    runModel(t.Result))

            let analyzeHistoricalDataTask =
                normalizeHistoricalDataTask.ContinueWith(fun (t:Task<_>) ->
                    analyzeData(t.Result))

            // Run both models and compare them
            let modelHistoricalDataTask =
                analyzeHistoricalDataTask.ContinueWith(fun (t:Task<_>) ->
                    runModel(t.Result))

            let compareModelsTask =
                Task.Factory.ContinueWhenAll
                  ( [| modelMarketDataTask; modelHistoricalDataTask |],
                    fun (tasks:Task<_>[]) -> compareModels [ for t in tasks -> t.Result ])

            // Construct error handler and return tasks as result
            let errorHandler = 
              createErrorHandler 
                [ loadNyseDataTask; loadNasdaqDataTask; loadFedHistoricalDataTask; 
                  mergeMarketDataTask; normalizeHistoricalDataTask; 
                  normalizeMarketDataTask; analyzeHistoricalDataTask; 
                  analyzeMarketDataTask; modelHistoricalDataTask; 
                  modelMarketDataTask; compareModelsTask ] 

            { new AnalysisNotifications with
                member x.AddLoadNyseData(f) = addNotification loadNyseDataTask f
                member x.AddLoadNasdaqData(f) = addNotification loadNasdaqDataTask f
                member x.AddMergeMarketData(f) = addNotification mergeMarketDataTask f
                member x.AddNormalizeMarketData(f) = addNotification normalizeMarketDataTask f
                member x.AddLoadFedHistoricalData(f) = addNotification loadFedHistoricalDataTask f
                member x.AddNormalizeHistoricalData(f) = addNotification normalizeHistoricalDataTask f
                member x.AddAnalyzeMarketData(f) = addNotification analyzeMarketDataTask f
                member x.AddAnalyzeHistoricalData(f) = addNotification analyzeHistoricalDataTask f
                member x.AddModelMarketData(f) = addNotification modelMarketDataTask f
                member x.AddModelHistoricalData(f) = addNotification modelHistoricalDataTask f
                member x.AddCompareModels(f) = addNotification compareModelsTask f
                member x.AddErrorHandler(f) = 
                    errorHandler.ContinueWith
                      ( new Action<_>(fun (t:Task<_>) -> 
                            if t.Status = TaskStatus.Faulted then f ()), 
                        TaskScheduler.FromCurrentSynchronizationContext()) |> ignore } 


        /// Initiates market analysis using asynchronous workflows (F# async).
        /// Returns an object that can be used to register notifications with 
        /// the background calculations (and can be used to get the results)
        ///
        /// To notify the caller, we create events that are triggered (on the
        /// GUI thread) when asynchronous workflow produces partial result.
        /// The returned object registers handlers with the events.
        ///
        /// Note: Compare with the DoAnalysisSequential method
        member x.DoAnalysisAsync() =
            if cts <> null then cts.Dispose()
            cts <- new CancellationTokenSource()

            // Create events that are used to notify the 
            // caller about partial results of the computation
            let loadNyseDataEvt = new Event<_>()
            let loadNasdaqDataEvt = new Event<_>()
            let mergeMarketDataEvt = new Event<_>()
            let normalizeMarketDataEvt = new Event<_>()
            let loadFedHistoricalDataEvt = new Event<_>()
            let normalizeHistoricalDataEvt = new Event<_>()
            let analyzeMarketDataEvt = new Event<_>()
            let analyzeHistoricalDataEvt = new Event<_>()
            let modelMarketDataEvt = new Event<_>()
            let modelHistoricalDataEvt = new Event<_>()
            let compareModelsEvt = new Event<_>()
            let errorHandlerEvt = new Event<_>()

            // Load & process NYSE and NASDAQ data
            let marketModel = async {
                // Start loading of data from two sources in background (both I/O
                // operations are perfomed asynchronously without blocking threads)
                let! nyse = 
                  asyncLoadNyseData() 
                  |> triggerWhenCompletes loadNyseDataEvt |> Async.StartChild
                let! nasdaq = 
                  asyncLoadNasdaqData() 
                  |> triggerWhenCompletes loadNasdaqDataEvt |> Async.StartChild
                
                // Wait for both tasks to complete and continue
                let! nyseData = nyse
                let! nasdaqData = nasdaq
                let merged = mergeMarketData [ nyseData; nasdaqData ] 
                triggerGuiEvent mergeMarketDataEvt merged
                
                // Perform analysis of the merged data
                let normalized = normalizeData merged
                triggerGuiEvent normalizeMarketDataEvt normalized
                let analyzed = analyzeData normalized 
                triggerGuiEvent analyzeMarketDataEvt analyzed
                let res = runModel analyzed 
                triggerGuiEvent modelMarketDataEvt res
                return res }

            // Load & process FED data
            let historicalModel = async {
                // Obtain data asynchronously using non-blocking I/O
                let! fed = asyncLoadFedHistoricalData()
                triggerGuiEvent loadFedHistoricalDataEvt fed

                // Perform CPU-intensive analysis of the data
                let normalized = normalizeData fed
                triggerGuiEvent normalizeHistoricalDataEvt normalized
                let analyzed = analyzeData normalized
                triggerGuiEvent analyzeHistoricalDataEvt analyzed
                let res = runModel analyzed 
                triggerGuiEvent modelHistoricalDataEvt res
                return res }

            // Run both of the models and compare them to get recommendation
            // When error occurs, it is propagated here and handled using 'try'
            let compare = async {
                try
                    let! models = Async.Parallel [ marketModel; historicalModel ]
                    let res = compareModels models 
                    triggerGuiEvent compareModelsEvt res 
                with e ->
                    triggerGuiEvent errorHandlerEvt ()  }

            // Construct error handler and return tasks as result
            // (if we pass cancellation token to 'Async.Start', the workflow
            // will be terminated automatically, so we wouldn't return 'null'
            // to the caller - which is expected in the current design)
            Async.Start(compare)

            { new AnalysisNotifications with
                member x.AddLoadNyseData(f) = loadNyseDataEvt.Publish.Add(f)
                member x.AddLoadNasdaqData(f) = loadNasdaqDataEvt.Publish.Add(f)
                member x.AddMergeMarketData(f) = mergeMarketDataEvt.Publish.Add(f)
                member x.AddNormalizeMarketData(f) = normalizeMarketDataEvt.Publish.Add(f)
                member x.AddLoadFedHistoricalData(f) = loadFedHistoricalDataEvt.Publish.Add(f)
                member x.AddNormalizeHistoricalData(f) = normalizeHistoricalDataEvt.Publish.Add(f)
                member x.AddAnalyzeMarketData(f) = analyzeMarketDataEvt.Publish.Add(f)
                member x.AddAnalyzeHistoricalData(f) = analyzeHistoricalDataEvt.Publish.Add(f)
                member x.AddModelMarketData(f) = modelMarketDataEvt.Publish.Add(f)
                member x.AddModelHistoricalData(f) = modelHistoricalDataEvt.Publish.Add(f)
                member x.AddCompareModels(f) = compareModelsEvt.Publish.Add(f)
                member x.AddErrorHandler(f) = errorHandlerEvt.Publish.Add(f) } 

        /// Cancel the running parallel or asynchronous workflow based
        /// analysis (this causes the analysis to return 'null' as the result)
        member x.TryCancelAnalysis() =
            if cts <> null then cts.Cancel()

    // --------------------------------------------------------------------------
    // Implementation of the disposable pattern
    // --------------------------------------------------------------------------
    
    member x.Dispose(disposing) =
        if disposing then
            if cts <> null then
                cts.Dispose()
                cts <- null

    interface IDisposable with 
        member x.Dispose() =
            x.Dispose(true)
            GC.SuppressFinalize(x)

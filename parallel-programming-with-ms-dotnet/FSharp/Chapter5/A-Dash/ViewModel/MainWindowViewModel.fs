//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel

open System
open System.ComponentModel
open System.Threading
open System.Threading.Tasks
open System.Windows.Input
open Microsoft.Practices.ParallelGuideSamples.ADash
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

type State = 
    | Ready = 0 
    | Calculating = 1 
    | Canceling = 2

type MainWindowViewModel(engine:IAnalysisEngine) as x = 

    // --------------------------------------------------------------------------
    // Private fields (state & events) of the view model
    // --------------------------------------------------------------------------
    
    // Analysis engine 
    let mutable engine = engine

    // View model's current mode of operation
    let mutable modelState = State.Ready

    // Results of analysis or null if not yet computed
    let mutable nyseMarketData : StockDataCollection = null
    let mutable nasdaqMarketData : StockDataCollection = null
    let mutable mergedMarketData : StockDataCollection = null
    let mutable normalizedMarketData : StockDataCollection = null
    let mutable fedHistoricalData : StockDataCollection = null
    let mutable normalizedHistoricalData : StockDataCollection = null
    let mutable analyzedStockData : StockAnalysisCollection = null
    let mutable analyzedHistoricalData : StockAnalysisCollection = null
    let mutable modeledMarketData : MarketModel = null
    let mutable modeledHistoricalData : MarketModel = null
    let mutable recommendation : MarketRecommendation = null

    // Status string that appears in the UI
    let mutable statusText = "" 

    // Raised when a public property of this class changes
    let propertyChanged = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    // Raised when the corresponding command is invoked. 
    let requestClose = new Event<_, _>()
    let requestNyse = new Event<_, _>()
    let requestNasdaq = new Event<_, _>()
    let requestMerged = new Event<_, _>()
    let requestNormalized = new Event<_, _>()
    let requestFedHistorical = new Event<_, _>()
    let requestNormalizedHistorical = new Event<_, _>()
    let requestAnalyzed = new Event<_, _>()
    let requestAnalyzedHistorical = new Event<_, _>()
    let requestModeled = new Event<_, _>()
    let requestModeledHistorical = new Event<_, _>()
    let requestRecommendation = new Event<_, _>()

    // --------------------------------------------------------------------------
    // Command objects exposed by this view model for use by the view
    // --------------------------------------------------------------------------
    
    let makeCommand (evt:Event<_, _>) reader = 
        lazy Command((fun _ -> evt.Trigger(x, EventArgs.Empty)), (fun _ -> reader() <> null))

    let closeCommand = lazy Command(ignore >> x.OnRequestClose)
    let calculateCommand = lazy Command(ignore >> x.OnRequestCalculate)
    let cancelCommand = lazy Command(ignore >> x.OnRequestCancel, fun _ -> modelState = State.Calculating)
    let nyseCommand = makeCommand requestNyse (fun _ -> nyseMarketData)
    let nasdaqCommand = makeCommand requestNasdaq (fun _ -> nasdaqMarketData)
    let mergedCommand = makeCommand requestMerged (fun _ -> mergedMarketData)
    let normalizedCommand = makeCommand requestNormalized (fun _ -> normalizedMarketData)
    let fedHistoricalCommand = makeCommand requestFedHistorical (fun _ -> fedHistoricalData)
    let normalizedHistoricalCommand = makeCommand requestNormalizedHistorical (fun _ -> normalizedHistoricalData)
    let analyzedCommand = makeCommand requestAnalyzed (fun _ -> analyzedStockData)
    let analyzedHistoricalCommand = makeCommand requestAnalyzedHistorical (fun _ -> analyzedHistoricalData)
    let modeledCommand = makeCommand requestModeled (fun _ -> modeledMarketData)
    let modeledHistoricalCommand = makeCommand requestModeledHistorical (fun _ -> modeledHistoricalData)
    let recommendationCommand = makeCommand requestRecommendation (fun _ -> recommendation)

    // --------------------------------------------------------------------------
    // Implementation of the view model interface
    // --------------------------------------------------------------------------
    
    interface IMainWindowViewModel with
        
        // Public events of the ViewModel

        member x.RequestNyse = requestNyse.Publish
        member x.RequestNasdaq = requestNasdaq.Publish
        member x.RequestMerged = requestMerged.Publish
        member x.RequestNormalized = requestNormalized.Publish
        member x.RequestFedHistorical = requestFedHistorical.Publish
        member x.RequestNormalizedHistorical = requestNormalizedHistorical.Publish
        member x.RequestAnalyzed = requestAnalyzed.Publish
        member x.RequestAnalyzedHistorical = requestAnalyzedHistorical.Publish
        member x.RequestModeled = requestModeled.Publish
        member x.RequestModeledHistorical = requestModeledHistorical.Publish
        member x.RequestRecommendation = requestRecommendation.Publish

        // Public data properties of the ViewModel

        member x.IsCancelEnabled = 
            (modelState = State.Calculating ||
              modelState = State.Canceling)

        member x.IsCalculateEnabled =
            modelState <> State.Calculating

        member x.StatusTextBoxText 
            with get() = statusText
            and set(value) = 
                statusText <- value
                x.OnPropertyChanged("StatusTextBoxText")

        member x.NyseMarketData
            with get() = nyseMarketData
            and set(value) =
                nyseMarketData <- value
                x.OnPropertyChanged("NyseMarketData")
                nyseCommand.Value.NotifyExecuteChanged()

        member x.NasdaqMarketData
            with get() = nasdaqMarketData
            and set(value) =
                nasdaqMarketData <- value
                x.OnPropertyChanged("NasdaqMarketData")
                nasdaqCommand.Value.NotifyExecuteChanged()

        member x.MergedMarketData
            with get() = mergedMarketData
            and set(value) =
                mergedMarketData <- value
                x.OnPropertyChanged("MergedMarketData")
                mergedCommand.Value.NotifyExecuteChanged()

        member x.NormalizedMarketData
            with get() = normalizedMarketData
            and set(value) =
                normalizedMarketData <- value
                x.OnPropertyChanged("NormalizedMarketData")
                normalizedCommand.Value.NotifyExecuteChanged()

        member x.FedHistoricalData
            with get() = fedHistoricalData
            and set(value) =
                fedHistoricalData <- value
                x.OnPropertyChanged("FedHistoricalData")
                fedHistoricalCommand.Value.NotifyExecuteChanged()

        member x.NormalizedHistoricalData
            with get() = normalizedHistoricalData
            and set(value) =
                normalizedHistoricalData <- value
                x.OnPropertyChanged("NormalizedHistoricalData")
                normalizedHistoricalCommand.Value.NotifyExecuteChanged()

        member x.AnalyzedStockData
            with get() = analyzedStockData
            and set(value) =
                analyzedStockData <- value
                x.OnPropertyChanged("AnalyzedMarketData")
                analyzedCommand.Value.NotifyExecuteChanged()

        member x.AnalyzedHistoricalData
            with get() = analyzedHistoricalData
            and set(value) =
                analyzedHistoricalData <- value
                x.OnPropertyChanged("AnalyzedHistoricalData")
                analyzedHistoricalCommand.Value.NotifyExecuteChanged()

        member x.ModeledMarketData
            with get() = modeledMarketData
            and set(value) =
                modeledMarketData <- value
                x.OnPropertyChanged("ModeledMarketData")
                modeledCommand.Value.NotifyExecuteChanged()

        member x.ModeledHistoricalData
            with get() = modeledHistoricalData
            and set(value) =
                modeledHistoricalData <- value
                x.OnPropertyChanged("ModeledHistoricalData")
                modeledHistoricalCommand.Value.NotifyExecuteChanged()

        member x.Recommendation
            with get() = recommendation
            and set(value) =
                recommendation <- value
                x.OnPropertyChanged("Recommendation")
                recommendationCommand.Value.NotifyExecuteChanged()

        // Raised when the associated view 
        // window should be closed (i.e. application shutdown)
        member x.RequestClose = requestClose.Publish

        // Publicly exposed commands for working with the ViewModel

        member x.CloseCommand = closeCommand.Value :> ICommand
        member x.CalculateCommand = calculateCommand.Value :> ICommand
        member x.CancelCommand = cancelCommand.Value :> ICommand
        member x.NyseCommand = nyseCommand.Value :> ICommand
        member x.NasdaqCommand = nasdaqCommand.Value :> ICommand
        member x.MergedCommand = mergedCommand.Value :> ICommand
        member x.NormalizedCommand = normalizedCommand.Value :> ICommand
        member x.FedHistoricalCommand = fedHistoricalCommand.Value :> ICommand
        member x.NormalizedHistoricalCommand = normalizedHistoricalCommand.Value :> ICommand
        member x.AnalyzedCommand = analyzedCommand.Value :> ICommand
        member x.AnalyzedHistoricalCommand = analyzedHistoricalCommand.Value :> ICommand
        member x.ModeledCommand = modeledCommand.Value :> ICommand
        member x.ModeledHistoricalCommand = modeledHistoricalCommand.Value :> ICommand
        member x.RecommendationCommand = recommendationCommand.Value :> ICommand

    // --------------------------------------------------------------------------
    // Public properties that expose the state and commands
    //
    // ..because WPF binding requires these to be ordinary properties - they 
    // are exposed as part of interface implementation, but that's ignored
    // --------------------------------------------------------------------------
    
    member x.ModelState 
        with get() = modelState
        and set(value) =
            modelState <- value
            // issue notification of property change, including derived properties
            // and commands whose "CanExecute" status depend on model state.
            x.OnPropertyChanged("IsCancelEnabled")
            x.OnPropertyChanged("IsCalculateEnabled")
            calculateCommand.Value.NotifyExecuteChanged()
            cancelCommand.Value.NotifyExecuteChanged()
    
    member x.StatusTextBoxText = statusText
    member x.CloseCommand = closeCommand.Value :> ICommand
    member x.CalculateCommand = calculateCommand.Value :> ICommand
    member x.CancelCommand = cancelCommand.Value :> ICommand
    member x.NyseCommand = nyseCommand.Value :> ICommand
    member x.NasdaqCommand = nasdaqCommand.Value :> ICommand
    member x.MergedCommand = mergedCommand.Value :> ICommand
    member x.NormalizedCommand = normalizedCommand.Value :> ICommand
    member x.FedHistoricalCommand = fedHistoricalCommand.Value :> ICommand
    member x.NormalizedHistoricalCommand = normalizedHistoricalCommand.Value :> ICommand
    member x.AnalyzedCommand = analyzedCommand.Value :> ICommand
    member x.AnalyzedHistoricalCommand = analyzedHistoricalCommand.Value :> ICommand
    member x.ModeledCommand = modeledCommand.Value :> ICommand
    member x.ModeledHistoricalCommand = modeledHistoricalCommand.Value :> ICommand
    member x.RecommendationCommand = recommendationCommand.Value :> ICommand

    // --------------------------------------------------------------------------
    // Command implementations
    // --------------------------------------------------------------------------

    /// Start the calculation in background and register notifications
    member x.OnRequestCalculate() =
        // Initialize the result properties to null
        x.ResetResultProperties()
        // Place the view model into calculation mode
        x.ModelState <- State.Calculating
        // Update the property containing the status text
        (x :> IMainWindowViewModel).StatusTextBoxText <- "...calculating..."

        // Start the analysis (using F# workflows)
        let tasks = engine.DoAnalysisAsync()
        
        // Uncomment the following line to use Task-based solution
        // let tasks = engine.DoAnalysisParallel()

        // Add continuations so that view model properties are updated when each subtask completes
        x.AddNotifications(tasks)


    /// Register notifications with tasks/asyncs so that the view model's 
    /// properties are updated when each task has results available for display.
    member x.AddNotifications (tasks:AnalysisNotifications) =
        let m = x :> IMainWindowViewModel
        tasks.AddLoadNyseData (fun t -> m.NyseMarketData <- t)
        tasks.AddLoadNasdaqData (fun t -> m.NasdaqMarketData <- t)
        tasks.AddLoadFedHistoricalData (fun t -> m.FedHistoricalData <- t)
        tasks.AddMergeMarketData (fun t -> m.MergedMarketData <- t)
        tasks.AddNormalizeHistoricalData  (fun t -> m.NormalizedHistoricalData <- t)
        tasks.AddNormalizeMarketData (fun t -> m.NormalizedMarketData <- t)
        tasks.AddAnalyzeHistoricalData  (fun t -> m.AnalyzedHistoricalData <- t)
        tasks.AddAnalyzeMarketData (fun t -> m.AnalyzedStockData <- t)
        tasks.AddModelHistoricalData  (fun t -> m.ModeledHistoricalData <- t)
        tasks.AddModelMarketData (fun t -> m.ModeledMarketData <- t)
        tasks.AddCompareModels (fun t ->
            m.Recommendation <- t
            (x :> IMainWindowViewModel).StatusTextBoxText <-
                if m.Recommendation = null then "Canceled"
                else m.Recommendation.Value
            x.ModelState <- State.Ready )
        tasks.AddErrorHandler (fun () ->
            (x :> IMainWindowViewModel).StatusTextBoxText <- "Error"
            x.ModelState <- State.Ready)

    /// Set default values of results (called before starting calculation)
    member x.ResetResultProperties() =
        let m = x :> IMainWindowViewModel
        m.NyseMarketData <- null
        m.NasdaqMarketData <- null
        m.MergedMarketData <- null
        m.NormalizedMarketData <- null
        m.FedHistoricalData <- null
        m.NormalizedHistoricalData <- null
        m.AnalyzedStockData <- null
        m.AnalyzedHistoricalData <- null
        m.ModeledMarketData <- null
        m.ModeledHistoricalData <- null
        m.Recommendation <- null

    /// Close the application - cancel analysis
    member x.OnRequestClose() =
        engine.TryCancelAnalysis()
        requestClose.Trigger(x, EventArgs.Empty)

    /// Cancel analysis and wait until it finishes
    member x.OnRequestCancel() =
        engine.TryCancelAnalysis()
        x.ModelState <- State.Canceling
        (x :> IMainWindowViewModel).StatusTextBoxText <- "Canceling..."

    /// Value of a property has been changed (INotifyPropertyChanged)
    member x.OnPropertyChanged(name) =
        propertyChanged.Trigger(x, new PropertyChangedEventArgs(name))

    // --------------------------------------------------------------------------
    // Implementation of INotifyPropertyChanged and IDisposable
    // --------------------------------------------------------------------------
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChanged.Publish
      
    member x.Dispose(disposing) =
        if disposing then 
            if engine <> null then
                engine.Dispose()
                engine <- null

    interface IDisposable with 
        member x.Dispose() = 
            x.Dispose(true)
            GC.SuppressFinalize(x)

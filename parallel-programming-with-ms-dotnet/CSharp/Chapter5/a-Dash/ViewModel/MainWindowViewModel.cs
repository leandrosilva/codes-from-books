//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.ComponentModel;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects;

namespace Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel
{
    public enum State { Ready, Calculating, Canceling };

    public class MainWindowViewModel : IMainWindowViewModel, INotifyPropertyChanged, IDisposable
    {
        #region Private Fields

        IAnalysisEngine engine;

        // view model's current mode of operation
        State modelState = State.Ready;

        // results of analysis or null if not yet computed
        StockDataCollection nyseMarketData;
        StockDataCollection nasdaqMarketData;
        StockDataCollection mergedMarketData;
        StockDataCollection normalizedMarketData;
        StockDataCollection fedHistoricalData;
        StockDataCollection normalizedHistoricalData;
        StockAnalysisCollection analyzedStockData;
        StockAnalysisCollection analyzedHistoricalData;
        MarketModel modeledMarketData;
        MarketModel modeledHistoricalData;
        MarketRecommendation recommendation;

        // Command objects exposed by this view model for use by the view
        Command calculateCommand;
        Command cancelCommand;
        Command closeCommand;
        Command nyseCommand;
        Command nasdaqCommand;
        Command mergedCommand;
        Command normalizedCommand;
        Command fedHistoricalCommand;
        Command normalizedHistoricalCommand;
        Command analyzedCommand;
        Command analyzedHistoricalCommand;
        Command modeledCommand;
        Command modeledHistoricalCommand;
        Command recommendationCommand;

        // status string that appears in the UI
        string statusText = ""; 

        #endregion // Private Fields

        public MainWindowViewModel(IAnalysisEngine eng)
        {
            engine = eng;
        }

        #region Events
       
        // Raised when a public property of this class changes
        public event PropertyChangedEventHandler PropertyChanged;

        // Raised when the associated view window should be closed (i.e. application shutdown).
        public event EventHandler RequestClose;

        // Raised when the corresponding command is invoked. 
        public event EventHandler RequestNyse;
        public event EventHandler RequestNasdaq;
        public event EventHandler RequestMerged;
        public event EventHandler RequestNormalized;
        public event EventHandler RequestFedHistorical;
        public event EventHandler RequestNormalizedHistorical;
        public event EventHandler RequestAnalyzed;
        public event EventHandler RequestAnalyzedHistorical;
        public event EventHandler RequestModeled;
        public event EventHandler RequestModeledHistorical;
        public event EventHandler RequestRecommendation;

        #endregion

        #region Publicly Readable Data Properties

        public State ModelState
        {
            get
            {
                return modelState;
            }

            private set
            {
                modelState = value;

                // issue notification of property change, including derived properties
                // and commands whose "CanExecute" status depend on model state.
                OnPropertyChanged("IsCancelEnabled");
                OnPropertyChanged("IsCalculateEnabled");
                calculateCommand.NotifyExecuteChanged();
                cancelCommand.NotifyExecuteChanged();
            }
        }

        public string StatusTextBoxText
        {
            get { return statusText; }
            private set
            {
                statusText = value;
                OnPropertyChanged("StatusTextBoxText");
            }
        }

        public bool IsCancelEnabled
        {
            get
            {
                return (modelState == State.Calculating ||
                        modelState == State.Canceling);
            }
        }

        public bool IsCalculateEnabled
        {
            get { return ModelState != State.Calculating; }
        }

        public StockDataCollection NyseMarketData
        {
            get
            {
                return nyseMarketData;
            }
            private set
            {
                nyseMarketData = value;
                OnPropertyChanged("NyseMarketData");
                nyseCommand.NotifyExecuteChanged();
            }
        }

        public StockDataCollection NasdaqMarketData
        {
            get
            {
                return nasdaqMarketData;
            }
            private set
            {
                nasdaqMarketData = value;
                OnPropertyChanged("NasdaqMarketData");
                nasdaqCommand.NotifyExecuteChanged();
            }
        }

        public StockDataCollection MergedMarketData
        {
            get
            {
                return mergedMarketData;
            }
            private set
            {
                mergedMarketData = value;
                OnPropertyChanged("MergedMarketData");
                mergedCommand.NotifyExecuteChanged();
            }
        }

        public StockDataCollection NormalizedMarketData
        {
            get
            {
                return normalizedMarketData;
            }
            private set
            {
                normalizedMarketData = value;
                OnPropertyChanged("NormalizedMarketData");
                normalizedCommand.NotifyExecuteChanged();
            }
        }

        public StockDataCollection FedHistoricalData
        {
            get
            {
                return fedHistoricalData;
            }
            private set
            {
                fedHistoricalData = value;
                OnPropertyChanged("FedHistoricalData");
                fedHistoricalCommand.NotifyExecuteChanged();
            }
        }

        public StockDataCollection NormalizedHistoricalData
        {
            get
            {
                return normalizedHistoricalData;
            }
            private set
            {
                normalizedHistoricalData = value;
                OnPropertyChanged("NormalizedHistoricalData");
                normalizedHistoricalCommand.NotifyExecuteChanged();
            }
        }

        public StockAnalysisCollection AnalyzedStockData
        {
            get
            {
                return analyzedStockData;
            }
            private set
            {
                analyzedStockData = value;
                OnPropertyChanged("AnalyzedMarketData");
                analyzedCommand.NotifyExecuteChanged();
            }
        }

        public StockAnalysisCollection AnalyzedHistoricalData
        {
            get
            {
                return analyzedHistoricalData;
            }
            private set
            {
                analyzedHistoricalData = value;
                OnPropertyChanged("AnalyzedHistoricalData");
                analyzedHistoricalCommand.NotifyExecuteChanged();
            }
        }

        public MarketModel ModeledMarketData
        {
            get
            {
                return modeledMarketData;
            }
            private set
            {
                modeledMarketData = value;
                OnPropertyChanged("ModeledMarketData");
                modeledCommand.NotifyExecuteChanged();
            }
        }

        public MarketModel ModeledHistoricalData
        {
            get
            {
                return modeledHistoricalData;
            }
            private set
            {
                modeledHistoricalData = value;
                OnPropertyChanged("ModeledHistoricalData");
                modeledHistoricalCommand.NotifyExecuteChanged();
            }
        }

        public MarketRecommendation Recommendation
        {
            get
            {
                return recommendation;
            }
            private set
            {
                recommendation = value;
                OnPropertyChanged("Recommendation");
                recommendationCommand.NotifyExecuteChanged();
            }
        }

        #endregion // Publicly Readable Data Properties

        #region Commands

        public ICommand CloseCommand
        {
            get
            {
                return closeCommand ?? (closeCommand = new Command(_ => OnRequestClose()));
            }
        }

        public ICommand CalculateCommand
        {
            get
            {
                return calculateCommand ?? (calculateCommand = new Command(_ => OnRequestCalculate()));
            }
        }

        public ICommand CancelCommand
        {
            get
            {
                if (cancelCommand == null)
                    cancelCommand = new Command(
                        _ => OnRequestCancel(),
                        _ => ModelState == State.Calculating
                        );

                return cancelCommand;
            }
        }

        public ICommand NyseCommand
        {
            get
            {
                if (nyseCommand == null)
                    nyseCommand = new Command(
                        _ => RaiseEvent(RequestNyse), 
                        _ => nyseMarketData != null
                        );

                return nyseCommand;
            }
        }

        public ICommand NasdaqCommand
        {
            get
            {
                if (nasdaqCommand == null)
                    nasdaqCommand = new Command(
                        _ => RaiseEvent(RequestNasdaq),
                        _ => nasdaqMarketData != null
                        );

                return nasdaqCommand;
            }
        }

        public ICommand MergedCommand
        {
            get
            {
                if (mergedCommand == null)
                    mergedCommand = new Command(
                        _ => RaiseEvent(RequestMerged),
                        _ => mergedMarketData != null
                        );

                return mergedCommand;
            }
        }

        public ICommand NormalizedCommand
        {
            get
            {
                if (normalizedCommand == null)
                    normalizedCommand = new Command(
                        _ => RaiseEvent(RequestNormalized),
                        _ => normalizedMarketData != null
                        );

                return normalizedCommand;
            }
        }

        public ICommand FedHistoricalCommand
        {
            get
            {
                if (fedHistoricalCommand == null)
                    fedHistoricalCommand = new Command(
                        _ => RaiseEvent(RequestFedHistorical),
                        _ => fedHistoricalData != null
                        );

                return fedHistoricalCommand;
            }
        }

        public ICommand NormalizedHistoricalCommand
        {
            get
            {
                if (normalizedHistoricalCommand == null)
                    normalizedHistoricalCommand = new Command(
                        _ => RaiseEvent(RequestNormalizedHistorical),
                        _ => normalizedHistoricalData != null
                        );

                return normalizedHistoricalCommand;
            }
        }

        public ICommand AnalyzedCommand
        {
            get
            {
                if (analyzedCommand == null)
                    analyzedCommand = new Command(
                        _ => RaiseEvent(RequestAnalyzed),
                        _ => analyzedStockData != null
                        );

                return analyzedCommand;
            }
        }

        public ICommand AnalyzedHistoricalCommand
        {
            get
            {
                if (analyzedHistoricalCommand == null)
                    analyzedHistoricalCommand = new Command(
                        _ => RaiseEvent(RequestAnalyzedHistorical),
                        _ => analyzedHistoricalData != null
                        );

                return analyzedHistoricalCommand;
            }
        }

        public ICommand ModeledCommand
        {
            get
            {
                if (modeledCommand == null)
                    modeledCommand = new Command(
                        _ => RaiseEvent(RequestModeled),
                        _ => modeledMarketData != null
                        );

                return modeledCommand;
            }
        }

        public ICommand ModeledHistoricalCommand
        {
            get
            {
                if (modeledHistoricalCommand == null)
                    modeledHistoricalCommand = new Command(
                        _ => RaiseEvent(RequestModeledHistorical),
                        _ => modeledHistoricalData != null
                        );

                return modeledHistoricalCommand;
            }
        }

        public ICommand RecommendationCommand
        {
            get
            {
                if (recommendationCommand == null)
                    recommendationCommand = new Command(
                        _ => RaiseEvent(RequestRecommendation),
                        _ => recommendation != null
                        );

                return recommendationCommand;
            }
        }

        #endregion // Commands

        #region Command Implementations

        // helper
        void RaiseEvent(EventHandler handler)
        {
            if (handler != null)
                handler(this, EventArgs.Empty);
        }

        void OnRequestCalculate()
        {
            // Initialize the result properties to null
            ResetResultProperties();

            // Place the view model into calculation mode
            ModelState = State.Calculating;

            // Update the property containing the status text
            StatusTextBoxText = "...calculating...";
    
            // Start the analysis
            AnalysisTasks tasks = engine.DoAnalysisParallel();

            // Add continuations so that view model properties are updated when each subtask completes
            AddButtonContinuations(tasks);
        }

        void AddButtonContinuation<T>(Task<T> task, Action<Task<T>> action)
        {
            task.ContinueWith(
                action,
                CancellationToken.None,
                TaskContinuationOptions.OnlyOnRanToCompletion, 
                TaskScheduler.FromCurrentSynchronizationContext());

        }
        /// <summary>
        /// Adds continuations to analysis tasks so that the view model's properties are updated when 
        /// each task has results available for display.
        /// </summary>
        /// <param name="tasks">The task record</param>
        void AddButtonContinuations(AnalysisTasks tasks)
        {
            AddButtonContinuation(tasks.LoadNyseData, 
                t => { NyseMarketData = t.Result; });

            AddButtonContinuation(tasks.LoadNasdaqData,
                t => { NasdaqMarketData = t.Result; });

            AddButtonContinuation(tasks.LoadFedHistoricalData,
                t => { FedHistoricalData = t.Result; });

            AddButtonContinuation(tasks.MergeMarketData,
                t => { MergedMarketData = t.Result; });

            AddButtonContinuation(tasks.NormalizeHistoricalData,
                t => { NormalizedHistoricalData = t.Result; });

            AddButtonContinuation(tasks.NormalizeMarketData,
                t => { NormalizedMarketData = t.Result; });

            AddButtonContinuation(tasks.AnalyzeHistoricalData,
                t => { AnalyzedHistoricalData = t.Result; });

            AddButtonContinuation(tasks.AnalyzeMarketData,
                t => { AnalyzedStockData = t.Result; });

            AddButtonContinuation(tasks.ModelHistoricalData,
                t => { ModeledHistoricalData = t.Result; });

            AddButtonContinuation(tasks.ModelMarketData,
                t => { ModeledMarketData = t.Result; });

            AddButtonContinuation(tasks.CompareModels,
                t =>
                {
                    this.Recommendation = t.Result;
                    this.StatusTextBoxText =
                        (this.Recommendation == null)
                            ? "Canceled"
                            : this.Recommendation.Value;
                    this.ModelState = State.Ready;
                });

            tasks.ErrorHandler.ContinueWith(
                t =>
                {
                    if (t.Status == TaskStatus.Faulted)
                        this.StatusTextBoxText = "Error";
                    this.ModelState = State.Ready;
                }, TaskScheduler.FromCurrentSynchronizationContext());
        }

        void ResetResultProperties()
        {
            NyseMarketData = null;
            NasdaqMarketData = null;
            MergedMarketData = null;
            NormalizedMarketData = null;
            FedHistoricalData = null;
            NormalizedHistoricalData = null;
            AnalyzedStockData = null;
            AnalyzedHistoricalData = null;
            ModeledMarketData = null;
            ModeledHistoricalData = null;
            Recommendation = null;
        }

        void OnRequestClose()
        {
            engine.TryCancelAnalysis();
            RaiseEvent(RequestClose);
        }

        void OnRequestCancel()
        {
            engine.TryCancelAnalysis();
            ModelState = State.Canceling;
            StatusTextBoxText = "Canceling...";
        }

        #endregion // Command Implementations

        #region INotifyPropertyChanged Implementation

        protected void OnPropertyChanged(string name)
        {
            PropertyChangedEventHandler handler = PropertyChanged;
            if (handler != null)
            {
                handler(this, new PropertyChangedEventArgs(name));
            }
        }
 
        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (engine != null)
                {
                    engine.Dispose();
                    engine = null;
                }
            }
        }

        #endregion
    }
}

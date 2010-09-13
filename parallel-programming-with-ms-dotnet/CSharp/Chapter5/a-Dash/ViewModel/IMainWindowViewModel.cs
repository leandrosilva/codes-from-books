//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Windows.Input;
using Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects;

namespace Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel
{
    public interface IMainWindowViewModel : IDisposable
    {
        // Events

        event EventHandler RequestAnalyzed;
        event EventHandler RequestAnalyzedHistorical;
        event EventHandler RequestClose;
        event EventHandler RequestFedHistorical;
        event EventHandler RequestMerged;
        event EventHandler RequestModeled;
        event EventHandler RequestModeledHistorical;
        event EventHandler RequestNasdaq;
        event EventHandler RequestNormalized;
        event EventHandler RequestNormalizedHistorical;
        event EventHandler RequestNyse;
        event EventHandler RequestRecommendation;

        // Publicly Readable Data Properties

        string StatusTextBoxText { get; }
        bool IsCancelEnabled { get; }
        bool IsCalculateEnabled { get; }

        StockDataCollection NyseMarketData { get; }
        StockDataCollection NasdaqMarketData { get; }
        StockDataCollection MergedMarketData { get; }
        StockDataCollection NormalizedMarketData { get; }
        StockDataCollection FedHistoricalData { get; }
        StockDataCollection NormalizedHistoricalData { get; }
        StockAnalysisCollection AnalyzedStockData { get; }
        StockAnalysisCollection AnalyzedHistoricalData { get; }
        MarketModel ModeledMarketData { get; }
        MarketModel ModeledHistoricalData { get; }
        MarketRecommendation Recommendation { get; }

        // Commands

        ICommand CloseCommand { get; }
        ICommand CalculateCommand { get; }
        ICommand CancelCommand { get; }

        ICommand NyseCommand { get; }
        ICommand NasdaqCommand { get; }
        ICommand MergedCommand { get; }
        ICommand NormalizedCommand { get; }
        ICommand FedHistoricalCommand { get; }
        ICommand NormalizedHistoricalCommand { get; }
        ICommand AnalyzedCommand { get; }
        ICommand AnalyzedHistoricalCommand { get; }
        ICommand ModeledCommand { get; }
        ICommand ModeledHistoricalCommand { get; }
        ICommand RecommendationCommand { get; }
    }
}

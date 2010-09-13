//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

/// Interface that allows the ViewModel component to react to completion
/// of a task or asynchronous workflow running in background.
/// This type is returned from AnalysisEnginge from both Task-based
/// 'DoAnalysisParallel' and async-based 'DoAnalysisAsync' methods
///
/// Note: This is different than in the C# version, which exposed 
/// .NET 4.0 tasks directly here. We allow registration of notification
/// to make the design compatible with continuation-based asyncs.
type AnalysisNotifications = 
    abstract AddLoadNyseData : (StockDataCollection -> unit) -> unit
    abstract AddLoadNasdaqData : (StockDataCollection -> unit) -> unit
    abstract AddMergeMarketData : (StockDataCollection -> unit) -> unit
    abstract AddNormalizeMarketData : (StockDataCollection -> unit) -> unit
    abstract AddLoadFedHistoricalData : (StockDataCollection -> unit) -> unit
    abstract AddNormalizeHistoricalData : (StockDataCollection -> unit) -> unit
    abstract AddAnalyzeMarketData : (StockAnalysisCollection -> unit) -> unit
    abstract AddAnalyzeHistoricalData : (StockAnalysisCollection -> unit) -> unit
    abstract AddModelMarketData : (MarketModel -> unit) -> unit
    abstract AddModelHistoricalData : (MarketModel -> unit) -> unit
    abstract AddCompareModels : (MarketRecommendation -> unit) -> unit
    abstract AddErrorHandler : (unit -> unit) -> unit

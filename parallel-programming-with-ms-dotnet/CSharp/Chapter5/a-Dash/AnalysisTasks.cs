//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects;

namespace Microsoft.Practices.ParallelGuideSamples.ADash
{
	/// <summary>
	/// Task record for market analysis. 
	/// </summary>
	/// <remarks>
	/// Call CompareModels.Result to retrieve the finished analysis. Intermediate results can be retrieved
	/// from the Result method of the other properties. For example, LoadNyseData.Result returns the NYSE data.
	/// </remarks>
	public class AnalysisTasks
	{
		public Task<StockDataCollection> LoadNyseData { get; set; }
		public Task<StockDataCollection> LoadNasdaqData { get; set; }
		public Task<StockDataCollection> MergeMarketData { get; set; }
		public Task<StockDataCollection> NormalizeMarketData { get; set; }
		public Task<StockDataCollection> LoadFedHistoricalData { get; set; }
		public Task<StockDataCollection> NormalizeHistoricalData { get; set; }
		public Task<StockAnalysisCollection> AnalyzeMarketData { get; set; }
		public Task<StockAnalysisCollection> AnalyzeHistoricalData { get; set; }
		public Task<MarketModel> ModelMarketData { get; set; }
		public Task<MarketModel> ModelHistoricalData { get; set; }
		public Task<MarketRecommendation> CompareModels { get; set; }
        public Task ErrorHandler { get; set; }
	}
}
//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.ADash
{
    /// <summary>
    /// Component for data analysis
    /// </summary>
    public class AnalysisEngine : IAnalysisEngine
    {
        // internal scaling factor
        readonly double speedFactor;

        CancellationTokenSource cts = null;

        public AnalysisEngine()
            : this(1.0d)
        {
        }

        public AnalysisEngine(double speed)
        {
            speedFactor = speed;
        }

        #region CreateSampleData

        static IList<StockData> MakeNyseSecurityInfo()
        {
            return GenerateSecurities("NYSE", 100);
        }

        static IList<StockData> MakeNasdaqSecurityInfo()
        {
            return GenerateSecurities("NASDAQ", 100);
        }

        static IList<StockData> MakeFedSecurityInfo()
        {
            return GenerateSecurities("", 100);
        }

        static IList<StockData> GenerateSecurities(string exchange, int size)
        {
            var result = new List<StockData>();
            for (int i = 0; i < size; i++)
                result.Add(new StockData(exchange + " Stock " + i, new[] { 0.0d, 1.0d, 2.0d }));
            return result;
        }

        #endregion

        #region Analysis Helper Methods

        StockDataCollection LoadNyseData()
        {
            SampleUtilities.DoIoIntensiveOperation(2.5, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;
            return new StockDataCollection(MakeNyseSecurityInfo());
        }

        StockDataCollection LoadNasdaqData()
        {
            SampleUtilities.DoIoIntensiveOperation(2.0 * speedFactor, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;

            return new StockDataCollection(MakeNasdaqSecurityInfo());
        }

        StockDataCollection LoadFedHistoricalData()
        {
            SampleUtilities.DoIoIntensiveOperation(3.0 * speedFactor, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;

            return new StockDataCollection(MakeFedSecurityInfo());
        }

        StockDataCollection MergeMarketData(IEnumerable<StockDataCollection> allMarketData)
        {
            SampleUtilities.DoCpuIntensiveOperation(2.0 * speedFactor, cts.Token);
            var securities = new List<StockData>();

            if (!cts.Token.IsCancellationRequested)
            {
                foreach (StockDataCollection md in allMarketData)
                    securities.AddRange(md);
            }

            if (cts.Token.IsCancellationRequested)
                return null;
            else
                return new StockDataCollection(securities);
        }

        /// <summary>
        /// Normalize stock data.
        /// </summary>
        StockDataCollection NormalizeData(StockDataCollection marketData)
        {
            SampleUtilities.DoCpuIntensiveOperation(2.0 * speedFactor, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;
            else
                return new StockDataCollection(marketData);
        }

        StockAnalysisCollection AnalyzeData(StockDataCollection data)
        {
            if (cts.Token.IsCancellationRequested)
                return null;
            return MarketAnalyzer.Run(data);
        }

        MarketModel RunModel(StockAnalysisCollection data)
        {
            SampleUtilities.DoCpuIntensiveOperation(2.0 * speedFactor, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;
            else
                return MarketModeler.Run(data);
        }

        MarketRecommendation CompareModels(IEnumerable<MarketModel> models)
        {
            SampleUtilities.DoCpuIntensiveOperation(2.0 * speedFactor, cts.Token);
            if (cts.Token.IsCancellationRequested)
                return null;
            else
                return ModelComparer.Run(models.ToArray());
        }

        #endregion

        #region Analysis Public Methods

        /// <summary>
        /// Creates a market recommendation using a fully sequential operation
        /// </summary>
        /// <returns>A market recommendation</returns>
        public MarketRecommendation DoAnalysisSequential()
        {
            StockDataCollection nyseData = 
                LoadNyseData();
            StockDataCollection nasdaqData = 
                LoadNasdaqData();
            StockDataCollection mergedMarketData = 
                MergeMarketData(new[]{nyseData, nasdaqData});
            StockDataCollection normalizedMarketData = 
                NormalizeData(mergedMarketData);
            StockDataCollection fedHistoricalData = 
                LoadFedHistoricalData();
            StockDataCollection normalizedHistoricalData = 
                NormalizeData(fedHistoricalData);
            StockAnalysisCollection analyzedStockData = 
                AnalyzeData(normalizedMarketData);
            MarketModel modeledMarketData = 
                RunModel(analyzedStockData);
            StockAnalysisCollection analyzedHistoricalData = 
                AnalyzeData(normalizedHistoricalData);
            MarketModel modeledHistoricalData =
                RunModel(analyzedHistoricalData);
            MarketRecommendation recommendation = 
                CompareModels(new[] { modeledMarketData, 
                                      modeledHistoricalData});
            return recommendation;
        }

        /// <summary>
        /// Initiates market analysis using parallel computation.
        /// </summary>        
        /// <returns>Task record that may be queried for results of the analysis</returns>
        /// <remarks>Compare with the DoAnalysisSequential method</remarks>
        public AnalysisTasks DoAnalysisParallel()
        {
            TaskFactory factory = Task.Factory;

            if (cts != null)
            {
                cts.Dispose();
                cts = null;
            }
            cts = new CancellationTokenSource();

            Task<StockDataCollection> loadNyseData =
                Task<StockDataCollection>.Factory.StartNew(
                    () => LoadNyseData(),
                    TaskCreationOptions.LongRunning);

            Task<StockDataCollection> loadNasdaqData =
                Task<StockDataCollection>.Factory.StartNew(
                    () => LoadNasdaqData(),
                    TaskCreationOptions.LongRunning);

            Task<StockDataCollection> mergeMarketData =
                factory.ContinueWhenAll<StockDataCollection, StockDataCollection>(
                    new[] { loadNyseData, loadNasdaqData },
                    (tasks) => MergeMarketData(from t in tasks select t.Result));

            Task<StockDataCollection> normalizeMarketData =
                mergeMarketData.ContinueWith(
                    (t) => NormalizeData(t.Result));

            Task<StockDataCollection> loadFedHistoricalData =
                Task<StockDataCollection>.Factory.StartNew(
                    () => LoadFedHistoricalData(),
                    TaskCreationOptions.LongRunning);

            Task<StockDataCollection> normalizeHistoricalData =
                loadFedHistoricalData.ContinueWith(
                    (t) => NormalizeData(t.Result));

            Task<StockAnalysisCollection> analyzeMarketData =
                normalizeMarketData.ContinueWith(
                    (t) => AnalyzeData(t.Result));

            Task<MarketModel> modelMarketData =
                analyzeMarketData.ContinueWith(
                    (t) => RunModel(t.Result));

            Task<StockAnalysisCollection> analyzeHistoricalData =
                normalizeHistoricalData.ContinueWith(
                    (t) => AnalyzeData(t.Result));

            Task<MarketModel> modelHistoricalData =
                analyzeHistoricalData.ContinueWith(
                    (t) => RunModel(t.Result));

            Task<MarketRecommendation> compareModels =
                factory.ContinueWhenAll<MarketModel, MarketRecommendation>(
                    new[] { modelMarketData, modelHistoricalData },
                    (tasks) => CompareModels(from t in tasks select t.Result));

            Task errorHandler = CreateErrorHandler(loadNyseData, 
                loadNasdaqData, loadFedHistoricalData, 
                mergeMarketData, normalizeHistoricalData, 
                normalizeMarketData, analyzeHistoricalData, 
                analyzeMarketData, modelHistoricalData, 
                modelMarketData, compareModels);

            return new AnalysisTasks()
                    {
                        LoadNyseData = loadNyseData,
                        LoadNasdaqData = loadNasdaqData,
                        MergeMarketData = mergeMarketData,
                        NormalizeMarketData = normalizeMarketData,
                        LoadFedHistoricalData = loadFedHistoricalData,
                        NormalizeHistoricalData = normalizeHistoricalData,
                        AnalyzeMarketData = analyzeMarketData,
                        AnalyzeHistoricalData = analyzeHistoricalData,
                        ModelMarketData = modelMarketData,
                        ModelHistoricalData = modelHistoricalData,
                        CompareModels = compareModels,
                        ErrorHandler = errorHandler
                    };
        }

        Task CreateErrorHandler(params Task[] tasks)
        {
            return Task.Factory.ContinueWhenAll(tasks, (t) =>
            {
                try
                {
                    Task.WaitAll(tasks);
                }
                catch (AggregateException e)
                {
                    Console.WriteLine(e.Flatten());
                }
            });
        }

        public void TryCancelAnalysis()
        {
            if (cts != null)
                cts.Cancel();
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
                if (cts != null)
                {
                    cts.Dispose();
                    cts = null;
                }
            }
        }

        #endregion
    }
}

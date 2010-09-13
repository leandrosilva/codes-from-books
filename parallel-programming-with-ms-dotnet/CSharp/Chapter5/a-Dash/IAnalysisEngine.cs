//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects;

namespace Microsoft.Practices.ParallelGuideSamples.ADash
{
    public interface IAnalysisEngine : IDisposable
    {
        AnalysisTasks DoAnalysisParallel();
        MarketRecommendation DoAnalysisSequential();
        void TryCancelAnalysis();
    }
}

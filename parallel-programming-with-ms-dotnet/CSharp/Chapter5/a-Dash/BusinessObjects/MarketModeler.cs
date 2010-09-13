//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Diagnostics.CodeAnalysis;
namespace Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
{
    // This class is simply a placeholder to show that tasks in the graph can take
    // different data types as inputs and outputs. They illustrate data moving through
    // the model.
    public static class MarketModeler
    {
        // data parameter here is placeholder intended for illustrative purposes
        [SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "data")]
        public static MarketModel Run(StockAnalysisCollection data)
        {
            return new MarketModel();
        }
    }
}
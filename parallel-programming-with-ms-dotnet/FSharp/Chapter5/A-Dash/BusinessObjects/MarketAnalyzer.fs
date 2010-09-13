//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

/// This class is simply a placeholder to show that tasks in the graph can 
/// take different data types as inputs and outputs. They illustrate 
/// data moving through the model.
type MarketAnalyzer =
    [<SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "data")>]
    static member Run (data:StockDataCollection) =
        new StockAnalysisCollection(new List<StockAnalysis>())
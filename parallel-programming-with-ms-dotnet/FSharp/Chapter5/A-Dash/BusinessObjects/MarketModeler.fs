//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

open System.Diagnostics.CodeAnalysis

// This class is simply a placeholder to show that tasks in the graph can 
// take different data types as inputs and outputs. They illustrate 
// data moving through the model.
type MarketModeler =
    // Data parameter here is placeholder intended for illustrative purposes
    [<SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "data")>]
    static member Run(data:StockAnalysisCollection) =
        new MarketModel()

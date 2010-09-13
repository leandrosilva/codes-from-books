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
open System.Collections.ObjectModel

/// This class is simply a placeholder to show that tasks in the graph can 
/// take different data types as inputs and outputs. They illustrate 
/// data moving through the model.
type StockAnalysis(name:string, volatility:float) =
    member x.Name = name
    member x.Volatility = volatility

/// This class is simply a placeholder to show that tasks in the graph can 
/// take different data types as inputs and outputs. They illustrate 
/// data moving through the model.
[<AllowNullLiteral>]
type StockAnalysisCollection(data) = 
    inherit ReadOnlyCollection<StockAnalysis>(data)
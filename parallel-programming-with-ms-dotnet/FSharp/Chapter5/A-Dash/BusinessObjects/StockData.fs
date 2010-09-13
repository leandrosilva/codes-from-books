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

// This class is simply a placeholder to show that tasks in the graph can 
// take different data types as inputs and outputs. They illustrate 
// data moving through the model.
type StockData =
    // In C#, this class implements structural equality, so we use record 
    // in F# and store history as an immutable list, to get this for free
    { name : string
      priceHistory : float list }

    member x.Name = x.name
    member x.PriceHistory = x.priceHistory

/// A data set with time series price information for various financial assets
[<AllowNullLiteral>]
type StockDataCollection(data) = 
    inherit ReadOnlyCollection<StockData>(data)

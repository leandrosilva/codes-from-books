//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
{
    // This class is simply a placeholder to show that tasks in the graph can take
    // different data types as inputs and outputs. They illustrate data moving through
    // the model.
    public sealed class StockAnalysis
    {
        readonly string name;
        readonly double volatility;

        public string Name
        {
            get { return name; }
        }

        public double Volatility 
        { 
            get { return volatility; } 
        }

        public StockAnalysis(string name, double volatility)
        {
            this.name = name;
            this.volatility = volatility;
        }
    }

    // This class is simply a placeholder to show that tasks in the graph can take
    // different data types as inputs and outputs. They illustrate data moving through
    // the model.
    public class StockAnalysisCollection : ReadOnlyCollection<StockAnalysis>
    {
        public StockAnalysisCollection(IList<StockAnalysis> data)
            : base(data)
        {
        }
    }
}
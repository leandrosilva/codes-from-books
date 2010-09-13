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
    public sealed class StockData
    {
        readonly string name;
        readonly ReadOnlyCollection<double> priceHistory;

        public string Name
        {
            get { return name; }
        }

        public ReadOnlyCollection<double> PriceHistory
        {
            get { return priceHistory; }
        }

        public StockData(string name, double[] priceHistory)
        {
            this.name = name;
            this.priceHistory = 
                     new ReadOnlyCollection<double>(priceHistory);
        }

        // Implement value equality

        public static bool operator ==(StockData a, StockData b)
        {
            if (System.Object.ReferenceEquals(a, b))
                return true;
            if (((object)a == null) || ((object)b == null))
                return false;
            if (a.Name != b.Name)
                return false;
            if (a.PriceHistory.Count != b.PriceHistory.Count)
                return false;
            for (int i = 0; i < a.PriceHistory.Count; i++)
            {
                if (a.PriceHistory[i] != b.PriceHistory[i])
                    return false;
            }

            return true;
        }

        public static bool operator !=(StockData a, StockData b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
                return false;
            return Equals(obj as StockData);
        }

        public bool Equals(StockData d)
        {
            if (d == null)
                return false;
            return (this == d);
        }

        public override int GetHashCode()
        {
            int result = name.GetHashCode() ^ priceHistory.Count;
            for (int i = 0; i < priceHistory.Count; i++)
                result ^= priceHistory[i].GetHashCode();
            return result;
        }
    }

    /// <summary>
    /// A data set with time series price information for various financial assets
    /// </summary>
    public sealed class StockDataCollection : ReadOnlyCollection<StockData>
    {
        public StockDataCollection(IList<StockData> data)
            : base(data)
        {
        }
    }
}
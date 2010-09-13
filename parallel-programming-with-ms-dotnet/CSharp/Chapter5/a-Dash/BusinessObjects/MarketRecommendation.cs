//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
{
    // This class is simply a placeholder to show that tasks in the graph can take
    // different data types as inputs and outputs. They illustrate data moving through
    // the model.
    public class MarketRecommendation
    {
        readonly string recommendation;

        public string Value 
        { 
            get { return recommendation; }
        }

        public MarketRecommendation(string recommendation)
        {
            this.recommendation = recommendation;
        }
    }
}
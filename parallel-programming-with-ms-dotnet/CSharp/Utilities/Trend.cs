//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Diagnostics.CodeAnalysis;

namespace Microsoft.Practices.ParallelGuideSamples.Utilities
{
    /// <summary>
    /// Linear trend from slope and intercept. Predict y given any x value using the formula
    /// y = slope * x + intercept.
    /// </summary>
    [SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes")] 
    public struct Trend
    {
        /// <summary>
        /// The change in y per unit of x.
        /// </summary>
        public double Slope { get; set; }

        /// <summary>
        /// The value of y when x is zero.
        /// </summary>
        public double Intercept { get; set; }

        /// <summary>
        /// Predicts a y value given any x value using the formula y = slope * x + intercept.
        /// </summary>
        /// <param name="ordinate">The x value</param>
        /// <returns>The predicted y value</returns>
        public double Predict(double ordinate) 
        { 
            return Slope * ordinate + Intercept; 
        }
    }
}

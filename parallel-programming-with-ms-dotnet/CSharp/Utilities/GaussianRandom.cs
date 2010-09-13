//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;

namespace Microsoft.Practices.ParallelGuideSamples.Utilities
{
    /// <summary>
    /// Normally distributed random value generator
    /// </summary>
    public class GaussianRandom
    {
        readonly Random random = new Random();
        readonly double mean;
        readonly double standardDeviation; 

        #region Constructors
        
        /// <summary>
        /// Creates a new instance of a normally distributed random value generator
        /// using the specified mean and standard deviation.
        /// </summary>
        /// <param name="mean">The average value produced by this generator</param>
        /// <param name="standardDeviation">The amount of variation in the values produced by this generator</param>
        public GaussianRandom(double mean, double standardDeviation)
        {
            random = new Random();
            this.mean = mean;
            this.standardDeviation = standardDeviation;
        }
        
        /// <summary>
        /// Creates a new instance of a normally distributed random value generator
        /// using the specified mean, standard deviation and seed.
        /// </summary>
        /// <param name="mean">The average value produced by this generator</param>
        /// <param name="standardDeviation">The amount of variation in the values produced by this generator</param>
        /// <param name="seed">A number used to calculate a starting value for the pseudo-random number
        /// sequence. If a negative number is specified, the absolute value of the number
        /// is used.</param>
        public GaussianRandom(double mean, double standardDeviation, int seed)
        {
            random = new Random(seed);
            this.mean = mean;
            this.standardDeviation = standardDeviation;
        } 
        #endregion

        #region Public Methods
        
        /// <summary>
        /// Samples the distribution and returns a random integer
        /// </summary>
        /// <returns>A normally distributed random number rounded to the nearest integer</returns>
        public int NextInteger()
        {
            return (int)Math.Floor(Next() + 0.5);
        }

        /// <summary>
        /// Samples the distribution
        /// </summary>
        /// <returns>A random sample from a normal distribution</returns>
        public double Next()
        {
            double x = 0.0;

            // get the next value in the interval (0, 1) from the underlying uniform distribution
            while (x == 0.0 || x == 1.0)
                x = random.NextDouble();

            // transform uniform into normal
            return SampleUtilities.GaussianInverse(x, mean, standardDeviation);
        } 
        #endregion
    }
}

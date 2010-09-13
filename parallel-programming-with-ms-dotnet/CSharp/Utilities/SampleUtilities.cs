//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Threading;

namespace Microsoft.Practices.ParallelGuideSamples.Utilities
{
    /// <summary>
    /// Static class that contains timing and numerical utilities
    /// </summary>
    public static class SampleUtilities
    {
        #region Timing utilities

        /// <summary>
        /// Format and print elapsed time returned by Stopwatch
        /// </summary>
        public static void PrintTime(TimeSpan ts)
        {
            Console.WriteLine(FormattedTime(ts));
        }

        /// <summary>
        /// TimeSpan pretty printer
        /// </summary>
        /// <param name="ts">The TimeSpan to format</param>
        /// <returns>A formatted string</returns>
        public static string FormattedTime(TimeSpan ts)
        {
            return String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10);
        }

        /// <summary>
        /// Executes a function and prints timing results
        /// </summary>
        public static void TimedAction(Action test, string label)
        {
            Console.WriteLine("Starting {0}", label);
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();

            test();

            stopWatch.Stop();
            TimeSpan seqT = stopWatch.Elapsed;
            Console.WriteLine("{0}: {1}", label, FormattedTime(seqT));
            Console.WriteLine();
        }

        /// <summary>
        /// Executes a function and prints timing results
        /// </summary>
        public static void TimedRun<T>(Func<T> test, string label)
        {
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();

            var result = test();

            stopWatch.Stop();
            TimeSpan seqT = stopWatch.Elapsed;
            Console.WriteLine("{0} (result={1}): {2}", label, result.ToString(), FormattedTime(seqT));
        }

        /// <summary>
        /// Simulates a CPU-intensive operation on a single core. The operation will use approximately 100% of a
        /// single CPU for a specified duration.
        /// </summary>
        /// <param name="seconds">The approximate duration of the operation in seconds</param>
        /// <returns>true if operation completed normally; false if the user canceled the operation</returns>
        public static bool DoCpuIntensiveOperation(double seconds)
        {
            return DoCpuIntensiveOperation(seconds, CancellationToken.None, false);
        }

        /// <summary>
        /// Simulates a CPU-intensive operation on a single core. The operation will use approximately 100% of a
        /// single CPU for a specified duration.
        /// </summary>
        /// <param name="seconds">The approximate duration of the operation in seconds</param>
        /// <param name="token">A token that may signal a request to cancel the operation.</param>
        /// <param name="throwOnCancel">true if an execption should be thrown in response to a cancellation request.</param>
        /// <returns>true if operation completed normally; false if the user canceled the operation</returns>
        public static bool DoCpuIntensiveOperation(double seconds, CancellationToken token, bool throwOnCancel = false)
        {
            if (token.IsCancellationRequested)
            {
                if (throwOnCancel)
                    token.ThrowIfCancellationRequested();
                return false;
            }

            int ms = (int)(seconds * 1000);
            Stopwatch sw = new Stopwatch();
            sw.Start();
            int checkInterval = Math.Min(20000000, (int)(20000000 * seconds));

            // loop to simulate a computationally intensive operation
            int i = 0;
            while (true)
            {
                i += 1;

                // periodically check to see if the user has requested cancellation 
                // or if the time limit has passed
                if (seconds == 0.0d || i % checkInterval == 0)
                {
                    if (token.IsCancellationRequested)
                    {
                        if (throwOnCancel) token.ThrowIfCancellationRequested();
                        return false;
                    }

                    if (sw.ElapsedMilliseconds > ms)
                        return true;
                }
            }
        }

        // vary to simulate I/O jitter
        readonly static int[] SleepTimeouts = new int[] { 65, 165, 110, 110, 185, 160, 40, 125, 275, 110, 80,
            190, 70, 165, 80, 50, 45, 155, 100, 215, 85, 115, 180, 195, 135, 265, 120, 60, 130, 115, 200, 105, 310,
            100, 100, 135, 140, 235, 205, 10, 95, 175, 170, 90, 145, 230, 365, 340, 160, 190, 95, 125, 240, 145,
            75, 105, 155, 125, 70, 325, 300, 175, 155, 185, 255, 210, 130, 120, 55, 225, 120, 65, 400, 290, 205,
            90, 250, 245, 145, 85, 140, 195, 215, 220, 130, 60, 140, 150, 90, 35, 230,
            180, 200, 165, 170, 75, 280, 150, 260, 105
        };

        /// <summary>
        /// Simulates an I/O-intensive operation on a single core. The operation will use only a small percent of a
        /// single CPU's cycles; however, it will block for the specified number of seconds.
        /// </summary>
        /// <param name="seconds">The approximate duration of the operation in seconds</param>
        /// <param name="token">A token that may signal a request to cancel the operation.</param>
        /// <param name="throwOnCancel">true if an execption should be thrown in response to a cancellation request.</param>
        /// <returns>true if operation completed normally; false if the user canceled the operation</returns>
        public static bool DoIoIntensiveOperation(double seconds, CancellationToken token, bool throwOnCancel = false)
        {
            if (token.IsCancellationRequested) return false;
            int ms = (int)(seconds * 1000);
            Stopwatch sw = new Stopwatch();
            sw.Start();
            int timeoutCount = SleepTimeouts.Length;

            // loop to simulate i/o intensive operation
            int i = (Math.Abs(sw.GetHashCode()) % timeoutCount);
            while (true)
            {
                int timeout = SleepTimeouts[i];
                i += 1;
                i = i % timeoutCount;

                // simulate i/o latency
                Thread.Sleep(timeout);

                // Has the user requested cancellation? 
                if (token.IsCancellationRequested)
                {
                    if (throwOnCancel) token.ThrowIfCancellationRequested();
                    return false;
                }

                // Is the computation finished?
                if (sw.ElapsedMilliseconds > ms)
                    return true;
            }
        }

        #endregion

        #region File utilities

        /// <summary>
        /// Check whether directory exists, if not write message and exit immediately.
        /// </summary>
        /// <param name="dirName">Directory name</param>
        public static void CheckDirectoryExists(string dirName)
        {
            if (!Directory.Exists(dirName))
            {
                Console.WriteLine("Directory does not exist: {0}", dirName);
                Environment.Exit(0);
            }
        }

        /// <summary>
        /// Check whether file exists, if not write message and exit immediately.
        /// (can't use this method to check whether directory exists)
        /// </summary>
        /// <param name="path">Fully qualified file name including directory</param>
        public static void CheckFileExists(string path)
        {
            if (!File.Exists(path))
            {
                Console.WriteLine("File does not exist: {0}", path);
                Environment.Exit(0);
            }
        }

        /// <summary>
        /// Repeatedly loop through all of the files in the source directory. This
        /// enumerable has an infinite number of values.
        /// </summary>
        /// <param name="sourceDir"></param>
        /// <param name="maxImages"></param>
        /// <returns></returns>
        public static IEnumerable<string> GetImageFilenames(string sourceDir, int maxImages)
        {
            var names = GetImageFilenamesList(sourceDir, maxImages);
            while (true)
            {
                foreach (var name in names)
                    yield return name;
            }
        }

        /// <summary>
        /// Get names of image files in directory
        /// </summary>
        /// <param name="sourceDir">Name of directory</param>
        /// <param name="maxImages">Maximum number of image file names to return</param>
        /// <returns>List of image file names in directory (basenames not including directory path)</returns>
        static IEnumerable<string> GetImageFilenamesList(string sourceDir, int maxImages)
        {
            List<string> fileNames = new List<string>();
            var dirInfo = new DirectoryInfo(sourceDir);

            foreach (var file in dirInfo.GetFiles())
            {
                if (file.Extension.ToUpper(CultureInfo.InvariantCulture) == ".JPG") // LIMITATION - only handles jpg, not gif, png etc.
                {
                    fileNames.Add(file.Name);
                }
            }
            return fileNames.Take(Math.Min(maxImages, fileNames.Count)).OrderBy(f => f).ToList();
        }

        #endregion

        #region Numerical Routines

        /// <summary>
        /// Return array of floats for indices 0 .. count-1
        /// </summary>
        public static double[] Range(int count)
        {
            if (count < 0)
                throw new ArgumentOutOfRangeException("count");

            double[] x = new double[count];
            for (int i = 0; i < count; i++)
            {
                x[i] = i;
            }
            return x;
        }

        /// <summary>
        /// Linear regression with x-values given implicity by the y-value indices
        /// </summary>
        /// <param name="ordinateValues">A series of two or more values</param>
        /// <returns>A trend line</returns>
        public static Trend Fit(double[] ordinateValues)
        {
            if (ordinateValues == null)
                throw new ArgumentNullException("ordinateValues");
            // special case - x values are just the indices of the y's
            return Fit(Range(ordinateValues.Length), ordinateValues);
        }

        /// <summary>
        /// Linear regression of (x, y) pairs
        /// </summary>
        /// <param name="abscissaValues">The x values</param>
        /// <param name="ordinateValues">The y values corresponding to each x value</param>
        /// <returns>A trend line that best predicts each (x, y) pair</returns>
        public static Trend Fit(double[] abscissaValues, double[] ordinateValues)
        {
            if (abscissaValues == null)
                throw new ArgumentNullException("abscissaValues");
            if (ordinateValues == null)
                throw new ArgumentNullException("ordinateValues");
            if (abscissaValues.Length != ordinateValues.Length)
                throw new ArgumentException("abscissaValues and ordinateValues must contain the same number of values.");
            if (abscissaValues.Length < 2)
                throw new ArgumentException("abscissaValues must contain at least two elements");

            double xx = 0, xy = 0;
            double abscissaMean = abscissaValues.Average();
            double ordinateMean = ordinateValues.Average();

            // calculate the sum of squared differences
            for (int i = 0; i < abscissaValues.Length; i++)
            {
                double xi = abscissaValues[i] - abscissaMean;
                xx += xi * xi;
                xy += xi * (ordinateValues[i] - ordinateMean);
            }

            if (xx == 0.0d)
                throw new ArgumentException("abscissaValues must not all be coincident");
            double slope = xy / xx;
            return new Trend { Slope = slope, Intercept = ordinateMean - slope * abscissaMean };
        }

        /// <summary>
        /// Calculates an approximation of the inverse of the cumulative normal distribution.
        /// </summary>
        /// <param name="cumulativeDistribution">The percentile as a fraction (.50 is the fiftieth percentile). 
        /// Must be greater than 0 and less than 1.</param>
        /// <param name="mean">The underlying distribution's average (i.e., the value at the 50th percentile) (</param>
        /// <param name="standardDeviation">The distribution's standard deviation</param>
        /// <returns>The value whose cumulative normal distribution (given mean and stddev) is the percentile given as an argument.</returns>
        public static double GaussianInverse(double cumulativeDistribution, double mean, double standardDeviation)
        {
            if (!(0.0 < cumulativeDistribution && cumulativeDistribution < 1.0))
                throw new ArgumentOutOfRangeException("cumulativeDistribution");

            double result = GaussianInverse(cumulativeDistribution);
            return mean + result * standardDeviation;
        }

        // Adaptation of Peter J. Acklam's Perl implementation. See http://home.online.no/~pjacklam/notes/invnorm/
        // This approximation has a relative error of 1.15 × 10−9 or less. 
        static double GaussianInverse(double value)
        {
            // Lower and upper breakpoints
            const double plow = 0.02425;
            const double phigh = 1.0 - plow;

            double p = (phigh < value) ? 1.0 - value : value;
            double sign = (phigh < value) ? -1.0 : 1.0;
            double q;

            if (p < plow)
            {
                // Rational approximation for tail
                var c = new double[]{-7.784894002430293e-03, -3.223964580411365e-01,
                                         -2.400758277161838e+00, -2.549732539343734e+00,
                                         4.374664141464968e+00, 2.938163982698783e+00};

                var d = new double[]{7.784695709041462e-03, 3.224671290700398e-01,
                                       2.445134137142996e+00, 3.754408661907416e+00};
                q = Math.Sqrt(-2 * Math.Log(p));
                return sign * (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
                                                ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
            }
            else
            {
                // Rational approximation for central region
                var a = new double[]{-3.969683028665376e+01, 2.209460984245205e+02,
                                         -2.759285104469687e+02, 1.383577518672690e+02,
                                         -3.066479806614716e+01, 2.506628277459239e+00};

                var b = new double[]{-5.447609879822406e+01, 1.615858368580409e+02,
                                         -1.556989798598866e+02, 6.680131188771972e+01,
                                         -1.328068155288572e+01};
                q = p - 0.5;
                var r = q * q;
                return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q /
                                         (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
            }
        }

        #endregion
        
        #region Other Utilities
       
        /// <summary>
        /// Creates a seed that does not depend on the system clock. A unique value will be created with each invocation.
        /// </summary>
        /// <returns>An integer that can be used to seed a random generator</returns>
        /// <remarks>This method is thread safe.</remarks>
        public static int MakeRandomSeed()
        {
            return Guid.NewGuid().ToString().GetHashCode();
        } 
        #endregion
    }
}

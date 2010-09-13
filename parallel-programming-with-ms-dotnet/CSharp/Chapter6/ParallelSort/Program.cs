//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Globalization;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.ParallelSort
{
    public static class Program
    {
        /// <summary>
        /// Create array of given length and populate with random integers 
        /// </summary>
        public static int[] MakeArray(int length, int seed)
        {
            const int max = 1000000;
            var r = new Random(seed); 
            var a = new int[length];
            for (int i = 0; i < length; i++) a[i] = r.Next(max); 
            return a;
        }

        /// <summary>
        /// Print the first and last few elements in given array
        /// </summary>
        static void PrintElements(int[] array, int count)
        {
            Console.Write("[");
            for (int i = 0; i < count/2; i++) Console.Write("{0} ", array[i]);
            Console.Write("... ");
            for (int i = array.Length - count / 2; i < array.Length;  i++) Console.Write("{0} ", array[i]);
            Console.WriteLine("], {0} elements", array.Length);
        }

        /// <summary>
        /// Command line arguments are:
        ///   length - of array to sort
        ///   threshold -  array length to use InsertionSort instead of SequentialQuickSort
        /// </summary>
        static void Main(string[] args)
        {
            Console.WriteLine("Sort Sample\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            
            int length = 40000000; // default
            int seed = 1; // seed for reproducible runs
            if (args.Length > 0) length = Int32.Parse(args[0], CultureInfo.CurrentCulture);
            if (args.Length > 1) Sort.Threshold = Int32.Parse(args[1], CultureInfo.CurrentCulture);

            Console.WriteLine();
            var a = MakeArray(length, seed); 
            PrintElements(a, 8);
            SampleUtilities.TimedRun(() => { Sort.SequentialQuickSort(a); return a.Length; }, "  Sequential QuickSort");
            PrintElements(a, 8);

            Console.WriteLine();
            a = MakeArray(length, seed); 
            PrintElements(a, 8);
            SampleUtilities.TimedRun(() => {Sort.ParallelQuickSort(a); return a.Length; }, "      Parallel QuickSort");
            PrintElements(a, 8);

            Console.WriteLine("\nRun complete... press enter to finish."); 
            Console.ReadKey();
        }
    }
}

//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Threading.Tasks;
using System.Windows.Controls;
using Microsoft.Practices.ParallelGuideSamples.Utilities;
using System.Diagnostics.CodeAnalysis;

namespace Microsoft.Practices.ParallelGuideSamples.BasicFutures
{
    /// <summary>
    /// This program shows the simplest use case for the futures and continuations pattern.
    /// Refer to Chapter 5 of the text.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// A computationally intensive function
        /// </summary>
        static int F1(int value)
        {
            SampleUtilities.DoCpuIntensiveOperation(2.0);
            return value * value;
        }

        /// <summary>
        /// A computationally intensive function
        /// </summary>
        static int F2(int value)
        {
            SampleUtilities.DoCpuIntensiveOperation(1.0);
            return value - 2;
        }

        /// <summary>
        /// A computationally intensive function
        /// </summary>
        static int F3(int value)
        {
            SampleUtilities.DoCpuIntensiveOperation(1.0);
            return value + 1;
        }

        /// <summary>
        /// A computationally intensive function
        /// </summary>
        static int F4(int value1, int value2)
        {
            SampleUtilities.DoCpuIntensiveOperation(0.1);
            return value1 + value2;
        }

        /// <summary>
        /// Sequential example
        /// </summary>
        public static int Example1()
        {
            var a = 22;

            var b = F1(a); 
            var c = F2(a); 
            var d = F3(c); 
            var f = F4(b, d); 
            return f;
        }

        /// <summary>
        /// A parallel example that uses the futures pattern for F1
        /// </summary>
        public static int Example2()
        {
            var a = 22;

            var bf = Task<int>.Factory.StartNew(() => F1(a));
            var c = F2(a);
            var d = F3(c);
            var f = F4(bf.Result, d);
            return f;
        }

        /// <summary>
        /// A parallel example that uses the futures pattern for F2/F3
        /// </summary>
        public static int Example3()
        {
            var a = 22;

            var df = Task<int>.Factory.StartNew(() => F3(F2(a)));
            var b = F1(a);
            var f = F4(b, df.Result);
            return f;
        }

        /// <summary>
        /// A parallel example that uses the futures and continations pattern.
        /// This is to illustrate syntax only; there is no performance benefit in this case over Example 2 or 3 above.
        /// </summary>
        public static int Example4()
        {
            var a = 22;

            var cf = Task<int>.Factory.StartNew(() => F2(a));
            var df = cf.ContinueWith((t) => F3(t.Result));
            var b = F1(a);
            var f = F4(b, df.Result);
            return f;
        }

        /// <summary>
        /// A parallel example that uses the futures pattern applied to two values.
        /// This is for comparison only; there is no performance benefit in this case over Example 2 or 3 above.
        /// You should pattern your own code after either Example 2 or 3, not this method.
        /// </summary>
        public static int Example5()
        {
            var a = 22;
            var bf = Task<int>.Factory.StartNew(() => F1(a));
            var df = Task<int>.Factory.StartNew(() => F3(F2(a)));
            var f = F4(bf.Result, df.Result);
            return f;
        }

        [SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")]
        public static int Example6()
        {
            var a = 22;

            Task<int> futureD = Task<int>.Factory.StartNew(
                                          () => F3(F2(a)));
            try
            {
                int b = F1(a);
                int f = F4(b, futureD.Result);
                return f;
            }
            catch // any exception
            {
                Console.WriteLine("Saw exception");
                return -1;
            }
        }

        public static void Example7(TextBox myTextBox)
        {
            var a = 22; 
            
            var futureB = Task.Factory.StartNew<int>(() => F1(a));
            var futureD = Task.Factory.StartNew<int>(() => F3(F2(a)));

            var futureF = Task.Factory.ContinueWhenAll <int, int>(
                 new[] { futureB, futureD },
                            (tasks) => F4(futureB.Result, futureD.Result));
  
            futureF.ContinueWith((t) =>
                myTextBox.Dispatcher.Invoke(
                    (Action)(() => { myTextBox.Text = t.Result.ToString(); }))
                );
        }

        public static void Example8()
        {
            AsyncCallback cb = new AsyncCallback((iar1) => {});
            Action a = () => { Console.WriteLine("Hello"); };
            var t1 = Task.Factory.FromAsync(a.BeginInvoke(cb, null), a.EndInvoke);
            t1.Wait();
        }

        static void Main()
        {
            // Note: for consistent timing results, run these without the debugger. 
            // Observe CPU usage using the task manager. On a multicore machine, the sequential 
            // version will use less CPU and execute more slowly than the parallel versions.

            Console.WriteLine("Basic Futures Samples\n");
#if(DEBUG)
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            Console.WriteLine("Starting...");

            // timed comparison between sequential and two ways of using the futures pattern
            SampleUtilities.TimedRun(Example1, "Sequential");
            SampleUtilities.TimedRun(Example2, "Parallel, using F1 future");
            SampleUtilities.TimedRun(Example3, "Parallel, using F2/F3 future");

            // additional variants for comparison
            Console.WriteLine();
            Console.WriteLine("Other variants, for comparison--");
            SampleUtilities.TimedRun(Example4, "Parallel, using F2 future and F3 continuation");
            SampleUtilities.TimedRun(Example5, "Parallel, using F1 and F2/F3 future");
            SampleUtilities.TimedRun(Example6, "Parallel, with try/catch block");

            Console.WriteLine("\nRun complete... press enter to finish."); 
            Console.ReadLine();
        }
    }
}
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
using Microsoft.Practices.ParallelGuideSamples.Utilities;
using System.Threading;
using System.Linq;
using System.IO;

namespace Microsoft.Practices.ParallelGuideSamples.BasicParallelTasks
{
    class Program
    {
        const double TaskSeconds = 1.0;

        static void Main()
        {
            Task.Factory.StartNew(() => MainTask()).Wait();
        }

        static void MainTask()
        {
            Console.WriteLine("Basic Parallel Tasks Samples\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            SampleUtilities.TimedAction(Chapter3Sample01Sequential, "2 steps, sequential");
            SampleUtilities.TimedAction(Chapter3Sample01ParallelTask, "2 steps (Task.Wait), parallel");
            SampleUtilities.TimedAction(Chapter3Sample01ParallelInvoke, "2 steps, parallel invoke");

            SampleUtilities.TimedAction(Chapter3Sample03, "Speculative Execution");
            SampleUtilities.TimedAction(Chapter3Sample04_1, "Task.WaitAny");

            ExampleOfIncorrectClosure();
            ExampleOfCorrectClosure();
            ExampleOfIncorrectDispose();
            ExampleOfCorrectDispose();

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadLine();
        }

        static void Chapter3Sample01Sequential()
        {
            DoLeft();
            DoRight();
        }

        static void Chapter3Sample01ParallelTask()
        {
            Task t1 = Task.Factory.StartNew(DoLeft);
            Task t2 = Task.Factory.StartNew(DoRight);

            Task.WaitAll(t1, t2);
        }

        static void Chapter3Sample01ParallelInvoke()
        {
            Parallel.Invoke(DoLeft, DoRight);
        }

        static void Chapter3Sample03()
        {
           Chapter3Sample03_1();
        }

        static void Chapter3Sample03_1()
        {
            SpeculativeInvoke(SearchLeft, SearchRight, SearchCenter);
        }

        public static void SpeculativeInvoke(params Action<CancellationToken>[] actions)
        {
            var cts = new CancellationTokenSource();
            var token = cts.Token;
            var tasks = (from a in actions
                         select Task.Factory.StartNew(() => a(token), token)).ToArray();
            Task.WaitAny(tasks);
            cts.Cancel();
            try 
            { 
                Task.WaitAll(tasks); 
            }
            catch (AggregateException ae)
            {
                ae.Flatten().Handle(e => e is OperationCanceledException);
            }
            finally
            {
                if (cts != null) cts.Dispose();
            }
        }

        static void DoLeft()
        {
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .2);
        }

        static void DoRight()
        {
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .3);
        }

        static void DoCenter()
        {
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .2);
        }

        static void SearchCenter(CancellationToken token)
        {
            token.ThrowIfCancellationRequested();
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .5, token);
            token.ThrowIfCancellationRequested();
        }

        static void SearchLeft(CancellationToken token)
        {
            token.ThrowIfCancellationRequested();
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .2, token);
            token.ThrowIfCancellationRequested();
        }

        static void SearchRight(CancellationToken token)
        {
            token.ThrowIfCancellationRequested();
            SampleUtilities.DoCpuIntensiveOperation(TaskSeconds * .3, token);
            token.ThrowIfCancellationRequested();
        }

        static void Chapter3Sample04_1()
        {
            var taskIndex = -1;
            Task[] tasks = new Task[] 
                            {
                                Task.Factory.StartNew(DoLeft),
                                Task.Factory.StartNew(DoRight),
                                Task.Factory.StartNew(DoCenter) 
                            };
            Task[] allTasks = tasks;

            while (tasks.Length > 0)
            {
                taskIndex = Task.WaitAny(tasks);
                Console.WriteLine("Finished task {0}.", taskIndex + 1);
                tasks = tasks.Where((t) => t != tasks[taskIndex]).ToArray();
            }

            try
            {
                Task.WaitAll(allTasks);
            }
            catch (AggregateException ae)
            {
                ae.Handle(e =>
                {
					// Modify DoCenter to throw an InvalidOperationException to see this message.
                    if (e is InvalidOperationException)
                    {
                        Console.WriteLine("Saw expected exception.");
                        return true;
                    }
                    else
                        return false;
                });
            }
        }

        static void ExampleOfIncorrectClosure()
        {
            Console.WriteLine("Incorrectly written closure returns unexpected values:");
            Task[] tasks = new Task[4];

            for (int i = 0; i < 4; i++)
            {
                tasks[i] = Task.Factory.StartNew(() => Console.WriteLine(i));
            }

            Task.WaitAll(tasks);
        }

        static void ExampleOfCorrectClosure()
        {
            Console.WriteLine("Correctly written closure returns expected values:");
            Task[] tasks = new Task[4];

            for (int i = 0; i < 4; i++)
            {
                var tmp = i;
                tasks[i] = Task.Factory.StartNew(() => Console.WriteLine(tmp));
            }

            Task.WaitAll(tasks);
        }

        static void ExampleOfIncorrectDispose()
        {
            try
            {
                Task<string> t;
                using (var file = new StringReader("text"))
                {
                    t = Task<string>.Factory.StartNew(() => file.ReadLine());
                }
                // WARNING: BUGGY CODE, file has been disposed
                Console.WriteLine(t.Result);
            }
            catch (AggregateException ae)
            {
                ae.Handle(e =>
                {
                    if (e is ObjectDisposedException)
                    {
                        Console.WriteLine("Saw expected error: {0}", e.Message);
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                });
            }        
        }

        static void ExampleOfCorrectDispose()
        {
            StringReader file = null;
            try
            {
                file = new StringReader("text");
                Task<string> t = Task<string>.Factory.StartNew(() => file.ReadLine());

                Console.WriteLine("Saw correct output: {0}", t.Result);
            }
            finally
            {
                if (file != null) file.Dispose();
            }
        }
    }
}

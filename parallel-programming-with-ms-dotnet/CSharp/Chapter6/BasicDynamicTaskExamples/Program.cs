//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.BasicDynamicTasks
{
    class Program
    {
        public class Tree<T>
        {
            public T Data { get; set; }
            public Tree<T> Left { get; set; }
            public Tree<T> Right { get; set; }
        }

        static void Main()
        {
            Task.Factory.StartNew(() => MainTask()).Wait();
        }

        const int N = 1;                   // number of timing runs per test
        const double Time = 0.01;          // CPU time in seconds to visit each node of the tree
        const int TreeSize = 2000;         // number of nodes in the tree
        const double TreeDensity = 0.75;   // P(left child node exists), P(right child node exists) for interior nodes

        static void MainTask()
        {
            Console.WriteLine("Basic Dynamic Task Samples\n");
#if(DEBUG)
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif

            Console.WriteLine("Tree Walking");
            var tree = MakeTree(TreeSize, TreeDensity);

            SampleUtilities.TimedAction(() => Chapter6Example1Sequential(tree),
                "tree traversal, sequential");

            SampleUtilities.TimedAction(() => Chapter6Example1Parallel(tree),
                "tree traversal, parallel");

            SampleUtilities.TimedAction(() => Chapter6Example1Parallel2(tree),
                "tree traversal, parallel - attached to parent");

            SampleUtilities.TimedAction(() => Chapter6Example01ParallelWhileNotEmpty(tree),
                "parallel while not empty - Parallel.ForEach");

            SampleUtilities.TimedAction(() => Chapter6Example01ParallelWhileNotEmpty2(tree),
                "parallel while not empty - parallel tasks");

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadKey();
        }

        static void SequentialWalk<T>(Tree<T> tree, Action<T> action)
        {
            if (tree == null) return;
            action(tree.Data);
            SequentialWalk(tree.Left, action);
            SequentialWalk(tree.Right, action);
        }

        static void ParallelWalk<T>(Tree<T> tree, Action<T> action)
        {
            if (tree == null) return;
            var t1 = Task.Factory.StartNew(
                       () => action(tree.Data));
            var t2 = Task.Factory.StartNew(
                       () => ParallelWalk(tree.Left, action));
            var t3 = Task.Factory.StartNew(
                       () => ParallelWalk(tree.Right, action));
            Task.WaitAll(t1, t2, t3);
        }

        static void ParallelWalk2<T>(Tree<T> tree, Action<T> action)
        {
            if (tree == null)
                return;
            var t1 = Task.Factory.StartNew(() => action(tree.Data),
                        TaskCreationOptions.AttachedToParent);
            var t2 = Task.Factory.StartNew(() => ParallelWalk2(tree.Left, action),
                        TaskCreationOptions.AttachedToParent);
            var t3 = Task.Factory.StartNew(() => ParallelWalk2(tree.Right, action),
                        TaskCreationOptions.AttachedToParent);
            Task.WaitAll(t1, t2, t3);
        }

        public static Tree<string> MakeTree(int nodeCount, double density)
        {
            if (nodeCount < 1) throw new ArgumentOutOfRangeException("nodeCount");
            if (!(0 < density && density <= 1.0)) throw new ArgumentOutOfRangeException("density"); 
            return MakeTree(nodeCount, density, 0, new Random());
        }

        static Tree<string> MakeTree(int nodeCount, double density, int offset, Random r)
        {
            var flip1 = r.NextDouble() > density;
            var flip2 = r.NextDouble() > density;
            var newCount = nodeCount - 1;
            int count1 = flip1 && flip2 ? newCount / 2 : flip1 ? newCount : 0;
            int count2 = newCount - count1;
            if (r.NextDouble() > 0.5)
            {
                var tmp = count1;
                count1 = count2;
                count2 = tmp;
            }

            return new Tree<string>()
            {
                Data = offset.ToString(),
                Left = count1 > 0 ? MakeTree(count1, density, offset + 1, r) : null,
                Right = count2 > 0 ? MakeTree(count2, density, offset + 1 + count1, r) : null
            };
        }

        static void Chapter6Example1Sequential(Tree<string> tree)
        {
            for (int i = 0; i < N; i++)
            {
                List<string> result = new List<string>();
                SequentialWalk(tree, (data) =>
                    {
                        SampleUtilities.DoCpuIntensiveOperation(Time);
                        result.Add(data);
                    });
            }
            Console.WriteLine();
        }

        static void Chapter6Example1Parallel(Tree<string> tree)
        {
            for (int i = 0; i < N; i++)
            {
                ConcurrentBag<string> result = new ConcurrentBag<string>();
                ParallelWalk(tree, (data) =>
                {
                    SampleUtilities.DoCpuIntensiveOperation(Time);
                    result.Add(data);
                });
            }
        }

        static void Chapter6Example1Parallel2(Tree<string> tree)
        {
            for (int i = 0; i < N; i++)
            {
                ConcurrentBag<string> result = new ConcurrentBag<string>();
                ParallelWalk2(tree, (data) =>
                {
                    SampleUtilities.DoCpuIntensiveOperation(Time);
                    result.Add(data);
                });
            }
        }

        static void ParallelWhileNotEmpty<T>(
            IEnumerable<T> initialValues,
            Action<T, Action<T>> body)
        {
            var from = new ConcurrentQueue<T>(initialValues);

            while (!from.IsEmpty)
            {
                var to = new ConcurrentQueue<T>();
                Action<T> addMethod = to.Enqueue; 
                Parallel.ForEach(from, v => body(v, addMethod));
                from = to;
            }
        }

        static void ParallelWalk4<T>(Tree<T> tree, Action<T> action)
        {
            if (tree == null) return;
            ParallelWhileNotEmpty(new[] { tree }, (item, adder) =>
            {
                if (item.Left != null) adder(item.Left);
                if (item.Right != null) adder(item.Right);
                action(item.Data);
            });
        }

        static void Chapter6Example01ParallelWhileNotEmpty(Tree<string> tree)
        {
            for (int i = 0; i < N; i++)
            {
                ConcurrentBag<string> result = new ConcurrentBag<string>();
                ParallelWalk4(tree, (data) =>
                {
                    SampleUtilities.DoCpuIntensiveOperation(Time);
                    result.Add(data);
                });
            }
        }

        static void ParallelWhileNotEmpty2<T>(
                            IEnumerable<T> initialValues,
                            Action<T, Action<T>> body)
        {
            var items = new ConcurrentBag<T>(initialValues);
            var taskList = new List<Task>();
            var maxTasks = Environment.ProcessorCount * 10;
            var taskCount = 0;
            Action<T> addMethod = v => items.Add(v);
            while (true)
            {
                var tasks = taskList.ToArray();
                if (tasks.Length > 0)
                {
                    Task.WaitAll(tasks);
                    taskList.Clear();
                    taskCount = 0;
                }
                if (items.IsEmpty)
                    break;
                else
                {
                    T item;
                    while (taskCount < maxTasks && items.TryTake(out item))
                    {
                        var v = item;
                        var task = Task.Factory.StartNew(() => body(v, addMethod));
                        taskList.Add(task);
                        taskCount += 1;
                    }
                }
            }
        }

        static void Walk5<T>(Tree<T> tree, Action<T> action)
        {
            if (tree == null) return;
            ParallelWhileNotEmpty2(new[] { tree }, (item, adder) =>
            {
                if (item.Left != null) adder(item.Left);
                if (item.Right != null) adder(item.Right);
                action(item.Data);
            });
        }

        static void Chapter6Example01ParallelWhileNotEmpty2(Tree<string> tree)
        {
            for (int i = 0; i < N; i++)
            {
                ConcurrentBag<string> result = new ConcurrentBag<string>();
                Walk5(tree, (data) =>
                {
                    SampleUtilities.DoCpuIntensiveOperation(Time);
                    result.Add(data);
                });
            }
        }
    }
}

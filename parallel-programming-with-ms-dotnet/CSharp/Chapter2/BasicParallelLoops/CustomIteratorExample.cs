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
using System.Threading.Tasks;

namespace Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops
{
    public class CustomIteratorExample
    {
        public void Example()
        {
            Tree<int> myTree = new Tree<int>(5)
                                   {
                                       Left = new Tree<int>(2)
                                                  {
                                                      Left = new Tree<int>(1),
                                                      Right = new Tree<int>(3)
                                                  },
                                       Right = new Tree<int>(7)
                                   };

            Console.WriteLine("Traverse tree with custom iterator:");
            Parallel.ForEach(myTree.Iterator(), node =>
                {
                    Console.WriteLine("  Node.Data = {0}", node.Data);
                });
            Console.WriteLine();
        }
    }

    public class Tree<T>
    {
        public Tree<T> Left, Right;
        public T Data;

        public Tree(T data)
        {
            Data = data;
        }

        public IEnumerable<Tree<T>> Iterator()
        {
            var queue = new Queue<Tree<T>>();
            queue.Enqueue(this);
            while (queue.Count > 0)
            {
                var node = queue.Dequeue();
                yield return node;
                if (node.Left != null) queue.Enqueue(node.Left);
                if (node.Right != null) queue.Enqueue(node.Right);
            }
        }
    }
}

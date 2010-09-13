//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.BasicParallelLoops.CustomIteratorExample

open System
open System.Collections.Generic
open System.Threading.Tasks

type Tree<'T when 'T : equality> = 
    | Node of Tree<'T> * 'T * Tree<'T>
    | Leaf
    // Returns a sequence of all nodes in the tree
    member x.Iterator() =
      seq { let queue = new Queue<_>()
            queue.Enqueue(x)
            while queue.Count > 0 do
                let node = queue.Dequeue()
                yield node
                // If we have a pending node, add its children to the working queue
                match node with
                | Node(left, _, right) ->
                    queue.Enqueue(left)
                    queue.Enqueue(right) 
                | _ -> () }


module CustomIteratorExample =
    let Example() : unit =
        // Create a sample tree containing values 1, 2, 3, 5, 7
        let myTree = 
            Node(Node(Node(Leaf, 1, Leaf), 2, 
                      Node(Leaf, 3, Leaf)), 5, 
                  Node(Leaf, 7, Leaf))
        
        // Print the data stored in a node for each node in the tree
        Console.WriteLine("Traverse tree with custom iterator:")
        Parallel.ForEach(myTree.Iterator(), function
          | Node(_, data, _) -> Console.WriteLine("  Node.Data = {0}", (data : int)) 
          | _ -> () ) |> ignore
        Console.WriteLine("")

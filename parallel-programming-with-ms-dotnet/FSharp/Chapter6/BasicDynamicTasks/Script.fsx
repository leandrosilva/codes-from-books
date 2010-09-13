//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#load "Examples.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicDynamicTasks.Examples

let n = 1                // number of timing runs per test
let time = 0.01          // CPU time in seconds to visit each node of the tree
let treeSize = 2000      // number of nodes in the tree
let treeDensity = 0.75   // P(left/right child node exists) for interior nodes

// Initialize tree
let tree = makeTree treeSize treeDensity

// Turn on F# Interactive timing
#time "on"

// *** TODO *** 
// Select an implementation to use for tree processing

// Tree traversal, sequential
Chapter6.Example1Sequential n time tree
// Tree traversal, parallel
Chapter6.Example1Parallel n time tree
// Tree traversal, parallel - attached to parent
Chapter6.Example1ParallelAttached n time tree
// Parallel while not empty - Parallel.ForEach
Chapter6.Example01ParallelWhileNotEmpty n time tree
// Parallel while not empty - parallel tasks
Chapter6.Example01ParallelWhileNotEmpty2 n time tree

// F# specific - Traverse tree in parallel using F# asynchronous workflows
Chapter6.Example1ParallelAsync n time tree 

#time "off"
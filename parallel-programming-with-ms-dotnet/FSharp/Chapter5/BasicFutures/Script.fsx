//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"
#r "System.Xaml.dll"
#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"

#load "Examples.fs"

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicFutures

// Turn on F# Interactive timing
#time "on"

// Sequential
Examples.Example1() 
// Parallel, using F1 future
Examples.Example2() 
// Parallel, using F2/F3 future
Examples.Example3() 
// Parallel, using F2 future and F3 continuation
Examples.Example4() 
// Parallel, using F1 and F2/F3 future
Examples.Example5() 
// Parallel, with try/catch block
Examples.Example6() 

#time "off"
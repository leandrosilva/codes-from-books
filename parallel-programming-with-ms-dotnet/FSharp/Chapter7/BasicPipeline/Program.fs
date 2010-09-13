//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicPipeline.Main

open System
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.BasicPipeline.Examples

// ------------------------------------------------------------------------------
// Configuration of the sample and pipelined execution

let opts = 
  { Phrases = [| "the"; "<Adjective>"; "<Adjective>"; "<Noun>"; 
                 "jumped over the"; "<Adjective>"; "<Noun>"; "." |]
    Adjectives = [| "quick"; "brown"; "lazy" |]
    Nouns = [| "fox"; "dog" |]

    TargetSentence = "The quick brown fox jumped over the lazy dog."
    SuccessString = "Surprise!!!"

    NumberOfSentences = 1000
    BufferSize = 32
    StageTime = [| 0.0025; 0.0025; 0.0025; 0.0025 |]

    PathForSequentialResults = @".\Chapter7Sequential.txt"
    PathForPipelineResults = @".\Chapter7Pipeline.txt" }


// ------------------------------------------------------------------------------
// Main - Run the sequential & parallel implementation and compare results

do 
    Console.WriteLine("Basic Pipeline Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    let seed = Environment.TickCount
    SampleUtilities.TimedAction "Write sentences, sequential" (fun () -> 
        Chapter7.Example01Sequential opts seed )
    SampleUtilities.TimedAction "Write sentences, pipeline" (fun () -> 
        Chapter7.Example01Pipeline opts seed )
    checkResults opts
    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadKey() |> ignore

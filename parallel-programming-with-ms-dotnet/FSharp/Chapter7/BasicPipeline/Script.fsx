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
open System.IO
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
// Run the sequential & parallel implementation and compare results

let seed = Environment.TickCount

// Turn on timing in F# Interactive
#time "on"

// Write sentences, sequential
Chapter7.Example01Sequential opts seed
// Write sentences, pipeline
Chapter7.Example01Pipeline opts seed

#time "off"

// Verify that results are correct 
checkResults opts

// Cleanup - delete the created files (by default in Temp)
File.Delete(opts.PathForSequentialResults)
File.Delete(opts.PathForPipelineResults)

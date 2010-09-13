//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.BasicPipeline.Examples

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Utilities etc.
// ------------------------------------------------------------------------------

/// Stores configuration of the sample
type SampleOptions = 
  { Phrases : string[]
    Adjectives : string[] 
    Nouns : string[] 
    TargetSentence : string
    SuccessString : string
    NumberOfSentences : int
    BufferSize : int 
    StageTime : float[] 
    PathForSequentialResults : string
    PathForPipelineResults : string }
    
// Simulate some additional CPU-intensive operation at each stage
let private stage1AdditionalWork opts =
    SampleUtilities.DoCpuIntensiveOperationSimple (opts.StageTime.[0] / float opts.Phrases.Length) |> ignore
let private stage2AdditionalWork opts =
    SampleUtilities.DoCpuIntensiveOperationSimple (opts.StageTime.[1] / float opts.Phrases.Length) |> ignore
let private stage3AdditionalWork opts =
    SampleUtilities.DoCpuIntensiveOperationSimple (opts.StageTime.[2] / float opts.Phrases.Length) |> ignore
let private stage4AdditionalWork opts =
    SampleUtilities.DoCpuIntensiveOperationSimple (opts.StageTime.[3]) |> ignore

/// Show progress of the processing 
let private outputProgress opts sentenceCount =
    if sentenceCount % Math.Max(1, (opts.NumberOfSentences / 10)) = 0 then
        Console.Write(".")

/// Generate opts.Phrases from the 'opts.Phrases' array by replacing <Noun>
/// and <Adjective> with randomly selected opts.Nouns and opts.Adjectives
let private phraseSource opts seed = seq { 
    let r = new Random(seed)
    for i in 0 .. opts.NumberOfSentences - 1 do 
        for line in opts.Phrases do
            match line with 
            | "<Adjective>" ->
                yield opts.Adjectives.[r.Next(0, opts.Adjectives.Length)]
            | "<Noun>" ->
                yield opts.Nouns.[r.Next(0, opts.Nouns.Length)]
            | _ -> yield line }


// ------------------------------------------------------------------------------
// Individual stages of the processing
// ------------------------------------------------------------------------------

/// Stage 1: Add generated opts.Phrases to the blocking collection
let readStrings opts (output:BlockingCollection<string>) seed =
    try
        for phrase in phraseSource opts seed do
            stage1AdditionalWork opts
            output.Add(phrase)
    finally
        output.CompleteAdding()

/// Stage 2: Correct case - uppercase first letter of words after '.'
let correctCase opts (input:BlockingCollection<string>) (output:BlockingCollection<string>) =
    try
        let mutable isFirstPhrase = true
        for phrase in input.GetConsumingEnumerable() do
            stage2AdditionalWork opts
            if isFirstPhrase then
                let capitalized = phrase.Substring(0, 1).ToUpper() + phrase.Substring(1)
                isFirstPhrase <- false
                output.Add(capitalized)
            else
                output.Add(phrase)
                isFirstPhrase <- phrase = "."
    finally
        output.CompleteAdding()

/// Stage 3: Aggregate words that form sentences into individual (merged) strings
let createSentences opts (input:BlockingCollection<string>) (output:BlockingCollection<string>) =
    try
        let sentenceBuilder = new StringBuilder()
        let mutable isFirstPhrase = true
        for phrase in input.GetConsumingEnumerable() do
            stage3AdditionalWork opts
            if not isFirstPhrase && phrase <> "." then
                sentenceBuilder.Append(" ") |> ignore
            sentenceBuilder.Append(phrase) |> ignore
            isFirstPhrase <- false
            if phrase = "." then
                let sentence = sentenceBuilder.ToString()
                sentenceBuilder.Clear() |> ignore
                output.Add(sentence) |> ignore
                isFirstPhrase <- true
    finally
        output.CompleteAdding()

/// Stage 4: Write all generated sentences to the output file
let writeSentences opts (input:BlockingCollection<string>) =
    use outfile = new StreamWriter(opts.PathForPipelineResults)
    let mutable sentenceCount = 1
    for sentence in input.GetConsumingEnumerable() do
        stage4AdditionalWork opts
        let printSentence = 
            if sentence <> opts.TargetSentence then sentence
            else sentence + "       " + opts.SuccessString
        outfile.WriteLine(sentenceCount.ToString() + " " + printSentence)
        outputProgress opts sentenceCount
        sentenceCount <- sentenceCount + 1

/// Contains public functions 
module Chapter7 = 
    // --------------------------------------------------------------------------
    // Sequential implementation 
    // --------------------------------------------------------------------------

    let Example01Sequential opts seed =
        let sentenceBuilder = new StringBuilder()
        let mutable isFirstPhrase = true
        let mutable sentenceCount = 1
        use outfile = new StreamWriter(opts.PathForSequentialResults)
    
        Console.Write("Begin Sequential Sentence Builder")
        for phrase in phraseSource opts seed do
            // Stage 1: Load the phrase from source
            stage1AdditionalWork opts

            // Stage 2: Correct case of first word
            stage2AdditionalWork opts
            let capitalizedPhrase = 
                if not isFirstPhrase then phrase 
                else phrase.Substring(0, 1).ToUpper() + phrase.Substring(1) 

            // Stage 3: Merge words into sentences
            stage3AdditionalWork opts
            if not isFirstPhrase && phrase <> "." then
                sentenceBuilder.Append(" ") |> ignore
            sentenceBuilder.Append(capitalizedPhrase) |> ignore
            isFirstPhrase <- false
            if phrase = "." then
                let sentence = sentenceBuilder.ToString()
                let sentence = 
                    if sentence <> opts.TargetSentence then sentence
                    else sentence + "       " + opts.SuccessString
                sentenceBuilder.Clear() |> ignore
                isFirstPhrase <- true

                // Stage 4: Write generated sentence to a file
                stage4AdditionalWork opts
                outfile.WriteLine(sentenceCount.ToString() + " " + sentence)
                outputProgress opts sentenceCount
                sentenceCount <- sentenceCount + 1
        Console.WriteLine("End")

    // --------------------------------------------------------------------------
    // Staged parallel implementation 
    // --------------------------------------------------------------------------

    /// Pipelined processing in parallel - 
    /// Generate opts.Phrases into an input collection. When a new phrase is
    /// added, the processing of the phrase will start (running all 4 stages
    /// one by one, until all opts.Phrases all processed)
    let Example01Pipeline opts seed =
        Console.Write("Begin Pipelined Sentence Builder")
        let f = new TaskFactory( TaskCreationOptions.LongRunning, 
                                  TaskContinuationOptions.None )
        let buffer1 = new BlockingCollection<_>(opts.BufferSize)
        let buffer2 = new BlockingCollection<_>(opts.BufferSize)
        let buffer3 = new BlockingCollection<_>(opts.BufferSize)

        // Stage 1: Read strings and merge into sentences
        let stage1 = f.StartNew(fun () -> readStrings opts buffer1 seed)
        // Stage 2: Correct case
        let stage2 = f.StartNew(fun () -> correctCase opts buffer1 buffer2)
        // Stage 3: Merge into sentences
        let stage3 = f.StartNew(fun () -> createSentences opts buffer2 buffer3)
        // Stage 4: Write output
        let stage4 = f.StartNew(fun () -> writeSentences opts buffer3)

        Task.WaitAll(stage1, stage2, stage3, stage4)
        Console.WriteLine("End")

// ------------------------------------------------------------------------------
// Verification of the generated files
// ------------------------------------------------------------------------------

/// Calculates checksum of the specified file
let private getFileChecksum fileName =
    use fileStream = new FileStream(fileName, FileMode.Open)
    let md5 = new MD5CryptoServiceProvider()
    let result = md5.ComputeHash(fileStream)
    Convert.ToBase64String(result, 0, result.Length)


/// Verify that both sequential and parallel method
/// gives the same result (by checking checksums of files)
let checkResults opts =
    // Calculate checksums in parallel
    let file1ChecksumTask = Task.Factory.StartNew(fun () -> 
        getFileChecksum opts.PathForSequentialResults)
    let file2Checksum = getFileChecksum opts.PathForPipelineResults
    let file1Checksum = file1ChecksumTask.Result

    Console.WriteLine
      ( "Results written to files \"{0}\" and \"{1}\"",
        opts.PathForSequentialResults, opts.PathForPipelineResults)

    if file1Checksum = null || file2Checksum = null then
        Console.WriteLine("PROGRAM ERROR! Couldn't calculate file checksum.")
    elif file1Checksum.Equals(file2Checksum) then
        Console.WriteLine("Sequential and pipeline results were verified to be equal.")
    else
        Console.WriteLine("PROGRAM ERROR! Sequential and pipeline results don't match.")

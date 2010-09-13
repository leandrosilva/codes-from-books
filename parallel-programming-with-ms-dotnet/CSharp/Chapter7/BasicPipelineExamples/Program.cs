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
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.BasicPipeline
{
    class Program
    {
        static string[] Phrases = { "the", "<Adjective>", "<Adjective>", "<Noun>", 
                                    "jumped over the", "<Adjective>", "<Noun>", "." };
        static string[] Adjectives = { "quick", "brown", "lazy" };
        static string[] Nouns = { "fox", "dog" };
        const string TargetSentence = "The quick brown fox jumped over the lazy dog.";
        const string SuccessString = "Surprise!!!";

        const int NumberOfSentences = 1000;
        const int BufferSize = 32;
        static double[] StageTime = { 0.0025, 0.0025, 0.0025, 0.0025 };
        const string PathForSequentialResults = @".\Chapter7Sequential.txt";
        const string PathForPipelineResults = @".\Chapter7Pipeline.txt";

        static void Main()
        {
            Console.WriteLine("Basic Pipeline Samples\n");
#if(DEBUG)
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            int seed = Environment.TickCount;
            SampleUtilities.TimedAction(() => Chapter7Example01Sequential(seed), "Write sentences, sequential");
            SampleUtilities.TimedAction(() => Chapter7Example01Pipeline(seed), "Write sentences, pipeline");
            CheckResults();

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadKey();
        }

        static IEnumerable<string> PhraseSource(int seed)
        {
            Random r = new Random(seed);
            for (int i = 0; i < NumberOfSentences; i++)
                foreach (var line in Phrases)
                {
                    if (line == "<Adjective>")
                        yield return Adjectives[r.Next(0, Adjectives.Length)];
                    else if (line == "<Noun>")
                        yield return Nouns[r.Next(0, Nouns.Length)];
                    else
                        yield return line;
                }
        }

        static void Chapter7Example01Sequential(int seed)
        {
            var isFirstPhrase = true;
            var sentenceBuilder = new StringBuilder();
            int sentenceCount = 1;
            using (StreamWriter outfile = new StreamWriter(PathForSequentialResults))
            {
                Console.Write("Begin Sequential Sentence Builder");
                foreach (var phrase in PhraseSource(seed))
                {
                    Stage1AdditionalWork();
                    Stage2AdditionalWork();
                    string capitalizedPhrase = isFirstPhrase ?
                        phrase.Substring(0, 1).ToUpper() + phrase.Substring(1) : phrase;
                    Stage3AdditionalWork();
                    if (!isFirstPhrase && phrase != ".")
                        sentenceBuilder.Append(" ");
                    sentenceBuilder.Append(capitalizedPhrase);
                    isFirstPhrase = false;
                    if (phrase == ".")
                    {
                        string sentence = sentenceBuilder.ToString();
                        if (sentence == TargetSentence)
                            sentence = sentence + "       " + SuccessString;
                        sentenceBuilder.Clear();
                        isFirstPhrase = true;
                        Stage4AdditionalWork();
                        outfile.WriteLine(sentenceCount.ToString() + " " + sentence);
                        OutputProgress(sentenceCount);
                        sentenceCount += 1;
                    }
                }
                Console.WriteLine("End");
            }
        }

        static void Chapter7Example01Pipeline(int seed)
        {
            Console.Write("Begin Pipelined Sentence Builder");

            var buffer1 = new BlockingCollection<string>(BufferSize);
            var buffer2 = new BlockingCollection<string>(BufferSize);
            var buffer3 = new BlockingCollection<string>(BufferSize);

            var f = new TaskFactory(TaskCreationOptions.LongRunning, 
                                    TaskContinuationOptions.None);

            // Stage 1: Read strings and merge into sentences
            var stage1 = f.StartNew(() => ReadStrings(buffer1, seed));

            // Stage 2: Correct case
            var stage2 = f.StartNew(() => CorrectCase(buffer1, buffer2));

            // Stage 3: Merge into sentences
            var stage3 = f.StartNew(() => CreateSentences(buffer2, buffer3));

            // Stage 4: Write output
            var stage4 = f.StartNew(() => WriteSentences(buffer3));

            Task.WaitAll(stage1, stage2, stage3, stage4);
            Console.WriteLine("End");
        }

        static void ReadStrings(BlockingCollection<string> output, int seed)
        {
            try
            {
                foreach (var phrase in PhraseSource(seed))
                {
                    Stage1AdditionalWork();
                    output.Add(phrase);
                }
            }
            finally
            {
                output.CompleteAdding();
            }
        }

        static void Stage1AdditionalWork() { SampleUtilities.DoCpuIntensiveOperation(StageTime[0] / Phrases.Length); }
        static void Stage2AdditionalWork() { SampleUtilities.DoCpuIntensiveOperation(StageTime[1] / Phrases.Length); }
        static void Stage3AdditionalWork() { SampleUtilities.DoCpuIntensiveOperation(StageTime[2] / Phrases.Length); }
        static void Stage4AdditionalWork() { SampleUtilities.DoCpuIntensiveOperation(StageTime[3]); }

        static void CorrectCase(BlockingCollection<string> input, BlockingCollection<string> output)
        {
            try
            {
                bool isFirstPhrase = true;
                foreach (var phrase in input.GetConsumingEnumerable())
                {
                    Stage2AdditionalWork();
                    if (isFirstPhrase)
                    {
                        var capitalized = phrase.Substring(0, 1).ToUpper() + phrase.Substring(1);
                        isFirstPhrase = false;
                        output.Add(capitalized);
                    }
                    else
                    {
                        output.Add(phrase);
                        if (phrase == ".")
                            isFirstPhrase = true;
                    }
                }
            }
            finally
            {
                output.CompleteAdding();
            }
        }

        static void CreateSentences(
            BlockingCollection<string> input,
            BlockingCollection<string> output)
        {
            try
            {
                StringBuilder sentenceBuilder = new StringBuilder();
                bool isFirstPhrase = true;
                foreach (var phrase in input.GetConsumingEnumerable())
                {
                    Stage3AdditionalWork();
                    if (!isFirstPhrase && phrase != ".")
                        sentenceBuilder.Append(" ");
                    sentenceBuilder.Append(phrase);
                    isFirstPhrase = false;
                    if (phrase == ".")
                    {
                        var sentence = sentenceBuilder.ToString();
                        sentenceBuilder.Clear();
                        output.Add(sentence);
                        isFirstPhrase = true;
                    }
                }
            }
            finally
            {
                output.CompleteAdding();
            }
        }

        static void WriteSentences(BlockingCollection<string> input)
        {
            using (StreamWriter outfile = new StreamWriter(PathForPipelineResults))
            {
                var sentenceCount = 1;
                foreach (var sentence in input.GetConsumingEnumerable())
                {
                    var printSentence = sentence;
                    Stage4AdditionalWork();
                    if (printSentence == TargetSentence)
                        printSentence = printSentence + "       " + SuccessString;
                    outfile.WriteLine(sentenceCount.ToString() + " " + printSentence);
                    OutputProgress(sentenceCount);
                    sentenceCount += 1;
                }
            }
        }

        static void CheckResults()
        {
            var file1ChecksumTask = Task.Factory.StartNew<string>(() => GetFileChecksum(PathForSequentialResults));
            var file2Checksum = GetFileChecksum(PathForPipelineResults);
            var file1Checksum = file1ChecksumTask.Result;

            Console.WriteLine(string.Format("Results written to files \"{0}\" and \"{1}\"",
                PathForSequentialResults, PathForPipelineResults));
            if (file1Checksum == null || file2Checksum == null)
            {
                Console.WriteLine("PROGRAM ERROR! Couldn't calculate file checksum.");
                return;
            }
            if (file1Checksum.Equals(file2Checksum))
                Console.WriteLine("Sequential and pipeline results were verified to be equal.");
            else
                Console.WriteLine("PROGRAM ERROR! Sequential and pipeline results don't match.");
        }

        static string GetFileChecksum(string fileName)
        {
            byte[] result;
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open))
            {
                MD5 md5 = new MD5CryptoServiceProvider();
                result = md5.ComputeHash(fileStream);
            }
            return Convert.ToBase64String(result, 0, result.Length);
        }

        private static void OutputProgress(int sentenceCount)
        {
            if (sentenceCount % Math.Max(1, (NumberOfSentences / 10)) == 0)
                Console.Write(".");
        }
    }
}

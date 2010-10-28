// Expert F# 2.0
// Chapter 13 Example 06

let forkJoinParallel(taskSeq) =
    Async.FromContinuations (fun (cont,econt,ccont) ->
        let tasks = Seq.toArray taskSeq
        let count = ref tasks.Length
        let results = Array.zeroCreate tasks.Length
        tasks |> Array.iteri (fun i p ->
            Async.Start
               (async { let! res = p
                        results.[i] <- res;
                        let n = System.Threading.Interlocked.Decrement(count)
                        if n=0 then cont results })))

open System.Threading
open System

// Initialize an array by a parallel init using all available processors
// Note, this primitive doesn'’t support cancellation.
let parallelArrayInit n f = 
   let currentLine = ref -1
   let res = Array.zeroCreate n
   let rec loop () = 
       let y = Interlocked.Increment(&currentLine.contents)
       if y < n then res.[y] <- f y; loop()

   // Start just the right number of tasks, one for each physical CPU
   Async.Parallel [ for i in 1 .. Environment.ProcessorCount -> async { do loop()} ] 
      |> Async.Ignore 
      |> Async.RunSynchronously

   res

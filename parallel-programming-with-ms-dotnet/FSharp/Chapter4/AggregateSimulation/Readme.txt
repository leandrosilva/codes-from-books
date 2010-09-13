Aggregate Simulation Example
----------------------------

This sample has its own readme because it is not covered explicitly in the book.
It is included here because we feel it is a useful addition to the code content.

This program is an example of how to implement the Map/Reduce pattern using 
Parallel.ForEach and a partitioner object.

The program shows a simulation system that, for example, analyzes possible 
financial outcomes given various input assumptions. The program runs a top-level loop
with many trials. The results of each trial are summarized into a histogram.
The histogram shows how many times each result appeared.

Using a partitioner object solves the problem of task decomposition. When 
you write the program you don't know how many CPU cores will be available at
runtime. You want your program to scale automatically to the number of
cores available. Also, you may not want to create a task for each 
simulation trial, since there could be many (perhaps millions) of these.

Running the trials in a parallel loop makes sense, but you need to take care
to handle the aggregation in an scalable way. You don't want to have each
task compete for access to a single shared result. Instead, you create 
local, per task results and merge these results as a final step.

The Parallel.ForEach method provides a structured way to perform this kind
of aggregation. 

The code in this example contains a sequential version, 
DoSequentialAggregation, and a parallel version, DoParallelAggregation. The
program's Main method run's both methods for comparison.

Code samples for Parallel Programming with Microsoft® .NET
 
Use the BookSamples solution.  Here are descriptions of the projects, grouped by solution folder.

Sample timings here are from an Intel Core Duo (2 cores), 2 GHz, 32 KB L1 cache, 2 MB L2 cache, 2 GB RAM
Sample timings are from Release builds.  Each sample timing is from one typical run, can be quite variable.

Appendix A
----------

RelatedPatterns

Small examples from Appendix A in the book.

Appendix B
----------

ProfilerExamples

Small examples from Appendix B in the book. These examples are designed to be used in conjunction 
with the Concurrency Visualizer that ships with Microsoft Visual Studio 2010 Ultimate or Premium editions.

Command line arguments: deadlock, lockcontention, oversubscription, loadimbalance

Chapter2
--------

BasicParallelLoops

Small examples from Chapter 2 in the book.

CreditReview

Credit review example from Chapter 2 in the book.  Builds CreditReview.exe console application.
Creates repository of customers with credit histories, predicts future balances for all customers
by fitting list-squares regression line to credit history, extrapolating trend for next three months.
Generates random credit histories, but always uses the same random seed so runs are reproducible.
Creates repository, then executes and times three versions: sequential foreach, Parallel.ForEach, and PLINQ.
Prints number of customers, number of months, timings, also histories and predictions for several customers.
Defaults: 1,000,000 customers, each with 36 months credit history.

Sample timings: 5 sec before results appear: sequential foreach 1.76 sec > Parallel.ForEach 1.05 sec ~= PLINQ 1.04 sec

Optional command line arguments: number of customers, number of months of credit history

Chapter3
--------

BasicParallelTasks

Small examples from Chapter 3 in the book.

ImageBlender

Image blender example from Chapter 3 in the book.  Builds ImageBlender.exe console application.
Reads two images, rotates one, converts the other to grayscale, combines them by alpha blending.
Reads images, then executes and times three versions: sequential, parallel tasks (using Task.Factory.Invoke), 
parallel invoke (using Parallel.Invoke).
Prints timings, then opens window and displays blended image.
Defaults: rotates 600 Kb 1024x768 24 bit image (dog.jpg) and converts 320 Kb 1024x681 24 bit image (flowers.jpg)

Sample timings: 15 sec before results appear: sequential 12.21 sec > parallel tasks 7.05 ~= parallel invoke 6.86

Optional command line arguments: source directory, image 1 filename, image 2 filename, destination directory.

Chapter4
--------

AggregateSimulation

A small example NOT in the book.  It shows how to implement the Map/Reduce pattern using 
Parallel.ForEach and a partitioner object.  See its own readme.txt in the project.

BasicAggregation

Small examples from Chapter 4 in the book.

SocialNetwork

Social network example from Chapter 4 in the book.  Builds SocialNetwork.exe console application.
Creates repository of subscriber IDs with friends IDs, recommends new friends for one subscriber
by finding all friends-of-friends, ranking them by number of mutual friends, recommending highest ranked.
For each subscriber, generates number and IDs of friends at random, but uses same seed for reproducible runs.
Creates repository, then executes and times four versions: sequential for each, Parallel.ForEach, LINQ and PLINQ.
Prints table of first several subscribers: subscriber ID, number of friends, IDs of first several friends.
Prints total number of subscribers and average number of friends per subscriber.
Prints ID of one subscriber and IDs of all that suscriber's (many) friends.
Prints elapsed time and list of top-ranked recommended new friends for each version.
Defaults: 10,000 subscribers with average of 2,000 friends each

Sample timings: 10 sec before results appear: sequential 3.14 sec > Parallel.ForEach 1.40 > LINQ 0.67 ~= PLINQ 0.67
Timings can vary a lot in successive runs.  First run is often much slower.  LINQ/PLINQ are especially variable.

Optional command line arguments: number of subscribers, average number of friends.

Chapter5
--------

A-Dash
(Note: see project-specific README.txt for notes on the F# version of the sample)

Financial dashboard example from Chapter 5 in the book.  Builds ADash.exe Windows application.
Demonstrates task graph, futures, continuation tasks, integration of tasks with WPF GUI.
Opens window with dashboard GUI. Click Calculate to start tasks.  
Status window shows "..calculating" as tasks execute.  
Click Cancel to stop tasks, status windows shows "Canceled".  
Buttons become active as tasks complete. 
Click an active button to view data from that task (just a brief message, there is no actual data in this sample).
When all tasks are complete, status window shows recommendation.
Click Quit to close window and exit application.
Defaults: The recommendation is always Buy.

Sample timings: After clicking Calculate, takes about 10 sec to finish all tasks.

Optional command line arguments: None.

BasicFutures

Small examples from Chapter 5 in the book.

Chapter6
--------

BasicDynamicTasks

Small examples from Chapter 6 in the book.

ParallelSort

Parallel QuickSort example from Chapter 6 in book (based on S. Toub's example cited in Further Reading).
Builds ParallelSort.exe console application.
Creates array with integer elements, prints first and last several elements. 
Generates random array elements, but always uses the same random seed so runs are reproducible.
Executes sequential and parallel QuickSort, prints timings and first and last several elements of sorted array.
Defaults: Array length: 4,000,000  Threshonsold array length to use non-recursive insertion sort: 150
Maximum recursion depth where recursive calls are made in new tasks: log2(ProcessorCount) + 4

Sample timings: 10 sec before results appear: sequential 8.96 sec, parallel 5.70 sec.

Optional command line arguments: array length, threshold length for insertion sort.

Chapter7 
--------

BasicPipeline

Small examples from Chapter 7 in the book.

ImagePipeline

Image pipeline sample from Chapter 7 in book.  Builds ImagePipeline.exe Windows application.

The program cycles through the jpg images located in a directory and performs a series of steps: 
it resizes each image and adds a black border and then applies a Gaussian noise filter operation to give 
the image a grainy effect. Finally, the program displays the image on the user interface.
Images are processed in sequential order: they appear on the display in exactly the same order
as they appear in the directory listing.

The program opens a window that provides some controls, displays the most recently processed image,
and shows some performance statistics.  Select a processing mode: Sequential, Pipelined, or Load Balanced.  
Click Start to process images. The program will continue processing, cycling through the images in the directory.
Click Stop to stop processing so you can select another mode or study the statistics.  
Click Quit to close the window and exit the program.

Defaults: there are 3 24-bit images: dog.jpg (1024x768, 600 KB), flowers.jpg (1024x681 300KB) 
and butterfly.jpg (1024x681, 496 KB)

Sample timings, Time per image: Sequential 212 msec > Piplined 135 msec > Load Balanced 120 msec

Optional command line arguments: None.

Utilities
---------

Types and methods used by other projects.

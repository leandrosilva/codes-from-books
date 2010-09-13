-------------------------------------------------------------------------------
A-Dash
-------------------------------------------------------------------------------

Financial dashboard example from Chapter 5 in the book. Builds ADash.exe 
Windows application. Demonstrates task graph, futures, continuation tasks, 
integration of tasks with WPF GUI.

-------------------------------------------------------------------------------
F# Specific Notes
-------------------------------------------------------------------------------

The analysis engine contains three implementations, two that are also available
in the C# version of the demo and one that implements the functionality
using F# asynchronous workflow.

The sample simulates both I/O intesive operations (e.g. loading of data from
disk or network) and CPU intensive operations (e.g. analysis of data and 
execution of models), which makes it a perfect fit for F# asynchronous 
workflows. In the 'async' version, we use non-blocking I/O (which makes the
version more efficient than Task-based solution) and we call I/O operations
using 'let!'. The call graph is constructed using 'Async.StartChild' (to 
create an async task) and 'Async.Parallel' when waiting for multiple 
asynchrnous workflows.

The implementation is available in 'AnalysisEngine.fs' which contains
the following three methods:

* DoAnalysisSequential
		Creates a market recommendation using a fully sequential operation;
    returns a MarketRecomendation value (as the result of analysis)

* DoAnalysisParallel
		Initiates market analysis using parallel computation (Tasks);
    returns an object that can be used to register notifications with 
    the background calculations (and can be used to get the results

* DoAnalysisAsync
		Initiates market analysis using asynchronous workflows (F# async);
    returns an object that can be used to register notifications with 
    the background calculations (and can be used to get the results)
		This implementation uses F# asaynchronous workflows to compose
		the execution graph and we create events that are triggered to 
		notify the caller (on the GUI thread) when asynchronous workflow 
		produces partial result

By default, the sample uses 'DoAnalysisAsync' which can be changed in the
'MainWindowViewModel.fs' file in the 'OnRequestCalculate' method
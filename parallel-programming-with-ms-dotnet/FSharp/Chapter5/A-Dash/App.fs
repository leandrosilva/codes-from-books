#nowarn "40"
//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open System.Windows
open Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel

type App() = 
    inherit Application() 
    
    // Create analysis engine (model)
    let engine = new AnalysisEngine()

    // Create ViewModel for main window View, pass it a reference to the Model.
    let viewModel = new MainWindowViewModel(engine) :> IMainWindowViewModel

    override x.OnStartup(e:StartupEventArgs) =
        base.OnStartup(e)

        // Create the main window (view)
        let window = new MainWindow() 

        // When the ViewModel asks to be closed, close the window
        let rec handler = new EventHandler(fun _ _ ->
            viewModel.RequestClose.RemoveHandler(handler)
            viewModel.Dispose()
            window.Close() )
        viewModel.RequestClose.AddHandler(handler)

        // When the ViewModel asks to view result data, show a message box.
        // This is sample behavior. A more realistic approach would be to
        // open a new data tab and allow the user to scroll through the results.
        viewModel.RequestNyse.Add(fun _ -> 
            MessageBox.Show("View Nyse market data", "Nyse") |> ignore)
        viewModel.RequestNasdaq.Add(fun _ -> 
            MessageBox.Show("View Nasdaq market data", "Nasdaq") |> ignore)
        viewModel.RequestMerged.Add(fun _ -> 
            MessageBox.Show("View merged market data", "Merged") |> ignore)
        viewModel.RequestNormalized.Add(fun _ -> 
            MessageBox.Show("View normalized market data", "Normalized") |> ignore)
        viewModel.RequestFedHistorical.Add(fun _ -> 
            MessageBox.Show("View Fed historical data", "Fed") |> ignore)
        viewModel.RequestNormalizedHistorical.Add(fun _ -> 
            MessageBox.Show("View normalized Fed historical data", "Normalized Fed") |> ignore)
        viewModel.RequestAnalyzed.Add(fun _ -> 
            MessageBox.Show("View market data analysis", "Analysis") |> ignore)
        viewModel.RequestAnalyzedHistorical.Add(fun _ -> 
            MessageBox.Show("View historical analysis", "Historical analysis") |> ignore)
        viewModel.RequestModeled.Add(fun _ -> 
            MessageBox.Show("View market model", "Model") |> ignore)
        viewModel.RequestModeledHistorical.Add(fun _ -> 
            MessageBox.Show("View historical model", "Analysis") |> ignore)
        viewModel.RequestRecommendation.Add(fun _ -> 
            MessageBox.Show("View comparision and recommendation", "Comparison") |> ignore)

        // Set the ViewModel as the window's data context to connect 
        // the ViewModel to the View. This allows UI property bindings to retrieve 
        // their values from properties supplied by the ViewModel.
        window.ViewModel <- viewModel
        
        // Display the view
        window.Show()

    override x.OnExit(e:ExitEventArgs) =
        viewModel.Dispose()
        base.OnExit(e)

module Main =
    /// Application entry point
    [<STAThreadAttribute; EntryPoint>]
    let main _ =
        let app = new App()
        app.Run()
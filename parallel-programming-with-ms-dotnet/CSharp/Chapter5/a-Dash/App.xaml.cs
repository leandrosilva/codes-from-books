//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Windows;
using Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel;

namespace Microsoft.Practices.ParallelGuideSamples.ADash
{
    public partial class App : Application
    {
        IMainWindowViewModel viewModel;

        protected override void OnStartup(StartupEventArgs e)
        {
            base.OnStartup(e);

            // Create the View.
            MainWindow window = new MainWindow();

            // Create Model.
            IAnalysisEngine engine = new AnalysisEngine();

            // Create ViewModel for main window View, pass it a reference to the Model.
            viewModel = new MainWindowViewModel(engine);

            // When the ViewModel asks to be closed, close the window.
            EventHandler handler = null;
            handler = delegate
            {
                viewModel.RequestClose -= handler;
                viewModel.Dispose();
                window.Close();
            };
            viewModel.RequestClose += handler;

            // When the ViewModel asks to view result data, show a message box.
            // This is sample behavior. A more realistic approach would be to
            // open a new data tab and allow the user to scroll through the results.
            viewModel.RequestNyse += delegate { MessageBox.Show("View Nyse market data", "Nyse"); };
            viewModel.RequestNasdaq += delegate { MessageBox.Show("View Nasdaq market data", "Nasdaq"); };
            viewModel.RequestMerged += delegate { MessageBox.Show("View merged market data", "Merged"); };
            viewModel.RequestNormalized += delegate { MessageBox.Show("View normalized market data", "Normalized"); };
            viewModel.RequestFedHistorical += delegate { MessageBox.Show("View Fed historical data", "Fed"); };
            viewModel.RequestNormalizedHistorical += delegate { MessageBox.Show("View normalized Fed historical data", "Normalized Fed"); };
            viewModel.RequestAnalyzed += delegate { MessageBox.Show("View market data analysis", "Analysis"); };
            viewModel.RequestAnalyzedHistorical += delegate { MessageBox.Show("View historical analysis", "Historical analysis"); };
            viewModel.RequestModeled += delegate { MessageBox.Show("View market model", "Model"); };
            viewModel.RequestModeledHistorical += delegate { MessageBox.Show("View historical model", "Analysis"); };
            viewModel.RequestRecommendation += delegate { MessageBox.Show("View comparision and recommendation", "Comparison"); };

            // Set the ViewModel as the window's data context to connect the ViewModel to the View.
            // This allows UI property bindings to retrieve their values from properties supplied by the ViewModel.
            window.ViewModel = viewModel;

            // Display the View.
            window.Show();
        }

        protected override void OnExit(ExitEventArgs e)
        {
            if (viewModel != null)
                viewModel.Dispose();
            base.OnExit(e);
        }
    }
}
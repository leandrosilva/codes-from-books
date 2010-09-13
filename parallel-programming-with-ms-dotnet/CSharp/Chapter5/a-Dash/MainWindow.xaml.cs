//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Windows;
using Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel;

namespace Microsoft.Practices.ParallelGuideSamples.ADash
{
    public partial class MainWindow : Window
    {
        // Provide a strongly typed ViewModel property for the View.
        // If your application uses a dependency injection container this
        // makes it simpler to configure the container to inject the 
        // ViewModel.
        public IMainWindowViewModel ViewModel
        {
            get { return DataContext as IMainWindowViewModel; }
            set { DataContext = value; }
        }

        public MainWindow()
        {            
            InitializeComponent();            
        }
    }
}

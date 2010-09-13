//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System.Windows
open Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel

type MainWindow() =
    
    let res = "/A_Dash;component/MainWindow.xaml"
    let resourceLocater = new System.Uri(res, System.UriKind.Relative)
    let win = Application.LoadComponent(resourceLocater) :?> Window

    member x.Show() = win.Show()
    member x.Close() = win.Close()

    // Provide a strongly typed ViewModel property for the View.
    // If your application uses a dependency injection container this
    // makes it simpler to configure the container to inject the 
    // ViewModel.
    member x.ViewModel
      with get() = win.DataContext :?> IMainWindowViewModel
      and set(v) = win.DataContext <- v

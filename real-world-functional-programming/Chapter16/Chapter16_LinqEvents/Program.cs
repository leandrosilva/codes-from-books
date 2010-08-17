using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.IO;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 16
// --------------------------------------------------------------------------

namespace Chapter16_LinqEvents
{
  static class Program
  {
    [STAThread]
    static void Main()
    {
      Application.EnableVisualStyles();
      Application.Run(new MainForm());

      // FileSystemWatcherDemo();
    }

    // --------------------------------------------------------------------------
    // Not included: Working with events using LINQ

    // Tests attributes of the file. Returns 'true' 
    // if the specifed file isn't marked as hidden.
    static bool IsNotHidden(FileSystemEventArgs fse)
    {
      var hidden = FileAttributes.Hidden;
      return (File.GetAttributes(fse.FullPath) & hidden) != hidden;
    }

    private static void FileSystemWatcherDemo()
    {
      // Initialize the file system watcher
      var w = new FileSystemWatcher("C:\\Temp")
      {
        EnableRaisingEvents = true
      };
      // Convert event to a value
      var watcherEvt = Observable.FromEvent<RenamedEventArgs>(w, "Renamed");

      // Filter events and yield string with file names
      var renamedEvt =
        from fse in watcherEvt
        where IsNotHidden(fse.EventArgs)
        select String.Format("{0} renamed to {1}",
          fse.EventArgs.OldFullPath, fse.EventArgs.FullPath);

      // Print the information when event occurs
      renamedEvt.Subscribe(Console.WriteLine);
      Console.ReadLine();
    }
  }
}

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Chapter16_LinqEvents
{
  public partial class MainForm : Form
  {
    public MainForm()
    {
      InitializeComponent();
    }

    private void MainForm_Load(object sender, EventArgs e)
    {
      // Convert .NET events to Reactive LINQ event values
      var upEvent = Observable.FromEvent<EventArgs>(btnUp, "Click");
      var downEvent = Observable.FromEvent<EventArgs>(btnDown, "Click");

      // Create events carrying -1 or +1 as values
      var up = from clickArgs in upEvent select +1;
      var down = from clickArgs in downEvent select -1;

      // Merge events 
      Observable.Merge(up, down)
        // Calculate sum of values carried by the event
        .Scan(0, (state, num) => state + num)
        .Subscribe(sum =>
          lblCount.Text = string.Format("Count: {0}", sum));
    }
  }
}

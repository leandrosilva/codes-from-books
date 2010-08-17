Imports System.Linq
Imports Chapter16_VbEvents.ObservableExtensions

Public Class MainForm

  Private Sub MainForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
    ' Convert .NET events to Reactive LINQ event values
    Dim upEvent = Observable.FromEvent(Of EventArgs)(btnUp, "Click")
    Dim downEvent = Observable.FromEvent(Of EventArgs)(btnDown, "Click")

    ' Create events carrying +1/-1, merge them and aggregate them using 'Sum'
    Dim sumEvent = _
      Aggregate num In Observable.Merge( _
        (From clickArgs1 In upEvent Select +1), _
        (From clickArgs2 In downEvent Select -1)) _
      Into MovingSum(num)
    sumEvent.Subscribe(AddressOf UpdateCount)

  End Sub

  Sub UpdateCount(ByVal sum As Integer)
    lblCount.Text = String.Format("Count: {0}", sum)
  End Sub

End Class

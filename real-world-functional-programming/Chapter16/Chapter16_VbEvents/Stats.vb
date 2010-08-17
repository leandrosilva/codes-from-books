Imports System.Linq

Public Module ObservableExtensions
  <System.Runtime.CompilerServices.Extension()> _
  Public Function MovingSum(Of T)(ByVal nums As IObservable(Of T), ByVal selector As Func(Of T, Integer)) As IObservable(Of Integer)
    Return Observable.Scan(nums, 0, Function(a, b) a + selector(b))
  End Function
End Module

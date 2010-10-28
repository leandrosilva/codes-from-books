// Expert F# 2.0
// Chapter 13 Example 11

type MutablePair<'T,'U>(x:'T,y:'U) =
    let mutable currentX = x
    let mutable currentY = y
    member p.Value = (currentX,currentY)
    member p.Update(x,y) =
        // Race condition: This pair of updates is not atomic
        currentX <- x;
        currentY <- y

let p = new MutablePair<_,_>(1,2)
do Async.Start (async { do (while true do p.Update(10,10)) })
do Async.Start (async { do (while true do p.Update(20,20)) })

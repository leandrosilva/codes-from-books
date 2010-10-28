// Expert F# 2.0
// Chapter 18 Example 03

open System
open System.Threading


let t1 = Thread(fun () ->
    while true do
      printf "Thread 1\n"
)
let t2 = Thread(fun () ->
    while true do
      printf "Thread 2\n"
)
t1.Start()
t2.Start()

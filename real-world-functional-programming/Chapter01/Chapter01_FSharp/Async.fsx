#light
#r "FSharp.PowerPack.dll"

open System
open System.IO
open System.Net
open Microsoft.FSharp.Control

let op = async {
  let req = HttpWebRequest.Create("http://localhost")
  let! rsp = req.AsyncGetResponse()
  let rst = rsp.GetResponseStream()
  let reader = new StreamReader(rst)
  let! html = reader.AsyncReadToEnd()
  Console.WriteLine(html) }

Async.Run(op)

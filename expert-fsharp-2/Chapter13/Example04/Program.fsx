// Expert F# 2.0
// Chapter 13 Example 04

#r "FSharp.PowerPack.dll" // contains the definition for AsyncReadToEnd
open System.Net
open System.IO

let museums = ["MOMA",           "http://moma.org/";
               "British Museum", "http://www.thebritishmuseum.ac.uk/";
               "Prado",          "http://museoprado.mcu.es"]

let fetchAsync(nm,url:string) =
    async { do printfn "Creating request for %s..." nm
            let req  = WebRequest.Create(url)

            let! resp  = req.AsyncGetResponse()

            do printfn "Getting response stream for %s..." nm
            let stream = resp.GetResponseStream()

            do printfn "Reading response for %s..." nm
            let reader = new StreamReader(stream)
            let! html = reader.AsyncReadToEnd()

            do printfn "Read %d characters for %s..." html.Length nm }

for nm,url in museums do
    Async.Start (fetchAsync(nm,url))

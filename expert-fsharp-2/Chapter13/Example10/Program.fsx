// Expert F# 2.0
// Chapter 13 Example 10

open System.Collections.Generic
open System.Net
open System.IO
open System.Threading
open System.Text.RegularExpressions

let limit = 50
let linkPat = "href=\s*\"[^\"h]*(http://[^&\"]*)\""
let getLinks (txt:string) =
    [ for m in Regex.Matches(txt,linkPat)  -> m.Groups.Item(1).Value ]

// A type that helps limit the number of active web requests
type RequestGate(n:int) =
    let semaphore = new Semaphore(initialCount=n,maximumCount=n)
    member x.AsyncAcquire(?timeout) =
        async { let! ok = Async.AwaitWaitHandle(semaphore,
                                                ?millisecondsTimeout=timeout)
                if ok then
                   return
                     { new System.IDisposable with
                         member x.Dispose() =
                             semaphore.Release() |> ignore }
                else
                   return! failwith "couldn't acquire a semaphore" }

// Gate the number of active web requests
let webRequestGate = RequestGate(5)

// Fetch the URL, and post the results to the urlCollector.
let collectLinks (url:string) =
    async { // An Async web request with a global gate
            let! html =
                async { // Acquire an entry in the webRequestGate. Release
                        // it when 'holder' goes out of scope
                        use! holder = webRequestGate.AsyncAcquire()

                        let req = WebRequest.Create(url,Timeout=5)

                        // Wait for the WebResponse
                        use! response = req.AsyncGetResponse()

                        // Get the response stream
                        use reader = new StreamReader(response.GetResponseStream())

                        // Read the response stream (note: a synchronous read)
                        return reader.ReadToEnd()  }

            // Compute the links, synchronously
            let links = getLinks html

            // Report, synchronously
            do printfn "finished reading %s, got %d links" url (List.length links)

            // We're done
            return links }

/// 'urlCollector' is a single agent that receives URLs as messages. It creates new
/// asynchronous tasks that post messages back to this object.
let urlCollector =
    MailboxProcessor.Start(fun self ->

        // This is the main state of the urlCollector
        let rec waitForUrl (visited : Set<string>) =

           async { // Check the limit
                   if visited.Count < limit then

                       // Wait for a URL...
                       let! url = self.Receive()
                       if not (visited.Contains(url)) then
                           // Start off a new task for the new url. Each collects
                           // links and posts them back to the urlCollector.
                           do! Async.StartChild
                                   (async { let! links = collectLinks url
                                            for link in links do
                                               self.Post link }) |> Async.Ignore

                       // Recurse into the waiting state
                       return! waitForUrl(visited.Add(url)) }

        // This is the initial state.
        waitForUrl(Set.empty))

urlCollector.Post("http://news.google.com")

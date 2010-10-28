// Expert F# 2.0
// Chapter 13 Example 07

let counter =
    new MailboxProcessor<_>(fun inbox ->
        let rec loop n =
            async { printfn "n = %d, waiting..." n
                    let! msg = inbox.Receive()
                    return! loop (n+msg) }
        loop 0)

counter.Start()
counter.Post(1)
counter.Post(2)
counter.Post(1)

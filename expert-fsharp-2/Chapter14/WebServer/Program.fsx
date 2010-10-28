open System.Net
open System.Net.Sockets
open System.IO
open System.Text.RegularExpressions
open System.Text

/// A table of MIME content types
let mimeTypes =
    dict [".html", "text/html";
          ".htm",  "text/html";
          ".txt",  "text/plain";
          ".gif",  "image/gif";
          ".jpg",  "image/jpeg";
          ".png",  "image/png"]

/// Compute a MIME type from a file extension
let getMimeType(ext) =
    if mimeTypes.ContainsKey(ext) then mimeTypes.[ext]
    else "binary/octet"

/// The pattern Regex1 uses a regular expression to match
/// one element
let (|Regex1|_|) (patt: string) (inp: string) =
    try Some(Regex.Match(inp, patt).Groups.Item(1).Captures.Item(0).Value)
    with _ -> None

/// The root for the data we serve
let root = @"c:\inetpub\wwwroot"

/// Handle a TCP connection for an HTTP GET
let handleClient(client: TcpClient) =
    use stream = client.GetStream()
    let out = new StreamWriter(stream)
    let inp = new StreamReader(stream)
    match inp.ReadLine() with

    | Regex1 "GET (.*?) HTTP/1\\.[01]$" fileName ->
        let fname = root + @"\" + fileName.Replace("/", @"\")
        let mimeType = getMimeType(Path.GetExtension(fname))
        let content = File.ReadAllBytes(fname)
        fprintfn out "HTTP/1.0 200 OK"
        fprintfn out "Content-Length: %d" content.Length
        fprintfn out "Content-Type: %s" mimeType
        fprintfn out ""
        out.Flush()
        stream.Write(content, 0, content.Length)
    | line ->
        ()

/// The server as an asynchronous process. We handle requests
/// sequentially.
let server =
    async { let socket = new TcpListener(IPAddress.Parse("127.0.0.1"), 8090)
            do socket.Start()
            while true do
                use client = socket.AcceptTcpClient()
                do try handleClient(client) with _ -> ()
          }

open System.IO
open System.Net

/// Get the contents of the URL via a web request
let http(url: string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html


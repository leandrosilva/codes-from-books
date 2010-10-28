module LexingAndParsing

let splitLine (line: string) =
    line.Split [| ',' |] |> Array.map (fun s -> s.Trim())

let parseEmployee (line: string) =
    match splitLine line with
    | [| last; first; startDate; title |] ->
        last, first, System.DateTime.Parse(startDate), title
    | _ ->
        failwithf "invalid employee format: '%s'" line

open System.IO
let readEmployees (fileName : string) =
    seq {
        use reader = File.OpenText fileName
        while not reader.EndOfStream do
            yield reader.ReadLine() |> parseEmployee
    }


//let readEmployees (fileName : string) =
//    Seq.
//       (fun () -> File.OpenText(fileName))
//       (fun reader ->
//             if reader.EndOfStream then None
//             else Some(parseEmployee(reader.ReadLine())) )

open System.Text.RegularExpressions

let parseHttpRequest line =
    let result = Regex.Match(line, @"GET (.*?) HTTP/1\.([01])$")
    let file = result.Groups.Item(1).Value
    let version = result.Groups.Item(2).Value
    file, version


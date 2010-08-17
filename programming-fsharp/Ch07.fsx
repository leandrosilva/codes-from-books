module ProgrammingFS.Ch07

[<Measure>]
type fahrenheit

let printTemperature (temp : float<fahrenheit>) =

    if   temp < 32.0<_>  then
        printfn "Below Freezing!"
    elif temp < 65.0<_>  then
        printfn "Cold"
    elif temp < 75.0<_>  then
        printfn "Just right!"
    elif temp < 100.0<_> then
        printfn "Hot!"
    else
        printfn "Scorching!"

// ----------------------------------------------------------------------------

// Define seconds and hertz
[<Measure>]
type s

[<Measure>]
type Hz = s ^ -1

if (3.0<s ^ -1> <> 3.0<Hz>) then failwith "ERROR"

// ----------------------------------------------------------------------------

// Represents a point respecting the unit of measure
type Point< [<Measure>] 'u >(x : float<'u>, y : float<'u>) =

    member this.X = x
    member this.Y = y

    member this.UnitlessX = float x
    member this.UnitlessY = float y

    member this.Length =
        let sqr x = x * x
        sqrt <| sqr this.X + sqr this.Y

    override this.ToString() =
        sprintf
            "{%f, %f}"
            this.UnitlessX
            this.UnitlessY

// ----------------------------------------------------------------------------

open System.IO

// Convert a file path into its extension
let (|FileExtension|) filePath = Path.GetExtension(filePath)

let determineFileType (filePath : string) =
    match filePath with
    
    // Without active patterns
    | filePath when Path.GetExtension(filePath) = ".txt"
        -> printfn "It is a text file."
    
    // Converting the data using an active pattern
    | FileExtension ".jpg" 
    | FileExtension ".png"
    | FileExtension ".gif"
        -> printfn "It is an image file."
        
    // Binding a new value
    | FileExtension ext 
        -> printfn "Unknown file extension [%s]" ext

// ----------------------------------------------------------------------------

open System

let (|ToBool|_|) x = 
    let success, result = Boolean.TryParse(x)
    if success then Some(result)
    else            None
    
let (|ToInt|_|) x = 
    let success, result = Int32.TryParse(x)
    if success then Some(result)
    else            None
    
let (|ToFloat|_|) x = 
    let success, result = Double.TryParse(x)
    if success then Some(result)
    else            None
    
let describeString str = 
    match str with
    | ToBool  b -> printfn "%s is a bool with value %b" str b
    | ToInt   i -> printfn "%s is an integer with value %d" str i
    | ToFloat f -> printfn "%s is a float with value %f" str f
    | _         -> printfn "%s is not a bool, int, or float" str

// ----------------------------------------------------------------------------

open System
open System.Text.RegularExpressions

// Use a regular expression to capture three groups
let (|RegexMatch3|_|) (pattern : string) (input : string) =
    let result = Regex.Match(input, pattern)
    
    if result.Success then
        match (List.tail [ for g in result.Groups -> g.Value ]) with
        | fst :: snd :: trd :: [] 
             -> Some (fst, snd, trd)
        | [] -> failwith <| "Match succeeded, but no groups found.\n" +
                            "Use '(.*)' to capture groups"
        | _  -> failwith "Match succeeded, but did not find exactly three groups."
    else
        None
    
let parseTime input =
    match input with
    // Match input of the form "6/20/2008"
    | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year) 
    // Match input of the form "2004-12-8"
    | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day)
        -> Some( new DateTime(int year, int month, int day) )
    | _ -> None

// ----------------------------------------------------------------------------

open System

// This active pattern divides all strings into their various meanings.
let (|Paragraph|Sentence|Word|WhiteSpace|) (input : string) =
        let input = input.Trim()
        
        if input = "" then
            WhiteSpace
        elif input.IndexOf(".") <> -1 then
            // Paragraph contains a tuple of sentence counts and sentences.
            let sentences = input.Split([|"."|], StringSplitOptions.None)
            Paragraph (sentences.Length, sentences)
        elif input.IndexOf(" ") <> -1 then
            // Sentence contains an array of string words
            Sentence (input.Split([|" "|], StringSplitOptions.None))
        else
            // Word contains a string
            Word (input)
 
// Count the number of letters of a string by breaking it down
let rec countLetters str =
    match str with
    | WhiteSpace -> 0
    | Word x     -> x.Length
    | Sentence words
        -> words 
           |> Array.map countLetters 
           |> Array.sum
    | Paragraph (_, sentences)
        -> sentences
           |> Array.map countLetters  
           |> Array.sum

// ----------------------------------------------------------------------------

let (|EndsWithExtension|_|) ext (x : string) = 
    if x.EndsWith(ext) then Some() else None

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

open System.IO

let (|KBInSize|MBInSize|GBInSize|) filePath =
    let file = File.Open(filePath, FileMode.Open)
    if   file.Length < 1024L * 1024L then
        KBInSize
    elif file.Length < 1024L * 1024L * 1024L then
        MBInSize
    else
        GBInSize

let (|IsImageFile|_|) filePath = 
    match filePath with
    | EndsWithExtension ".jpg"
    | EndsWithExtension ".bmp"
    | EndsWithExtension ".gif"
        -> Some()
    | _ -> None

let ImageTooBigForEmail filePath =
    match filePath with
    | IsImageFile & (MBInSize | GBInSize)
        -> true
    | _ -> false

// ----------------------------------------------------------------------------

// This example requires a reference to System.Xml.dll
#r "System.Xml.dll"
open System.Xml

// Match an XML element
let (|Elem|_|) name (inp : XmlNode) =
    if inp.Name = name then Some(inp)
    else                    None

// Get the attributes of an element
let (|Attributes|) (inp : XmlNode) = inp.Attributes

// Match a specific attribute
let (|Attr|) attrName (inp : XmlAttributeCollection) =
    match inp.GetNamedItem(attrName) with
    | null -> failwithf "Attribute %s not found" attrName
    | attr -> attr.Value

// What we are actually parsing
type Part =
    | Widget   of float
    | Sprocket of string * int
    
let ParseXmlNode element = 
    match element with
    // Parse a Widget without nesting active patterns
    | Elem "Widget" xmlElement
        -> match xmlElement with
           | Attributes xmlElementsAttributes 
               -> match xmlElementsAttributes with
                  | Attr "Diameter" diameter
                      -> Widget(float diameter)

    // Parse a Sprocket using nested active patterns
    | Elem "Sprocket" (Attributes (Attr "Model" model & Attr "SerialNumber" sn))
        -> Sprocket(model, int sn)
        
    |_ -> failwith "Unknown element"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Load the XML Document
let xmlDoc =
    let doc = new System.Xml.XmlDocument()
    let xmlText =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
        <Parts>
            <Widget Diameter='5.0' />
            <Sprocket Model='A' SerialNumber='147' />
            <Sprocket Model='B' SerialNumber='302' />
        </Parts>
        "
    doc.LoadXml(xmlText)
    doc

// ----------------------------------------------------------------------------

open System.Net

type WebScraper(url) =

    let downloadWebpage (url : string) =
        let req = WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        reader.ReadToEnd()
        
    let extractImageLinks html =
        let results = Regex.Matches(html, "<img src=\"([^\"]*)\"")
        [
            for r in results do
                for grpIdx = 1 to r.Groups.Count - 1 do
                    yield r.Groups.[grpIdx].Value 
        ] |> List.filter (fun url -> url.StartsWith("http://"))
    
    let downloadToDisk (url : string) (filePath : string) =
        use client = new System.Net.WebClient()
        client.DownloadFile (url, filePath)

    let scrapeWebsite destPath (imageUrls : string list) =       
        imageUrls
        |> List.map(fun url -> 
                let parts = url.Split( [| '/' |] )
                url, parts.[parts.Length - 1])
        |> List.iter(fun (url, filename) -> 
                downloadToDisk url (Path.Combine(destPath, filename)))
                
    // Add class fields
    let m_html   = downloadWebpage url
    let m_images = extractImageLinks m_html
    
    // Add class members
    member this.SaveImagesToDisk(destPath) =
        scrapeWebsite destPath m_images 

// ----------------------------------------------------------------------------

let maxInt = System.Int32.MaxValue

open Checked

try
    let _ = maxInt + 1 
    // Game over man :(
    exit 1
with
| _ -> ()

// ----------------------------------------------------------------------------

// Slow implementation...
let removeConsecutiveDupes1 lst =

    let foldFunc acc item =
        let lastLetter, dupesRemoved = acc
        match lastLetter with
        | Some(c) when c = item  
                  -> Some(c), dupesRemoved
        | Some(c) -> Some(item), dupesRemoved @ [item]
        | None    -> Some(item), [item]

    let (_, dupesRemoved) = List.fold foldFunc (None, []) lst
    dupesRemoved
    
// Fast implementation...
let removeConsecutiveDupes2 lst = 
    let f item acc =
        match acc with
        | [] 
            -> [item]
        | hd :: tl when hd <> item 
            -> item :: acc
        | _ -> acc
    
    List.foldBack f lst []

// ----------------------------------------------------------------------------

type BinTree<'a> =
    | Node of 'a * BinTree<'a> * BinTree<'a>
    | Empty

let rec iterNonTR f binTree =
    match binTree with
    | Empty -> ()
    | Node(x, l, r) ->
        f x       
        iterNonTR f l  // NOT in tail position
        iterNonTR f r  // In tail position

// ----------------------------------------------------------------------------

// Print a list in revere using a continuation
let printListRev list =
    let rec printListRevTR list cont =
        match list with
        // For an empy list, execute the continuation
        | [] -> cont()
        // For other lists, add printing the current
        // node as part of the continuation.
        | hd :: tl ->
            printListRevTR tl (fun () -> printf "%d " hd
                                         cont() )

    printListRevTR list (fun () -> printfn "Done!")

// ----------------------------------------------------------------------------

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)
    
let iter f binTree =
    
    let rec linearize binTree cont =
        match binTree with
        | Empty -> cont()
        | Node(x, l, r) ->
            Step(x, (fun () -> linearize l (fun() -> linearize r cont)))
    
    let steps = linearize binTree (fun () -> Finished)
    
    let rec processSteps step =
        match step with
        | Finished -> ()
        | Step(x, getNext) 
            ->  f x
                processSteps (getNext())
                
    processSteps steps

// ----------------------------------------------------------------------------

// Data type for a set. Notice the implementation is 
// stored in record fields...
type Set = 
    { 
        // Add an item to the set
        Add    : int -> Set
        // Checks if an element exists in the set
        Exists : int -> bool 
    }
    
    // Returns an empty set
    static member Empty =
        let rec makeSet lst = 
            { 
                Add    = (fun item -> makeSet (item :: lst))
                Exists = (fun item -> List.exists ((=) item) lst)
            }
        makeSet [] 

// ----------------------------------------------------------------------------

open System.Collections.Generic

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true,  x -> x
        | false, _ ->
            // Evaluate and add to lookup table 
            let answer = f input
            dict.Add(input, answer)
            answer

    // Return our memoized version of f dict is captured in the closure
    memoizedFunc
    
// ----------------------------------------------------------------------------

#norwarn "40"

// CORRECT way to memoize the function - fib  does call
// the memoized version recursively.
let rec rightMemFib =
    let fib x =
        match x with
        | 0 | 1 -> 1
        | 2 -> 2
        | n -> rightMemFib (n - 1) + rightMemFib (n - 2)

    memoize fib

// ----------------------------------------------------------------------------

type LazyBinTree<'a> =
    | Node of 'a * LazyBinTree<'a> Lazy * LazyBinTree<'a> Lazy
    | Empty
    
let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(x, l, r) ->
        Node(
            f x, 
            lazy(
                let lfNode = l.Value
                map f lfNode
            ), 
            lazy(
                let rtNode = r.Value
                map f rtNode
            )
        )

// ----------------------------------------------------------------------------

open System
open System.IO

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)
        
        // Skip header row
        fileReader.ReadLine() |> ignore
        
        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split( [| ',' |] )
    }

let rootPath = @"D:\DataFiles\"
let csvFiles = Directory.GetFiles(rootPath, "*.csv")

let allCsvData = 
    csvFiles
    |> Seq.map processFile
    |> Seq.concat

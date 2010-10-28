module Text2Html

open System.IO
open System.Text

let main() =
   let args = System.Environment.GetCommandLineArgs()
   if args.Length <= 2 then
         let exe = Path.GetFileName(args.[0])
         eprintfn "Usage: %s dir pattern" exe
         exit 1
   let directory = args.[1]
   let pattern = args.[2]

   for fileName in Directory.GetFiles(directory, pattern) do

      // Open a file stream for the file name
      use inputReader = File.OpenText fileName

      // Create a lex buffer for use with the generated lexer. The lex buffer
      // reads the inputReader stream.
      let lexBuffer = Lexing.LexBuffer<_>.FromTextReader inputReader
      // Open an output channel
      let outputFile = Path.ChangeExtension(fileName,"html")
      use outputWriter = (new StreamWriter(outputFile) :> TextWriter)

      // Write the header
      fprintfn outputWriter "<html>\n<head></head>\n<pre>"

      // Run the generated lexer
      Text2HtmlLex.convertHtml outputWriter lexBuffer

      // Write the footer
      fprintfn outputWriter "</pre>\n</html>\n"

do main()

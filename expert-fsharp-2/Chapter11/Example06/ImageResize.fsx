// Expert F# 2.0
// Chapter 11

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System
open System.IO
open System.Windows.Media.Imaging

let transformFile quality width height srcFileName outFileName =
  let dec = new JpegBitmapDecoder(
              new Uri(srcFileName),
              BitmapCreateOptions.PreservePixelFormat,
              BitmapCacheOption.Default)
  let w = dec.Frames.[0].Width
  let h = dec.Frames.[0].Height

  let b = new BitmapImage()
  b.BeginInit()
  b.UriSource <- new Uri(srcFileName)
  if width > 0 then
    if w >= h then b.DecodePixelWidth <- width
    else b.DecodePixelHeight <- height
  b.EndInit()

  let metadata = dec.Frames.[0].Metadata

  let enc = new JpegBitmapEncoder()
  enc.Frames.Add(BitmapFrame.Create(b,null, metadata :?> BitmapMetadata, null))
  let fs = new FileStream(outFileName, FileMode.OpenOrCreate)
  enc.QualityLevel <- quality
  enc.Save(fs)
  fs.Close()
  let fin = new FileInfo(srcFileName)
  let fout = new FileInfo(outFileName)
  fout.CreationTime <- fin.CreationTime
  fout.LastWriteTime <- fin.LastWriteTime

let transformDir quality width height src dest =
  let rec visit (dirin:DirectoryInfo) (dirout:DirectoryInfo) =
    for f in dirin.EnumerateFiles() do
      if f.Extension.ToUpper() = ".JPG" then
        printfn "Processing file %s..." f.FullName
        transformFile quality width height f.FullName (dirout.FullName + "\\" + f.Name)

    for d in dirin.EnumerateDirectories() do
      visit d (dirout.CreateSubdirectory(d.Name))

  let dirin = new DirectoryInfo(src)
  let dirout = if (not(Directory.Exists(dest)))
                then Directory.CreateDirectory dest 
                else new DirectoryInfo(dest)
  visit dirin dirout

let dn = @"C:\Users\SomeUser\Pictures\Summer 2010"
let dno = @"e:\Summer PhotoFrame 2010"

transformDir 75 1027 768 dn dno

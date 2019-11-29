/// Split a string into words at spaces.
let splitAtSpaces (text: string) =
  text.Split ' '
  |> Array.toList

/// Analyze a string for duplicate words.
let wordCount text =
  let words = splitAtSpaces text
  let numWords = words.Length
  let distinctWords = List.distinct words
  let numDups = numWords - distinctWords.Length
  (numWords, numDups)

/// Analyze a string for duplicate words and display the results.
let showWordCount text =
  let numWords, numDups = wordCount text
  printfn "--> %d words in the text" numWords
  printfn "--> %d duplicate words" numDups

let (numWords, numDups) = wordCount "All the king's horses and all the king's men"

showWordCount "Couldn't put Humpty together again"

open System.IO
open System.Net

/// Get the contents of the URL via a web request
let http (url: string) =
  let req = WebRequest.Create(url)
  let resp = req.GetResponse()
  let stream = resp.GetResponseStream()
  let reader = new StreamReader(stream)
  let html = reader.ReadToEnd()
  resp.Close()
  html

http "http://www.google.com"

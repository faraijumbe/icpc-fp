module ICPC
open System

//this potion is to change the 
let arrayList (input:String) = 
    let array = input.Split(" ")
    Array.toList array

// checks which word has a comma
let findComma input = String.exists(fun x -> x = ',') input

//checks to see if there is a comma
let rec hasComma input = 
        match input with
        |[] -> false
        |first::rest -> 
                    match findComma first with
                    |true -> true
                    |false -> hasComma rest

let wordContainsComma word = String.exists(fun x -> x = ',') word

let addCommas word list = 
    List.map(fun x -> match x = word with
                      |true -> x + ";"
                      |false -> x) list 
let removeComma  word = String.filter(fun x -> x = ',' |> not) word

//adds comma to other words in the list
let addCommaToWords input word = 
    let wordNoComma = removeComma word
    List.map(fun x -> match x = wordNoComma with
                      |true ->  x + ","
                      |false -> x ) input

//adds the after commas
let rec useList input acc = 
    match input with
    |[] -> acc
    |first::rest -> match wordContainsComma first with
                    |true -> let list = addCommaToWords acc first 
                             useList rest list 
                    |false -> useList rest acc 

// add the before commas

let rec tostring list acc len = 
    match list with
    |[] -> acc
    |first::rest -> match len with
                    |1 -> tostring rest (acc + first) (len - 1)
                    |_-> tostring rest (acc + first + " ") (len - 1)

let exist (p) input = List.exists(p) input

let wordarrayList (input:String) = 
    Seq.toList input

let rec checkCommaEnd input = 
    match input with
    |[] -> false
    |first::rest -> 
                    match wordContainsComma first |> not with
                    |true -> checkCommaEnd rest
                    |false-> 
                        let len = String.length first
                        let wordList = wordarrayList first
                        match List.item(len - 1) wordList = ',' with
                        |true -> checkCommaEnd rest
                        |false -> true


let rec repeatingDots input char = 
    match input with
    |[] -> false 
    |first::rest -> let word = wordarrayList first
                    let rec count word acc = 
                        match word with
                        |[] -> acc
                        |first::rest -> match first = char with
                                        |true -> count rest (acc + 1)
                                        |false -> count rest acc
                    match (count word 0) > 1 with
                    |true -> true
                    |false -> repeatingDots rest char

let checkError input =
    match exist (fun x -> x = "") input || exist (fun x -> x = ".") input with 
    |true -> true
    |false -> let lastWord = input.Item(input.Length - 1)
              match wordContainsComma lastWord with
              |true -> true
                        // checks to see if the word with the comma has it at the end of the word
              |false -> match checkCommaEnd input with
                        |true -> true
                        |false -> repeatingDots input '.'       
                        

let commaSprinkler input =
    let list = arrayList input
    //checkErrors
    match hasComma list |> not with
    |true -> None
    |false -> // c
              match checkError list with
              |true -> None
              |false -> 
                  let someList =  useList list list
                  match someList.Equals(list) with
                  |true -> None
                  |false -> Some(tostring someList "" someList.Length)

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

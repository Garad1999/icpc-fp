module ICPC
open System

(*type ListStorage = {
    list : [string] 
    }*)


let printList mainlist (* : List<String> *)= //Check outputstatement
    
    let rec printrec list = 
        match list with
        | [] -> []
        | x::rest -> 
            (printf("%s ") x) 
            printrec rest

    printrec (mainlist(* : List<String>*) )


let toList input= 
    let spaceSplit = (fun (x:string) -> Seq.toList (x.Split ' '))
    spaceSplit input //COnverts To List

let toChar input = 
    let toChar = (fun (x:string) -> Seq.toList x)
    toChar input //COnverts string to chars

let commaChecker input = 
    
    let rec toEnd text =
        match text with
        | [] -> false
        | x::rest -> 
            match x with 
            | ',' -> true
            | ' ' -> false
            | _ -> toEnd rest

    toEnd (toChar input) //Can modify for spaces, returns a boolean //Checks the word if there is a comma if there isnt return false
    
//let ListToString listToString=
   // listToString.ToString //essentually toString method everywhere this is used .toString can work

let commaCutter input= //returns List without comma
    let x = toChar input
    let rec cutsCommaOut wordToCut=
        match wordToCut with
        | [] -> []
        | y::rest ->
            match y with
            | ',' -> rest
            | _ -> y::(cutsCommaOut rest)
    cutsCommaOut x


let tester input=
    let newlist = toList input
    let rec idividual input =
        match input with
        | [] -> []
        | x::input -> 
            commaCutter x 
    idividual newlist

let commaSprinkler (input : string) =


    let x = commaCutter input
    let y = x.ToString()
    let result = toList y
    printList result



let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    let x = commaSprinkler "HELLO, my, name, is, Garad"

    0 // return an integer exit code

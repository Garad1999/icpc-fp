module ICPC
open System

type ListStorage = {
    basis : List<String> //can be used to hold the basic list of the words
    beginning : List<String> //contais the word and the space and comma
    ending : List<String> //contains the comma
    }


let stringCon input =
    let rec listToString input holder = 
        match input with
        |[] -> holder
        |x::rest -> listToString rest (holder+x)
    listToString input ""

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
    spaceSplit input //COnverts To List at " "

let commaToList input= 
    let newInput = stringCon input
    printfn ("%s") newInput
    let spaceSplit = (fun (x:string) -> Seq.toList (x.Split ',') )
    spaceSplit newInput

let periodToList input= 
    let newInput = stringCon input
    printfn ("%s") newInput
    let spaceSplit = (fun (x:string) -> Seq.toList (x.Split '.') )
    spaceSplit newInput

let incorp input =
    let v = toList input
    let x = commaToList v
    periodToList x
    

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
    


(*let commaCutter input= //returns List without comma
    let x = toChar input
    let rec cutsCommaOut wordToCut=
        match wordToCut with
        | [] -> []
        | y::rest ->
            match y with
            | ',' -> rest
            | _ -> y::(cutsCommaOut rest)
    cutsCommaOut x*)


(*let tester input=
    let newlist = toList input
    let rec idividual input =
        match input with
        | [] -> []
        | x::input -> 
            commaCutter x 
    idividual newlist*)
let construction input=
    let checker :ListStorage = {basis = []; ending = []; beginning = []}
    let update = {checker with ending = toList input; basis = incorp input}
    update

let commaSprinkler input =
    
    let y = construction input
    printfn ("%s") (y.ToString())
    let x = stringCon (toList input)
    printfn ("%s") (x)

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    let x = commaSprinkler "HELLO my, name, is, Garad"
    

    0 // return an integer exit code

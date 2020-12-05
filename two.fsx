(*

Each line gives the password policy and then the password. 
The password policy indicates the lowest and highest number of times 
a given letter must appear for the password to be valid. For example, 
1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. 
The middle password, cdefg, is not; it contains no instances of b, 
but needs at least 1. The first and third passwords are valid: 
they contain one a or nine c, 
both within the limits of their respective policies.


How many passwords are valid according to their policies?

*)


#r "nuget: FSharp.Text.RegexProvider"
open System.Globalization
open FSharp.Text.RegexProvider
open System.Text.RegularExpressions
open System.Linq

type PasswordParser = FSharp.Text.RegexProvider.Regex< @"(?<Min>\d+)\-(?<Max>\d+)\s+(?<Character>\w)\:\s+(?<Password>\w+)">


let validationOne (m : PasswordParser.MatchType) =
    let occurrences = 
        m.Password.Value 
        |> Seq.where (fun c -> c = m.Character.Value.[0])
        |> Seq.length

    occurrences <= (int) m.Max.Value
    && occurrences >= (int) m.Min.Value  


let validatePassword validationPolicy passwordList = 
    passwordList
    |> List.toSeq
    |> Seq.mapi (fun i x -> 
        PasswordParser()
            .TryTypedMatch(x) 
            |> Option.map (fun m -> i, m))
    |> Seq.choose id
    |> Seq.map (fun (i,m) -> 
        let isValid = validationPolicy m  
        i, isValid
        )
    |> Seq.filter (fun (_,isValid) -> isValid)
    |> Seq.map (fun (i,_) -> passwordList.[i]) 
    
let sample = 
    [
        "1-3 a: abcde" //valid
        "1-3 b: cdefg" //invalid
        "2-9 c: ccccccccc" //valid
        "1-3 z: azaazaxyz" //valid
        "1-9 x: xwjgxtmrzxzmkx" //valid
        "5-16 m: pxmrtmbmqmcldmmm" //valid
    ]

//should be 5 valid
sample
|> validatePassword validationOne
|> Seq.length


//solution...


System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/two.input.txt")
|> List.ofSeq
|> validatePassword validationOne
|> Seq.length

//-------PART 2

(*
Each policy actually describes two positions in the password, 
where 1 means the first character, 
2 means the second character, 
and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) 
Exactly one of these positions must contain the given letter. <<<<<
Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

*)

//solution part 2

let validationTwo (m : PasswordParser.MatchType) =

    let firstExclusiveIndex = (int) m.Min.Value - 1
    let secondExcluiveIndex= (int) m.Max.Value - 1  

    let occurrences = 
        m.Password.Value 
        |> Seq.mapi (fun i c -> i, c = m.Character.Value.[0])
        |> Seq.filter (fun (_,charMatch) -> charMatch)
        |> Seq.filter (fun (i,_) -> 
            (i = firstExclusiveIndex) //exclusive or
            <> (i = secondExcluiveIndex))
        |> Seq.length 

    occurrences = 1

let sample2 = 
    [
        "1-3 a: abcde" //valid
        "1-3 b: cdefg" //invalid
        "2-9 c: ccccccccc" //invalid
        "1-3 z: zezxxx" //invalid
        "1-3 z: xezxxx" //valid
        "1-3 z: zexxxx" //valid
        "1-3 z: zexxzx" //valid
        "1-3 z: zeezzzzzz" //valid
    ]

//should be 5 valid
sample2
|> validatePassword validationTwo
|> Seq.length

//solution 2
System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/two.input.txt")
|> List.ofSeq
|> validatePassword validationTwo
|> Seq.length


let lines2 = 
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/two.input.txt")
    |> List.ofSeq
    
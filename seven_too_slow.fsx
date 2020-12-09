(*

Due to recent aviation regulations,
many rules (your puzzle input) are being enforced about bags and their contents; 
bags must be color-coded and must contain specific quantities of other color-coded bags. 
Apparently, nobody responsible for these regulations considered how long they would take 
to enforce!

For example, consider the following rules:
-----------------------------------------------------------------
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
-----------------------------------------------------------------
These rules specify the required contents for 9 bag types. 
In this example, every faded blue bag is empty, 
every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.

You have a shiny gold bag. 
If you wanted to carry it in at least one other bag, 
how many different bag colors would be valid for the outermost bag? 
(In other words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:
``````````````````````````````````````````````````````````````````````````
A bright white bag, which can hold your shiny gold bag directly.

A muted yellow bag, which can hold your shiny gold bag directly, 
plus some other bags.

A dark orange bag, which can hold bright white and muted yellow bags, 
either of which could then hold your shiny gold bag.

A light red bag, which can hold bright white and muted yellow bags, 
either of which could then hold your shiny gold bag.
`````````````````````````````````````````````````````````````````````````````

So, in this example, 
the number of bag colors that can eventually contain at least one shiny gold bag is 4. <<<

How many bag colors can eventually contain at least one shiny gold bag? 
(The list of rules is quite long; make sure you get all of it.)

*)

#r "nuget: FSharp.Text.RegexProvider"
open System.Globalization
open FSharp.Text.RegexProvider
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO

[<Literal>]
let bagKeyRegex = "^(?<Bag>(?<BagType>\w+)\s(?<Color>\w+)\sbags?)\scontain\s(?<NoBags>no\sother)?(?<Bags>.+\,?)?\sbags?\.$"

[<Literal>]
let extraBagsRegex = "^\s?(?<Number>\d+)\s(?<BagTypeNth>\w+)\s(?<ColorNth>\w+)(\sbags?(\,|\.)?)?"

type BagRegex = FSharp.Text.RegexProvider.Regex<bagKeyRegex>

type ExtraBagRegex = FSharp.Text.RegexProvider.Regex<extraBagsRegex>

//let r = BagRegex().TypedMatch("light red bags contain 1 bright white bag, 2 muted yellow bags.")

//r.BagType.Value

//let extras = 
//    r.Bags.Value.Split(',') 
//    |> Seq.map (ExtraBagRegex().TypedMatch)
//    |> Seq.toList

let bagRegexGlobal = BagRegex()
let extraBagRegexGlobal = ExtraBagRegex()


let getDictionaryEntryFromLine singleLine =
    let mo = bagRegexGlobal.TryTypedMatch(singleLine)
    match mo with
    |None -> 
        None
    |Some(matchType) -> 
        match matchType.Success with
        |true -> 
            Some(((matchType.BagType.Value, matchType.Color.Value),
                if matchType.NoBags.Success ||  not matchType.Bags.Success then
                    []
                else
                    matchType.Bags.Value.Split(',') 
                    |> Seq.map (extraBagRegexGlobal.TypedMatch)
                    |> Seq.filter (fun x -> x.Success)
                    |> Seq.map(fun m ->
                        ((int)m.Number.Value, (m.BagTypeNth.Value, m.ColorNth.Value))
                    )
                    |> Seq.toList
            ))
        |false -> None


"light red bags contain 1 bright white bag, 2 muted yellow bags."  
|> getDictionaryEntryFromLine

(*
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
*)

(*
^(?<Bag>(?<BagType>\w+)\s(?<Color>\w+)\sbag(s?))
\scontain
(?<NoBags>\sno\sother\sbags)?
(?<ExtraBag>\s(?<Number>\d+)\s\k<Bag>(\,)?)*
\.$
*)

let parseTypes =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.sample.txt")
    |> Seq.map (fun line -> 
        line.Split([|' '|], StringSplitOptions.None).[0..1]
        |> Array.toList
        )
    |> Set.ofSeq //dedup
    
let types = 
    parseTypes 
    |> Set.map (fun (x1::_) -> x1)

let colors = 
    parseTypes 
    |> Set.map (fun (_::x2::[]) -> x2)

// dictionary parsing from file
let BuildDictionary (inputSequence : string seq) =
    inputSequence 
    |> Seq.map getDictionaryEntryFromLine
    |> Seq.choose id
    |> dict

let testDictionary = 
    [
    //light red bags contain 1 bright white bag, 2 muted yellow bags.
        (("light","red"), 
            [
                (1, ("bright","white"))
                (2, ("muted","yellow"))
            ]
        )
    //dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        (("dark","orange"), 
            [
                (3, ("bright","white"))
                (4, ("muted","yellow"))
            ]
        )
    //bright white bags contain 1 shiny gold bag.
        (("bright","white"), 
            [
                (1, ("shiny","gold"))
            ]
        )
    //muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        (("muted","yellow"), 
            [
                (2, ("shiny","gold"))
                (9, ("faded","blue"))
            ]
        )
    //shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        (("shiny","gold"), 
            [
                (1, ("dark","olive"))
                (2, ("vibrant","plum"))
            ]
        )
    //dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        (("dark","olive"), 
            [
                (3, ("faded","blue"))
                (4, ("dotted","black"))
            ]
        )
    //vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        (("vibrant","plum"), 
            [
                (5, ("faded","blue"))
                (6, ("dotted","black"))
            ]
        )
    //faded blue bags contain no other bags.
        (("faded","blue"), [ ]
        )
    //dotted black bags contain no other bags.
        (("dotted","black"), [ ]
        )       
    ]
    //|> Seq.map (fun (tup, l) -> (tup, l |> Seq.toArray))

let testParsing =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.sample.txt")
    |> Seq.map getDictionaryEntryFromLine
    |> Seq.choose id
    |> Set.ofSeq
    |> Set.difference (Set.ofSeq testDictionary)
    |> Set.count //must be zero

type RulesDictionary = IDictionary<(string*string),(int * (string * string)) list>
type Type = Type of string
type Color = Color of string
type Bag = { Type : Type; Color: Color; InnerBags : Bag list }
with static member Build (rulesDictionary : RulesDictionary) (bagType,bagColor)  =

            let dictKey = (bagType, bagColor)
            let isSome, innerBags = rulesDictionary.TryGetValue dictKey
            match isSome with
            |true -> 
                Some({   
                    Type = Type(bagType) 
                    Color = Color(bagColor)
                    InnerBags = 
                        innerBags
                        |> List.collect (fun (number, (bt,bc)) -> 
                            [0..number] 
                            |> List.map (fun _ -> Bag.Build rulesDictionary (bt,bc))
                            |> List.choose id
                        )
                })
            |_ -> 
                None

//hyperslow
let rec hasBagContainingColorSlow (bagType, color) (bag : Bag) : Bag list =
    bag.InnerBags 
    |> Seq.fold (fun a x -> 
        if (x.InnerBags |> Seq.length) > 0 then
            match (x.Color = Color(color) && x.Type = Type(bagType)) with
            |true -> 
                x:: (hasBagContainingColorSlow (bagType, color) x)
            |false ->  
                hasBagContainingColorSlow (bagType, color) x
        else
            a
        ) []
    

//experimental version with break to limit search (we just want count)
let rec hasBagContainingColorTooSlowToo (bagType, color) (bag : Bag) : Bag list =
    let results = new ResizeArray<Bag>()
    try
        for b in bag.InnerBags do
            if b.Type = Type(bagType) && b.Color = Color(color) then
                results.Add(b)
                failwith "found"
            else
                let innerResults = hasBagContainingColorTooSlowToo (bagType, color) b 
                results.AddRange(innerResults)
        []
    with ex ->
        results |> List.ofSeq

let collectInnerBagsAsList (bag : Bag) : (Type*Color) list =
    let rec helper bags acc =
        match bags with
        |x::xs ->
             helper xs ((x.Type, x.Color) :: acc)
        |[] ->
            acc
    helper bag.InnerBags [] 

    

let dictionaryTest =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.toArray
    |> BuildDictionary 

let testPerf =
    let bag = Bag.Build dictionaryTest ("bright","white")
    let innerBags = collectInnerBagsAsList bag.Value
    innerBags.[0]


(*
A bright white ...
A muted yellow ...
A dark orange ...
A light red ...
So, in this example, the number... is 4.
*)
let lines =
     File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.sample.txt")
     |> Seq.toArray

let sample =
    lines
    |> Seq.map (bagRegexGlobal.TryTypedMatch)
    |> Seq.choose id
    |> Seq.map (fun m -> m.BagType.Value, m.Color.Value)
    |> Seq.map (fun x -> Bag.Build (lines |> BuildDictionary) x)
    |> Seq.choose id
    |> Seq.filter (fun x -> 
        (hasBagContainingColorSlow ("shiny","gold") x).Length > 0)
    |> Seq.map (fun x -> x.Type, x.Color)
    |> Seq.toList //4!

//-------SOLUTION part 1


let dictionary =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.toArray
    |> BuildDictionary 

let solution =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.take 2
    |> Seq.map (bagRegexGlobal.TryTypedMatch)
    |> Seq.choose id
    |> Seq.map (fun m -> m.BagType.Value, m.Color.Value)
    |> Seq.map (fun x -> Bag.Build dictionary x)
    |> Seq.choose id
    //|> Seq.length
    //to improve
    |> Seq.filter (fun x -> 
        (hasBagContainingColorSlow ("shiny","gold") x).Length > 0)
    |> Seq.map (fun x -> x.Type, x.Color)
    |> Seq.length //4! --> wrong

let parseTypes2 =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.map (fun line -> 
            line.Split([|' '|], StringSplitOptions.None).[0..1]
        )
    |> Set.ofSeq //dedup
    |> Set.count //594, a bit more variations :D 

File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
|> BuildDictionary
|> Seq.length //must be 594

    
let types2 = 
    parseTypes 
    |> Set.map (fun (x1::x2::[]) -> x1)
// ["bright"; "dark"; "dotted"; "faded"; "light"; "muted"; "shiny"; "vibrant"]

let color2 = 
    parseTypes 
    |> Set.map (fun (x1::x2::[]) -> x2)
//["black"; "blue"; "gold"; "olive"; "orange"; "plum"; "red"; "white"; "yellow"]

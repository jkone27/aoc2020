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
#r "nuget: FSharp.Collections.ParallelSeq"
open System.Globalization
open FSharp.Text.RegexProvider
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO
//open FSharp.Collections.ParallelSeq

[<Literal>]
let bagKeyRegex = "^(?<Bag>(?<BagType>\w+)\s(?<Color>\w+)\sbags?)\scontain\s(?<NoBags>no\sother)?(?<Bags>.+\,?)?\sbags?\.$"

[<Literal>]
let extraBagsRegex = "^\s?(?<Number>\d+)\s(?<BagTypeNth>\w+)\s(?<ColorNth>\w+)(\sbags?(\,|\.)?)?"

type BagRegex = FSharp.Text.RegexProvider.Regex<bagKeyRegex>

type ExtraBagRegex = FSharp.Text.RegexProvider.Regex<extraBagsRegex>

let bagRegexGlobal = BagRegex()
let extraBagRegexGlobal = ExtraBagRegex()

type Type = Type of string
type Color = Color of string

type BagType = { Type : Type; Color: Color }
with static member Build (bagType,bagColor) =
        { Type = Type(bagType); Color = Color(bagColor)}
//type BagType = string * string
type RulesDictionary = Map<BagType, (BagType * int) list>


let getDictionaryEntryFromLine singleLine : (BagType * (BagType * int) list) option =
    let mo = bagRegexGlobal.TryTypedMatch(singleLine)
    match mo with
    |None -> 
        None
    |Some(matchType) -> 
        match matchType.Success with
        |true -> 
            Some((BagType.Build(matchType.BagType.Value, matchType.Color.Value),
                if matchType.NoBags.Success ||  not matchType.Bags.Success then
                    []
                else
                    matchType.Bags.Value.Split(',') 
                    |> Seq.map (extraBagRegexGlobal.TypedMatch)
                    |> Seq.filter (fun x -> x.Success)
                    |> Seq.map(fun m ->
                        (BagType.Build(m.BagTypeNth.Value, m.ColorNth.Value), (int)m.Number.Value)
                    )
                    |> Seq.toList
            ))
        |false -> None

"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."  
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


// dictionary parsing from file
let BuildDictionary (inputSequence : string seq) : RulesDictionary =
    inputSequence 
    |> Seq.map getDictionaryEntryFromLine
    |> Seq.choose id
    |> Map.ofSeq //deduplicate dictionary

let testDictionary = 
    [
    //light red bags contain 1 bright white bag, 2 muted yellow bags.
        (("light","red"), 
            [
                (("bright","white"),1)
                (("muted","yellow"),2)
            ]
        )
    //dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        (("dark","orange"), 
            [
                (("bright","white"),3)
                (("muted","yellow"),4)
            ]
        )
    //bright white bags contain 1 shiny gold bag.
        (("bright","white"), 
            [
                (("shiny","gold"),1)
            ]
        )
    //muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        (("muted","yellow"), 
            [
                (("shiny","gold"),2)
                (("faded","blue"),9)
            ]
        )
    //shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        (("shiny","gold"), 
            [
                (("dark","olive"),1)
                (("vibrant","plum"),2)
            ]
        )
    //dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        (("dark","olive"), 
            [
                (("faded","blue"),3)
                (("dotted","black"),4)
            ]
        )
    //vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        (("vibrant","plum"), 
            [
                (("faded","blue"),5)
                (("dotted","black"),6)
            ]
        )
    //faded blue bags contain no other bags.
        (("faded","blue"), [ ]
        )
    //dotted black bags contain no other bags.
        (("dotted","black"), [ ]
        )       
    ]
    |> List.map(fun (k,v) ->
        BagType.Build k, 
            v |> List.map (fun (z,c) -> BagType.Build z, c)
        )
    |> Map.ofSeq

let testParsing =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.sample.txt")
    |> BuildDictionary
    |> Map.toSeq
    |> Set.ofSeq
    |> Set.difference (testDictionary|> Map.toSeq|> Set.ofSeq)
    |> Set.count //must be zero



type Bag =
    | LastBag of BagType
    | BagNode of BagType * ((Bag * int) seq)
with static member Build (rulesDictionary : RulesDictionary) bagType  =
        let rec BuildBag bt (rd: RulesDictionary) acc  =
            let isSome, buildSteps = rd.TryGetValue bt
            match isSome with
            |true -> 
                match buildSteps.Length > 0  with
                |true -> 
                    let innerBags = 
                        buildSteps
                        |> List.collect (fun (innerBagType, number) -> 
                        
                            let innerBag = BuildBag innerBagType rd None
                            match innerBag with
                            |Some(b) -> 
                                [(b,number)]
                            |None ->
                                []
                        )
    
                    match (innerBags |> Seq.length) > 0 with
                    |true -> 
                        Some(BagNode(bt, innerBags))
                    |false ->
                        Some(LastBag(bt))
                |false -> 
                    Some(LastBag(bt))
            |false ->
                acc
        BuildBag (BagType.Build(bagType)) rulesDictionary None

type Bag 
with member this.BagType =
        match this with
        |LastBag(bagType) -> bagType
        |BagNode(bagType,_) -> bagType


let rec getInnerBagTypes (bag: Bag) : BagType list  =
    let rec helper bag acc =
        match bag with
        |LastBag(_) ->
            acc
        |BagNode(_ , innerBags) ->
            innerBags 
            |> Seq.toList
            |> List.collect (fun (b,_) -> 
                helper b (b.BagType :: acc))
    helper bag []

////15ms
let hasInnerBag bag (bagType,bagColor) =
    getInnerBagTypes bag
    |> Set.ofList
    |> Set.contains(BagType.Build(bagType,bagColor))

//cata from fsharpforfunandprofit (bottom-up)
let rec cata fLeaf fNode bag =
    let recurse = cata fLeaf fNode  
    match bag with
    | LastBag leafInfo -> 
        fLeaf leafInfo 
    | BagNode (nodeInfo,subtrees) -> 
        fNode nodeInfo (
            subtrees 
            |> Seq.tryFind (fun (x,_) -> recurse x))  
        
//0ms
let hasInnerBagCata bag (bagType,bagColor) =
    match bag with
    |Bag.BagNode(_, innerBags) ->
        innerBags |> Seq.exists (fun (innerBag, _) -> 
            cata (fun l -> 
                l = BagType.Build(bagType,bagColor)) 
                 (fun l maybeFound -> 
                    l = BagType.Build(bagType,bagColor)
                    || maybeFound.IsSome) 
                innerBag
        )
    |Bag.LastBag(_) -> false    
    
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

//2ms
let sample =
    lines
    |> Seq.map (bagRegexGlobal.TryTypedMatch)
    |> Seq.choose id
    |> Seq.map (fun m -> m.BagType.Value, m.Color.Value)
    |> Seq.map (fun x -> Bag.Build (lines |> BuildDictionary) x)
    |> Seq.choose id
    |> Seq.filter (fun x ->  hasInnerBagCata x ("shiny","gold"))
    |> Seq.map (fun x -> x.BagType)
    |> Seq.length //4!

let vibrantPlumeDef = (lines |> BuildDictionary).[BagType.Build("vibrant","plum")]
//5 fadedblue, 6 dottedblack

let vpbag =
    (Bag.Build (lines |> BuildDictionary) 
    ("vibrant","plum")).Value

let vpchilds = getInnerBagTypes vpbag

//-------SOLUTION part 1

let linesSolution =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.toArray

//11ms
//posh crimson bags contain 4 drab plum bags, 5 dotted purple bags, 3 vibrant lavender bags, 2 striped plum bags."
let bigBag = 
    (Bag.Build (linesSolution |> BuildDictionary) ("posh","crimson")).Value


let nodes = getInnerBagTypes bigBag

//bigBagChilds 
//|> Seq.toList
//|> List.groupBy (fun x -> x.BagType)
//|> List.map (fun (x,y) -> x, y |> Seq.length )

let solution =
    
    //7ms
    linesSolution
    |> Seq.map (bagRegexGlobal.TryTypedMatch)
    |> Seq.choose id
    |> Seq.map (fun m -> m.BagType.Value, m.Color.Value)
    |> Seq.map (Bag.Build (linesSolution |> BuildDictionary))
    |> Seq.choose id
    |> Seq.groupBy (fun b -> b.BagType)
    |> Seq.map (fun (t,g) -> g |> Seq.head)
    |> Seq.filter (fun x ->  hasInnerBagCata x ("shiny","gold"))
    |> Seq.map (fun x -> x.BagType)
    |> Seq.length

//to Part 2
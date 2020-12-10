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

type BagType = { Type : Type; Color: Color; }
with static member Build (bagType,bagColor) =
        { Type = Type(bagType); Color = Color(bagColor); }
//type BagType = string * string
type RulesDictionary = Map<string * string, ((string * string) * int) list>


let getDictionaryEntryFromLine singleLine : ((string * string) * ((string * string) * int) list) option =
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
                        ((m.BagTypeNth.Value, m.ColorNth.Value), (int)m.Number.Value)
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
with static member Build (rulesDictionary : RulesDictionary) (bagType : string * string)  =
        let rec BuildBag bt (rd: RulesDictionary) acc  =
            let isSome, buildSteps = rd.TryGetValue bt
            match isSome with
            |true -> 
                let btt = BagType.Build bt
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
                        Some(BagNode(btt, innerBags))
                    |false ->
                        Some(LastBag(btt))
                |false -> 
                    Some(LastBag(btt))
            |false ->
                acc
        BuildBag bagType rulesDictionary None

type Bag 
with member this.BagType =
        match this with
        |LastBag(bagType) -> bagType
        |BagNode(bagType,_) -> bagType

//BUG, not working correctly..
let getAllInnerBags (bag: Bag) : BagType list  =
    let rec helper bag acc =
        match bag with
        |LastBag(_) ->
            acc
        |BagNode(_ , innerBags) ->
            let innerBags =
                innerBags 
                |> Seq.toList 
                |> List.collect (fun (b,number) ->
                    let innerBags = 
                        [1..number] 
                        |> List.map (fun _ -> b.BagType)

                    innerBags @ helper b innerBags
                )
            innerBags @ acc
    helper bag []

////15ms
let hasInnerBag bag (bagType,bagColor) =
    getAllInnerBags bag
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

let vibrantPlumeDef = (lines |> BuildDictionary).["vibrant","plum"]
//5 fadedblue, 6 dottedblack

let vpbag =
    (Bag.Build (lines |> BuildDictionary) 
    ("vibrant","plum")).Value

let vpchilds = 
    getAllInnerBags vpbag
    |> Set.ofList

//-------SOLUTION part 1

let linesSolution =
    File.ReadLines(__SOURCE_DIRECTORY__ + "/seven.input.txt")
    |> Seq.toArray

//11ms
//posh crimson bags contain 4 drab plum bags, 5 dotted purple bags, 3 vibrant lavender bags, 2 striped plum bags."
let bigBag = 
    (Bag.Build (linesSolution |> BuildDictionary) ("posh","crimson")).Value


let nodes = getAllInnerBags bigBag

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

(*

It's getting pretty expensive to fly these days - 
not because of ticket prices, 
but because of the ridiculous number of bags you need to buy!

Consider again your shiny gold bag and the rules from the above example:

faded blue bags contain 0 other bags.
dotted black bags contain 0 other bags.
vibrant plum bags contain 11 other bags: 
5 faded blue bags and 6 dotted black bags.
dark olive bags contain 7 other bags: 
3 faded blue bags and 4 dotted black bags.

So, a single shiny gold bag must contain 
1 dark olive bag (and the 7 bags within it) 
plus 2 vibrant plum bags (and the 11 bags within each of those): 
1 + 1*7 + 2 + 2*11 = 32 bags!

Of course, the actual rules have a small chance 
of going several levels deeper than this example; 
be sure to count all of the bags, 
even if the nesting becomes topologically impractical!

Here's another example:

shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.

In this example, a single shiny gold bag must contain 126 other bags.

How many individual bags are required inside your single shiny gold bag?

*)

        
//0ms
let countBags bag =
    let rec countInnerBags bb (acc : int) : int =
        match bb with
        |Bag.BagNode(_, innerBags) ->
            let innerTotal = 
                innerBags 
                |> Seq.sumBy(fun (b,n) -> 
                    n * countInnerBags b 1 //needs to be one as it's product neutral
                )
            innerTotal + acc
        |Bag.LastBag(_) ->  
            acc
        
    countInnerBags bag 0

let linesTest2 =
   """
   shiny gold bags contain 2 dark red bags.
   dark red bags contain 2 dark orange bags.
   dark orange bags contain 2 dark yellow bags.
   dark yellow bags contain 2 dark green bags.
   dark green bags contain 2 dark blue bags.
   dark blue bags contain 2 dark violet bags.
   dark violet bags contain no other bags.
   """
    .Split([|'\n'|])
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (not << String.IsNullOrWhiteSpace)
        |> Seq.toList


let shinyGold = 
    (Bag.Build (linesTest2 |> BuildDictionary) ("shiny","gold")).Value

let innerCount = 
    countBags shinyGold //126!


// solution day 2

let shinyGoldSol = 
    (Bag.Build (linesSolution |> BuildDictionary) ("shiny","gold")).Value
    
let sol2 =
    countBags shinyGoldSol //155802

(*

You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); 
start by counting all the trees you would encounter for the slope right 3, down 1:
From your starting position at the top-left, check the position that is right 3 and down 1. 
Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
*)


type TreeMap = ``E❄️`` | ``T🎄``

type ResultPath = ``O🎿`` | ``X🔥``

//bash paste file1 file1
//https://stackoverflow.com/questions/32122433/is-there-a-built-in-function-for-horizontal-string-concatenation
//could also do with bash on file and then "try" but is not "nice" :) 

let infiniteStringSeq stringFragment = 
    seq {
        while true do 
            for char in stringFragment do
                yield char
    }

// "xxx.yyy".Split('.')
//     |> Seq.map infiniteStringSeq
//     |> Seq.map (fun x -> (x |> Seq.take 20 |> Seq.toArray))
//     |> Seq.toList

let infiniteForest (inputSequence : string seq) =
    inputSequence
    |> Seq.map infiniteStringSeq
    |> Seq.map (fun x -> x |> Seq.take 10000)
    |> array2D
    |> Array2D.mapi (fun x y el -> 
        match el with
        |'.' -> x,y,``E❄️``
        |'#' -> x,y,``T🎄``
        |_ -> failwith "parsing error"
        )
    |> Seq.cast<int*int*TreeMap>


let ``Right 3, down 1.`` x y =
    x > 0 && y % 3 = 0 && x = y/3 

let findCrashesLength slopeFunction (inputSequence : string seq) =
    let forestHeight = inputSequence |> Seq.length
    infiniteForest inputSequence 
    |> Seq.map (fun (x,y,spot) ->
        if (x >= forestHeight) then
            None
        else
            if slopeFunction x y then
                match spot with
                |``T🎄`` -> Some(``X🔥``)
                |``E❄️`` -> Some(``O🎿``)
            else
                None
    )
    |> Seq.cast<ResultPath option>
    |> Seq.choose id
    |> Seq.filter (fun x -> x = ``X🔥``)
    |> Seq.length


let inputMap =
    """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""
    
let sampleLines = 
    inputMap.Split('\n')

let test1 =  
    sampleLines
    |> findCrashesLength  ``Right 3, down 1.`` 
        

System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/three.input.txt")
|> findCrashesLength ``Right 3, down 1.``

(*

--- Part Two ---
Time to check the rest of the slopes - 
you need to minimize the probability of a sudden arboreal stop,
after all.

Determine the number of trees you would encounter if, 
for each of the following slopes, 
you start at the top-left corner and traverse the map all the way 
to the bottom:

Right 1, down 1.
Right 3, down 1. (This is the slope you already checked.)
Right 5, down 1.
Right 7, down 1.
Right 1, down 2.
In the above example, these slopes would find 2, 7, 3, 4, 
and 2 tree(s) respectively; multiplied together, 
these produce the answer 336.

What do you get if you multiply together the number of trees encountered 
on each of the listed slopes?

*)

//fixed by F# community thanks Kris
let SlopeFunction rightX downY x y =
    if rightX >= downY then
        y % rightX = 0 && x = y/rightX  
    else
        x % downY = 0 && y = x/downY  

let test2 =  
    sampleLines
    |> findCrashesLength (SlopeFunction 3 1) //7

let rightDownInputArray =
    [(1,1);(3,1);(5,1);(7,1);(1,2)]

let totalNumber inputSequence = 
    rightDownInputArray
    |> Seq.fold (fun acc tup -> 
        let right,down = tup
        let crashes = 
            inputSequence 
            |> findCrashesLength (SlopeFunction right down)
            
        ((int64)crashes * acc)    
        ) ((int64)1)

let test3 = sampleLines |> totalNumber //336!

System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/three.input.txt")
|> totalNumber 
//3584591857L



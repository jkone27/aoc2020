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

let infiniteTree (inputSequence : string seq) =
    inputSequence
    |> Seq.map infiniteStringSeq
    |> Seq.map (fun x -> x |> Seq.take 5000)
    |> array2D
    |> Array2D.mapi (fun x y el -> 
        match el with
        |'.' -> x,y,``E❄️``
        |'#' -> x,y,``T🎄``
        |_ -> failwith "parsing error"
        )
    |> Seq.cast<int*int*TreeMap>

let findCrashesLength (inputSequence : string seq) =
    infiniteTree inputSequence 
    |> Seq.map (fun (x,y,spot) -> 
        if x > 0 && y % 3 = 0 && x = y/3 then
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

let test1 = findCrashesLength sampleLines

System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/three.input.txt")
|> findCrashesLength




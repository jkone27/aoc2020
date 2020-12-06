(*
You write a quick program to use your phone's camera to scan 
all of the nearby boarding passes (your puzzle input); 
perhaps you can find your seat through process of elimination.

Instead of zones or groups, this airline uses binary space partitioning to seat people. 
A seat might be specified like FBFBBFFRLR, 

where F means "front", 
B means "back", 
L means "left", 
and R means "right".

The first 7 characters will either be F or B; 
these specify exactly one of the 128 rows on the plane (numbered 0 through 127). 

Each letter tells you which half of a region the given seat is in. 
Start with the whole list of rows; 
the first letter indicates whether the seat is in the front (0 through 63) 
or the back (64 through 127). 

The next letter indicates which half of that region the seat is in, 
and so on until you're left with exactly one row.

For example, consider just the first seven characters of FBFBBFF-<RLR>:

Start by considering the whole range, rows 0 through 127.
F means to take the lower half, keeping rows 0 through 63.
B means to take the upper half, keeping rows 32 through 63.
F means to take the lower half, keeping rows 32 through 47.
B means to take the upper half, keeping rows 40 through 47.
B keeps rows 44 through 47.
F keeps rows 44 through 45.
The final F keeps the lower of the two, row 44.
The last three characters will be either L or R; 

these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). 
The same process as above proceeds again, this time with only three steps. 
L means to keep the lower half, while R means to keep the upper half.

For example, consider just the last 3 characters of <FBFBBFF>-RLR:

Start by considering the whole range, columns 0 through 7.
R means to take the upper half, keeping columns 4 through 7.
L means to take the lower half, keeping columns 4 through 5.
The final R keeps the upper of the two, column 5.
So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

Every seat also has a unique seat ID: 
multiply the row by 8, then add the column. 
In this example, the seat has ID 44 * 8 + 5 = 357.

-------------------------------

Here are some other boarding passes:

BFFFBBFRRR: row 70, column 7, seat ID 567.
FFFBBBFRRR: row 14, column 7, seat ID 119.
BBFFBBFRLL: row 102, column 4, seat ID 820.
----------------------------------------------
As a sanity check, look through your list of boarding passes. 
What is the highest seat ID on a boarding pass?

*)

#r "nuget: FSharp.Text.RegexProvider"
open System.Globalization
open FSharp.Text.RegexProvider
open System
open System.Text.RegularExpressions


let sampleSeat = "FBFBBFFRLR"

[<Literal>]
let parserString = "^(?<PlaneRow>(F|B){7})(?<PlaneColumn>(L|R){3})"

type ParserRegex = FSharp.Text.RegexProvider.Regex<parserString>

type PlaneColumn = PlaneColumn of int

type PlaneRow = PlaneRow of int * PlaneColumn[]

let plane = Array.init 128 (fun i -> 
    let planeColumn = Array.init 8 PlaneColumn
    PlaneRow(i, planeColumn))

let binarySelector (low,high) symbol (planeSection : 'c[]) : 'c[] =
    let sectionLength = planeSection.Length - 1
    if symbol = low then
        //lower-half
        planeSection.[0..sectionLength/2]
    else if symbol = high then
        //upper-half
        planeSection.[sectionLength/2+1..sectionLength]
    else  
        Array.Empty<'c>()

binarySelector ('F','B') 'F' plane.[0..1]

let seatStringParser inputSeatCode =
    match ParserRegex().TryTypedMatch(inputSeatCode) with
    |Some(rowInfo) ->
        rowInfo.PlaneRow.Value 
        |> Seq.fold (fun (planeSection : PlaneRow[]) symbol ->
                binarySelector ('F','B') symbol planeSection
            ) plane
        |> Seq.tryHead
        |> Option.bind (fun (PlaneRow(row, columns)) ->
                rowInfo.PlaneColumn.Value
                |> Seq.fold (fun (colSelection: PlaneColumn[]) symbol ->
                   binarySelector ('L','R') symbol colSelection
                ) columns
                |> Seq.tryHead
                |> Option.map (fun (PlaneColumn(column)) ->
                    //seat ID: multiply the row by 8, then add the column.
                    let seatId = row * 8 + column
                    (row,column,seatId)
                )
        )
    |None -> 
        None

//So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
sampleSeat |> seatStringParser

(*
BFFFBBFRRR: row 70, column 7, seat ID 567.
FFFBBBFRRR: row 14, column 7, seat ID 119.
BBFFBBFRLL: row 102, column 4, seat ID 820.
*)
[
"BFFFBBFRRR"
"FFFBBBFRRR"
"BBFFBBFRLL"
]
|> Seq.map seatStringParser
|> Seq.choose id
|> Seq.maxBy(fun (row,column,seatId) -> seatId) //820

//solution
System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/five.input.txt")
|> Seq.map seatStringParser
|> Seq.choose id
|> Seq.maxBy(fun (row,column,seatId) -> seatId) //topmost seat, row 106, 855

(*
--- Part Two ---
Ding! The "fasten seat belt" signs have turned on. Time to find your seat.

It's a completely full flight, 
so your seat should be the only missing boarding pass in your list. 
However, there's a catch: 
some of the seats at the very front and back of the plane 
don't exist on this aircraft, so they'll be missing from your list as well.

Your seat wasn't at the very front or back, though; 
the seats with IDs +1 and -1 from yours will be in your list.

What is the ID of your seat?

*)

let otherPassengerSeats =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/five.input.txt")
    |> Seq.map seatStringParser
    |> Seq.choose id
    |> Set.ofSeq

let backmostSeat =
    otherPassengerSeats
    |> Seq.minBy (fun (row, column, seatId) -> seatId) //(10, 1, 81)

let topmostSeat =
    otherPassengerSeats
    |> Seq.maxBy (fun (row, column, seatId) -> seatId) //(106, 7, 855)

// remove nonexisting seats
let fullPlane = 
    plane 
    |> Seq.collect (fun (PlaneRow(row, columns)) -> 
        columns
        |> Seq.map (fun (PlaneColumn(column)) ->
            (row, column,  row * 8 + column)
        )
    )
    |> Seq.skipWhile (fun (_,_,seatId) -> seatId < 81)
    |> Seq.takeWhile (fun (_,_,seatId) -> seatId <= 855)
    |> Set.ofSeq

let mySeat =
    Set.difference fullPlane otherPassengerSeats //552

//check +-1 exist
otherPassengerSeats |> Seq.find (fun (_,_,seatId) -> seatId = 552+1)

otherPassengerSeats |> Seq.find (fun (_,_,seatId) -> seatId = 552-1)
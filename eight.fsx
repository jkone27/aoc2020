(*
You narrow the problem down to a strange infinite loop in the boot code 
(your puzzle input) of the device. You should be able to fix it, 
but first you need to be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of text.
Each instruction consists of an operation (acc, jmp, or nop) 
and an argument (a signed number like +4 or -20).

acc increases or decreases a single global value called the accumulator 
by the value given in the argument. 
For example, acc +7 would increase the accumulator by 7. 
The accumulator starts at 0. 

After an acc instruction, the instruction immediately below it is executed next.
jmp jumps to a new instruction relative to itself. 
The next instruction to execute is found using the argument 
as an offset from the jmp instruction; 

for example, jmp +2 would skip the next instruction, 
jmp +1 would continue to the instruction immediately below it, 
and jmp -20 would cause the instruction 20 lines above to be executed next.

nop stands for No OPeration - it does nothing. 
The instruction immediately below it is executed next.
For example, consider the following program:
```````````````````````````
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
``````````
These instructions are visited in this order:
```````````````

nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |
``````````

This is an infinite loop: 
with this sequence of jumps, the program will run forever. 
The moment the program tries to run any instruction a second time, 
you know it will never terminate.

Immediately before the program would run an instruction a second time, 
the value in the accumulator is 5.

Run your copy of the boot code. 
Immediately before any instruction is executed a second time,
what value is in the accumulator?
*)
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open System.Diagnostics

type System.Text.RegularExpressions.Match 
with member this.Get (key:string) = 
    this.Groups.[key].Value

type Op = Nop = 0 | Jmp = 1 | Acc = 2
type Sign = ``+`` = 0 | ``-`` = 1
type Arg = Arg of int

type Input = { Operation: Op; Sign : Sign; Argument: Arg }
with static member Build (op,sign,arg) = 
        let isSuccess,opResult = Enum.TryParse<Op>(op, ignoreCase=true)
        match isSuccess with
        |true -> 
            let isSuccess,signResult = Enum.TryParse<Sign>(sign, ignoreCase=true)
            match isSuccess with
            |true -> 
                let isSuccess, argResult = Int32.TryParse arg
                match isSuccess with
                |true -> 
                    Some({ 
                            Operation = opResult 
                            Sign = signResult 
                            Argument = Arg(argResult)
                        })
                |false -> None
            |false -> None
        |false -> None

type Calculator = { 
    Ip: int; 
    State: int64; 
    CallStack: (int * Input) list; 
    Program: (int * Input) list }
with member this.Exec () : Calculator option =
        
        let lineNumber, input = this.Program.[this.Ip]

        let { Operation = op; Sign = sign; Argument = Arg(arg)} = input
    
        let newState = 
            match (op,sign) with
            |(Op.Acc, Sign.``+``) ->
                this.State + (arg |> int64)
            |(Op.Acc, Sign.``-``) ->
                this.State - (arg |> int64)
            |_ -> 
                this.State

        let newIp =
            match (op,sign) with
            |(Op.Jmp, Sign.``+``) ->
                this.Ip + arg
            |(Op.Jmp, Sign.``-``) ->
                this.Ip - arg
            |_ ->
                this.Ip + 1

        if newIp = this.Program.Length then
            None
        else
            { this with 
                State = newState
                CallStack = this.CallStack @ [lineNumber, input]
                Ip = newIp } |> Some

type Calculator with static member Load (program : Input list) =
        { 
            Ip = 0
            State = int64 0
            CallStack = []
            Program = program |> List.mapi (fun l i -> l,i)
        }

fsi.AddPrinter<Input>(fun dt -> 
        let (Arg(arg)) = dt.Argument
        $"{(dt.Operation, dt.Sign, arg)}"
    )

let inputSample = 
    """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6""".Split('\n')

let readProgram input =
    input
    |> Seq.map (fun x -> Regex.Match(x, 
        @"(?<Op>\w{3})\s(?<Sign>[\+\-]{1})(?<Digit>\d+)"))
    |> Seq.map (fun x -> (x.Get "Op", x.Get "Sign", x.Get "Digit"))
    |> Seq.map Input.Build
    |> Seq.choose id
    |> Seq.toList

let calculatorHasRepeatedProgram input =
    let mutable calculator = 
        readProgram input
        |> Calculator.Load
    try
        while(true) do
            let nextOp = calculator.Program.[calculator.Ip]
            let opAlreadyEx = 
                calculator.CallStack 
                |> List.contains nextOp

            if(opAlreadyEx) then
                failwith $"error: line repeat : {nextOp}"
            
            match calculator.Exec() with
            |Some(next) -> 
                calculator <- next
            |None ->
                failwith "program completed."

    with x ->
        printfn $"{x.Message}"
    calculator.State

calculatorHasRepeatedProgram inputSample //5

//sol 1
File.ReadLines (__SOURCE_DIRECTORY__ + "/eight.input.txt")
|> calculatorHasRepeatedProgram


//-------PART 2
(*
--- Part Two ---
After some careful analysis, 
you believe that exactly one instruction is corrupted.
Somewhere in the program, either a jmp is supposed to be a nop, 
or a nop is supposed to be a jmp. 

(No acc instructions were harmed in the corruption of this boot code.)
The program is supposed to terminate by attempting to execute 
an instruction immediately after the last instruction in the file. 

By changing exactly one jmp or nop, 
you can repair the boot code and make it terminate correctly.

For example, consider the same program from above:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4 <<<<<<
acc +6

If you change the first instruction from nop +0 to jmp +0, 
it would create a single-instruction infinite loop, 
never leaving that instruction. 

If you change almost any of the jmp instructions, 
the program will still eventually find another jmp instruction and loop forever.

However, if you change the second-to-last instruction (from jmp -4 to nop -4), 
the program terminates! The instructions are visited in this order:

nop +0  | 1
acc +1  | 2
jmp +4  | 3
acc +3  |
jmp -3  |
acc -99 |
acc +1  | 4
nop -4  | 5 <<<<<<
acc +6  | 6

After the last instruction (acc +6), 
the program terminates by attempting to run the instruction 
below the last instruction in the file. 
With this change, after the program terminates, 
the accumulator contains the value 8 (acc +1, acc +1, acc +6).

Fix the program so that it terminates normally 
by changing exactly one jmp (to nop) or nop (to jmp). 
What is the value of the accumulator after the program terminates?
*)


let rec traverseCalc (calculator: Calculator) =
    let nextOp = calculator.Program.[calculator.Ip]
    let opAlreadyEx = 
        calculator.CallStack 
        |> List.contains nextOp
        
    if opAlreadyEx then
        let faultyStack =
            calculator.CallStack |> List.filter (fun (ln,i) -> 
            [Op.Nop;Op.Jmp] |> Seq.contains i.Operation)
        (None, faultyStack)
    else
        match calculator.Exec() with
        |Some(c) -> traverseCalc c
        |None -> 
            Some(calculator),[]


let ``some result at some point?`` =
    let lines =
        File.ReadLines (__SOURCE_DIRECTORY__ + "/eight.input.txt")
        |> Seq.toArray
    
    let _, faultyCallStack = 
        lines
        |> readProgram
        |> Calculator.Load
        |> traverseCalc

    for (l,f) in faultyCallStack do
        let newLines = 
            lines 
            |> readProgram
            |> List.mapi (fun i x -> i,x)
            |> dict
            |> Dictionary<_,_>

        let newOp =
            match f.Operation with
            |Op.Jmp -> Op.Nop
            |Op.Nop -> Op.Jmp
            |_ -> f.Operation

        newLines.[l] <- { f with Operation = newOp }
            
        let calcOption, faulty = 
            newLines
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.toList
            |> Calculator.Load
            |> traverseCalc

        match calcOption, faulty with
        |Some(c), [] -> 
            printfn $"replaced line: {l}, {newLines.[l]}, result: {c.State}"
        |_, _ -> 
            ()


//verify sol : 2204, too high

let lines =
    File.ReadLines (__SOURCE_DIRECTORY__ + "/eight.input.sol.txt")
    |> Seq.toArray

let sol, faultyCallStack = 
    lines
    |> readProgram
    |> Calculator.Load
    |> traverseCalc

sol.Value.State
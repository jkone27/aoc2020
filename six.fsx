(*

The form asks a series of 26 yes-or-no questions marked a through z. 
All you need to do is identify the questions for which anyone in your group answers "yes". 
Since your group is just you, this doesn't take very long.

However, the person sitting next to you seems to be experiencing a language barrier 
and asks if you can help. For each of the people in their group, 
you write down the questions for which they answer "yes", one per line. 
For example:
````````````
abcx
abcy
abcz
````````````
In this group, there are 6 questions to which anyone answered "yes"
: a, b, c, x, y, and z. (Duplicate answers to the same question don't count extra; 
each question counts at most once.)

Another group asks for your help, then another, 
and eventually you've collected answers from every group on the plane (your puzzle input). 
Each group's answers are separated by a blank line, and within each group, 
each person's answers are on a single line. For example:

````
abc

a
b
c

ab
ac

a
a
a
a

b
````

This list represents answers from five groups:

The first group contains one person who answered "yes" to 3 questions: a, b, and c.
The second group contains three people; 
combined, they answered "yes" to 3 questions: a, b, and c.
The third group contains two people; 
combined, they answered "yes" to 3 questions: a, b, and c.
The fourth group contains four people; 
combined, they answered "yes" to only 1 question, a.
The last group contains one person who answered "yes" to only 1 question, b.

In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.

For each group, 
count the number of questions to which anyone answered "yes". 
What is the sum of those counts?

*)

open System
open System.IO

//a, b, c, x, y, and z.

type Answer = A = 0 | B = 1 | C = 2 | X = 3 | Y = 4 | Z = 5 
type PersonAnswers = { Id: int; Answers: Answer Set } //abc
type Group = { GroupId: int; GroupAnswers : PersonAnswers Set } //a ab ca
// 26 yes-or-no  questions
//let fullGroup = Array.init 


let groups (inputData : string) =
   inputData
    .Split([|Environment.NewLine + Environment.NewLine|], StringSplitOptions.None)
    |> Seq.mapi (fun g x -> 
            let groupAnswers = 
                x.Split([|Environment.NewLine|], StringSplitOptions.None)
                |> Seq.mapi (fun i (answersRaw : string) -> 
                    let answers = 
                        answersRaw
                        |> Seq.map (fun c -> 
                            //might need tryparse later
                            Enum.Parse(typeof<Answer>, new String([|c|]), true) :?> Answer
                        )
                        |> Set.ofSeq

                    { Id = i; Answers = answers}
                )
                |> Set.ofSeq
            { GroupId = g; GroupAnswers = groupAnswers}
        )
    |> Set.ofSeq
    

let TotalSharedAnswersInGroups inputData = //15
    [Answer.A ; Answer.B; Answer.C; Answer.X; Answer.Y; Answer.Z]
    |> Seq.sumBy (fun answer -> 
        inputData
        |> groups 
        |> Seq.sumBy (fun g ->
            g.GroupAnswers |>
            Seq.sumBy (fun a ->
                if a.Answers |> Seq.contains answer then
                    1
                else
                    0
            )
        )
    )

let test =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.sample.txt")
    |> TotalSharedAnswersInGroups //15 but should be 11

       
// sol part 1
let inputData1 =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.input.txt")

    
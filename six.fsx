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
open System.Text.RegularExpressions

//a, b, c, x, y, and z.

type Answer = Answer of char
with static member Parse inputChar =
        match Regex.IsMatch(new String([|inputChar|]), "^\w{1}$") with
        |true -> Some(Answer(inputChar))
        |false -> None

type Person = { Id: int; Answers: Answer Set } //abc
type Group = { GroupId: int; Persons : Person Set } //a ab ca
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
                        |> Seq.map Answer.Parse
                        |> Seq.choose id
                        |> Set.ofSeq

                    { Id = i; Answers = answers}
                )
                |> Set.ofSeq
            { GroupId = g; Persons = groupAnswers}
        )
    |> Set.ofSeq

let groupAnswersAnyoneYes group = 
    group.Persons 
    |> Seq.collect (fun a -> 
        a.Answers
    )
    |> Set.ofSeq //remove dups
    
let totalAnswersPerGroup selectionFunction groupSet = 
    groupSet
    |> Seq.map (fun g ->
        (g.GroupId, g |> selectionFunction)
    )
    |> Set.ofSeq

let test =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.sample.txt")
    |> groups
    |> totalAnswersPerGroup groupAnswersAnyoneYes
    |> Seq.sumBy (fun (_, answers) -> answers.Count) //11

  
// sol part 1
let inputData1 =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.input.txt")
    |> groups
    |> totalAnswersPerGroup groupAnswersAnyoneYes
    |> Seq.sumBy (fun (_, answers) -> answers.Count) //6633


(*

--- Part Two ---
As you finish the last group's customs declaration, 
you notice that you misread one word in the instructions:

You don't need to identify the questions to which anyone answered "yes"; 
you need to identify the questions to which >>>everyone<<<< answered "yes"!

Using the same example as above:

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
This list represents answers from five groups:

In the first group, everyone (all 1 person) answered "yes" to 3 questions: a, b, and c.
In the second group, there is no question to which everyone answered "yes".
In the third group, everyone answered yes to only 1 question, a. 
Since some people did not answer "yes" to b or c, they don't count.
In the fourth group, everyone answered yes to only 1 question, a.
In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.
In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.

For each group, count the number of questions to which everyone answered "yes". 
What is the sum of those counts?

*)

let groupAnswersEveryoneYes group = 
    let allAnswersSet = 
        ['a'..'z'] 
        |> Seq.map Answer.Parse
        |> Seq.choose id
        |> Set.ofSeq
    group.Persons 
    |> Seq.fold (fun acc person -> 
        person.Answers |> Set.intersect acc //intersect while folding
    ) allAnswersSet
    |> Set.ofSeq //remove dups

let test2 =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.sample.txt")
    |> groups
    |> totalAnswersPerGroup groupAnswersEveryoneYes
    |> Seq.sumBy (fun (_, answers) -> answers.Count) //6!


// sol part 2
let inputData2 =
  System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/six.input.txt")
  |> groups
  |> totalAnswersPerGroup groupAnswersEveryoneYes
  |> Seq.sumBy (fun (_, answers) -> answers.Count) //3202
    
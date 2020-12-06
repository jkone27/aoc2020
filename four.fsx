(*
The automatic passport scanners are slow because 
they're having trouble detecting which passports have all required fields. 
The expected fields are as follows:

byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
Passport data is validated in batch files (your puzzle input). 
Each passport is represented as a sequence of key:value pairs 
separated by spaces or newlines. 
Passports are separated by blank lines.

Here is an example batch file containing four passports:
-------------------------------------------------
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
--------------------------------------------------------
The first passport is valid - all eight fields are present. 
The second passport is invalid - it is missing hgt (the Height field).

The third passport is interesting; 
the only missing field is cid, 
so it looks like data from North Pole Credentials, 
not a passport at all! Surely, 
nobody would mind if you made the system temporarily ignore missing cid fields. 
Treat this "passport" as valid.

The fourth passport is missing two fields, 
cid and byr. 
Missing cid is fine, but missing any other field is not, 
so this passport is invalid.

According to the above rules, 
your improved system would report 2 valid passports.

Count the number of valid passports - 
those that have all required fields. 
Treat cid as optional. In your batch file, how many passports are valid?

*)


#r "nuget: FSharp.Text.RegexProvider"
open System.Globalization
open FSharp.Text.RegexProvider
open System
open System.Text.RegularExpressions



type Color = Col of string

type Height =
    |Centimiters of int
    |Inches of int
    |Unknown of string
    with static member Parse (inputString : string) : Height option =
            if (String.IsNullOrWhiteSpace(inputString)) then
                None
            else
                try
                    let unit = inputString.[(inputString.Length-2)..]
                    let value = (int) (inputString.Replace(unit,""))
                    match unit with
                    |"cm" -> Some(Centimiters(value))
                    |"in" -> Some(Inches(value))
                with ex ->
                    Some(Unknown(inputString))




type PasswordId = Pid of string

type CountryId = Cid of string

type Year = Yr of int

[<Literal>]
let regexString = @"(?<PKey>\w{3,3})\:(?<PValue>[^\s]+)"

type PassportRegex = FSharp.Text.RegexProvider.Regex<regexString>

let test = 
    """hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm
"""

let rec getMatches multilineString =
    let rec getMatches (matchType : PassportRegex.MatchType) acc =
        match matchType.Success with
        |true -> 
            let current = matchType.PKey.Value, matchType.PValue.Value
            getMatches (matchType.TypedNextMatch()) (current::acc) 
        |false ->
            acc
    let mo = PassportRegex().TryTypedMatch(multilineString)
    let r = 
        match mo with
        |None -> 
            []
        |Some(m) -> getMatches m []
    r |> dict

let dictResult = getMatches test;; //7 ok



let tryGetFromDict (dict : System.Collections.Generic.IDictionary<_,_>) key =
   let found, res = dict.TryGetValue(key)
   match found with
   |true -> key, Some(res)
   |false -> key, None

type Passport = {
    Pid : PasswordId
    Cid : CountryId option
    Byr: Year
    Iyr: Year
    Eyr: Year
    Hgt: Height
    Hcl : Color
    Ecl : Color
}
with static member Parse multilineString :  Result<Passport,string> =
        let matchesDictionary = getMatches multilineString
   
        let validationResults =
            ["pid";"cid"; "ecl"; "hcl"; "byr"; "iyr"; "eyr"; "hgt"]
            |> Seq.map (fun key -> tryGetFromDict matchesDictionary key )
            |> Seq.toArray

        let failureResults =
            validationResults 
            |> Array.filter (fun (_, opt) -> opt.IsNone)
            |> Array.filter (fun (key,_) -> key <> "cid")

        if (failureResults.Length > 0) then
            let missingKeys = 
                failureResults 
                |> Array.map (fun (key,_) -> key)

            let error = String.Join(",", missingKeys)

            Error(error)
        else
        
            let pid = PasswordId.Pid(matchesDictionary.["pid"])
            let found, cids = matchesDictionary.TryGetValue("cid")

            let ecl = Color.Col(matchesDictionary.["ecl"])
            let hcl = Color.Col(matchesDictionary.["hcl"])
            let byr = Year.Yr((int)matchesDictionary.["byr"])
            let iyr = Year.Yr((int)matchesDictionary.["iyr"])
            let eyr = Year.Yr((int)matchesDictionary.["eyr"])

            let cidOption = 
                match found with
                |true -> Some(CountryId.Cid(cids))
                |false -> None

             
            match Height.Parse(matchesDictionary.["hgt"]) with
            |Some(h) -> 
                let record : Passport = {   
                    Pid = pid
                    Cid = cidOption
                    Ecl = ecl
                    Byr = byr
                    Eyr = eyr
                    Iyr = iyr
                    Hgt = h
                    Hcl = hcl
                }

                Ok(record)
            |None -> 
                let errorMsg = "error parsing height " + matchesDictionary.["hgt"]
                Error(errorMsg)



System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/four.sample.txt") //v,i,v,i
    .Split([|
        Environment.NewLine + Environment.NewLine |], 
        StringSplitOptions.RemoveEmptyEntries)
    |> Seq.mapi (fun i x -> i, Passport.Parse x)
    |> Seq.map (fun (i,x) -> 
        match x with
        |Ok(passport) -> Some(passport)
        |Error(k) -> None
    )
    |> Seq.choose id
    |> Seq.length //2

System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/four.input.txt") //??
    .Split([|
        Environment.NewLine + Environment.NewLine |], 
        StringSplitOptions.RemoveEmptyEntries)
    |> Seq.mapi (fun i x -> i, Passport.Parse x)
    |> Seq.map (fun (i,x) -> 
        match x with
        |Ok(passport) -> Some(passport)
        |Error(k) -> None
    )
    |> Seq.choose id
    //|> Seq.filter (fun x -> 
    //    match x.Hgt with
    //    |Unknown(z) -> true
    //    |_ -> false)
    |> Seq.length //210


(*
PART 2

ut each other field has strict rules about what values are valid 
for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
Your job is to count the passports where all required fields 
are both present and valid according to the above rules.

requested output example
------------------------------
byr valid:   2002
byr invalid: 2003

hgt valid:   60in
hgt valid:   190cm
hgt invalid: 190in
hgt invalid: 190

hcl valid:   #123abc
hcl invalid: #123abz
hcl invalid: 123abc

ecl valid:   brn
ecl invalid: wat

pid valid:   000000001
pid invalid: 0123456789
*)
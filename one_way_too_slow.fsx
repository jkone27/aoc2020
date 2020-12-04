//Find the two entries that sum to 2020; what do you get if you multiply them together?

let input = 
    [
    1
    2
    3
    ]   

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let permutedInput = permute input


let ifSumIsExpected couple expectedSum =
    let (fst,snd) = couple
    if fst + snd = expectedSum then
        Some(fst, snd, fst * snd)
    else
        None    

let test1 = ifSumIsExpected (1,2) 3

let chunkInputInCouples sequence = 
    let rec helperChunckInput s acc =
        match s with
        |x1::x2::xs -> helperChunckInput xs [Some(x1,x2)] @ acc
        |_ -> acc
    helperChunckInput sequence []

let test2 = chunkInputInCouples input;;

let checkIfSumIsExpectedForList sequence expectedSum  =
    sequence
    |> chunkInputInCouples
    |> Seq.choose id
    |> Seq.map (fun x -> ifSumIsExpected x expectedSum)
    |> Seq.choose id


let test3 = checkIfSumIsExpectedForList [1;2;1;2;4;5] 3


let findTwoEntries list expectedSum =
   permute list 
   |> Seq.map (fun x -> checkIfSumIsExpectedForList x expectedSum )
   |> Seq.filter (not << Seq.isEmpty)
   |> Seq.head
   |> Seq.tryHead
                 

let test = findTwoEntries input 3

let sample = 
    [
        1721
        979
        366
        299
        675
        1456
    ]

let finalTest = findTwoEntries sample 2020

//solution... too slow!

let s =
    [
        1899
        1358
        1824
        1541
        1801
        1950
        1441
        1848
        1259
        1715
        1222
        1097
        1127
        1531
        1330
        1841
        1662
        1075
        1631
        1844
        1209
        1940
        2006
        1085
        1615
        1132
        1345
        1736
        1885
        1919
        1995
        1893
        1464
        1701
        1373
        1390
        1717
        1238
        1707
        1088
        1566
        1971
        1804
        1630
        1920
        1445
        1948
        1123
        1917
        1944
        1448
        1965
        1118
        1986
        1498
        1847
        1730
        1418
        1771
        1352
        1207
        1276
        1716
        1502
        1922
        1473
        1528
        1038
        1228
        1983
        1746
        1695
        1698
        1910
        1283
        1145
        1967
        1304
        1651
        1898
        1450
        1042
        1051
        1619
        1505
        1643
        1136
        1517
        1907
        1354
        1960
        1733
        1769
        1942
        43
        1743
        1981
        1711
        1141
        1169
        1239
        1032
        1148
        1045
        1768
        1173
        1389
        2007
        1614
        1028
        1729
        1083
        1165
        1461
        1850
        1913
        1958
        1121
        1613
        1341
        1055
        1510
        1054
        1064
        1875
        1429
        1799
        1570
        1293
        1702
        1313
        1576
        1050
        1340
        1117
        1342
        1914
        1773
        1281
        1176
        1371
        1520
        1131
        1438
        1741
        1583
        1840
        1412
        1792
        1656
        1628
        1089
        1124
        1291
        1476
        1367
        1467
        1529
        1925
        1555
        1518
        1496
        1745
        1533
        1557
        1861
        1091
        1364
        1894
        1760
        1622
        1149
        1776
        1547
        1329
        1988
        1697
        989
        1604
        1296
        319
        1459
        1860
        1916
        1838
        527
        1370
        1881
        1213
        2003
        1223
        1095
        1560
        1784
        1157
        1573
        1023
        1758
        1857
        1723
    ]
    |> findTwoEntries 
    <| 2020
module Tests

open System
open FSharp.Foldl
open FSharp.Foldl.Operators
open Xunit
open Hedgehog
open FsUnit.Xunit
open Hedgehog.Xunit

let inline flip f a b = f b a

let (===) a b = a |> should equal b

[<Property>]
let ``Fold sum`` (xs: int list) =
    let xs = xs |> List.map int64
    Fold.fold Fold.sum xs === List.sum xs


[<Fact>]
let ``Compose folds into average`` () =
    let average = (/) <!> Fold.sum <*> Fold.length
    Fold.fold average [ 1..100 ] === 50

[<Property>]
let ``Composed and built-in averages should match`` (x: int, xs : int list) =
    let xs = x :: xs
    let average = (/) <!> Fold.sum <*> Fold.length
    Fold.fold average xs === Fold.fold Fold.average xs

[<Fact>]
let ``Taking the sum, the sum of squares, ..., upto the sum of x^5`` () =
    [ for n in 1.0 .. 5.0 -> Fold.premap (fun x -> x ** n) Fold.sum ]
    |> Fold.sequence
    |> flip Fold.fold [ 1.0 .. 10.0 ]
    |> should equal [ 55.0; 385; 3025; 25333; 220825 ]


[<Fact>]
let ``Take last N elements``() =
    property {
        let! n = Gen.int32 (Range.linear 0 20)
        let! size = Gen.int32 (Range.linear 0 512)
        let! xs = Gen.list (Range.singleton size) (Gen.int32 (Range.linearBounded()))

        Fold.fold (Fold.lastN n) xs === List.skip (max (size - n) 0) xs
    } |> Property.check

[<Property>]
let ``Prefilter filters inputs`` (xs : int list) =
    let p x = x > 5
    Fold.fold (Fold.prefilter p Fold.sum) xs === Fold.fold Fold.sum (List.filter p xs)

[<Property>]
let ``PredropWhile drops until predicate`` (xs : int list) =
    let p x = x > 5
    Fold.fold (Fold.predropWhile p Fold.sum) xs === Fold.fold Fold.sum (List.skipWhile p xs)

[<Fact>]
let ``Drop drops n values`` () =
    property {
        let! n = Gen.int32 (Range.linear 0 20)
        let! xs = Gen.list (Range.linear 0 100) (Gen.int32 (Range.linearBounded()))
        Fold.fold (Fold.drop n Fold.sum) xs === Fold.fold Fold.sum (List.skip (min n xs.Length) xs)
    } |> Property.check

[<Property>]
let ``Max value`` (x: int, xs : int list) =
    let xs = x :: xs
    Fold.fold Fold.maximum xs === Some (List.max xs)

[<Property>]
let ``Min value`` (x: int, xs : int list) =
    let xs = x :: xs
    Fold.fold Fold.minimum xs === Some (List.min xs)

[<Property>]
let ``MaxBy value`` (x: int * string, xs : (int * string) list) =
    let xs = x :: xs
    Fold.fold (Fold.maximumBy fst) xs === Some (List.maxBy fst xs)

[<Property>]
let ``MinBy value`` (x: int * string, xs : (int * string) list) =
    let xs = x :: xs
    Fold.fold (Fold.minimumBy fst) xs === Some (List.minBy fst xs)

[<Fact>]
let ``Sqrt value`` () =
    property {
        let! xs = Gen.double (Range.linear 0 Double.MaxValue) |> Gen.list (Range.linear 0 100)
        Fold.fold (Fold.sqrt Fold.sum) xs === sqrt(List.sum xs)
    } |> Property.check

[<Property>]
let ``SumAndLength`` (x: int, xs : int list) =
    let mySum = Fold.create 0 (+) id

    let step (x, y) n = x + n, y + 1
    let sumAndLen =
        Seq.fold step (0, 0) xs

    Fold.fold (Fold.zip mySum Fold.length) xs === sumAndLen

[<Property>]
let ``CE features`` (xs : int list) =
    let xs = xs |> List.map int64

    let resultFold =
        fold {
            let! x = Fold.sum
            and! y = Fold.length
            return (x, y)
        }

    Fold.fold resultFold xs === (List.sum xs, List.length xs)

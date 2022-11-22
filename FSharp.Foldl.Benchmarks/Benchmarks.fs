module FSharp.Foldl.Benchmarks

open BenchmarkDotNet.Attributes
type Pair<'a, 'b> = Pair of 'a * 'b
let sum a b = a + b
type Foo<'a> = Foo of 'a * ('a -> 'a -> 'a)

type Benchmarks() =
    let collection : double list = [ 1..1000 ]

    [<Benchmark(Description = "Plain handcrafted averages", Baseline = true)>]
    member this.ManualAverage() =
        let step (x, y) n = x + n, y + 1
        collection |> Seq.fold step (0.0, 0)

    [<Benchmark(Description = "Fold average")>]
    member this.FoldAverage() = Fold.fold Fold.average collection

    [<Benchmark(Description = "Composed 'sum / length'")>]
    member this.ComposedAverage() =
        let avg =
            fold {
                let! sum = Fold.sum
                and! len = Fold.length
                return double sum / double len
            }

        Fold.fold avg collection

    [<Benchmark(Description = "zip2")>]
    member this.Zip2() =
        collection |> Fold.fold (Fold.zip Fold.sum Fold.sum)

    [<Benchmark(Description = "zip3")>]
    member this.Zip3() =
        collection |> Fold.fold (Fold.zip3 Fold.sum Fold.sum Fold.sum)

    [<Benchmark(Description = "zip4")>]
    member this.Zip4() =
        collection |> Fold.fold (Fold.zip4 Fold.sum Fold.sum Fold.sum Fold.sum)

    [<Benchmark(Description = "Manual zip4")>]
    member this.ManualZip4() =
        let z : double = 0
        let Foo(a, _), Foo(b, _), Foo(c, _), Foo(d, _) =
            collection
            |> Seq.fold
                (fun (Foo (a1, f1), Foo (a2, f2), Foo (a3, f3), Foo (a4, f4)) a ->
                    Foo(f1 a1 a, f1), Foo(f2 a2 a, f2), Foo(f3 a3 a, f3), Foo(f4 a4 a, f4))

                (Foo(z, sum), Foo(z, sum), Foo(z, sum), Foo(z, sum))
        a, b, c, d

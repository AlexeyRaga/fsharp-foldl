module FSharp.Foldl.Benchmarks

open BenchmarkDotNet.Attributes

type Pair<'a, 'b> = Pair of 'a * 'b

type Benchmarks() =
    let collection : double list = [ 1..1000 ]

    [<Benchmark(Description = "Plain handcrafted averages", Baseline = true)>]
    member this.ManualAverage() =
        let step (x, y) n = x + n, y + 1
        collection
        |> Seq.fold step (0.0, 0)

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
    member this.Zip4 () =
        collection |> Fold.fold (Fold.zip4 Fold.sum Fold.sum Fold.sum Fold.sum)

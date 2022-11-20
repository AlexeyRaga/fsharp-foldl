# FSharp.Foldl

An F# port of a wonderful [foldl](https://github.com/Gabriella439/foldl)
by [Gabriella Gonzalez](https://github.com/Gabriella439).

Use this foldl library when you want to compute multiple folds over a collection
in one pass over the data without space leaks.

For example, suppose that you want to simultaneously compute the sum of the list
and the length of the list:

```fsharp
let sumAndLength (xs : int seq) : (int, int) =
    (Seq.sum xs, Seq.length xs)
```

However, this solution is suboptimal because it goes over the sequence in two passes.
It may not even be possible (or correct) when the sequence is "hot".

Usually people work around this by hand-writing a strict left fold that looks
something like this:

```fsharp
let sumAndLength (xs : int seq) : (int, int) =
    let step (x, y) n = x + n, y + 1
    xs |> Seq.fold step (0, 0)
```

That now goes over the sequence in one pass, however, this is not satisfactory
because you have to reimplement the guts of every fold that you care about.

What if you just stored the step function and accumulator for each individual fold
and let some high-level library do the combining for you?
That's exactly what this library does! Using this library you can instead write:

```fsharp
open FSharp.Foldl

let sumAndLength (xs : int seq) : (int, int) =
    Fold.fold (Fold.tuple Fold.sum Fold.length) xs

```

To see how this works, the `Fold.sum` value is just a datatype storing
the step function and the starting state (and a final extraction function):

```fsharp
let sum = Fold.create 0 (+) id
```

Same thing for the Fold.length value:

```fsharp
let length = Fold.create 0 (fun n _ -> n + 1) id
```

... and `Fold.tuple` combines them into a new datatype storing
the composite step function and starting state:

```fsharp
// Fold<int, int * int>
let sumAndLengthFold = Fold.zip sum length

let result = Fold.fold delta [1.0 .. 10] // (55.0, 10)
```

Alternatively the Applicative syntax can be used to compose folds:

```fsharp
open FSharp.Foldl.Operators

let tuple a b = (a, b)

let sumAndLengthFold = tuple <!> sum <*> length

let result = Fold.fold sumAndLengthFold [1.0 .. 10] // (55.0, 10)
```

Or a computation expression:

```fsharp
let sumAndLengthFold =
    fold {
        let! x = sum
        and! y = length
        return (x, y)
    }

let result = Fold.fold sumAndLengthFold [1.0 .. 10] // (55.0, 10)
```

namespace rec FSharp.Foldl

open System.Numerics

type Folder<'value, 'result, 'x> =
    abstract RunStep : ('state -> 'value -> 'state) -> 'state -> ('state -> 'result) -> 'x

type Fold<'value, 'result> =
    abstract RunFolder : Folder<'value, 'result, 'x> -> 'x

module private ValueTuple =
    let inline toTuple (struct (a, b)) = (a, b)
    let inline toTuple3 (struct (a, b, c)) = (a, b, c)
    let inline toTuple4 (struct (a, b, c, d)) = (a, b, c, d)
    let inline fst (struct (a, _)) = a
    let inline snd (struct (_, b)) = b
    let inline map f (struct (a, b)) = struct (a, f b)
    let inline map2 f g (struct (a, b)) = struct (f a, g b)
    let inline map3 f g h (struct (a, b, c)) = struct (f a, g b, h c)
    let inline map4 f g h k (struct (a, b, c, d)) = struct (f a, g b, h c, k d)
    let inline toTuple' f g (struct (a, b)) = (f a, g b)
    let inline toTuple3' f g h (struct (a, b, c)) = (f a, g b, h c)
    let inline toTuple4' f g h t (struct (a, b, c, d)) = (f a, g b, h c, t d)

module Fold =
    let inline private unstruct2 (struct (a, b, c)) = (a, b, c)

    let inline create (current : 'state) (step : 'state -> 'value -> 'state) (extract : 'state -> 'result) : Fold<'value, 'result> =
        { new Fold<_, _> with
            member this.RunFolder cont = cont.RunStep step current extract }

    let inline retn x =
        create () (fun () _ -> ()) (fun () -> x)

    let fold (fold : Fold<'a, 'b>) (sequence : seq<'a>) : 'b =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    extract (Seq.fold step current sequence) }

    /// Functorial map (transform results)
    let map (f : 'b -> 'c) (fold : Fold<'a, 'b>) : Fold<'a, 'c> =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract = create current step (extract >> f) }

    /// Cofunctorial map (adapt inputs)
    let premap (f : 'c -> 'a) (fold : Fold<'a, 'b>) : Fold<'c, 'b> =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    create current (fun s a -> step s (f a)) extract }

    /// Profunctorial dimap (both premap and map at once)
    let dimap (f : 'b -> 'a) (g : 'c -> 'd) (fold : Fold<'a, 'c>) : Fold<'b, 'd> =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    create current (fun s a -> step s (f a)) (extract >> g) }

    /// Returns a new 'Fold' where the folder's input is used
    /// only when the input satisfies a predicate f
    let prefilter (f : 'a -> bool) (fold : Fold<'a, 'b>) : Fold<'a, 'b> =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    create current (fun s a -> if f a then step s a else s) extract }

    /// Transforms a 'Fold' into one which ignores elements
    /// until they stop satisfying a predicate
    let predropWhile (f : 'a -> bool) (fold : Fold<'a, 'b>) =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    let step' (struct (isDropping : bool, s)) a =
                        if isDropping && f a then
                            (struct (true, s))
                        else
                            struct (false, step s a)

                    create (struct (true, current)) step' (ValueTuple.snd >> extract) }

    /// returns a new 'FoldM' that ignores the first @n@ inputs but
    /// otherwise behaves the same as the original fold
    let drop n (fold : Fold<'a, 'b>) : Fold<'a, 'b> =
        fold.RunFolder
            { new Folder<_, _, _> with
                member this.RunStep step current extract =
                    let step' (struct (n', s)) a =
                        if n' > 0 then struct (n' - 1, s) else struct (0, step s a)

                    create (struct (n, current)) step' (ValueTuple.snd >> extract) }

    /// Computes the sum of all elements
    let sum<'a when 'a :> IAdditionOperators<'a, 'a, 'a> and 'a :> IAdditiveIdentity<'a, 'a>> : Fold<'a, 'a> =
        create 'a.AdditiveIdentity (+) id

    /// Computes the products of all elements
    let product<'a when 'a :> IMultiplyOperators<'a, 'a, 'a> and 'a :> IMultiplicativeIdentity<'a, 'a>> : Fold<'a, 'a> =
        create 'a.MultiplicativeIdentity (*) id

    /// Computes the length of the sequence
    let length<'a> = create 0 (fun x (_ : 'a) -> x + 1) id

    let average<'a
        when 'a :> IAdditionOperators<'a, 'a, 'a>
        and 'a :> IAdditiveIdentity<'a, 'a>
        and 'a :> IDivisionOperators<'a, 'a, 'a>
        and 'a :> IIncrementOperators<'a>> : Fold<'a, 'a> =
        create
            (struct ('a.AdditiveIdentity, 'a.AdditiveIdentity))
            (fun (struct (sum, num)) value -> sum + value, ('a.op_Increment num))
            (fun (struct (sum, num)) -> sum / num)

    /// Return the last N elements
    let lastN n : Fold<'a, 'a list> =
        if n = 0 then
            create () (fun _ _ -> ()) (fun _ -> [])
        else
            create
                (struct (0, System.Collections.Immutable.ImmutableQueue<'a>.Empty))
                (fun (struct (num, q)) value ->
                    if num >= n then
                        (num, q.Dequeue().Enqueue value)
                    else
                        (num + 1, q.Enqueue value))
                (ValueTuple.snd >> List.ofSeq)

    /// Returns 'True' if the sequence is empty, 'False' otherwise
    let isEmpty<'a> : Fold<'a, bool> = create true (fun _ _ -> false) id
    /// returns 'True' if all elements satisfy the predicate, 'False' otherwise
    let all (p : 'a -> bool) : Fold<'a, bool> = create true (fun x a -> x && p a) id
    /// returns 'True' if any element satisfies the predicate, 'False' otherwise
    let any (p : 'a -> bool) : Fold<'a, bool> = create false (fun x a -> x || p a) id
    /// Returns 'True' if all elements are 'True', 'False' otherwise
    let and' : Fold<bool, bool> = create true (&&) id
    /// Returns 'True' if any element is 'True', 'False' otherwise
    let or' : Fold<bool, bool> = create false (||) id
    let sqrt<'a when 'a :> IRootFunctions<'a>> (a : Fold<'a, 'a>) = Fold.map 'a.Sqrt a

    /// Compute a numerically stable arithmetic mean of all elements
    let mean<'a when 'a :> IFloatingPoint<'a>> : Fold<'a, 'a> =
        let step (struct (x, n)) y =
            let n' = 'a.op_Increment n
            let diff = 'a.op_Subtraction (y, x)
            struct (x + diff / n', n')

        create (struct ('a.Zero, 'a.Zero)) step ValueTuple.fst

    /// Compute a numerically stable (population) variance over all elements
    let variance<'a when 'a :> IFloatingPoint<'a>> : Fold<'a, 'a> =
        let inline (/) a b = 'a.op_Division (a, b)
        let inline (+) a b = 'a.op_Addition (a, b)
        let inline (*) a b = 'a.op_Multiply (a, b)
        let inline (-) a b = 'a.op_Subtraction (a, b)

        let step (struct (n, m, m2)) x =
            let n' = 'a.op_Increment n
            let m' = (n * m + x) / n'
            let delta = x - m
            let m2' = m2 + delta * delta * n / n'
            struct (n', m', m2')

        let extract (struct (n, _, m2)) = m2 / n
        create (struct ('a.Zero, 'a.Zero, 'a.Zero)) step extract

    /// Compute a numerically stable (population) standard deviation over all elements
    let std<'a when 'a :> IRootFunctions<'a> and 'a :> IFloatingPoint<'a>> = sqrt<'a> variance<'a>

    /// Combine two folds into a fold over inputs for either of them.
    let choice (fold1 : Fold<'a1, 'b1>) (fold2 : Fold<'a2, 'b2>) : Fold<Choice<'a1, 'a2>, 'b1 * 'b2> =
        let folder =
            { new Folder<_, _, _> with
                member this.RunStep step1 current1 extract1 =
                    { new Folder<_, _, _> with
                        member this.RunStep step2 current2 extract2 =
                            let step' (struct (x1, x2)) a =
                                match a with
                                | Choice1Of2 x -> struct (step1 x1 x, x2)
                                | Choice2Of2 x -> struct (x1, step2 x2 x)

                            create (struct (current1, current2)) step' (ValueTuple.toTuple' extract1 extract2) } }

        folder |> fold1.RunFolder |> fold2.RunFolder

    let private fold1_<'a> (f : 'a -> 'a -> 'a) : Fold<'a, 'a option> =
        let step mx a =
            match mx with
            | None -> Some a
            | Some x -> Some(f x a)

        create None step id

    /// Get the first element of a sequence or return 'None' if the sequence is empty
    let head<'a> : Fold<'a, 'a option> = fold1_<'a> (fun a b -> a)
    /// Get the last element of a sequence or return 'None' if the sequence is empty
    let last<'a> : Fold<'a, 'a option> = fold1_<'a> (fun a b -> b)
    /// Get the last element of a sequence or return a default value if the sequence is empty
    let lastOrDefault a : Fold<'a, 'a> = create a (fun _ x -> x) id
    /// Computes the maximum element
    let maximum<'a when 'a : comparison> : Fold<'a, 'a option> = fold1_<'a> max
    /// Computes the minimum element
    let minimum<'a when 'a : comparison> : Fold<'a, 'a option> = fold1_<'a> min

    /// Computes the maximum element with respect to the given comparison function
    let maximumBy<'a, 'k when 'k : comparison> (f : 'a -> 'k) : Fold<'a, 'a option> =
        let max' a b = if f a >= f b then a else b
        fold1_<'a> max'

    /// Computes the maximum element with respect to the given comparison function
    let minimumBy<'a, 'k when 'k : comparison> (f : 'a -> 'k) : Fold<'a, 'a option> =
        let min' a b = if f a > f b then b else a
        fold1_<'a> min'

    /// returns 'True' if the container has an element equal to @a@, 'False' otherwise
    let elem<'a when 'a : equality> (a : 'a) : Fold<'a, bool> = any (fun x -> x = a)
    /// returns 'False' if the container has an element equal to @a@, 'True' otherwise
    let notElem<'a when 'a : equality> (a : 'a) : Fold<'a, bool> = all (fun x -> x <> a)

    /// returns the first element that satisfies the predicate or 'None' if no element satisfies the predicate
    let find (f : 'a -> bool) : Fold<'a, 'a option> =
        create
            None
            (fun x a ->
                match x with
                | None when f a -> Some a
                | _ -> x)
            id

    /// Computes the sum of all elements
    let zip (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) : Fold<'a, 'b * 'c> =
        let folder =
            { new Folder<_, _, _> with
                member this.RunStep step1 current1 extract1 =
                    { new Folder<_, _, _> with
                        member this.RunStep step2 current2 extract2 =
                            let step' (struct (x1, x2)) a = struct (step1 x1 a, step2 x2 a)
                            create (struct (current1, current2)) step' (ValueTuple.toTuple' extract1 extract2) } }

        folder |> fold1.RunFolder |> fold2.RunFolder

    let inline liftOp (f : 'b -> 'c -> 'd) (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) : Fold<'a, 'd> =
        zip fold1 fold2 |> map (fun (b, c) -> f b c)

    let inline ap (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'b -> 'c>) : Fold<'a, 'c> = liftOp id fold2 fold1
    let inline apply (fold1 : Fold<'a, 'b -> 'c>) (fold2 : Fold<'a, 'b>) : Fold<'a, 'c> = liftOp id fold1 fold2

    let sequence (folds : Fold<'a, 'b> list) : Fold<'a, 'b list> =
        let inline cons x xs = x :: xs
        let inline f x xs' = ap xs' (map cons x)
        List.foldBack f folds (retn [])

    let zip3 (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) (fold3 : Fold<'a, 'd>) : Fold<'a, 'b * 'c * 'd> =
        let folder =
            { new Folder<_, _, _> with
                member this.RunStep step1 current1 extract1 =
                    { new Folder<_, _, _> with
                        member this.RunStep step2 current2 extract2 =
                            { new Folder<_, _, _> with
                                member this.RunStep step3 current3 extract3 =
                                    let step' (struct (x1, x2, x3)) a =
                                        struct (step1 x1 a, step2 x2 a, step3 x3 a)

                                    let extract' = ValueTuple.toTuple3' extract1 extract2 extract3
                                    create (struct (current1, current2, current3)) step' extract' } } }

        folder |> fold1.RunFolder |> fold2.RunFolder |> fold3.RunFolder

    let zip4 (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) (fold3 : Fold<'a, 'd>) (fold4 : Fold<'a, 'e>) : Fold<'a, 'b * 'c * 'd * 'e> =
        let folder =
            { new Folder<_, _, _> with
                member this.RunStep step1 current1 extract1 =
                    { new Folder<_, _, _> with
                        member this.RunStep step2 current2 extract2 =
                            { new Folder<_, _, _> with
                                member this.RunStep step3 current3 extract3 =
                                    { new Folder<_, _, _> with
                                        member this.RunStep step4 current4 extract4 =
                                            let step' (struct (x1, x2, x3, x4)) a =
                                                struct (step1 x1 a, step2 x2 a, step3 x3 a, step4 x4 a)

                                            let extract' = ValueTuple.toTuple4' extract1 extract2 extract3 extract4
                                            create (struct (current1, current2, current3, current4)) step' extract' } } } }

        folder |> fold1.RunFolder |> fold2.RunFolder |> fold3.RunFolder |> fold4.RunFolder

    let zip4' (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) (fold3 : Fold<'a, 'd>) (fold4 : Fold<'a, 'e>) : Fold<'a, 'b * 'c * 'd * 'e> =
        let folder =
            { new Folder<_, _, _> with
                member this.RunStep step1 current1 extract1 =
                    { new Folder<_, _, _> with
                        member this.RunStep step2 current2 extract2 =
                            { new Folder<_, _, _> with
                                member this.RunStep step3 current3 extract3 =
                                    { new Folder<_, _, _> with
                                        member this.RunStep step4 current4 extract4 =
                                            let step' struct (x1, x2, x3, x4) a =
                                                struct (step1 x1 a, step2 x2 a, step3 x3 a, step4 x4 a)

                                            let extract' = ValueTuple.toTuple4' extract1 extract2 extract3 extract4
                                            create struct (current1, current2, current3, current4) step' extract' } } } }

        folder |> fold1.RunFolder |> fold2.RunFolder |> fold3.RunFolder |> fold4.RunFolder

module Operators =
    let inline (<!>) f a = Fold.map f a
    let inline (<*>) f a = Fold.ap a f

[<AutoOpen>]
module FoldCE =
    type FoldBuilder() =
        member this.MergeSources(x1, x2) = Fold.zip x1 x2
        member this.MergeSources3(x1, x2, x3) = Fold.zip3 x1 x2 x3
        member this.MergeSources4(x1, x2, x3, x4) = Fold.zip4 x1 x2 x3 x4
        member this.BindReturn(x, f) = Fold.map f x
        member this.Bind2Return(x1, x2, f : 'a * 'b -> 'r) = Fold.zip x1 x2 |> Fold.map f
        member this.Bind3Return(x1, x2, x3, f : 'a * 'b * 'c -> 'r) = Fold.zip3 x1 x2 x3 |> Fold.map f
        member this.Bind4Return(x1, x2, x3, x4, f : 'a * 'b * 'c * 'd -> 'r) = Fold.zip4 x1 x2 x3 x4 |> Fold.map f
        member this.Return x = Fold.retn x

    let fold = FoldBuilder()

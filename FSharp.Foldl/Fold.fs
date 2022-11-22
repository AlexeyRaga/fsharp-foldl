namespace rec FSharp.Foldl

open System
open System.Numerics

[<Struct>]
type Fold<'value, 'result> = private Fold of step: (obj -> 'value -> obj) * current: obj *  extract: (obj -> 'result)

[<RequireQualifiedAccess>]
module Fold =
    /// Creates a new fold
    let create (zero: 'x) (step: 'x -> 'a -> 'x) (extract: 'x -> 'b) =
        Fold(
            (fun (x : obj) a -> box (step (x :?> 'x) a)),
            box zero,
            (fun (x : obj) -> extract (x :?> 'x)))


    /// Executes the fold on a given sequence and returns its result
    let fold (Fold(step, current, extract)) xs =
        let res = xs |> Seq.fold step current
        extract res

    /// Functorial map (transform results)
    let map (f : 'b -> 'c) (Fold(step, current, extract)) =
        Fold(step, current, extract >> f)

    /// Cofunctorial map (adapt inputs)
    let premap (f : 'b -> 'a) (Fold(step, current, extract)) : Fold<'b, 'r> =
        Fold((fun x b -> step x (f b)), current, extract)

    /// Profunctorial dimap (both map and comap at once)
    let dimap (f : 'b -> 'a) (g: 'c -> 'd) (Fold(step, current, extract)) : Fold<'b, 'd> =
        Fold((fun x b -> step x (f b)), current, extract >> g)

    /// Returns a new 'Fold' where the folder's input is used
    /// only when the input satisfies a predicate f
    let prefilter (f : 'a -> bool) (Fold(step, current, extract)) : Fold<'a, 'b> =
        let step' x a = if f a then step x a else x
        Fold(step', current, extract)

    /// Transforms a 'Fold' into one which ignores elements
    /// until they stop satisfying a predicate
    let predropWhile (f : 'a -> bool) (Fold(step, current, extract)) : Fold<'a, 'b> =
        let step' (s : obj) a =
            let dropping, x = s :?> bool * obj
            if dropping && f a then box (true, x) else (false, step x a)
        let extract' (s : obj) =
            let _, x = s :?> bool * obj
            extract x
        Fold(step', box (true, current), extract')

    /// returns a new 'FoldM' that ignores the first @n@ inputs but
    /// otherwise behaves the same as the original fold
    let drop n (Fold(step, current, extract)) : Fold<'a, 'b> =
        let step' (s : obj) a =
            let n', x = s :?> int * obj
            if n' = 0 then box (0, step x a) else (n' - 1, x)
        let extract' (s : obj) =
            let _, x = s :?> int * obj
            extract x
        Fold(step', box (n, current), extract')

    /// Lifts the operation on the fold results into the operation on folds
    let liftOp (op : 'b -> 'c -> 'd) (Fold(stepL, currentL, extractL)) (Fold(stepR, currenR, extractR)) : Fold<'a, 'd> =
        let current = box (currentL, currenR)
        let step (x : obj) a : obj =
            let xL, xR = x :?> obj * obj
            box (stepL xL a, stepR xR a)
        let extract (x : obj) =
            let xL, xR = x :?> obj * obj
            op (extractL xL) (extractR xR)
        Fold(step, current, extract)


    let zip (fold1: Fold<'a, 'b>) (fold2: Fold<'a, 'c>) : Fold<'a, 'b*'c> =
        liftOp (fun a b -> (a, b)) fold1 fold2

    let zip3 (Fold(step1, current1, extract1)) (Fold(step2, curren2, extract2)) (Fold(step3, current3, extract3)) =
        let step (x : obj) a : obj =
            let x1, x2, x3 = x :?> obj * obj * obj
            (step1 x1 a, step2 x2 a, step3 x3 a)
        let extract (x : obj) =
            let x1, x2, x3 = x :?> obj * obj * obj
            extract1 x1, extract2 x2, extract3 x3
        Fold(step, (current1, curren2, current3), extract)

    let zip4 (Fold(step1, current1, extract1)) (Fold(step2, curren2, extract2)) (Fold(step3, current3, extract3)) (Fold(step4, current4, extract4)) =
        let step (x : obj) a : obj =
            let x1, x2, x3, x4 = x :?> obj * obj * obj * obj
            (step1 x1 a, step2 x2 a, step3 x3 a, step4 x4 a)
        let extract (x : obj) =
            let x1, x2, x3, x4 = x :?> obj * obj * obj * obj
            extract1 x1, extract2 x2, extract3 x3, extract4 x4
        Fold(step, (current1, curren2, current3, current4), extract)

    let retn x =
        create () (fun () _ -> ()) (fun () -> x)

    let inline ap (fold2 : Fold<'a, 'c>) (fold1 : Fold<'a, 'c -> 'd>) =
        liftOp id fold1 fold2

    let sequence (folds : Fold<'a, 'b> list) : Fold<'a, 'b list> =
        let inline cons x xs = x :: xs
        let inline f x xs' = ap xs' (map cons x)
        List.foldBack f folds (retn [])

    /// Computes the sum of all elements
    let sum<'a when 'a :> IAdditionOperators<'a, 'a, 'a>
               and  'a :> IAdditiveIdentity<'a, 'a>> : Fold<'a, 'a> =
        create 'a.AdditiveIdentity (+) id

    /// Computes the products of all elements
    let product<'a when 'a :> IMultiplyOperators<'a, 'a, 'a>
                   and  'a :> IMultiplicativeIdentity<'a, 'a>> =
        create 'a.MultiplicativeIdentity (*) id

    /// Computes the length of the sequence
    let length<'a> = create 0 (fun x (_ : 'a) -> x + 1) id

    let average<'a when 'a :> IAdditionOperators<'a, 'a, 'a>
                   and  'a :> IAdditiveIdentity<'a, 'a>
                   and  'a :> IDivisionOperators<'a, 'a, 'a>
                   and  'a :> IIncrementOperators<'a>> : Fold<'a, 'a> =
        create
            ('a.AdditiveIdentity, 'a.AdditiveIdentity)
            (fun (sum, num) value -> sum + value, ('a.op_Increment num) )
            (fun (sum, num) -> sum / num)

     /// Return the last N elements
    let lastN n : Fold<'a, 'a list> =
        if n = 0 then
            create () (fun _ _ -> ()) (fun _ -> [])
        else
            create
                (0, System.Collections.Immutable.ImmutableQueue<'a>.Empty)
                (fun (num, q) value ->
                    if num >= n then
                        (num, q.Dequeue().Enqueue value)
                    else
                        (num + 1, q.Enqueue value))
                (fun (_, q) -> List.ofSeq q)

    /// Returns 'True' if the sequence is empty, 'False' otherwise
    let isEmpty<'a> : Fold<'a, bool> =
        create true (fun _ _ -> false) id

    /// returns 'True' if all elements satisfy the predicate, 'False' otherwise
    let all (p : 'a -> bool) : Fold<'a, bool> =
        create true (fun x a -> x && p a) id

    /// returns 'True' if any element satisfies the predicate, 'False' otherwise
    let any (p : 'a -> bool) : Fold<'a, bool> =
        create false (fun x a -> x || p a) id

    /// Returns 'True' if all elements are 'True', 'False' otherwise
    let and' : Fold<bool, bool> =
        create true (&&) id

    /// Returns 'True' if any element is 'True', 'False' otherwise
    let or' : Fold<bool, bool> =
        create false (||) id

    let sqrt<'a when 'a :> IRootFunctions<'a>> (a : Fold<'a, 'a>) =
        Fold.map (fun x -> 'a.Sqrt x) a

    /// Compute a numerically stable arithmetic mean of all elements
    let mean<'a when 'a :> IFloatingPoint<'a>> : Fold<'a, 'a> =
        let step (x, n) y =
            let n' = 'a.op_Increment n
            let diff = 'a.op_Subtraction(y, x)
            x + diff / n', n'

        create ('a.Zero, 'a.Zero) step fst

    /// Compute a numerically stable (population) variance over all elements
    let variance<'a when 'a :> IFloatingPoint<'a>> : Fold<'a, 'a> =
        let inline (/) a b = 'a.op_Division(a, b)
        let inline (+) a b = 'a.op_Addition(a, b)
        let inline (*) a b = 'a.op_Multiply(a, b)
        let inline (-) a b = 'a.op_Subtraction(a, b)
        let step (n, m, m2) x =
            let n' = 'a.op_Increment n
            let m' = (n * m + x) / n'
            let delta = x - m
            let m2' = m2 + delta * delta * n / n'
            (n', m', m2')
        create ('a.Zero, 'a.Zero, 'a.Zero) step (fun (n, _, m2) -> m2 / n)

    /// Compute a numerically stable (population) standard deviation over all elements
    let std<'a when 'a :> IRootFunctions<'a> and 'a :> IFloatingPoint<'a>> = sqrt<'a> variance<'a>

    /// Combine two folds into a fold over inputs for either of them.
    let choice (Fold(stepL, currentL, extractL)) (Fold(stepR, currenR, extractR)) : Fold<Choice<'a1, 'a2>, 'b1 * 'b2> =
        let step x a =
            let xL, xR = Unchecked.unbox<obj * obj> x
            match a with
            | Choice1Of2 x -> box (stepL xL x, xR)
            | Choice2Of2 x -> box (xL, stepR xR x)

        let extract x =
            let xL, xR = Unchecked.unbox<obj * obj> x
            (extractL xL), (extractR xR)

        Fold(step, (currentL, currenR), extract)


    let private fold1_<'a> (f : 'a -> 'a -> 'a) : Fold<'a, 'a option> =
        let step mx a =
            match mx with
            | None -> Some a
            | Some x -> Some (f x a)
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
    let elem<'a when 'a : equality> (a : 'a) : Fold<'a, bool> =
        any (fun x -> x = a)

    /// returns 'False' if the container has an element equal to @a@, 'True' otherwise
    let notElem<'a when 'a : equality> (a : 'a) : Fold<'a, bool> =
        all (fun x -> x <> a)

    /// returns the first element that satisfies the predicate or 'None' if no element satisfies the predicate
    let find (f : 'a -> bool) : Fold<'a, 'a option> =
        create
            None
            (fun x a ->
                match x with
                | None when f a -> Some a
                | _ -> x)
            id

module Operators =
    let inline (|+|) a b = Fold.liftOp (+) a b
    let inline (|-|) a b = Fold.liftOp (-) a b

    let inline (|/|) a b = Fold.liftOp (/) a b
    let inline (|*|) a b = Fold.liftOp (*) a b

    let inline (<!>) f a = Fold.map f a
    let inline (<*>) f a = Fold.ap a f

[<AutoOpen>]
module FoldCE =
    type FoldBuilder() =
        member _.MergeSources(x1, x2) = Fold.zip x1 x2
        member _.MergeSources3(x1, x2, x3) = Fold.zip3 x1 x2 x3
        member _.MergeSources4(x1, x2, x3, x4) = Fold.zip4 x1 x2 x3 x4

        member _.BindReturn(x, f) = Fold.map f x
        member _.Bind2Return(x1, x2, f : 'a * 'b -> 'r) = Fold.zip x1 x2 |> Fold.map f
        member _.Bind3Return(x1, x2, x3, f : 'a * 'b * 'c -> 'r) = Fold.zip3 x1 x2 x3 |> Fold.map f
        member _.Bind4Return(x1, x2, x3, x4, f : 'a * 'b * 'c * 'd -> 'r) = Fold.zip4 x1 x2 x3 x4 |> Fold.map f

        member _.Return x = Fold.retn x

    let fold = FoldBuilder()

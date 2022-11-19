namespace rec FSharp.Foldl

open System
open System.Numerics

// Public interface for a fold: accepts values and folds them into results
type Fold<'value, 'result> =
    abstract member Accept: 'value -> Fold<'value, 'result>
    abstract member Extract: unit -> 'result

[<RequireQualifiedAccess>]
module Fold =

    // Hidden implementation, to hide an intermediate state
    type private FoldImpl<'value, 'state, 'result>
        (current: 'state, step: 'state -> 'value -> 'state, extract: 'state -> 'result) =
        interface Fold<'value, 'result> with
            member this.Accept(value) = FoldImpl(step current value, step, extract)
            member this.Extract() = extract current

    /// Creates a new fold
    let create (zero: 'x) (step: 'x -> 'a -> 'x) (extract: 'x -> 'b) =
        FoldImpl(zero, step, extract) :> Fold<'a, 'b>

    /// Executes the fold on a given sequence and returns its result
    let fold (fld: Fold<'a, 'b>) xs =
        let res = xs |> Seq.fold (fun (s: Fold<'a, 'b>) -> s.Accept) fld
        res.Extract()

    /// Functorial map (transform results)
    let map (f : 'b -> 'c) (fold : Fold<'a, 'b>) =
        create fold (fun x -> x.Accept) (fun x -> f (x.Extract()))

    /// Cofunctorial map (adapt inputs)
    let premap (f : 'b -> 'a) (fold : Fold<'a, 'r>) : Fold<'b, 'r> =
        create fold (fun x v -> x.Accept(f v)) (fun x -> x.Extract())

    /// Profunctorial dimap (both map and comap at once)
    let dimap (f : 'b -> 'a) (g: 'c -> 'd) (fold : Fold<'a, 'c>) : Fold<'b, 'd> =
        create fold (fun x v -> x.Accept(f v)) (fun x -> g (x.Extract()))

    /// Returns a new 'Fold' where the folder's input is used
    /// only when the input satisfies a predicate f
    let prefilter (f : 'a -> bool) (a : Fold<'a, 'b>) : Fold<'a, 'b> =
        create a (fun x v -> if f v then x.Accept v else x) (fun x -> x.Extract())

    /// Transforms a 'Fold' into one which ignores elements
    /// until they stop satisfying a predicate
    let predropWhile (f : 'a -> bool) (a : Fold<'a, 'b>) : Fold<'a, 'b> =
        create
            (true, a)
            (fun (dropping, x) v -> if dropping && f v then (true, x) else (false, x.Accept v))
            (fun (_, x) -> x.Extract())

    /// returns a new 'FoldM' that ignores the first @n@ inputs but
    /// otherwise behaves the same as the original fold
    let drop n (a : Fold<'a, 'b>) : Fold<'a, 'b> =
        create
            (n, a)
            (fun (n', x) v -> if n' = 0 then (0, x.Accept v) else (n' - 1, x))
            (fun (_, x) -> x.Extract())

    /// Lifts the operation on the fold results into the operation on folds
    let liftOp (op : 'b -> 'c -> 'd) (fold1 : Fold<'a, 'b>) (fold2 : Fold<'a, 'c>) : Fold<'a, 'd> =
        create
            (fold1, fold2)
            (fun (x1, x2) value -> (x1.Accept value, x2.Accept value))
            (fun (x1, x2) -> op (x1.Extract()) (x2.Extract()))

    let tuple (fold1: Fold<'a, 'b>) (fold2: Fold<'a, 'c>) : Fold<'a, 'b*'c> =
        liftOp (fun a b -> (a, b)) fold1 fold2

    let retn x =
        create () (fun () _ -> ()) (fun () -> x)

    let ap (fold2 : Fold<'a, 'c>) (fold1 : Fold<'a, 'c -> 'd>) =
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
    let choice (a : Fold<'a1, 'b1>) (b : Fold<'a2, 'b2>) : Fold<Choice<'a1, 'a2>, 'b1 * 'b2> =
        create
            (a, b)
            (fun (x1, x2) v ->
                match v with
                | Choice1Of2 v1 -> x1.Accept v1, x2
                | Choice2Of2 v2 -> x1, x2.Accept v2)
            (fun (x1, x2) -> x1.Extract(), x2.Extract())

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
        let max' a b = if f a > f b then a else b
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

module Symbolic.Expressions.Utils

open Symbolic.Expressions

/// A helper function to map/select across a list while threading state
/// through the computation
let selectFold f l s =
    let l,s' = List.fold
                  (fun (l',s') x ->
                       let x',s'' = f x s'
                       (List.rev x') @ l',s'')
                  ([],s) l
    List.rev l,s'

/// Collect constants
let rec collect = function
    | Prod (e1, e2) ->
        match collect e1, collect e2 with
        | Num num1, Num num2       -> Num (num1 * num2)
        | Num n1, Prod (Num n2, e)
        | Prod (Num n2, e), Num n1 -> Prod (Num (n1 * n2), e)
        | Num n, e | e, Num n      -> Prod (Num n, e)
        | Prod (Num n1, e1), Prod (Num n2, e2) ->
            Prod (Num (n1 * n2), Prod (e1, e2))
        | e1', e2'                 -> Prod (e1', e2')
    | Num _ | Var _ as e   -> e
    | Neg e                -> Neg (collect e)
    | Add exprs            -> Add (List.map collect exprs)
    | Sub (e1, exprs)      -> Sub (collect e1, List.map collect exprs)
    | Frac (e1, e2)        -> Frac (collect e1, collect e2)
    | Pow (e1, num)        -> Pow (collect e1, num)
    | Sin e                -> Sin (collect e)
    | Cos e                -> Cos (collect e)
    | Exp _ as e           -> e

/// Push negations through an expression
let rec negate = function
    | Num num           -> Num (-num)
    | Var v as exp      -> Neg exp
    | Neg e             -> e
    | Add exprs         -> Add (List.map negate exprs)
    | Sub _             -> failwith "unexpected Sub"
    | Prod (e1, e2)     -> Prod (negate e1, e2)
    | Frac (e1, e2)     -> Frac (negate e1, e2)
    | exp               -> Neg exp

/// Simplify an expression
let rec simp = function
    | Num num           -> Num num
    | Var v             -> Var v
    | Neg e             -> negate (simp e)
    | Add exprs ->
        let filterNums (e:Expr) n =
           if e.IsNumber
           then [], n + e.NumOf
           else [e], n
        let summands = function | Add es -> es | e -> [e]
        let exprs', num =
            selectFold (simp >> summands >> selectFold filterNums) exprs 0M
        match exprs' with
        | [Num _ as n] when num = 0M -> n
        | []                         -> Num num
        | [e] when num = 0M          -> e
        | _ when num = 0M            -> Add exprs'
        | _                          -> Add (exprs' @ [Num num])
    | Sub (e1, exprs) ->
         simp (Add (e1 :: List.map Neg exprs))
    | Prod (e1, e2) ->
        match simp e1, simp e2 with
        | Num num, _ | _, Num num when num = 0M -> Num 0M
        | Num num, e | e, Num num when num = 1M -> e
        | Num num1, Num num2                    -> Num (num1 * num2)
        | e1, e2                                -> Prod (e1, e2)
    | Frac (e1, e2) ->
        match simp e1, simp e2 with
        | Num num, _ when num = 0M  -> Num num
        | e1, Num num when num = 1M -> e1
        | Num (_  as num), Frac (Num (_ as num2), e) ->
             Prod (Frac (Num num, Num num2), e)
        | Num (_  as num), Frac (e, Num (_ as num2)) ->
             Frac (Prod (Num num, Num num2), e)
        | e1, e2                    -> Frac (e1, e2)
    | Pow (e, n) when n=1M -> simp e
    | Pow (e, n)           -> Pow (simp e, n)
    | Sin e                -> Sin (simp e)
    | Cos e                -> Cos (simp e)
    | Exp e                -> Exp (simp e)

let Simplify e = e |> simp |> simp |> collect

let Differentiate v e =
    let rec diff v = function
        | Num num               -> Num 0M
        | Var v' when v'=v      -> Num 1M
        | Var v'                -> Num 0M
        | Neg e                 -> diff v (Prod ((Num -1M), e))
        | Add exprs             -> Add (List.map (diff v) exprs)
        | Sub (e1, exprs)       -> Sub (diff v e1, List.map (diff v) exprs)
        | Prod (e1, e2)         -> Add [Prod (diff v e1, e2); Prod (e1, diff v e2)]
        | Frac (e1, e2) ->
            Frac (Sub (Prod (diff v e1, e2), [Prod (e1, diff v e2)]), Pow (e2, 2M))
        | Pow (e1, num) ->
            Prod (Prod(Num num, Pow (e1, num - 1M)), diff v e1)
        | Sin e                 -> Prod (Cos e, diff v e)
        | Cos e                 -> Neg (Prod (Sin e, diff v e))
        | Exp (Var v') as e when v'=v  -> e
        | Exp (Var v') as e when v'<>v -> Num 0M
        | Exp e                 -> Prod (Exp e, diff v e)
    diff v e

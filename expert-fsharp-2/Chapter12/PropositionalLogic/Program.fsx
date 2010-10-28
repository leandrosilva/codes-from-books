type Var = string

type Prop =
    | And of Prop * Prop
    | Var of Var
    | Not of Prop
    | Exists of Var * Prop
    | False

let True = Not(False)
let Or(p,q)      = Not(And(Not(p),Not(q)))
let Iff(p,q)     = Or(And(p,q),And(Not(p),Not(q)))
let Implies(p,q) = Or(Not(p),q)
let Forall(v,p)  = Not(Exists(v,Not(p)))

let (&&&) p q = And(p,q)
let (|||) p q = Or(p,q)
let (~~~) p   = Not (p)
let (<=>) p q = Iff(p,q)
let (===) p q = (p <=> q)
let (==>) p q = Implies(p,q)
let (^^^) p q = Not (p <=> q)

let var (nm:Var) = Var(nm)

let fresh =
    let count = ref 0
    fun nm -> incr count; (sprintf "_%s%d" nm !count : Var)

let rec eval (env : Map<Var,bool>) inp =
    match inp with
    | Exists(v,p) -> eval (env.Add(v,false)) p || eval (env.Add(v,true)) p
    | And(p1,p2)  -> eval env p1 && eval env p2
    | Var(v)      -> if env.ContainsKey(v) then env.[v]
                     else failwithf "env didn't contain a value for %A" v
    | Not(p)      -> not (eval env p)
    | False       -> false

let rec support f =
    match f with
    | And(x,y)    -> Set.union (support x) (support y)
    | Exists(v,p) -> (support p).Remove(v)
    | Var(p)      -> Set.singleton p
    | Not(x)      -> support x
    | False       -> Set.empty

let rec cases supp =
    seq { match supp with
          | [] ->  yield Map.empty
          | v::rest ->
              yield! rest |> cases |> Seq.map (Map.add v false)
              yield! rest |> cases |> Seq.map (Map.add v true) }

let truthTable x =
    x |> support |> Set.toList |> cases |> Seq.map (fun env -> env,eval env x)

let satisfiable x =
    x |> truthTable |> Seq.exists(fun (env,res) -> res)

let tautology x =
    x |> truthTable |> Seq.forall (fun (env,res) -> res)

let tautologyWithCounterExample x =
    x |> truthTable |> Seq.tryFind (fun (env,res) -> not res) |> Option.map fst

let printCounterExample =
    (function None -> printfn "tautology verified OK"
            | Some env -> printfn "tautology failed on %A" (Seq.toList env))

let stringOfBit b = (if b then "T" else "F")
let stringOfEnv env =
    Map.fold (fun acc k v -> sprintf "%s=%s;" k (stringOfBit v)+acc) "" env
let stringOfLine (env,res) = sprintf "%20s %s" (stringOfEnv env) (stringOfBit res)
let stringOfTruthTable tt =
    "\n" + (tt |> Seq.toList |> List.map stringOfLine |> String.concat "\n")

let f tt = tt |> Seq.truncate 20 |> stringOfTruthTable


let sumBit x y = (x ^^^ y)
let carryBit x y = (x &&& y)
let halfAdder x y sum carry =
    (sum === sumBit x y)  &&&
    (carry === carryBit x y)

let fullAdder x y z sum carry =
    let xy = (sumBit x y)
    (sum === sumBit xy z) &&&
    (carry === (carryBit x y ||| carryBit xy z))

let twoBitAdder (x1,x2) (y1,y2) (sum1,sum2) carryInner carry =
    halfAdder x1 y1 sum1 carryInner &&&
    fullAdder x2 y2 carryInner sum2 carry

type bit = Prop
type bitvec = bit[]

let Lo : bit = False
let Hi : bit = True
let vec n nm : bitvec = Array.init n (fun i -> var (sprintf "%s%d" nm i))
let bitEq (b1:bit) (b2:bit) = (b1 <=> b2)
let AndL l = Seq.reduce (fun x y -> And(x,y)) l
let vecEq (v1:bitvec) (v2:bitvec) = AndL (Array.map2 bitEq v1 v2)

let fourBitAdder  (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec) =
    halfAdder  x.[0] y.[0]           sum.[0] carry.[0] &&&
    fullAdder  x.[1] y.[1] carry.[0] sum.[1] carry.[1] &&&
    fullAdder  x.[2] y.[2] carry.[1] sum.[2] carry.[2] &&&
    fullAdder  x.[3] y.[3] carry.[2] sum.[3] carry.[3]

let Blocks l = AndL l

let nBitCarryRippleAdder (n:int) (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec) =
    Blocks [ for i in 0 .. n-1 ->
                if i = 0
                then halfAdder x.[i] y.[i] sum.[i] carry.[i]
                else fullAdder x.[i] y.[i] carry.[i-1] sum.[i] carry.[i]  ]

let rippleAdder (n:int) (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec)  =
    Blocks [ for i in 0 .. n-1 ->
                fullAdder x.[i] y.[i] carry.[i] sum.[i] carry.[i+1] ]

let twoBitAdderWithHiding (x1,x2) (y1,y2) (sum1,sum2) carry =
    let carryInnerVar = fresh "carry"
    let carryInner = var(carryInnerVar)
    Exists(carryInnerVar, halfAdder x1 y1 sum1 carryInner &&&
                          fullAdder x2 y2 carryInner sum2 carry)


open System.Collections.Generic

let memoize f =
    let tab = new Dictionary<_,_>()
    fun x -> if tab.ContainsKey(x) then tab.[x]
             else let res = f x in tab.[x] <- res; res

type BddIndex = int
type Bdd = Bdd of BddIndex
type BddNode = Node of Var * BddIndex * BddIndex
type BddBuilder(order : Var -> Var -> int) =

    // The core data structures that preserve uniqueness
    let uniqueTab = new Dictionary<BddNode,BddIndex>()
    let nodeTab   = new Dictionary<BddIndex,BddNode>()

    // Keep track of the next index
    let mutable nextIdx = 2
    let trueIdx = 1
    let falseIdx = -1
    let trueNode = Node("",trueIdx,trueIdx)
    let falseNode = Node("",falseIdx,falseIdx)

    // Map indexes to nodes. Negative indexes go to their negation. The special
    // indexes -1 and 1 go to special true/false nodes.
    let idxToNode(idx) =
        if idx = trueIdx then trueNode
        elif idx = falseIdx then falseNode
        elif idx > 0 then nodeTab.[idx]
        else let (Node(v,l,r)) = nodeTab.[-idx]
             Node(v,-l,-r)

    // Map nodes to indexes. Add an entry to the table if needed.
    let nodeToUniqueIdx(node) =
        if uniqueTab.ContainsKey(node) then uniqueTab.[node]
        else
            let idx = nextIdx
            uniqueTab.[node] <- idx
            nodeTab.[idx] <- node
            nextIdx <- nextIdx + 1
            idx

    // Get the canonical index for a node. Preserve the invariant that the
    // left-hand node of a conditional is always a positive node
    let mkNode(v:Var,l:BddIndex,r:BddIndex) =
        if l = r then l
        elif l >= 0 then nodeToUniqueIdx(Node(v,l,r) )
        else -nodeToUniqueIdx(Node(v,-l,-r))

    // Construct the BDD for a conjunction "m1 AND m2"
    let rec mkAnd(m1,m2) =
        if m1 = falseIdx || m2 = falseIdx then falseIdx
        elif m1 = trueIdx then m2 elif m2 = trueIdx then m1
        else
            let (Node(x,l1,r1)) = idxToNode(m1)
            let (Node(y,l2,r2)) = idxToNode(m2)
            let v,(la,lb),(ra,rb) =
                match order x y with
                | c when c = 0 -> x,(l1,l2),(r1,r2)
                | c when c < 0 -> x,(l1,m2),(r1,m2)
                | c            -> y,(m1,l2),(m1,r2)
            mkNode(v,mkAnd(la,lb), mkAnd(ra,rb))

    // Memoize this function
    let mkAnd = memoize mkAnd


    // Publish the construction functions that make BDDs from existing BDDs
    member g.False = Bdd falseIdx
    member g.And(Bdd m1,Bdd m2) = Bdd(mkAnd(m1,m2))
    member g.Not(Bdd m) = Bdd(-m)
    member g.Var(nm) = Bdd(mkNode(nm,trueIdx,falseIdx))
    member g.NodeCount = nextIdx

    member g.ToString(Bdd idx) =
        let rec fmt depth idx =
            if depth > 3 then "..." else
            let (Node(p,l,r)) = idxToNode(idx)
            if p = "" then if l = trueIdx then "T" else "F"
            else sprintf "(%s => %s | %s)" p (fmt (depth+1) l) (fmt (depth+1) r)
        fmt 1 idx

    member g.Build(f) =
        match f with
        | And(x,y) -> g.And(g.Build x, g.Build y)
        | Var(p) -> g.Var(p)
        | Not(x) -> g.Not(g.Build x)
        | False -> g.False
        | Exists(v,p) -> failwith "Exists node"

    member g.Equiv p1 p2 = (g.Build(p1) = g.Build(p2))


let mux a b c = ((~~~a ==> b) &&& (a ==> c))

let carrySelectAdder
       totalSize maxBlockSize
       (x:bitvec) (y:bitvec)
       (sumLo:bitvec) (sumHi:bitvec)
       (carryLo:bitvec) (carryHi:bitvec)
       (sum:bitvec) (carry:bitvec) =
  Blocks
    [ for i in 0..maxBlockSize..totalSize-1 ->
        let sz = min (totalSize-i) maxBlockSize
        let j = i+sz-1
        let carryLo = Array.append [| False |] carryLo.[i+1..j+1]
        let adderLo = rippleAdder sz x.[i..j] y.[i..j] sumLo.[i..j] carryLo
        let carryHi = Array.append [| True  |] carryHi.[i+1..j+1]
        let adderHi = rippleAdder sz x.[i..j] y.[i..j]  sumHi.[i..j] carryHi
        let carrySelect = (carry.[j+1] === mux carry.[i] carryLo.[sz] carryHi.[sz])
        let sumSelect =
            Blocks [for k in i..j ->
                         sum.[k] === mux carry.[i] sumLo.[k] sumHi.[k]]
        adderLo &&& adderHi &&& carrySelect &&& sumSelect ]


let checkAdders n k =
    let x = (vec n "x")
    let y = (vec n "y")
    let sumA    = (vec n "sumA")
    let sumB    = (vec n "sumB")
    let sumLo   = (vec n "sumLo")
    let sumHi   = (vec n "sumHi")
    let carryA  = (vec (n+1) "carryA")
    let carryB  = (vec (n+1) "carryB")
    let carryLo = (vec (n+1) "carryLo")
    let carryHi = (vec (n+1) "carryHi")
    let adder1 = carrySelectAdder n k x y sumLo sumHi carryLo  carryHi  sumA carryA
    let adder2 = rippleAdder n x y sumB carryB
    (adder1 &&& adder2 &&& (carryA.[0] === carryB.[0]) ==>
         (vecEq sumA sumB &&& bitEq carryA.[n] carryB.[n]))


let approxCompareOn f x y =
    let c = compare (f x) (f y)
    if c <> 0 then c else compare x y
let bddBuilder2 = BddBuilder(approxCompareOn hash)


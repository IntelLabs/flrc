(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* The Core HS IL *)

structure CoreHs = struct

  type strictness = bool

  type identifier = string

  type var  = identifier
  type tvar = identifier
  type tcon = identifier
  type dcon = identifier

  datatype pName
      = P of identifier

  type anMName'
      = pName * identifier list * identifier

  datatype anMName
      = M of anMName'

  type mName = anMName option
  
  type 'a qualified = mName * 'a

  datatype ty
      = Tvar of tvar
      | Tcon of tcon qualified
      | Tapp of ty * ty
      | Tforall of (tvar * kind) * ty   (* Tforall of tBind * ty *)
      | TransCoercion of ty * ty
      | SymCoercion of ty
      | UnsafeCoercion of ty * ty
      | InstCoercion of ty * ty
      | LeftCoercion of ty
      | RightCoercion of ty
      | NthCoercion of (int * ty)

  and kind
      = Klifted
      | Kunlifted
      | Kopen
      | Karrow of kind * kind
      | Keq of ty * ty

  type vBind = var * ty

  type tBind = tvar * kind

  datatype callconv = Prim | CCall | StdCall

  datatype bind
      = Vb of vBind
      | Tb of tBind

  datatype coreLit
      = Lint of IntInf.t
      | Lrational of Rat.t
      | Lchar of int
      | Lstring of string

  datatype lit
      = Literal of coreLit * ty

  datatype coercionKind
      = DefinedCoercion of tBind list * ty * ty

  datatype kindOrCoercion
      = Kind of kind
      | Coercion of coercionKind

  datatype alt
      = Acon of dcon qualified * tBind list * vBind list * exp
      | Alit of lit * exp
      | Adefault of exp

  and exp
      = Var of var qualified
      | Dcon of dcon qualified
      | Lit of lit
      | App of exp * exp
      | Appt of exp * ty
      | Lam of bind * exp
      | Let of vDefg * exp
      | Case of exp * vBind * ty * alt list (* non-empty list *)
      | Cast of exp * ty
      | Note of string * exp
      | External of string * callconv * string * ty

  and vDef
      = Vdef of var qualified * ty * exp

  and vDefg
      = Rec of vDef list
      | Nonrec of vDef

  datatype cDef
      = Constr of dcon qualified * tBind list * (ty * strictness) list

  datatype tDef
      = Data of tcon qualified * tBind list * cDef list
      | Newtype of tcon qualified * tcon qualified * tBind list * ty

  datatype module
      = Module of anMName * tDef list * vDefg list

  type t = module
end

structure CoreHsUtils =
struct
  open CoreHs
  structure UF = Utils.Function

  fun toHexString i =
    if i < 16
      then Char.toString (String.sub ("0123456789abcdef", i))
      else toHexString (Int.div (i, 16)) ^ toHexString (Int.div (i, 16))

  val unencodedChar : char -> bool =
    fn #"Z" => false
     | #"z" => false
     | c    => Char.isAlphaNum c

  val encode_ch : char -> string =
    fn c => if unencodedChar c then Char.toString c else
      case c
        of #"(" => "ZL"
         | #")" => "ZR"
         | #"[" => "ZM"
         | #"]" => "ZN"
         | #":" => "ZC"
         | #"Z" => "ZZ"
         | #"z" => "zz"
         | #"&" => "za"
         | #"|" => "zb"
         | #"^" => "zc"
         | #"$" => "zd"
         | #"=" => "ze"
         | #">" => "zg"
         | #"#" => "zh"
         | #"." => "zi"
         | #"<" => "zl"
         | #"-" => "zm"
         | #"!" => "zn"
         | #"+" => "zp"
         | #"'" => "zq"
         | #"\\"=> "zr"
         | #"/" => "zs"
         | #"*" => "zt"
         | #"_" => "zu"
         | #"%" => "zv"
         | c    =>
           let val hex_str = toHexString (ord c) ^ "U"
           in "z" ^ (if Char.isDigit (String.sub (hex_str, 0))
                      then hex_str
					  else "0" ^ hex_str)
           end

  val maybeTuple : string -> string option =
    fn "(# #)" => SOME "Z1H"
     | "()"    => SOME "Z0T"
     | s       => 
       let fun count_commas n (#"," :: cs) = count_commas (n+1) cs
             | count_commas n cs	       = (n,cs)
       in 
         case explode s 
           of #"(" :: #"#" :: cs =>
              (case count_commas 0 cs
                 of (n, #"#" :: #")" :: _) => SOME ("Z" ^ Int.toString (n+1) ^ "H")
                  | _                      => NONE)
            | #"(" :: cs =>
              (case count_commas 0 cs
                 of (n, #")" :: _) => SOME ("Z" ^ Int.toString (n+1) ^ "T")
                  | _              => NONE)
            | _ => NONE
       end

  val zEncodeString : string -> string =
   fn cs => 
      let fun go [] = ""
            | go (c::cs) = encode_ch c ^ go cs
      in case maybeTuple cs
           of SOME n => n
            | _      => go (explode cs)
      end

  val decode_upper : char -> char =
    fn #"L" => #"("
     | #"R" => #")"
     | #"M" => #"["
     | #"N" => #"]"
     | #"C" => #":"
     | #"Z" => #"Z"
     | ch   => ch

  val decode_lower : char -> char =
    fn #"z" => #"z"
     | #"a" => #"&"
     | #"b" => #"|"
     | #"c" => #"^"
     | #"d" => #"$"
     | #"e" => #"="
     | #"g" => #">"
     | #"h" => #"#"
     | #"i" => #"."
     | #"l" => #"<"
     | #"m" => #"-"
     | #"n" => #"!"
     | #"p" => #"+"
     | #"q" => #"'"
     | #"r" => #"\\"
     | #"s" => #"/"
     | #"t" => #"*"
     | #"u" => #"_"
     | #"v" => #"%"
     | ch   => ch

  fun decode [] = []
    | decode (#"Z" :: d :: rest) = if Char.isDigit d then decode_tuple (d, rest)
                                     else decode_upper d :: decode rest
    | decode (#"z" :: d :: rest) = if Char.isDigit d then decode_num_esc (d, rest) 
                                     else decode_lower d :: decode rest
    | decode (c   :: rest)       = c :: decode rest

  and decode_num_esc (d, rest) =
      let fun fail () = raise Fail ("decode_num_esc: " ^ implode (d :: rest))
          fun go n (c :: cs) =
             if Char.isHexDigit c
               then go (16 * n + Char.toHexDigit c) cs
               else if c = #"U"
                 then chr n :: decode cs
                 else fail ()
            | go n [] = fail ()
      in go (Char.toHexDigit d) rest
      end

  and decode_tuple (d, rest) =
      let
        fun fail () = raise Fail ("decode_tuple: " ^ implode (d :: rest))
        fun go 0 (#"T" :: cs) = #"(" :: #")" :: decode cs
          | go n (#"T" :: cs) = (#"(" :: List.duplicate (n - 1, fn _ => #",")) @
                                (#")" :: decode cs)
          | go 1 (#"H" :: cs) = explode "(# #)" @ decode cs
          | go n (#"H" :: cs) = (#"(" :: #"#" :: List.duplicate (n-1,  fn _ => #",")) @
                                (#"#" :: #")" :: decode cs)
          | go n (c :: cs)    = if Char.isDigit c then go (10 * n + Char.toInt c) cs else fail ()
          | go _ []           = fail ()
    in go (Char.toHexDigit d) rest
    end

  val zDecodeString : string -> string = implode o decode o explode

  val splitModuleName : string -> string list * string =
    fn mn =>
       let val decoded = zDecodeString mn
           val parts   = List.map (String.split (decoded, #"."), zEncodeString)
       in List.splitLast parts
       end

  val qual   : anMName -> 't -> 't qualified = fn mn => fn t => (SOME mn, t)
  val unqual : 't -> 't qualified = fn t => (NONE, t)

  val getModule : 't qualified -> mName = #1

  (* eqKind : kind * kind -> bool *)
  fun eqKind (Klifted,         Klifted)         = true
    | eqKind (Kunlifted,       Kunlifted)       = true
    | eqKind (Kopen,           Kopen)           = true
    | eqKind (Karrow (k1, k2), Karrow (l1, l2)) = eqKind (k1, l1) andalso eqKind (k2, l2)
    | eqKind (Keq (t1, t2),    Keq (u1,u2))     = eqTy (t1, u1) andalso eqTy (t2, u2)
    | eqKind _                                  = false

  (* eqTy : ty * ty -> bool *)
  and eqTy (t1, t2) =
      let fun eqty e1 e2 (Tvar v1) (Tvar v2) =
                  (case (List.index (e1, fn x => x = v1), List.index (e2, fn x => x = v2))
                     of (SOME i1, SOME i2) => i1 = i2
                      | (NONE,    NONE)    => v1 = v2
                      | _                  => false)
            | eqty _ _ (Tcon c1) (Tcon c2) = c1 = c2
            | eqty e1 e2 (Tapp (t1a, t1b)) (Tapp (t2a, t2b)) =
                  eqty e1 e2 t1a t2a andalso eqty e1 e2 t1b t2b
            | eqty e1 e2 (Tforall ((tv1,tk1), b1)) (Tforall ((tv2,tk2), b2)) =
                  eqKind (tk1, tk2) andalso eqty (tv1::e1) (tv2::e2) b1 b2
            | eqty _ _ _ _ = false
      in eqty [] [] t1 t2
      end

  (* splitTyConApp_maybe : ty -> (tcon qualified, ty list) option *)
  fun splitTyConApp_maybe t =
      case t
        of Tvar _             => NONE
         | Tcon t             => SOME (t,[])
         | Tapp (rator, rand) => (case splitTyConApp_maybe rator 
                                    of SOME (r,rs) => SOME (r,rs @ [rand])
                                     | NONE => case rator 
                                                  of Tcon tc => SOME (tc,[rand])
                                                   | _       => NONE)
         | _                  => NONE


  (* subKindOf : kind * kind -> bool *)
  fun subKindOf (k1, k2) =
      case (k1, k2)
        of (_,               Kopen)           => true
         | (Karrow (a1, r1), Karrow (a2, r2)) => subKindOf (a2, a1) andalso subKindOf (r1, r2)
         | _                                  => eqKind (k1, k2)

  fun compareList ([], []) = Relation.EQUAL
    | compareList (x::xs, []) = Relation.GREATER
    | compareList ([], x::xs) = Relation.LESS
    | compareList (x::xs, y::ys) = case String.compare (x, y)
                                     of Relation.EQUAL => compareList (xs, ys)
                                      | r => r

  fun comparePName (P p, P q) = String.compare (p, q)

  fun compareAnMName' ((p, m, n), (p', m', n')) =
    case comparePName (p, p')
      of Relation.EQUAL => (case compareList (m, m')
         of Relation.EQUAL => String.compare (n, n')
          | r => r)
       | r => r

  fun compareAnMName (M p, M q) = compareAnMName' (p, q)

  fun compareMName (NONE, NONE) = Relation.EQUAL
    | compareMName (SOME _, NONE) = Relation.GREATER
    | compareMName (NONE, SOME _) = Relation.LESS
    | compareMName (SOME m, SOME n) = compareAnMName (m, n)

  fun compareQName ((m, n), (m', n')) =
      let
      in
        case compareMName (m, m')
          of Relation.EQUAL => String.compare (n, n')
           | r => r
      end

  val baseKind : kind -> bool =
    fn k => 
      case k 
        of Karrow _ => false
         | _        => true

  val ghcPrefix = ["GHC"]
  val basePkg = P "base"
  val mainPkg = P "main"
  val primPkg = P (zEncodeString "ghc-prim")
  val mkBaseMname : identifier -> anMName = fn mn => M (basePkg, ghcPrefix, mn)
  val mkPrimMname : identifier -> anMName = fn mn => M (primPkg, ghcPrefix, mn)
  val primMname = mkPrimMname "Prim"
  val errMname  = mkBaseMname "Err"
  val mainPrefix = []
  val boolMname = mkPrimMname "Types"
  val mainMname = M (mainPkg, mainPrefix, "Main")
  val mainVar = qual mainMname "main"
  val wrapperMainMname = M (mainPkg, mainPrefix, "ZCMain")
  val wrapperMainAnMname = SOME wrapperMainMname
  val wrapperMainVar = qual wrapperMainMname "main"

  fun isPrimVar (SOME mn,_) = mn = primMname
    | isPrimVar _           = false

  val dcTrue  : dcon = "True"
  val dcFalse : dcon = "False"

  val tcArrow : tcon qualified = (SOME primMname, "ZLzmzgZR")

  val tArrow : ty * ty -> ty =
    fn (t1, t2) => Tapp (Tapp (Tcon tcArrow, t1), t2)

  val ktArrow : kind = Karrow (Kopen, Karrow (Kopen, Klifted))

  val maxUtuple : int = 100

  val tcUtuple : int -> tcon qualified = 
    fn n => (SOME primMname, "Z" ^ Int.toString n ^ "H")

  val ktUtuple : int -> kind =
    fn n => List.foldr (List.tabulate (n, fn _ => Kopen), Kunlifted, Karrow)

  val tUtuple : ty list -> ty =
    fn ts => List.fold (ts, (Tcon (tcUtuple (length ts))), UF.flipIn Tapp)

  val isUtupleDc : dcon qualified -> int option =
    fn (p, s) =>
          (case (p, explode s)
             of (SOME pm, #"Z" :: rest) =>
               if pm = primMname andalso List.last rest = #"H"
                 then 
                   let val mid = List.firstN (rest, (length rest) - 1)
                       val num = Int.fromString (implode mid)
                   in case num 
                     of SOME n => if List.forall (mid, Char.isDigit) andalso 1 <= n andalso n <= maxUtuple
                                    then SOME n else NONE
                      | NONE   => NONE 
                   end
                 else NONE
              | _ => NONE)

  val rec isUtupleTy : ty -> int option =
    fn Tapp (t, _) => isUtupleTy t
     | Tcon tcon => isUtupleDc tcon
     | _ => NONE

  val dcUtuple : int -> dcon qualified =
    fn n => (SOME primMname, "Z" ^ Int.toString n ^ "H")

  val dcUtupleTy : int -> ty =
    fn n =>
      let
        val tvs = List.tabulate (n, fn i => "a" ^ Int.toString (i+1))
        val t   = List.foldr (tvs, tUtuple (map Tvar tvs),
                      fn (tv, t) => tArrow (Tvar tv, t))
      in List.foldr (tvs,  t, fn (tv, t) => Tforall ((tv, Kopen), t))
      end

  val utuple : ty list * exp list -> exp =
    fn (ts, es) =>
      List.fold (es, List.fold (ts, Dcon (dcUtuple (length es)), UF.flipIn Appt), UF.flipIn App)

  val rec flattenBinds : vDefg list -> vDef list =
    fn (Nonrec vd :: binds) => vd :: flattenBinds binds
     | (Rec prs1  :: binds) => prs1 @ flattenBinds binds
     | []                   => []

  val unitMname : anMName = mkPrimMname "Unit"

  (* substitute a type variable v in type vty with an actual type t *)
  fun substTy (v, vty, t)
    = let
        fun sub ty =
            case ty
              of Tvar u => if u = v then vty else ty
               | Tcon _ => ty
               | Tapp (t1, t2) => Tapp (sub t1, sub t2)
               | Tforall (tb, t1) => Tforall (tb, sub t1)
               | TransCoercion (t1, t2) => TransCoercion (sub t1, sub t2)
               | SymCoercion t1 => SymCoercion (sub t1)
               | UnsafeCoercion (t1, t2) => UnsafeCoercion (sub t1, sub t2)
               | InstCoercion (t1, t2) => InstCoercion (sub t1, sub t2)
               | LeftCoercion t1 => LeftCoercion (sub t1)
               | RightCoercion t1 => RightCoercion (sub t1)
               | NthCoercion (i, t1) => NthCoercion (i, sub t1)
      in
        sub t
      end
end

structure CoreHsPrims =
struct
  open CoreHs
  structure UF = Utils.Function
  structure CHU = CoreHsUtils

  val pv = CHU.qual CHU.primMname
  val pvz : identifier -> identifier qualified = fn i => pv (i ^ "zh")

  val symCoercion    : tcon qualified = pv "sym"
  val transCoercion  : tcon qualified = pv "trans"
  val unsafeCoercion : tcon qualified = pv "CoUnsafe"
  val leftCoercion   : tcon qualified = pv "left"
  val rightCoercion  : tcon qualified = pv "right"
  val instCoercion   : tcon qualified = pv "inst"

  val tcAddrzh = pvz "Addr"
  val tAddrzh  = Tcon tcAddrzh
  val ktAddrzh = Kunlifted
  val tcAny : tcon qualified = pv "Any"
  val tAny  : ty             = Tcon tcAny

  val tcBool : tcon qualified = (SOME CHU.boolMname, "Bool")
  val tBool  : ty             = Tcon tcBool

  val dTrue  : dcon qualified = (SOME CHU.boolMname, "True")
  val dFalse : dcon qualified = (SOME CHU.boolMname, "False")

  val boolTcs : (tcon * kindOrCoercion) list = [(#2 tcBool, Kind Klifted)]

  val boolDcs : (dcon * ty) list = [(CHU.dcTrue, tBool), (CHU.dcFalse, tBool)]

  val primDcs : (dcon * ty) list =
      List.tabulate (CHU.maxUtuple,
	   fn i => let val n = i + 1
                   val ((_, c), t) = (CHU.dcUtuple n, CHU.dcUtupleTy n)
               in (c, t)
               end)

  val tcRealWorld   : tcon qualified = pv "RealWorld"
  val vRealWorldzh : var qualified = pvz "realWorld"
  val tRealWorld    : ty        = Tcon tcRealWorld

  val tcStatezh : tcon qualified = pvz "State"
  val tStatezh  : ty -> ty  = fn t => Tapp (Tcon tcStatezh, t)

  val tRWS : ty = tStatezh tRealWorld

  val opsState : (var * ty) list = [("realWorldzh", tRWS)]

  val tcArrayzh            : tcon qualified = pvz "Array"
  val tArrayzh             : ty -> ty       = fn t => Tapp (Tcon tcArrayzh, t)
  val tcByteArrayzh        : tcon qualified = pvz "ByteArray"
  val tByteArrayzh         : ty             = Tcon tcByteArrayzh
  val ktByteArrayzh        : kind           = Kunlifted
  val tcMutableArrayzh     : tcon qualified = pvz "MutableArray"
  val tMutableArrayzh      : ty -> ty -> ty = fn s => fn a => Tapp (Tapp (Tcon tcMutableArrayzh, s), a)
  val ktMutableArrayzh     : kind           = Karrow (Klifted, Karrow (Klifted, Kunlifted))
  val tcMutableByteArrayzh : tcon qualified = pvz "MutableByteArray"
  val tMutableByteArrayzh  : ty -> ty       = fn s => Tapp (Tcon tcMutableByteArrayzh, s)
  val ktMutableByteArrayzh : kind           = Karrow (Klifted, Kunlifted)

  val tcTVarzh             : tcon qualified = pvz "TVar"
  val tTVarzh              : ty -> ty -> ty = fn s => fn a => Tapp (Tapp (Tcon tcTVarzh, s), a)
  val tcMVarzh             : tcon qualified = pvz "MVar"
  val tMVarzh              : ty -> ty -> ty = fn s => fn a => Tapp (Tapp (Tcon tcMVarzh, s), a)
  val tcMutVarzh           : tcon qualified = pvz "MutVar"
  val tMutVarzh            : ty -> ty -> ty = fn s => fn a => Tapp (Tapp (Tcon tcMutVarzh, s), a)
  val errorVals : (var * ty) list = []

 
  val primId = pv o CHU.zEncodeString

  val tIntzh       : ty = Tcon (primId "Int#")
  val tInt64zh     : ty = Tcon (primId "Int64#")
  val tWordzh      : ty = Tcon (primId "Word#")
  val tWord64zh    : ty = Tcon (primId "Word64#")
  val tByteArrayzh : ty = Tcon (primId "ByteArray#")
  val tCharzh      : ty = Tcon (primId "Char#")
  val tFloatzh     : ty = Tcon (primId "Float#")
  val tDoublezh    : ty = Tcon (primId "Double#")
  val tIntegerzh   : ty = Tcon (primId "Integer#")

  val tcWeakPtrzh : tcon qualified = pvz "Weak"
  val tWeakPtrzh  : ty -> ty       = fn t => Tapp (Tcon tcWeakPtrzh, t)
  val tcStablePtrzh : tcon qualified = pvz "StablePtr"
  val tStablePtrzh  : ty -> ty       = fn t => Tapp (Tcon tcStablePtrzh, t)
  val tcIO          : tcon qualified = (SOME (CHU.mkBaseMname "IOBase"), "IO")
end



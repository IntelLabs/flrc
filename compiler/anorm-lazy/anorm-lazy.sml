(* COPYRIGHT_NOTICE_1 *)
(*
 * ANormLazy is a lazy variant of A-normal form with:
 * 1. inlined and simplified type information (no more type declarations);
 * 2. explicit effects and strictness annotation.
 *
 * effect Annotation:
 *
 * Since in a lazy language we do not consider non-termination to be
 * a side effect, only functions can be effectful. We annotate function's 
 * type with its effect. 
 *
 * Since ANormLazy is a result from translations from GHC Core, all 
 * effectful function application returns a unboxed tuple (multiple 
 * return). We also annotate multi-binding (VbMulti) with a boolean 
 * flag to indicate if the binding is a result of effectful computation.
 * Effectful bindings need to be treated specially during strictness
 * analysis.
 * 
 * Strictness Annotation:
 *
 * In addition to noting the strictness of constructor application,
 * we annotate all variable introduction with strictness. For
 * example, let x = e1 in e2 is equivalent to (\x -> e2) e1, by
 * probing function \x -> e2, we can figure out whether it is 
 * strict in x, and may choose to evaluate e1 strictly in subsequent
 * optimizations.
 *)
structure ANormLazy =
struct
  type strictness = CoreHs.strictness
  type effect = Effect.set
  type effectful = bool
  type var   = Identifier.variable
  type con   = Identifier.name * int
  type lit   = CoreHs.coreLit
  type mname = string
  type pname = string
  type cc    = CoreHs.callconv
  type 'a primTy = 'a GHCPrimType.primTy

  datatype ty 
      = Data                             (* lifted type   *)
      | Prim  of ty GHCPrimType.primTy   (* unlifted type *)
      | Arr   of ty * ty * effect option (* function type *)
      | Sum   of (con * (ty * strictness) list) list   (* Sum type *)
      | Tname of var                     (* named type (whose definition can be looked up) *)

  datatype vBind = VbSingle of var * ty * strictness   (* single binding, may introduce thunk if not strict *)
                 | VbMulti of (var * ty) List.t * effectful (* multiple return binding, no thunk introduced *)

  datatype exp 
      = Var      of var
      | PrimApp  of GHCPrimOp.primOp * var list           (* saturated prim application *)
      | ExtApp   of pname * cc * string * ty * var list   (* saturated extern application *)
      | ConApp   of con * (var * strictness) list         (* saturated data constructor *)
      | Multi    of var list                              (* unboxed tuple / multi-value *)
      | App      of exp * var                             (* application *)
      | Lam      of (var * ty * strictness) * exp         (* lambda *)
      | Let      of vDefg * exp
      | Case     of exp * (var * ty) * ty * alt list 
      | Lit      of lit * ty
      | Cast     of exp * ty * ty  (* from, to *)

  and alt
      = Acon of con * (var * ty * strictness) list * exp
      | Alit of lit * ty * exp
      | Adefault of exp

  and vDef
      = Vdef of vBind * exp

  and vDefg 
      = Rec of vDef list
      | Nonrec of vDef

  type tDef = var * ty 

  type im = ty Identifier.symbolTable

  datatype module 
      = Module of var * vDefg list 

  type t = module * im
end

signature ANORM_LAZY_LAYOUT =
sig
  val layoutTy     : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.ty     -> Layout.t
  val layoutAlt    : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.alt    -> Layout.t
  val layoutExp    : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.exp    -> Layout.t
  val layoutVDef   : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.vDef   -> Layout.t
  val layoutVDefg  : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.vDefg  -> Layout.t
  val layoutTDef   : ANormLazy.ty Identifier.SymbolInfo.t -> ANormLazy.tDef   -> Layout.t
  val layoutModule : ANormLazy.t -> Layout.t
end

structure ANormLazyLayout :> ANORM_LAZY_LAYOUT =
struct
  open ANormLazy
  structure CP = CoreHsPrims
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure U = Utils
  structure L = Layout
  structure I = Identifier
  structure IS = Identifier.SymbolInfo
  structure UO = Utils.Option

  fun indent t = L.indent (t, 2)
  fun angleList l = L.sequence ("<", ">", ",") l

  fun semiMap f 
    = fn []  => []
      |  [x] => [f x]
      |  l   => map (fn v => L.seq [f v, L.str ";"]) l

  fun layoutTy im 
    = fn Data         => L.str "%data"
      |  Tname v      => IS.layoutVariable (v, im)
      |  Prim ty      => L.seq [L.str "%primtype ", GHCPrimTypeLayout.layoutPrimTy (L.paren o layoutTy im) ty]
      |  Arr (t1, t2, effect) => L.mayAlign [layoutTy im t1, 
                                  L.seq [ UO.dispatch (effect, Effect.layout, fn () => L.empty), L.str "-> ", layoutTy im t2]]
      |  Sum cons     =>
         let 
           fun layoutTy1 (ty, strict) = 
               if strict then L.seq [L.str "!", layoutTy im ty] else layoutTy im ty
           fun layCon ((con, _), tys) = L.seq [IS.layoutName (con, im), L.str " ", angleList (map layoutTy1 tys)]
         in
           L.sequence ("{", "}", ",") (List.map (cons, layCon))
         end

  fun layoutTDef im (v, t) = L.seq [L.str "%data ", IS.layoutVariable (v, im), L.str  " = ", layoutTy im t]

  fun isPrimTy (Prim _) = true
    | isPrimTy _ = false

  fun layoutVBind im (v, ty, strict) = 
      L.seq [L.str "<", IS.layoutVariable (v, im), L.str " :: ", 
             if strict then L.seq [L.str "!", layoutTy im ty] else layoutTy im ty, L.str ">"]

  fun layoutVBinds im vbs = L.sequence ("", "", " ") (List.map (vbs, layoutVBind im))

  fun layoutVBinds1 im vbs = 
      let
        fun layoutVBind1 (v, t, s) = 
          let
            val l = layoutVBind im (v, t, s)
          in
            if s then L.seq [L.str "!", l] else l
          end
      in L.sequence ("", "", " ") (List.map (vbs, layoutVBind1))
      end

  fun layoutLiteral l = CL.layoutCoreLit l

  fun layoutVariables im vs = angleList (List.map (vs, fn v => IS.layoutVariable (v, im)))
  
  fun layoutVariableTys im vts = angleList (List.map (vts, fn (v, t) => 
        L.seq [IS.layoutVariable (v, im), L.str " :: ", layoutTy im t]))

  fun layoutAlt im 
    = fn Acon ((con, _), vbs, e) =>
        L.mayAlign [ L.seq [IS.layoutName (con, im), L.str " ", layoutVBinds1 im vbs, L.str " ->"]
                   , indent (layoutExp im e)]
      |  Alit (l, ty, e)    =>
        L.mayAlign [ L.seq [layoutLiteral l, L.str " :: ", layoutTy im ty, L.str " ->"]
                   , indent (layoutExp im e)]
      | Adefault e          => L.mayAlign [ L.str "%_ -> ", indent (layoutExp im e)]

  and layoutAExp im 
    = fn Var x => IS.layoutVariable (x, im)
      |  PrimApp (f, xs) => 
         L.seq [CL.layoutQName (CP.pv (GHCPrimOp.toString f)), L.str " ", layoutVariables im xs]
      |  ExtApp (p, cc, n, t, xs) =>
         L.paren (L.seq [ L.paren (L.seq [ L.str "%external "
                                         , CL.layoutCC cc
                                         , L.str (" \"" ^ CL.escape p ^ "\" \"" ^ CL.escape n ^ "\"")
                                         , L.str "::", L.paren (layoutTy im t) ])
                        , L.str " ", layoutVariables im xs])
      |  ConApp ((c, _), xs) => L.seq [IS.layoutName (c, im), layoutVariables im (List.map (xs, #1))]
      |  Multi xs => layoutVariables im xs
      |  App (f, x)  => L.seq [layoutAExp im f, L.str " ", IS.layoutVariable (x, im)]
      |  Lit (l, ty) => L.paren (L.seq [layoutLiteral l, L.str " :: ", layoutTy im ty])
      |  Cast (e, tf, tt) => L.paren (L.seq [ L.str "%cast ", layoutAExp im e, 
                                              L.str " %from ", layoutTy im tf,
                                              L.str " %to ", layoutTy im tt])
      |  e           => L.paren (layoutExp im e)

  and layoutLamExp (im, bs)
    = fn Lam (b, e) => layoutLamExp (im, b :: bs) e
      |  e => L.mayAlign [ L.seq [ layoutVBinds im (List.rev bs), L.str " ->" ], layoutExp im e]

  and layoutExp im
    = fn Lam (b, e)  => L.seq [L.str "\\ ", layoutLamExp (im, [b]) e]
      |  Let (vd, e) => L.align [ L.seq [L.str "%let ", layoutVDefg im vd ], L.seq [L.str "%in ", layoutExp im e]]
      |  Case (e, (v, vty), ty, alts) =>
         L.align [ L.seq [L.str "%case ", layoutTy im ty, L.str " ", layoutExp im e, L.str " of "
                         , layoutVBind im (v, vty, true) ]
                 , indent (L.sequence ("{", "}", ";") (map (layoutAlt im) alts))]
      |  e => layoutAExp im e

  and layoutVDef im (Vdef (bind, e)) = 
      let
        val (vts, header) = 
            case bind
             of VbSingle (v, t, s) => ([(v, t)], if s then L.str "!" else L.empty)
              | VbMulti (vts, effectful) => (vts, L.str (if effectful then "multi# " else "multi "))
      in
        L.mayAlign [ L.seq [ header, layoutVariableTys im vts, L.str " =" ], indent (layoutExp im e)]
      end

  and layoutVDefg im 
    = fn Rec vdefs   => L.mayAlign [ L.str "%rec {"
                                 , indent (L.align (semiMap (layoutVDef im) vdefs))
                                 , L.str "}"]
      |  Nonrec vdef => layoutVDef im vdef

  fun layoutModule (Module (_, vdefgs), im) = 
      let
        val variables = I.listVariables im
        val im = IS.SiTable im
      in
      L.align [ L.str "%variables"
              , indent (L.align (List.map (variables,
                                           fn x => L.seq [ IS.layoutVariable (x, im)
                                                         , L.str " : "
                                                         , layoutTy im (IS.variableInfo (im, x)) ])))
              , L.seq [L.str "%module"]
              , indent (L.align (semiMap (layoutVDefg im) vdefgs))
              , L.str "\n"]
      end

end

(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

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
  type name  = Identifier.name
  type var   = Identifier.variable
  type con   = name * int
  type lit   = CoreHs.coreLit
  type mname = string
  type pname = string
  type cc    = CoreHs.callconv
  type 'a primTy = 'a GHCPrimType.primTy

  datatype ty_
      = Data                             (* lifted type   *)
      | Prim  of ty primTy               (* unlifted type *)
      | Arr   of ty * ty * effect option (* function type *)
      | Sum   of (con * (ty * strictness) list) list   (* Sum type *)

  withtype ty = ty_ TypeRep.rep

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

  type symbolTable = ty Identifier.symbolTable

  type symbolInfo = ty Identifier.SymbolInfo.t

  type typeManager = ty_ TypeRep.manager

  datatype module 
      = Module of var * vDefg list 

  type t = module * symbolTable * typeManager

  val eqTy_ : ty_ TypeRep.baseEq
    = fn f => fn (x, y) =>
      (case (x, y)
        of (Data, Data) => true
         | (Prim a, Prim b) => GHCPrimType.eqPrimTy f (a, b)
         | (Arr (a, b, c), Arr (u, v, w)) => 
          f (a, u) andalso f (b, v) andalso c = w
         | (Sum xs, Sum ys) => 
          let
            fun h ((t1, s1), (t2, s2)) = f (t1, t2) andalso s1 = s2
            fun g (((u, i), l), ((v, j), m)) 
              = u =v andalso i = j andalso List.length l = List.length m andalso 
                List.forall (List.zip (l,m), h)
          in
            List.length xs = List.length ys andalso 
            List.forall (List.zip (xs, ys), g)
          end
         | _ => false)

  val rec hashTy_ : ty_ TypeRep.baseHash
    = fn hashRep => fn t =>
       (case t 
          of Data   => 0w1
           | Prim p => TypeRep.hash2 (0w2, GHCPrimType.hashPrimTy hashRep p)
           | Arr (t1, t2, _) => TypeRep.hash3 (0w3, hashRep t1, hashRep t2)
           | Sum arms => 
            let
              fun doArm ((_, i), tys) 
                = TypeRep.hash2 (Word.fromInt i, TypeRep.hashList (List.map (tys, hashRep o #1)))
            in
              TypeRep.hash2 (0w4, TypeRep.hashList (List.map (arms, doArm)))
            end)
end

signature ANORM_LAZY_LAYOUT =
sig
  type env = Config.t * ANormLazy.symbolInfo
  val controls     : Config.Control.control list
  val layoutTy     : env * ANormLazy.ty     -> Layout.t
  val layoutAlt    : env * ANormLazy.alt    -> Layout.t
  val layoutExp    : env * ANormLazy.exp    -> Layout.t
  val layoutVDef   : env * ANormLazy.vDef   -> Layout.t
  val layoutVDefg  : env * ANormLazy.vDefg  -> Layout.t
  val layoutTDef   : env * ANormLazy.tDef   -> Layout.t
  val layoutModule : Config.t * ANormLazy.t -> Layout.t
end

structure ANormLazyLayout :> ANORM_LAZY_LAYOUT =
struct
  open ANormLazy
  structure CP = CoreHsPrims
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure U = Utils
  structure L = Layout
  structure LU = LayoutUtils
  structure I = Identifier
  structure IS = Identifier.SymbolInfo
  structure UO = Utils.Option

  val modulename = "ANormLazyLayout"
  val indent = LU.indent 
  fun angleList l = L.sequence ("<", ">", ",") l

  type options = {
       showBinderTypes : bool,
       showSymbolTable : bool
  }
                 
  val describe =
   fn () =>
      L.align [L.str (modulename ^ " control string consists of:"),
               LU.indent (L.align [L.str "b => show types on variable binders",
                                   L.str "S => show symbol table",
                                   L.str "+ => show all of the above"]),
               L.str "default is nothing"]
      
  val parse =
   fn str =>
      let
        val binderTyps = ref false
        val symbols    = ref false
        fun doOne c =
            case c
             of #"b" => let val () = binderTyps := true in true end
              | #"S" => let val () = symbols := true in true end
              | #"+" =>
                let
                  val () = binderTyps := true
                  val () = symbols := true
                in true
                end
              | _    => false
      in
        if List.forall (String.explode str, doOne) then
          SOME ({showBinderTypes = !binderTyps, showSymbolTable = !symbols})
        else
          NONE
      end
      
  val dft = fn _ =>({showBinderTypes = false, showSymbolTable = false})
                   
  val (control, controlGet) = Config.Control.mk (modulename, describe, parse, dft)

  type env = Config.t * ANormLazy.symbolInfo

  val getCfg = #1 
  val getIM  = #2
  val showBinderTypes = fn env => #showBinderTypes (controlGet (getCfg env))
  val showSymbolTable = fn env => #showSymbolTable (controlGet (getCfg env))

  fun semiMap (l, f)
    = (case l
        of [] => []
         | [x] => [f x]
         | l => List.map (l, fn v => L.seq [f v, L.str ";"]))

  and layoutTy (env, ty) 
    = (case TypeRep.repToBase ty
        of Data         => L.str "%data"
         | Prim ty      => L.seq [L.str "%primtype ", 
                                  GHCPrimTypeLayout.layoutPrimTy (fn t => L.paren (layoutTy (env, t))) ty]
         | Arr (t1, t2, effect) => L.mayAlign [layoutTy (env, t1), 
                                     L.seq [ UO.dispatch (effect, Effect.layout, fn () => L.empty), 
                                             L.str "-> ", layoutTy (env, t2) ]]
         | Sum cons     =>
          let 
            fun layoutTy1 (ty, strict) = 
                if strict then L.seq [L.str "!", layoutTy (env, ty)] else layoutTy (env, ty)
            fun layCon ((con, _), tys) = L.seq [IS.layoutName (con, getIM env), L.str " ", angleList (map layoutTy1 tys)]
          in
            L.sequence ("{", "}", ",") (List.map (cons, layCon))
          end)

  fun layoutTDef (env, (v, t)) = L.seq [L.str "%data ", IS.layoutVariable (v, getIM env), L.str  " = ", layoutTy (env, t)]

  fun isPrimTy (Prim _) = true
    | isPrimTy _ = false

  fun layoutVBind (env, (v, ty, strict)) = 
      if showBinderTypes env then
        L.seq [L.str "<", IS.layoutVariable (v, getIM env), L.str " :: ", 
               if strict then L.seq [L.str "!", layoutTy (env, ty)] else layoutTy (env, ty), L.str ">"]
      else IS.layoutVariable (v, getIM env)

  fun layoutVBinds (env, vbs) 
    = L.sequence ("", "", " ") (List.map (vbs, fn b => layoutVBind (env, b)))

  fun layoutVBinds1 (env, vbs) = 
      let
        fun layoutVBind1 (v, t, s) = 
          let
            val l = layoutVBind (env, (v, t, s))
          in
            if s then L.seq [L.str "!", l] else l
          end
      in L.sequence ("", "", " ") (List.map (vbs, layoutVBind1))
      end

  fun layoutLiteral (env, l) = CL.layoutCoreLit (getCfg env, l)

  fun layoutVariables (env, vs) = angleList (List.map (vs, fn v => IS.layoutVariable (v, getIM env)))
  
  fun layoutVariableTys (env, vts) = angleList (List.map (vts, fn (v, t) => 
      if showBinderTypes env then
        L.seq [IS.layoutVariable (v, getIM env), L.str " :: ", layoutTy (env, t)]
      else IS.layoutVariable (v, getIM env)))

  fun layoutAlt (env, alt) 
    = (case alt
       of Acon ((con, _), vbs, e) =>
         L.mayAlign [ L.seq [IS.layoutName (con, getIM env), L.str " ", layoutVBinds1 (env, vbs), L.str " ->"]
                    , indent (layoutExp (env, e))]
        | Alit (l, ty, e) =>
         L.mayAlign [ L.seq [layoutLiteral (env, l), L.str " :: ", layoutTy (env, ty), L.str " ->"]
                    , indent (layoutExp (env, e))]
        | Adefault e => L.mayAlign [ L.str "%_ -> ", indent (layoutExp (env, e))])

  and layoutAExp (env, e) 
    = (case e
        of Var x => IS.layoutVariable (x, getIM env)
         | PrimApp (f, xs) => 
          L.seq [CL.layoutQName (getCfg env, CP.pv (GHCPrimOp.toString f)), L.str " ", layoutVariables (env, xs)]
         | ExtApp (p, cc, n, t, xs) =>
          L.paren (L.seq [ L.paren (L.seq [ L.str "%external "
                                          , CL.layoutCC (getCfg env, cc)
                                          , L.str (" \"" ^ CL.escape p ^ "\" \"" ^ CL.escape n ^ "\"")
                                          , L.str "::", L.paren (layoutTy (env, t)) ])
                          , L.str " ", layoutVariables (env, xs)])
         | ConApp ((c, _), xs) => L.seq [IS.layoutName (c, getIM env), layoutVariables (env, List.map (xs, #1))]
         | Multi xs => layoutVariables (env, xs)
         | App (f, x)  => L.seq [layoutAExp (env, f), L.str " ", IS.layoutVariable (x, getIM env)]
         | Lit (l, ty) => L.paren (L.seq [layoutLiteral (env, l), L.str " :: ", layoutTy (env, ty)])
         | Cast (e, tf, tt) => L.paren (L.seq [ L.str "%cast ", layoutAExp (env, e),
                                                L.str " %from ", layoutTy (env, tf),
                                                L.str " %to ", layoutTy (env, tt)])
         | e           => L.paren (layoutExp (env, e)))

  and layoutLamExp (env, bs)
    = fn Lam (b, e) => layoutLamExp (env, b :: bs) e
      |  e => L.mayAlign [ L.seq [ layoutVBinds (env, List.rev bs), L.str " ->" ], layoutExp (env, e)]

  and layoutExp (env, e)
    = (case e
        of Lam (b, e)  => L.seq [L.str "\\ ", layoutLamExp (env, [b]) e]
         | Let (vd, e) => L.align [ L.seq [L.str "%let ", layoutVDefg (env, vd)], 
                                    L.seq [L.str "%in ", layoutExp (env, e)]]
         | Case (e, (v, vty), ty, alts) =>
           L.align [ L.seq [L.str "%case ", layoutTy (env, ty), L.str " ", layoutExp (env, e), 
                     L.str " of ", layoutVBind (env, (v, vty, true)) ]
                   , indent (L.sequence ("{", "}", ";") (List.map (alts, fn a => layoutAlt (env, a))))]
         | e => layoutAExp (env, e))

  and layoutVDef (env, Vdef (bind, e)) = 
      let
        val (vts, header) = 
            case bind
             of VbSingle (v, t, s) => ([(v, t)], if s then L.str "!" else L.empty)
              | VbMulti (vts, effectful) => (vts, L.str (if effectful then "multi# " else "multi "))
      in
        L.mayAlign [ L.seq [ header, layoutVariableTys (env, vts), L.str " =" ], indent (layoutExp (env, e))]
      end

  and layoutVDefg (env, vdef)
    = (case vdef
        of Rec vdefs => L.mayAlign [ L.str "%rec {"
                                   , indent (L.align (semiMap (vdefs, fn d => layoutVDef (env, d))))
                                   , L.str "}"]
      |  Nonrec vdef => layoutVDef (env, vdef))

  fun layoutModule (cfg, (Module (_, vdefgs), im, tm)) = 
      let
        val variables = I.listVariables im
        val im = IS.SiTable im
        val env = (cfg, im)
        val variables = L.align (List.map (variables,
                fn x => L.seq [ IS.layoutVariable (x, im)
                              , L.str " :: "
                              , layoutTy (env, IS.variableInfo (im, x)) ]))
      in
      L.align [ if showSymbolTable env then L.align [ L.str "%variables", indent variables ] else L.empty
              , L.seq [L.str "%module"]
              , indent (L.align (semiMap (vdefgs, fn d => layoutVDefg (env, d))))
              , L.str "\n"]
      end

  val controls = [control]

end

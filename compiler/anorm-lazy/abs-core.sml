(* COPYRIGHT_NOTICE_1 *)

signature ABS_CORE =
sig
  structure Dom : ABS_DOMAIN
  type strictness = CoreHs.strictness
  type effectful = ANormLazy.effectful
  type var   = Identifier.variable
  type con   = Identifier.name 
  type lit   = CoreHs.coreLit
  type mname = string
  type pname = string
  type cc    = CoreHs.callconv

  datatype exp 
    = Var   of var
    | Multi of var list
    | Con   of con * (var * strictness) list
    | App   of exp * var
    | Lam   of var * exp
    | Let   of vDefg * exp
    | GLB   of var list
    | LUB   of var list
    | Cond  of exp * exp 
    | Const of Dom.t

  and vDefg 
    = Rec of vDef list
    | Nonrec of vDef

  and vDef 
    = Vdef of vBind * exp

  and vBind 
    = VbSingle of var
    | VbMulti  of var list * effectful

  type im = ANormLazy.ty Identifier.symbolTable

  datatype module 
      = Module of var * vDefg list 

  type t = module * im
end

functor AbsCoreF (structure Dom : ABS_DOMAIN) : ABS_CORE =
struct
  structure Dom = Dom
  type strictness = CoreHs.strictness
  type effectful = ANormLazy.effectful
  type var   = Identifier.variable
  type con   = Identifier.name 
  type lit   = CoreHs.coreLit
  type mname = string
  type pname = string
  type cc    = CoreHs.callconv
  type ty    = ANormLazy.ty

  datatype exp 
    = Var   of var
    | Multi of var list
    | Con   of con * (var * strictness) list
    | App   of exp * var
    | Lam   of var * exp
    | Let   of vDefg * exp
    | GLB   of var list
    | LUB   of var list
    | Cond  of exp * exp 
    | Const of Dom.t

  and vDefg 
    = Rec of vDef list
    | Nonrec of vDef

  and vDef 
    = Vdef of vBind * exp

  and vBind 
    = VbSingle of var
    | VbMulti  of var list * effectful

  type im = ty Identifier.symbolTable

  datatype module 
      = Module of var * vDefg list 

  type t = module * im
end

signature ABS_CORE_LAYOUT =
sig
  structure AbsCore : ABS_CORE
  type module 
  type exp
  type vDef
  type vDefg
  type ty
  val layoutExp    : ty Identifier.SymbolInfo.t -> exp    -> Layout.t
  val layoutVDef   : ty Identifier.SymbolInfo.t -> vDef   -> Layout.t
  val layoutVDefg  : ty Identifier.SymbolInfo.t -> vDefg  -> Layout.t
  val layout       : module * ty Identifier.symbolTable -> Layout.t
end

functor AbsCoreLayoutF 
  (structure AbsCore : ABS_CORE
   type ty
   val layoutTy     : ty Identifier.SymbolInfo.t -> ty -> Layout.t
  ) :> ABS_CORE_LAYOUT 
  where type module = AbsCore.module
    and type exp = AbsCore.exp 
    and type vDefg = AbsCore.vDefg
    and type ty = ty
  =
struct
  structure AbsCore = AbsCore
  structure Dom = AbsCore.Dom
  type ty = ty
  type exp = AbsCore.exp
  type vDef = AbsCore.vDef
  type vDefg = AbsCore.vDefg
  open AbsCore
  structure CP = CoreHsPrims
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure U = Utils
  structure L = Layout
  structure I = Identifier
  structure IS = Identifier.SymbolInfo

  fun indent t = L.indent (t, 2)
  fun angleList l = L.sequence ("<", ">", ",") l

  fun semiMap f 
    = fn []  => []
      |  [x] => [f x]
      |  l   => map (fn v => L.seq [f v, L.str ";"]) l

  fun layoutVBind im v = IS.layoutVariable (v, im)

  fun layoutVBinds im vbs = L.sequence ("", "", " ") (List.map (vbs, layoutVBind im))

  fun layoutVBinds1 im vbs = 
      let
        fun layoutVBind1 (v, t, s) = 
          let
            val l = layoutVBind im v
          in
            if s then L.seq [L.str "!", l] else l
          end
      in L.sequence ("", "", " ") (List.map (vbs, layoutVBind1))
      end

  fun layoutLiteral l = CL.layoutCoreLit l

  fun layoutVariables im vs = angleList (List.map (vs, fn v => IS.layoutVariable (v, im)))

  fun layoutVariables' im vs 
    = angleList (List.map (vs, fn (v, strict) => L.seq [ if strict then L.str "!" else L.empty, 
                                                         IS.layoutVariable (v, im) ]))
  
  and layoutAExp im 
    = fn Var x => IS.layoutVariable (x, im)
      |  Multi xs => layoutVariables im xs
      |  Con (c, xs) => L.seq [IS.layoutName (c, im), layoutVariables' im xs]
      |  App (f, x)  => L.seq [layoutAExp im f, L.str " ", IS.layoutVariable (x, im)]
      |  GLB xs => L.seq [L.str "GLB", layoutVariables im xs]
      |  LUB xs => L.seq [L.str "LUB", layoutVariables im xs]
      |  Cond (e1, e2) => L.align [ L.seq [L.str "%ifNotBottom ", layoutExp im e1]
                                  , indent (L.seq [L.str "%then ", layoutExp im e2])]
      |  Const p => Dom.layout p
      |  e       => L.paren (layoutExp im e)

  and layoutLamExp (im, bs)
    = fn Lam (b, e) => layoutLamExp (im, b :: bs) e
      |  e => L.mayAlign [ L.seq [ layoutVBinds im (List.rev bs), L.str " ->" ], layoutExp im e]

  and layoutExp im
    = fn Lam (b, e)  => L.seq [L.str "\\ ", layoutLamExp (im, [b]) e]
      |  Let (vd, e) => L.align [ L.seq [L.str "%let ", layoutVDefg im vd ], L.seq [L.str "%in ", layoutExp im e]]
      |  e => layoutAExp im e

  and layoutVDef im (Vdef (bind, e)) = 
      let
        val (vs, header) = 
            case bind
             of VbSingle v => ([v], L.empty)
              | VbMulti (vs, effectful) => (vs, L.str (if effectful then "multi# " else "multi "))
      in
        L.mayAlign [ L.seq [ header, layoutVariables im vs, L.str " =" ], indent (layoutExp im e)]
      end

  and layoutVDefg im 
    = fn Rec vdefs   => L.mayAlign [ L.str "%rec {"
                                 , indent (L.align (semiMap (layoutVDef im) vdefs))
                                 , L.str "}"]
      |  Nonrec vdef => layoutVDef im vdef

  fun layout (Module (_, vdefgs), im) = 
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

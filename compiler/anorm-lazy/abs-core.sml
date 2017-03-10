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
  type env = Config.t * ty Identifier.SymbolInfo.t
  val layoutExp    : env * exp    -> Layout.t
  val layoutVDef   : env * vDef   -> Layout.t
  val layoutVDefg  : env * vDefg  -> Layout.t
  val layout       : Config.t * ty Identifier.symbolTable * module -> Layout.t
end

functor AbsCoreLayoutF 
  (structure AbsCore : ABS_CORE
   type ty
   val layoutTy     : (Config.t * ty Identifier.SymbolInfo.t) * ty -> Layout.t
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

  type env = Config.t * ty Identifier.SymbolInfo.t
  val getCfg : env -> Config.t = #1
  val getIM  : env -> ty Identifier.SymbolInfo.t = #2

  fun indent t = L.indent (t, 2)
  fun angleList l = L.sequence ("<", ">", ",") l

  fun semiMap (l, f)
    = (case l
        of [] => []
         | [x] => [f x]
         | l => List.map (l, fn v => L.seq [f v, L.str ";"]))

  fun layoutVBind (env, v) = IS.layoutVariable (v, getIM env)

  fun layoutVBinds (env, vbs) = L.sequence ("", "", " ") (List.map (vbs, fn b => layoutVBind (env, b)))

  fun layoutVBinds1 (env, vbs) = 
      let
        fun layoutVBind1 (v, t, s) = 
          let
            val l = layoutVBind (env, v)
          in
            if s then L.seq [L.str "!", l] else l
          end
      in L.sequence ("", "", " ") (List.map (vbs, layoutVBind1))
      end

  fun layoutVariables (env, vs) = angleList (List.map (vs, fn v => IS.layoutVariable (v, getIM env)))

  fun layoutVariables' (env, vs)
    = angleList (List.map (vs, fn (v, strict) => L.seq [ if strict then L.str "!" else L.empty, 
                                                         IS.layoutVariable (v, getIM env) ]))
  
  and layoutAExp (env, e)
    = (case e
        of Var x => IS.layoutVariable (x, getIM env)
         | Multi xs => layoutVariables (env, xs)
         | Con (c, xs) => L.seq [IS.layoutName (c, getIM env), layoutVariables' (env, xs)]
         | App (f, x)  => L.seq [layoutAExp (env, f), L.str " ", IS.layoutVariable (x, getIM env)]
         | GLB xs => L.seq [L.str "GLB", layoutVariables (env, xs)]
         | LUB xs => L.seq [L.str "LUB", layoutVariables (env, xs)]
         | Cond (e1, e2) => L.align [ L.seq [L.str "%ifNotBottom ", layoutExp (env, e1)]
                                    , indent (L.seq [L.str "%then ", layoutExp (env, e2)])]
         | Const p => Dom.layout p
         | e       => L.paren (layoutExp (env, e)))

  and layoutLamExp (env, bs, e)
    = (case e 
        of Lam (b, e) => layoutLamExp (env, b :: bs, e)
         | _ => L.mayAlign [ L.seq [ layoutVBinds (env, List.rev bs), L.str " ->" ], layoutExp (env, e)])

  and layoutExp (env, e)
    = (case e
        of Lam (b, e)  => L.seq [L.str "\\ ", layoutLamExp (env, [b], e)]
         | Let (vd, e) => L.align [ L.seq [L.str "%let ", layoutVDefg (env, vd)]
                                  , L.seq [L.str "%in ", layoutExp (env, e)]]
         | _ => layoutAExp (env, e))

  and layoutVDef (env, Vdef (bind, e)) = 
      let
        val (vs, header) = 
            case bind
             of VbSingle v => ([v], L.empty)
              | VbMulti (vs, effectful) => (vs, L.str (if effectful then "multi# " else "multi "))
      in
        L.mayAlign [ L.seq [ header, layoutVariables (env, vs), L.str " =" ], indent (layoutExp (env, e))]
      end

  and layoutVDefg (env, vdefg)
    = (case vdefg 
        of Rec vdefs   => L.mayAlign [ L.str "%rec {"
                                 , indent (L.align (semiMap (vdefs, fn d => layoutVDef (env, d))))
                                 , L.str "}"]
         | Nonrec vdef => layoutVDef (env, vdef))

  fun layout (cfg, im, Module (_, vdefgs)) = 
      let
        val variables = I.listVariables im
        val im = IS.SiTable im
        val env = (cfg, im)
      in
      L.align [ L.str "%variables"
              , indent (L.align (List.map (variables,
                                           fn x => L.seq [ IS.layoutVariable (x, im)
                                                         , L.str " : "
                                                         , layoutTy (env, IS.variableInfo (im, x)) ])))
              , L.seq [L.str "%module"]
              , indent (L.align (semiMap (vdefgs, fn d => layoutVDefg (env, d))))
              , L.str "\n"]
      end

end

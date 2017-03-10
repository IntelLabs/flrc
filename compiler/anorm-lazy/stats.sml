(* HRC *)
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

signature ANORM_STRICT_STATS =
sig
  datatype options = O of {id: string option}
  val layout : options -> ANormLazy.t * Config.t -> Layout.t
(*  val module : Config.t * ANormLazy.tm * options * ANormLazy.module * Out.t -> unit *)
  val program : Config.t * options * ANormLazy.t * Out.t -> unit
end;

structure ANormLazyStats :> ANORM_STRICT_STATS =
struct

  val passname = "ANormLazyStats"

  structure AL = ANormLazy
  structure WS = WordSet
  structure I = Identifier
  structure L = Layout

  datatype options = O of {id: string option}

  datatype env = E of {config: Config.t, tm : AL.typeManager, options: options}

  val envMk = fn (c, tm, opts) => E {config = c, tm = tm, options = opts}

  val getTM = fn (E { tm = tm, ... }) => tm

  datatype state = S of {
                   tySet    : WS.t ref,
                   expNodes : int ref,
                   tyNodes  : int ref,
                   varUses  : int ref,
                   vDefs    : int ref
                 }

  val stateMk =
   fn () =>
      S {tySet = ref WS.empty, expNodes = ref 0, tyNodes = ref 0, varUses = ref 0, vDefs = ref 0}

  val incr = fn r => r := (!r) + 1

  val incrF = fn sel => fn (S r) => incr (sel r)
  val incrExpNodes = incrF #expNodes
  val incrTyNodes = incrF #tyNodes
  val incrVarUses = incrF #varUses
  val incrVDefs = incrF #vDefs
  val insertTyNode =
   fn (S { tySet = tySet, ... }, env, ty) =>
     tySet := WS.insert (!tySet, TypeRep.hashRepWithManager (getTM env, ty))

  val variableUse = fn (s, e, _) => incrVarUses s
  val analyzeExp = fn (s, e, _) => incrExpNodes s
  val analyzeTy = fn (s, e, t) => incrTyNodes s before insertTyNode (s, e, t)
  val analyzeVDef = fn (s, e, vd) => incrVDefs s

  structure A = ANormLazyAnalyzeF(type state = state
                                    type env = env
                                    val config = fn (E {config, ...}) => config
                                    val variableBind = NONE
                                    val variableUse = SOME variableUse
                                    val analyzeTy = SOME analyzeTy
                                    val analyzeExp = SOME analyzeExp
                                    val analyzeAlt = NONE
                                    val analyzeVDef = SOME analyzeVDef)
  val layoutStats =
   fn (s, e, O {id, ...}) =>
      let
        val S {tySet, expNodes, tyNodes, varUses, vDefs} = s
        val doOne = fn (s, r) => L.seq [L.str ("  Number of " ^ s), Int.layout r]
        val l = L.align [doOne ("exp nodes:          ", !expNodes),
                         doOne ("ty nodes:           ", !tyNodes),
                         doOne ("ty nodes (unique):  ", WS.size (!tySet)),
                         doOne ("ty nodes (managed): ", TypeRep.size (getTM e)),
                         doOne ("var uses:           ", !varUses),
                         doOne ("def bindings:       ", !vDefs)]
        val l =
            case id
             of NONE => l
              | SOME id =>
                L.align [L.str ("---------- Stats for: " ^ id),
                         l,
                         L.str ("---------- End stats for: " ^ id ^ "\n")]
      in l
      end

  val layoutMk =
   fn doIt =>
      fn opts => fn (p as (_, _, tm), config) =>
         let
           val s = stateMk ()
           val e = envMk (config, tm, opts)
           val () = doIt (s, e, p)
           val l = layoutStats (s, e, opts)
         in l
         end

  (*
  val module =
   fn (config, opts, m, out) =>
      Layout.outputWidth (layoutMk A.module opts (m, config), 78, out)
  *)

  val layout = layoutMk A.program

  val program =
   fn (config, opts, p, out) =>
      Layout.outputWidth (layout opts (p, config), 78, out)

end;

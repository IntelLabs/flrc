(* The Haskell Research Compiler *)
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

(* Collect free variables *)
signature ANORM_STRICT_FREE_VARS = 
sig
  type 'a t = Config.t * 'a -> Identifier.VariableSet.t
  val exp     : ANormStrict.exp t
  val alt     : ANormStrict.alt t
  (* vDef will include the bound variable in the free set *)
  val vDef    : ANormStrict.vDef t
  (* vDefg will not include the bound variables in the free set *)
  val vDefg   : ANormStrict.vDefg t
  val module  : ANormStrict.module t
end (* signature ANORM_STRICT_FREE_VARS *)

structure ANormStrictFreeVars :> ANORM_STRICT_FREE_VARS = 
struct

  structure AS = ANormStrict
  structure IM = Identifier.Manager
  structure VD = Identifier.VariableDict
  structure RC = ANormStrictRewriterClient

  structure VS = Identifier.VariableSet

  type 'a t = Config.t * 'a -> Identifier.VariableSet.t

  datatype state = S of {frees : VS.t ref, bound : VS.t ref}

  val mkState =
   fn () => S {frees = ref VS.empty, bound = ref VS.empty}

  val finish =
   fn (S {frees, bound, ...}) => VS.difference (!frees, !bound)

  val varUse =
   fn (s as S {frees, ...}, e, v) =>
      let
        val () = frees := VS.insert (!frees, v)
      in ()
      end

  val varBind =
   fn (s as S {bound, ...}, e, v) =>
      let
        val () = bound := VS.insert (!bound, v)
      in e
      end


  structure Analyze = 
  ANormStrictAnalyzeF(struct
                        type state = state
                        type env = Config.t
                        val config = fn c => c
                        val variableBind  = SOME varBind
                        val variableUse = SOME varUse
                        val analyzeTy = NONE
                        val analyzeExp = NONE
                        val analyzeAlt = NONE
                        val analyzeVDef = NONE
                        val analyzeVDefg = NONE
                      end)
  val lift = 
   fn f => 
   fn (c, e) => 
      let
        val state = mkState ()
        val _ = f (state, c, e)
      in finish state
      end

  val exp = lift Analyze.exp
  val alt = lift Analyze.alt
  val vDef = lift Analyze.vDef
  val vDefg = lift Analyze.vDefg
  val module = lift Analyze.module
end  (* structure ANormStrictFreeVars *)

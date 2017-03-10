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

(* Rename all bound variables *)
signature ANORM_STRICT_CLONE = 
sig
  type 'a t = ANormStrict.symbolTableManager * Config.t * 'a -> 'a


  val exp     : ANormStrict.exp t
  val alt     : ANormStrict.alt t
  val vDef    : ANormStrict.vDef t
  val vDefg   : ANormStrict.vDefg t
  val module  : ANormStrict.module t
end (* signature ANORM_STRICT_CLONE *)

structure ANormStrictClone :> ANORM_STRICT_CLONE = 
struct

  structure AS = ANormStrict
  structure IM = Identifier.Manager
  structure VD = Identifier.VariableDict
  structure RC = ANormStrictRewriterClient

  type 'a t = ANormStrict.symbolTableManager * Config.t * 'a -> 'a

  structure Rewrite = 
  ANormStrictRewriterF(struct
                         type state = AS.symbolTableManager
                         type env = Config.t * AS.var VD.t
                         val config   : env -> Config.t = 
                          fn (c, vd) => c
                         val bind     : (state, env, Identifier.variable) RC.binder = 
                          fn (im, (c, map), v) => 
                             let 
                               val v2 = IM.variableClone (im, v)
                               val map = VD.insert (map, v, v2)
                             in ((c, map), SOME v2)
                             end
                         val variable : (state, env, Identifier.variable) RC.rewriter = 
                             fn (im, (c, map), v) => 
                                (case VD.lookup (map, v)
                                  of SOME v2 => RC.StopWith ((c, map), v2)
                                   | NONE    => RC.Stop)
                         val exp      : (state, env, AS.exp) RC.rewriter = 
                          fn _ => RC.Continue
                         val alt      : (state, env, AS.alt) RC.rewriter = 
                          fn _ => RC.Continue
                         val vDef     : (state, env, AS.vDef) RC.rewriter =
                          fn _ => RC.Continue
                         val vDefg    : (state, env, AS.vDefg) RC.rewriter = 
                          fn _ => RC.Continue
                         val module   : (state, env, AS.module) RC.rewriter =
                          fn _ => RC.Continue
                       end)

  val lift = 
   fn f => fn (im, c, e) => f (im, (c, VD.empty), e)
  val exp = lift Rewrite.exp
  val alt = lift Rewrite.alt
  val vDef = lift Rewrite.vDef
  val vDefg = 
      fn args => 
         let
           val (_, vDefg) = lift Rewrite.vDefg args
         in vDefg
         end
  val module = lift Rewrite.module
end  (* structure ANormStrictClone *)

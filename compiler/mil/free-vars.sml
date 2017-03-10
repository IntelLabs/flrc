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


signature MIL_FREE_VARS =
sig
  type t
  val rhs : t * Mil.rhs -> Identifier.VariableSet.t
  val instruction : t * Mil.instruction -> Identifier.VariableSet.t
  val call : t * Mil.call -> Identifier.VariableSet.t
  val eval : t * Mil.eval -> Identifier.VariableSet.t
  val transfer : t * Mil.transfer -> Identifier.VariableSet.t
  val block : t * Mil.label * Mil.block -> Identifier.VariableSet.t
  val blocks : t * Mil.block Mil.LD.t -> Identifier.VariableSet.t
  val codeBody : t * Mil.codeBody -> Identifier.VariableSet.t
  val global : t * Mil.variable * Mil.global -> Identifier.VariableSet.t
  val program : t * Mil.t -> Identifier.VariableSet.t
end;

structure MilFreeVars :> MIL_FREE_VARS where type t = Config.t =
struct
  type t = Config.t

  structure VS = Identifier.VariableSet

  datatype state = S of {frees : VS.t ref, bound : VS.t ref}

  fun mkState () = S {frees = ref VS.empty, bound = ref VS.empty}
  fun finish (S {frees, bound, ...}) = VS.difference (!frees, !bound)

  fun varUse (s as S {frees, ...}, e, v) =
      let
        val () = frees := VS.insert (!frees, v)
      in ()
      end

  fun varBind (s as S {bound, ...}, e, v) =
      let
        val () = bound := VS.insert (!bound, v)
      in e
      end

  structure MA = MilAnalyseF (
                 struct 
                   type state = state
                   type env = Config.t
                   fun config c = c
                   val indent = 2
                   val externBind = SOME varBind
                   val variableBind = SOME varBind
                   val labelBind = NONE
                   val variableUse = SOME varUse
                   val analyseJump = NONE
                   val analyseCut = NONE
                   val analyseConstant = NONE
                   val analyseInstruction = NONE
                   val analyseTransfer = NONE
                   val analyseBlock = NONE
                   val analyseGlobal = NONE
                 end)

  fun mk1 af =
      fn (config, x) =>
         let
           val state = mkState ()
           val _ = af (state, config, x)
         in finish state
         end

  fun mk2 af =
      fn (config, x, y) =>
         let
           val state = mkState ()
           val _ = af (state, config, x, y)
         in finish state
         end

  val rhs = mk1 MA.analyseRhs
  val instruction = mk1 MA.analyseInstruction
  val call = mk1 MA.analyseCall
  val eval = mk1 MA.analyseEval
  val transfer = mk1 (fn (state, config, y) => MA.analyseTransfer (state, config, NONE, y))
  val block = mk2 MA.analyseBlock
  val blocks = mk1 MA.analyseBlocks
  val codeBody = mk1 MA.analyseCodeBody
  val global = mk2 MA.analyseGlobal
  val program = mk1 MA.analyseProgram

end;

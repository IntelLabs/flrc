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


signature MIL_REP_SHOW = 
sig
  val annotate : PassData.t * MilRepSummary.summary * Mil.t -> Mil.t 
  val printAnalysis : PassData.t * MilRepSummary.summary * Mil.t -> unit
  val printReasons : PassData.t * MilRepSummary.summary * Mil.t -> unit
end (* signature MIL_REP_SHOW *)

structure MilRepShow :> MIL_REP_SHOW = 
struct
  structure PD = PassData
  structure M = Mil
  structure MU = MilUtils
  structure ST = Identifier
  structure STM = Identifier.Manager

  structure MRB = MilRepBase
  structure MRS = MilRepSummary
  structure Node = MilRepNode

  structure ID = IntDict
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure ND = Mil.ND
  structure VS = Mil.VS
  structure LS = Mil.LS
  structure I = Identifier
  structure IVD = I.ImpVariableDict
  structure L = Layout
  structure LU = LayoutUtils

  val annotate = 
   fn (pd, summary, p) =>
      let
        val M.P {includes, externs, entry, globals, symbolTable} = p
        val stm = STM.fromExistingNoInfo symbolTable
        val renameVar = 
         fn (v, rename) => 
            if MRS.variableHasNode (summary, v) then 
              let
                val info = ST.variableInfo (symbolTable, v)
                val id = MRS.variableClassId (summary, v)
                val string = Int.toString id
                val string = 
                    if MRS.variableUsesKnown (summary, v) then
                      string
                    else 
                      string ^ "^"
                val string = 
                    if MRS.variableDefsKnown (summary, v) then
                      string
                    else
                      string ^ "?"
                val string = "id="^string
                val string = 
                    let
                      val info = STM.variableString (stm, v)
                      val info = 
                          case String.findSubstring (info, {substring = ".id="})
                           of SOME i => String.prefix (info, i)
                            | NONE => info
                      val string = info^"."^string
                    in string
                    end
                val newv = STM.variableFresh (stm, string, info)
              in Rename.renameTo (rename, v, newv)
              end
            else
              let
                val info = ST.variableInfo (symbolTable, v)
                val () = STM.variableSetInfo (stm, v, info)
              in rename
              end
        val keepVar = 
         fn v => 
            let
              val info = ST.variableInfo (symbolTable, v)
              val () = STM.variableSetInfo (stm, v, info)
            in ()
            end
        val vars = STM.variablesList stm
        val (vars, varsE) = 
            let
              val extern = 
               fn v => (case MilUtils.SymbolTable.variableKind (symbolTable, v)
                         of Mil.VkExtern => true
                          | _            => false)
              val {yes, no} = List.partition (vars, extern)
            in (no, yes)
            end
        val rename = List.fold (vars, Rename.none, renameVar)
        val () = List.foreach (varsE, keepVar)
        val st = STM.finish stm
        val p = M.P {includes = includes, externs = externs, entry = entry, globals = globals, symbolTable = st}
        val p = MilRename.Var.program (PD.getConfig pd, rename, p)
      in p
      end


  val printAnalysis = 
   fn (pd, summary, p) => 
      let
        val l = MilRepSummary.layout (summary, Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p))
        val l = L.align [L.str "ANALYSIS RESULTS",
                         LU.indent l]
      in LayoutUtils.printLayout l
      end

  val printReasons = 
   fn (pd, summary, p) => 
      let
        val l = MilRepSummary.layoutReasons (summary, Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p))
        val l = L.align [L.str "ANALYSIS ESCAPE/INTRUDES REASONS",
                         LU.indent l]
      in LayoutUtils.printLayout l
      end

end (* structure MilRepShow *)

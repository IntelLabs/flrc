(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Print multiple annotated call graphs to a single or multi files.  *)

signature ANNOTATED_CG_PRINTER =
sig

  val printCallGraph  : PassData.t * IMil.t * string -> unit  

  val printCallGraph' : PassData.t * IMil.t * string *
                        (Mil.label -> IntInf.t option) option -> unit  

end

structure AnnotatedCGPrinter :> ANNOTATED_CG_PRINTER = 
struct

  (* Aliases *)
  structure PD  = PassData
  structure M   = Mil
  structure L   = Layout
  structure ID  = Identifier
  structure IM  = ID.Manager
  structure MU = MilUtils

  structure MCG = MilCallGraph

  (* First time we print the call graph?. *)
  val firstPrint = ref true

  (* Print the call graph into a given file name *)
  val printCallGraph' : PassData.t * IMil.t * string *
                        (Mil.label -> IntInf.t option) option -> unit =
   fn (d, imil, graphLabel, blkFreq) =>
      let
        val red    = L.str ("color=red")
        val blue   = L.str ("color=blue")
        val black  = L.str ("color=black")
        val dotted = L.str ("style=dotted")
        val fillColorRed    = L.str ("fillcolor=red")
        val fillColorYellow = L.str ("fillcolor=red")
        (* Helper function to name a "function node" in the call graph. *)
        fun nameFunction (f: Mil.variable, iFunc): string =
            let
              val hint = MU.SymbolInfo.variableName (IMil.T.getSi imil, f)
              val size = IMil.IFunc.getSize (imil, iFunc)
              val recFlag = if IMil.IFunc.getRecursive (imil, iFunc) then
                              ", REC"
                            else
                              ""
            in
              hint ^ " (" ^ Int.toString (size) ^ recFlag ^ ")" 
            end

        fun getFirstBlk f = 
            let
              val iFunc = IMil.IFunc.getIFuncByName (imil, f)
              val blk = IMil.IFunc.getStart (imil, iFunc)
              val (lbl, _) = IMil.IBlock.getLabel' (imil, blk)
            in
              lbl
            end

        fun getBBFreq b = case blkFreq 
                            of NONE => NONE
                             | SOME freq => freq (b)

        fun getFunFreq f = getBBFreq (getFirstBlk f)

        fun freqStr f = case getFunFreq f  
                         of NONE => "[?]"
                          | SOME freq => "[" ^ IntInf.toString (freq) ^ "]"

        fun nodeOptions (f: Mil.variable, escapes: bool) : L.t list = 
            let
              val iFunc = IMil.IFunc.getIFuncByName (imil, f)
              val funName = nameFunction (f, iFunc)
              val label = L.str ("label=\"" ^ funName ^ freqStr (f) ^ "\"");
              val filled = L.str ("style=filled /*Escaping Function*/")
              val escaping = [filled, fillColorRed]
              val options = if escapes then
                              escaping
                            else
                              nil
              val options = if IMil.IFunc.isProgramEntry (imil, iFunc) then
                              L.str ("shape=box /*Program Entry*/")::options
                            else
                              options
            in
              label::options
            end

        fun edgeOptions (call : Mil.label,
                         srcF : Mil.variable, 
                         tgtF : Mil.variable option,
                         virtualCall : bool) : L.t list = 
            let
              val options = [black]

              val options = case getBBFreq call
                             of NONE => options
                              | SOME freq => 
                                let 
                                  val label = L.str ("label =\"" ^ 
                                                     IntInf.toString (freq) ^
                                                     "\"" )
                                in
                                  label::options
                                end

              val options = if virtualCall then
                              dotted::options
                            else 
                              options
            in
              options
            end
        val filename = "call_graphs.dot"
        val p = IMil.T.unBuild imil
        val cg = MCG.program (PD.getConfig d, IMil.T.getSi imil, p)
        val l = MCG.layoutDot (cg, {edgeOptions = edgeOptions,
                                    nodeOptions = nodeOptions,
                                    graphTitle = graphLabel})
      in 
        if !firstPrint then
          (LayoutUtils.writeLayout' (l, filename, false); firstPrint := false)
        else
          LayoutUtils.writeLayout' (l, filename, true)
      end

  val printCallGraph : PassData.t * IMil.t * string -> unit =
   fn (d, imil, graphLabel) => 
      printCallGraph' (d, imil, graphLabel, NONE)

end

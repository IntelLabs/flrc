(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, May 2008 *)
(* Description: Inline small leaf functions. *)

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
  structure MOU = MilOptUtils
  structure M   = Mil
  structure L   = Layout
  structure ID  = Identifier
  structure IM  = ID.Manager

  structure MCG = MilCallGraphF (type env = PD.t
                                 val layoutVariable = fn (e, v) => L.str ""
                                 val config = PD.getConfig
                                 val indent = 2)

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
        fun nameFunction (f: Mil.variable, cfg): string =
            let
              val hint = ID.variableName (IMil.getST (imil), f)
              val size = IMil.Cfg.getSize (imil, cfg)
              val recFlag = if IMil.Cfg.getRecursive (imil, cfg) then
                              ", REC"
                            else
                              ""
            in
              hint ^ " (" ^ Int.toString (size) ^ recFlag ^ ")" 
            end

        fun getFirstBlk f = 
            let
              val cfg = IMil.Cfg.getCfgByName (imil, f)
              val blk = IMil.Cfg.getStart (imil, cfg)
              val (lbl, _) = IMil.Block.getLabel' (imil, blk)
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
              val cfg = IMil.Cfg.getCfgByName (imil, f)
              val funName = nameFunction (f, cfg)
              val label = L.str ("label=\"" ^ funName ^ freqStr (f) ^ "\"");
              val filled = L.str ("style=filled /*Escaping Function*/")
              val escaping = [filled, fillColorRed]
              val options = if escapes then
                              escaping
                            else
                              nil
              val options = if IMil.Cfg.isProgramEntry (imil, cfg) then
                              L.str ("shape=box /*Program Entry*/")::options
                            else
                              options
            in
              label::options
            end

        fun edgeOptions (call : Mil.label,
                         srcF : Mil.variable, 
                         tgtF : Mil.variable option,
                         kind : MCG.callKind, 
                         toUnknown : bool, 
                         virtualCall : bool) : L.t list = 
            let
              val options = case kind
                             of MCG.CkCall      => [black]
                              | MCG.CkEval      => [red]
                              | MCG.CkBulkSpawn => [blue]

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

              val options = if toUnknown orelse virtualCall then
                              dotted::options
                            else 
                              options
            in
              options
            end
        val filename = "call_graphs.dot"
        val p = IMil.unBuild imil
        val cg = MCG.program (d, p)
        val l = MCG.layoutAnnotatedDot (cg, nodeOptions, 
                                        edgeOptions, graphLabel)
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

(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, September 2007 *)

signature MIL_COMPILE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilCompile :> MIL_COMPILE = 
struct

  val passname = "MilCompile"

  structure L = Layout
  structure LU = LayoutUtils

  val passes =
      [
       (#"C", MilContify.pass         ),
       (#"D", MilDblDiamond.pass      ),
       (#"f", MilLowerPFunctions.pass ),
       (#"I", MilInlineLeaves.pass    ),  
       (#"J", MilInlineAggressive.pass),
       (#"K", MilInlineProfile.pass   ),
       (#"L", MilLicm.pass            ),
       (#"R", MilRep.pass             ), 
       (#"S", MilSimplify.pass        ),
       (#"s", MilLowerPSums.pass      ),
       (#"t", MilLowerPTypes.pass     ),
       (#"V", MilCse.pass             ),
       (#"Z", MilVectorize.pass       ),
       (#"B", MilRemoveBranch.pass    )]

  val subPasses = List.map (passes, #2)

  datatype controlItem =
           CiPrint
         | CiCheck
         | CiPass of (BothMil.t, BothMil.t) Pass.t

  fun parseControl s =
      let
        fun matchPass c (c', p) = if c = c' then SOME (CiPass p) else NONE
        fun doOne c =
            case c
             of #"+" => SOME CiPrint
              | #"x" => SOME CiCheck
              | _ => List.peekMap (passes, matchPass c)
        fun loop (cs, cis) =
            case cs
             of [] => SOME (List.rev cis)
              | c::cs =>
                case doOne c
                 of NONE => NONE
                  | SOME ci => loop (cs, ci::cis)
        val cis = loop (String.explode s, [])
        fun check cis =
            let
              fun doOne ci = case ci of CiPass p => SOME p | _ => NONE
              val ps = List.keepAllMap (cis, doOne)
            in
              if Pass.check (subPasses, ps) then SOME cis else NONE
            end
        val cis = case cis of NONE => NONE | SOME cis => check cis
      in cis
      end

  val disabled = "R"
  val enabled = fn c => not (String.contains (disabled, c))
  val filter = fn s => String.keepAll (s, enabled)
  val o0String = filter "fst"
  val o1String = filter "Sfst"
  val o2String = filter "SVSISVSIJKBfst"
  val o3String = filter "VSCSVSIRDSCSVLSRSIJKBfst"

  val o0Control = Option.valOf (parseControl o0String)
  val o1Control = Option.valOf (parseControl o1String)
  val o2Control = Option.valOf (parseControl o2String)
  val o3Control = Option.valOf (parseControl o3String)

  val dftControls =
      Vector.fromList [o0Control, o1Control, o2Control, o3Control]

  fun dftControl c = Vector.sub (dftControls, Config.pOpt c)

  fun describePass (c, p) =
      L.seq [Char.layout c, L.str " => ", L.str (Pass.getName p), L.str ": ",
             L.str (Pass.getDescriptionS p)]

  fun describeControl () =
      L.align
      [L.str (passname ^ " control string consists of:"),
       LU.indent
         (L.align (List.map (passes, describePass) @
                   [L.str "+ => Print",
                    L.str "x => Check"])),
       L.str "defaults are:",
       LU.indent
         (L.align [L.seq [L.str "-O 0 => ", L.str o0String],
                   L.seq [L.str "-O 1 => ", L.str o1String],
                   L.seq [L.str "-O 2 => ", L.str o2String],
                   L.seq [L.str "-O 3 => ", L.str o3String]])]

  val (control, controlGet) =
      Config.Control.mk (passname, describeControl, parseControl, dftControl)

  fun check (pd, p) = 
      case p
       of BothMil.Mil mil   => MilCheck.program (PassData.getConfig pd, mil)
        | BothMil.IMil imil => IMil.T.check imil

  fun printMil (pd, p) = 
      let
        val p = BothMil.toMil (pd, p)
        val config = PassData.getConfig pd
        val l = MilLayout.layout (config, p)
        val () = LayoutUtils.printLayout l
      in BothMil.Mil p
      end

  fun runPass (pd, bn, pass, p) =
      Pass.apply (Pass.doSubPass pass) (pd, bn, p)

  fun runItem (pd, bn, ci, p) =
      case ci
       of CiPrint => printMil (pd, p)
        | CiCheck => let val () = check (pd, p) in p end
        | CiPass pass => runPass (pd, bn, pass, p)

  fun run (pd, bn, cis, p) =
      List.fold (cis, p, fn (ci, p) => runItem (pd, bn, ci, p))

  val (printAllD, printAll) =
      Config.Debug.mk (passname ^ ":print", "debug the Mil compiler")

  fun addPrintControl (config, cis) =
      if Config.debug andalso printAll config then
        let
          fun addPrintToFront cis =
              case cis
               of [] => [CiPrint]
                | CiPrint::_ => cis
                | CiCheck::_ => CiPrint::cis
                | (CiPass _)::_ => CiPrint::cis
          fun doOne (ci, cis) =
              case ci
               of CiPrint => ci::cis
                | CiCheck => ci::cis
                | CiPass _ => ci::(addPrintToFront cis)
          val cis = List.foldr (cis, [], doOne)
          val cis = addPrintToFront cis
        in cis
        end
      else
        cis

  fun addCheckControl (config, cis) =
      if Config.debug then
        List.foldr (cis, [CiCheck], fn (ci, cis) => CiCheck::ci::cis)
      else
        cis

  fun addDebugControl (config, cis) =
      addPrintControl (config, addCheckControl (config, cis))

  fun program (p, pd, bn) =
      let
        val config = PassData.getConfig pd
        val cis = controlGet config
        val cis = addDebugControl (config, cis)
        val p = run (pd, bn, cis, p)
        val () = check (pd, p)
        val () = PassData.report (pd, passname)
      in p
      end

  val description = {name        = passname,
                     description = "Mil optimise/lower",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls  = [control],
                    debugs    = [printAllD],
                    features  = [],
                    subPasses = subPasses}

  val pass = Pass.mkOptFullPass (description, associates, program) 

end

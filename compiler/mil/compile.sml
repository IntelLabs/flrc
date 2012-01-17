(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_COMPILE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilCompile :> MIL_COMPILE = 
struct

  val passname = "MilCompile"

  structure L = Layout
  structure LU = LayoutUtils
  structure Parse = StringParser 

  val passes =
      [
       (#"B", MilRemoveBranch.pass    ),
       (#"C", MilContify.pass         ),
       (#"D", MilDblDiamond.pass      ),
       (#"E", MilRep.Dce.pass         ),
       (#"F", MilRep.Flatten.pass     ),
       (#"f", MilLowerClosures.pass   ),
       (#"H", MilInlineSmall.pass     ),
       (#"I", MilInlineLeaves.pass    ),
       (#"J", MilInlineAggressive.pass),
       (#"K", MilInlineProfile.pass   ),
       (#"L", MilLicm.pass            ),
       (#"R", MilRep.Optimize.pass    ),
       (#"S", MilSimplify.pass        ),
       (#"s", MilLowerPSums.pass      ),
       (#"T", MilThunkOptimize.pass   ),
       (#"t", MilLowerPTypes.pass     ),
       (#"V", MilCse.pass             ),
       (#"Y", MilLoopInvert.pass      ),
       (#"Z", MilVectorize.pass       )
      ]

  val subPasses = List.map (passes, #2)

  datatype controlItem =
           CiPrint
         | CiCheck
         | CiWrite
         | CiPass of (BothMil.t, BothMil.t) Pass.t

  fun parseControl s =
      let
        val || = Parse.||
        val && = Parse.&&
        val -&& = Parse.-&&
        infixr 6 -&&
        infixr && ||
        val $ = Parse.$

        val simple : controlItem Parse.t = 
            let
              fun matchPass c (c', p) = if c = c' then SOME (CiPass p) else NONE
              fun doOne c =
                  case c
                   of #"+" => SOME CiPrint
                    | #"w" => SOME CiWrite
                    | #"x" => SOME CiCheck
                    | _ => List.peekMap (passes, matchPass c)
            in Parse.satisfyMap doOne
            end

        val consume = fn c => Parse.ignore (Parse.satisfy (fn c' => c = c'))

        val lparen = consume #"("
        val rparen = consume #")"
        val lbracket = consume #"["
        val rbracket = consume #"]"
        val lbrace = consume #"{"
        val rbrace = consume #"}"

        val whitespace = 
            let
              val whiteChar = 
               fn c => c = Char.space orelse c = Char.newline orelse c = #"\t" orelse c = #"\r"
              val white = Parse.satisfy whiteChar
            in Parse.ignore (Parse.oneOrMore white)
            end

        val delimited : unit Parse.t * 'a Parse.t * unit Parse.t -> 'a Parse.t =
         fn (left, item, right) => 
            let
              val p = left && item && right
              val f = fn ((), (i, ())) => i
            in Parse.map (p, f)
            end

        val nat : int Parse.t = 
            let
              val p = Parse.oneOrMore (Parse.satisfy Char.isDigit)
              val f = Int.fromString o String.implode
              val p = Parse.map (p, f)
              val p = Parse.required (p, "Expected natural number")
            in p
            end
        val exponentiated : 'a Parse.t -> 'a list Parse.t = 
         fn p => 
            let
              val suffix = (consume #"^") && nat
              val p = p && (Parse.optional suffix)
              val f = fn (p, opt) => 
                         (case opt
                           of SOME ((), n) => List.duplicate (n, fn () => p)
                            | NONE => [p])
              val p = Parse.map (p, f)
            in p
            end
        val rec pass' : unit -> controlItem list Parse.t = 
         fn () => Parse.map ($passSeq', List.concat)
        and rec passSeq' : unit -> controlItem list list Parse.t = 
         fn () => Parse.oneOrMore ($passHead')
        and rec passHead' : unit -> controlItem list Parse.t =
         fn () => Parse.oneOrMore simple || $iterated'  || $constructed' || (whitespace -&& $passHead')
        and rec iterated' : unit -> controlItem list Parse.t = 
         fn () => Parse.map(exponentiated ($parenthesized'), List.concat)
        and rec parenthesized' : unit -> controlItem list Parse.t = 
         fn () => delimited (lparen, $pass', rparen)
        and rec constructed' : unit -> controlItem list Parse.t =
         fn () => 
            let
              val p = $interleave' && $pass' && $interleave'
              val p = delimited (lbracket, p, rbracket)
              val f = fn (pre, (pass, post)) => List.concatMap (pass, fn e => pre @ [e] @ post)
            in Parse.map (p, f)
            end
        and rec interleave' : unit -> controlItem list Parse.t =
         fn () => Parse.map ($interleave0', fn opt => Utils.Option.get (opt, []))
        and rec interleave0' : unit -> controlItem list option Parse.t =
         fn () => Parse.optional ($interleave1')
        and rec interleave1' : unit -> controlItem list Parse.t =
         fn () => delimited (lbrace, $pass', rbrace)

        val eof = Parse.atEnd || Parse.error "Expected end of string"
        val pass = Parse.map ($pass' && eof, #1)

        val cis =
            case Parse.parse (pass, (s, 0))
             of Parse.Success (_, cis) => SOME cis
              | Parse.Failure          => NONE
              | Parse.Error _          => NONE

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

  (* Move to 3 levels.  None, simple, aggressive/expensive *)
  (* *RCIK R VBDL XXX CRfst*)
  (*  Where XXX is some future stuff *)
  (* *RCIKRVBDLCRfst *)
  (* o1 *RCIVBDLfst *)
  (* add symbol to do simplify between each pass *)
  val disabled = ""
  val enabled = fn c => not (String.contains (disabled, c))
  val filter = fn s => String.keepAll (s, enabled)
  val o0String = filter "fst"
  val o1String = filter "SfstS"
  val o2String = filter "[{S}VHIVHIBfst]S"
  val o3String = filter "[{S} TREV CHIV FREV HDCLV REV THIJKTV B FREV fst]S"
(*  val o3String = filter "[{S} TREV CHIV FREV HDCYLV REV THIJKTV B FREV fst]S " disable Loop inversion --leaf*)

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

  (* XXX this doesn't do the right thing with interpsersing print statements. -leaf *)
  fun describeControl () =
      L.align
      [L.align [L.str (passname ^ " control string is of the form:"),
                LU.indent (L.str "Desc ::= Pass* | [{Desc}Desc{Desc}] | ( Desc )^n")],
       L.str " ",
       L.align [L.str "A descriptor of the form [{Desc0}Desc{Desc1}] consists of the passes",
                LU.indent (L.str "described by Desc with Desc0 prepended to each pass, and Desc1 appended"),
                LU.indent (L.str "to each pass.  The {Desc} fields are optional. ")],
       L.str " ",
       L.align [L.str "A descriptor of the form (Desc)^n consists of n successive copies of Desc.",
                LU.indent (L.str "The ^n is optional, and is assumed to be 1 if elided.")],
       L.str " ",
       L.str "A pass consists of any of the following characters:",
       LU.indent
         (L.align (List.map (passes, describePass) @
                   [L.str "+ => Print",
                    L.str "w => Write",
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
        val l = MilExtendedLayout.layout (config, p)
        val () = LayoutUtils.printLayout l
      in BothMil.Mil p
      end

  fun runPass (pd, bn, pass, p) =
      Pass.apply (Pass.doSubPass pass) (pd, bn, p)

  fun writeToFile (pd : PassData.t, bn : Path.t, p : BothMil.t) : unit =
      let
        val config = PassData.getConfig pd
        val p = BothMil.toMil (pd, p)
        val l = MilLayout.layoutParseable (PassData.getConfig pd, p)
        val basename = Config.pathToHostString (config, bn)
        val outfile = basename ^ ".mil"
        val () = LU.writeLayout (l, outfile)
      in ()
      end

  fun runItem (pd, bn, ci, p) =
      case ci
       of CiPrint => printMil (pd, p)
        | CiCheck => let val () = check (pd, p) in p end
        | CiWrite => let val () = writeToFile (pd, bn, p) in p end
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
                | CiWrite::_ => CiPrint::cis
                | CiCheck::_ => CiPrint::cis
                | (CiPass _)::_ => CiPrint::cis
          fun doOne (ci, cis) =
              case ci
               of CiPrint => ci::cis
                | CiWrite => ci::cis
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

  val (writeFinalF, writeFinal) =
      Config.Feature.mk (passname ^ ":write-final", "write final Mil to file")

  fun program (p, pd, bn) =
      let
        val config = PassData.getConfig pd
        val cis = controlGet config
        val cis = addDebugControl (config, cis)
        val cis = if writeFinal config then cis @ [CiWrite] else cis
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
                    features  = PObjectModelCommon.features @ PObjectModelLow.features @ PObjectModelHigh.features @
                                [writeFinalF],
                    subPasses = subPasses}
  val pass = Pass.mkOptFullPass (description, associates, program)
end

(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Induction variable CSE.
 *
 * If two basic induction variables differ by a constant then replace one with an add in the loop body.
 *)

signature MIL_IV_CSE =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilIvCse :> MIL_IV_CSE =
struct

  val passname = "MilIvCse"
  val stats = [("ivCse", "induction variables CSEd")]

  fun fail (f, m) = Fail.fail (passname, f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure I = Identifier
  structure IM = I.Manager
  structure VD = I.VariableDict
  structure LD = I.LabelDict
  structure PD = PassData
  structure M = Mil
  structure ML = MilLoop

  datatype env = E of {config : Config.t, fmil : FMil.t}

  fun envMk (c : Config.t, f : FMil.t) : env =
      E {config = c, fmil = f}

  val ((_, getConfig), (_, getFMil)) =
      FunctionalUpdate.mk2 (fn (E {config, fmil}) => (config, fmil),
                            fn (c, f) => E {config = c, fmil = f})

  (* If a parameter is a basic induction variable and there exists another basic induction variable
   * of the same loop header with the same step, then we'll generate one of the following.
   *   idx      = index of the other BIV in the loop header's parameters
   *   variable = other BIV
   *   initO    = init of the other BIV
   *   initS    = init of this BIV
   * Invariant: this BIV should be other BIV - initO + initS
   *)
  datatype parameterInfo = PI of {idx : int, variable : M.variable, initO : M.operand, initS : M.operand}

  type parametersInfo = parameterInfo option Vector.t

  (* For each loop header we'll compute a vector equal to the number of its parameters that holds
   * a parameterInfo if that parameter is a BIV that should be CSEd in favour of another BIV.
   *)
  datatype state = S of {pd : PD.t, sm : M.symbolTableManager, psi : parametersInfo LD.t ref}

  fun stateMk (pd : PD.t, sm : M.symbolTableManager) : state =
      S {pd = pd, sm = sm, psi = ref LD.empty}

  fun click (S {pd, ...} : state, s : string) : unit = PD.click (pd, s)

  fun getSM (S {sm, ...} : state) : M.symbolTableManager = sm

  fun getSI (state : state) : M.symbolInfo = I.SymbolInfo.SiManager (getSM state)

  fun getTyp (state : state, v : M.variable) : M.typ =
      MilUtils.SymbolTableManager.variableTyp (getSM state, v)

  fun setTyp (state : state, v : M.variable, t : M.typ) : unit =
      MilUtils.SymbolTableManager.variableSetTyp (getSM state, v, t)

  fun variableRelated (state : state, v : M.variable, h : string, t : M.typ, k : M.variableKind) : M.variable =
      MilUtils.SymbolTableManager.variableRelated (getSM state, v, h, t, k)

  fun getParameterInfo (S {psi, ...} : state, h : M.label) : parametersInfo option =
      LD.lookup (!psi, h)

  fun addParameterInfo (S {psi, ...} : state, h : M.label, psi' : parametersInfo) : unit =
      psi := LD.insert (!psi, h, psi')

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the induction variable CSE")

  fun debugPrint (e : env, m : unit -> string) : unit =
      if debugPass (getConfig e) then
        print (passname ^ ": " ^ m () ^ "\n")
      else 
        ()

  local
    structure L = Layout
  in

  fun layoutPInfo (state : state, env : env, PI {idx, variable, initO, initS} : parameterInfo) : Layout.t =
      L.seq [I.SymbolInfo.layoutVariable (variable, getSI state),
             L.str "@", Int.layout idx,
             L.str "-", MilLayout.layoutOperand (getConfig env, getSI state, initO),
             L.str "+", MilLayout.layoutOperand (getConfig env, getSI state, initS)]

  fun layoutPInfoO (state : state, env : env, pio : parameterInfo option) : Layout.t =
      case pio
       of NONE => L.str "-"
        | SOME pi => layoutPInfo (state, env, pi)

  fun layoutPsInfo (state : state, env : env, psi : parametersInfo) : Layout.t =
      L.tuple (List.map (Vector.toList psi, fn pi => layoutPInfoO (state, env, pi)))

  end

  structure A =
  struct

    val modname = passname ^ ".A"

    datatype biv = BIV of {variable : M.variable, init : M.operand, step : Rat.t}

    local
      structure L = Layout
    in

    fun layoutBasicIV (state : state, env : env, BIV {variable, init, step} : biv) : Layout.t =
        L.seq [I.SymbolInfo.layoutVariable (variable, getSI state),
               L.str " = #*",
               Rat.layout step,
               L.str "+",
               MilLayout.layoutOperand (getConfig env, getSI state, init)]

    fun layoutBasicIVs (state : state, env : env, bivs : biv VD.t) : Layout.t =
        L.align (List.map (VD.toList bivs, fn (_, biv) => layoutBasicIV (state, env, biv)))

    end

    fun getBasicIVs (state : state, env : env, li : ML.t, h : M.label) : biv VD.t =
        let
          fun doOne (iv, bivs) =
              case iv
               of ML.BIV {variable, init, step} =>
                  VD.insert (bivs, variable, BIV {variable = variable, init = init, step = step})
                | ML.DIV _ => bivs
          val bivs = List.fold (ML.getInductionVariables (li, h), VD.empty, doOne)
        in bivs
        end

    fun forest (state : state, env : env, li : ML.t, f : ML.loopForest) : unit =
        Vector.foreach (f, fn t => tree (state, env, li, t))
    and tree (state : state, env : env, li : ML.t, t : ML.loopTree) : unit =
        Tree.foreachPre (t, fn l => loop (state, env, li, l))
    and loop (state : state, env : env, li : ML.t, ML.L {header, blocks} : ML.loop) : unit =
        let
          val () = debugPrint (env, fn () => "analysing loop " ^ I.labelString header)
          val bivs = getBasicIVs (state, env, li, header)
          val () = debugPrint (env, fn () => "basic induction variables:\n" ^
                                             LayoutUtils.toString (LayoutUtils.indent
                                                                     (layoutBasicIVs (state, env, bivs))))
          val hb = Option.valOf (LD.lookup (blocks, header))
          val M.B {parameters, ...} = hb
          (* For each parameter in the loop header's parameters that is a BIV, see if there is an earlier parameter
           * that is a BIV with the same step, if so, CSE in favour of the earlier parameter.
           *)
          fun chkOne (i, init', step', j) =
              if j<i then
                case VD.lookup (bivs, Vector.sub (parameters, j))
                 of NONE => chkOne (i, init', step', j + 1)
                  | SOME (BIV {variable, init, step}) =>
                    if Rat.equals (step, step') then
                      let
                        val () = click (state, "ivCse")
                      in
                        SOME (PI {idx = i, variable = variable, initO = init, initS = init'})
                      end
                    else
                      chkOne (i, init', step', j + 1)
              else
                NONE
          fun doOne (i, p) =
              case VD.lookup (bivs, p)
               of NONE => NONE
                | SOME (BIV {variable, init, step}) => chkOne (i, init, step, 0)
          val () = debugPrint (env, fn () => "computing parameter information")
          val psi = Vector.mapi (parameters, doOne)
          fun layout () = LayoutUtils.toString (LayoutUtils.indent (layoutPsInfo (state, env, psi)))
          val () = debugPrint (env, fn () => "parameter information:\n" ^ layout ())
          val () = addParameterInfo (state, header, psi)
        in ()
        end

  end

  structure T =
  struct

    val modname = passname ^ ".T"

    fun fail (f, m) = Fail.fail (modname, f, m)

    (* For a variable that is CSEd in favour of another BIV, we need to generate instructions
     * to initialise the CSEd BIV at the start of the loop.  We take the other BIV, subtract its init and add
     * this one's init.
     *)
    fun variable (state : state, env : env, p : M.variable, pi : parameterInfo) : M.instruction Vector.t =
        let
          val () = debugPrint (env, fn () => "making instructions for CSEd IV " ^ IM.variableString (getSM state, p))
          val PI {idx, variable, initO, initS} = pi
          val t = getTyp (state, p)
          val nt =
              case t
               of M.TNumeric nt => nt
                | _ => fail ("variable", "bad type: " ^ I.variableString' p)
          val t1 = variableRelated (state, p, "", t, M.VkLocal)
          val p' = M.Prims.Prim (M.Prims.PNumArith {typ = nt, operator = M.Prims.AMinus})
          val args = Vector.new2 (M.SVariable variable, initO)
          val rhs = M.RhsPrim {prim = p', createThunks = false, typs = Vector.new0 (), args = args}
          val i1 = M.I {dests = Vector.new1 t1, n = 0, rhs = rhs}
          val p' = M.Prims.Prim (M.Prims.PNumArith {typ = nt, operator = M.Prims.APlus})
          val args = Vector.new2 (M.SVariable t1, initS)
          val rhs = M.RhsPrim {prim = p', createThunks = false, typs = Vector.new0 (), args = args}
          val i2 = M.I {dests = Vector.new1 p, n = 0, rhs = rhs}
          fun layout () =
              LayoutUtils.indent (Layout.align [MilLayout.layoutInstruction (getConfig env, getSI state, i1),
                                                MilLayout.layoutInstruction (getConfig env, getSI state, i2)])
          val () = debugPrint (env, fn () => "added instructions:\n" ^ LayoutUtils.toString (layout ()))
          val is = Vector.new2 (i1, i2)
        in is
        end

    (* Remove information for CSEd parameters *)
    fun remCSEd (xs : 'a Vector.t, pios : parametersInfo) : 'a Vector.t =
        Vector.keepAllMap2 (xs, pios, fn (x, pio) => if Option.isSome pio then NONE else SOME x)

    fun block (state : state, env : env, l : M.label, b : M.block) : M.block =
        let
          val M.B {parameters, instructions, transfer} = b
          val psi = getParameterInfo (state, l)
          (* If block is a loop header, remove CSEd parameters *)
          val ps = case psi of NONE => parameters | SOME psi => remCSEd (parameters, psi)
          (* If block is a loop header, add instructions to recreate CSEd BIVs *)
          val is =
              case psi
               of NONE => instructions
                | SOME psi =>
                  let
                    fun doOne (p, pio) =
                        case pio
                         of NONE => NONE
                          | SOME pi => SOME (variable (state, env, p, pi))
                    val is = Vector.keepAllMap2 (parameters, psi, doOne)
                    val is = Vector.concat [Vector.concatV is, instructions]
                  in is
                  end
          (* The instructions might take continuation of a loop header that is transformed.
           * If so then rewrite the type of the continuation variable.
           *)
          fun doOne (M.I {dests, rhs, ...}) =
              case rhs
               of M.RhsCont l =>
                  (case getParameterInfo (state, l)
                    of NONE => ()
                     | SOME psi =>
                       (case getTyp (state, Vector.sub (dests, 0))
                         of M.TContinuation ts =>
                            setTyp (state, Vector.sub (dests, 0), M.TContinuation (remCSEd (ts, psi)))
                          | _ => ()))
                | _ => ()
          val () = Vector.foreach (instructions, doOne)
          (* Transform the targets of the transfer to remove arguments for CSEd parameters *)
          fun doOne (t as M.T {block = b, arguments}) =
              case getParameterInfo (state, b)
               of NONE => t
                | SOME psi => M.T {block = b, arguments = remCSEd (arguments, psi)}
          val t = MilUtils.Transfer.mapOverTargets (transfer, doOne)
          val b = M.B {parameters = ps, instructions = is, transfer = t}
        in b
        end

    fun blocks (state : state, env : env, bs : ML.blocks) : ML.blocks =
        LD.map (bs, fn (l, b) => block (state, env, l, b))

    fun forest (state : state, env : env, ls : ML.loopForest) : ML.loopForest =
        Vector.map (ls, fn lt => loopTree (state, env, lt))
    and loopTree (state : state, env : env, lt : ML.loopTree) : ML.loopTree =
        Tree.map (lt, fn l => loop (state, env, l))
    and loop (state : state, env : env, l : ML.loop) : ML.loop =
        let
          val ML.L {header, blocks = bs} = l
          val () = debugPrint (env, fn () => "rewriting loop " ^ I.labelString header)
          val bs = blocks (state, env, bs)
          val l = ML.L {header = header, blocks = bs}
        in l
        end

    fun loopInfo (state : state, env : env, li : ML.t) : ML.t =
        let
          val e = ML.getEntry li
          val ls = ML.getLoops li
          val () = debugPrint (env, fn () => "rewriting loops")
          val ls = forest (state, env, ls)
          val bs = ML.getBlocksNotInLoops li
          val () = debugPrint (env, fn () => "rewriting top blocks")
          val bs = blocks (state, env, bs)
          val li = ML.fromLoops (getConfig env, getSI state, {entry = e, loops = ls, blocksNotInLoops = bs})
          val () = debugPrint (env, fn () => "rewritten")
        in li
        end

  end

  fun doBody (state : state, env : env, body : M.codeBody) : M.codeBody =
      let
        val c = getConfig env
        val si = getSI state
        val () = debugPrint (env, fn () => "building CFG")
        val cfg = MilCfg.build (c, si, body)
        val () = debugPrint (env, fn () => "building dom tree")
        val dt = MilCfg.getLabelBlockDomTree cfg
        val () = debugPrint (env, fn () => "building loops")
        val li = ML.build (c, si, cfg, dt)
        val () = debugPrint (env, fn () => "building all nodes")
        val li = ML.genAllNodes li
        val () = debugPrint (env, fn () => "building induction variables")
        val li = ML.genInductionVariables (li, getFMil env, cfg)
        val () = debugPrint (env, fn () => "analysing")
        val () = A.forest (state, env, li, ML.getLoops li)
        val () = debugPrint (env, fn () => "rewriting")
        val li = T.loopInfo (state, env, li)
        val () = debugPrint (env, fn () => "unbuilding")
        val body = ML.unbuild li
        val () = debugPrint (env, fn () => "all done")
      in body
      end

  fun program (p : Mil.t, pd : PD.t) : Mil.t =
      let
        val c = PD.getConfig pd
        val (p, fmil) = FMil.program (c, p)
        val M.P {includes, externs, globals, symbolTable, entry} = p
        val sm = IM.fromExistingAll symbolTable
        val env = envMk (c, fmil)
        val state = stateMk (pd, sm)
        fun doCode (v, M.F {fx, escapes, recursive, cc, args, rtyps, body}) =
            let
              val () = debugPrint (env, fn () => "processing " ^ (IM.variableString (sm, v)))
              val cb = doBody (state, env, body)
            in
              M.F {fx = fx, escapes = escapes, recursive = recursive, cc = cc, args = args, rtyps = rtyps, body = cb}
            end
        fun doGlobal (v, g) =
            case g
             of M.GCode code => M.GCode (doCode (v, code))
              | _            => g
        val gs = VD.map (globals, doGlobal)
        val p = M.P {includes = includes, externs = externs, globals = gs, symbolTable = IM.finish sm, entry = entry}
        val () = PD.report (pd, passname)
      in p
      end

  val description = {name        = passname,
                     description = "Induction variable CSE",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = [debugPassD],
                    features  = [],
                    subPasses = []}

  val pass = Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end

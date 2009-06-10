(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, December 2007 *)

signature MIL_CSE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilCse :> MIL_CSE = 
struct

  val passname = "MilCse"

  val stats = [("GlobalCse", "Redundant globals eliminated")]

  structure G = ImpPolyLabeledGraph
  structure I = Identifier
  structure PD = PassData
  structure IM = Identifier.Manager
  structure VD = Identifier.VariableDict
  structure LD = Identifier.LabelDict
  structure IVD = Identifier.ImpVariableDict
  structure VS = Identifier.VariableSet
  structure M = Mil
  structure MU = MilUtils
  structure GD = MU.Global.Dict
  structure RD = MU.Rhs.Dict
  structure ED = MU.Eval.Dict
  structure FV = MilFreeVars
  structure RN = MilRename
  structure RNV = RN.Var
  structure Cfg = MilCfg

  datatype env = E of {
    rename  : Rename.t,
    gDict   : M.variable GD.t,
    rhsDict : M.variable RD.t,
    evlDict : M.variable Vector.t ED.t,
    data    : PD.t
  }

  datatype state = S of {stM : M.symbolTableManager}

  fun envGet sel (E t) = sel t 

  val envGetRename  = envGet #rename
  val envGetGDict   = envGet #gDict
  val envGetRhsDict = envGet #rhsDict
  val envGetEvlDict = envGet #evlDict                        
  val envGetData    = envGet #data

  fun envGetConfig env = PD.getConfig (envGetData env)

  fun envSet (rename, gDict, rhsDict, evlDict, data, E t) =
      E {rename = rename t,
         gDict  = gDict t,
         rhsDict = rhsDict t,
         evlDict = evlDict t,
         data   = data t}
      
  fun envSetRename (env, rename) = 
      envSet (fn _ => rename, #gDict, #rhsDict, #evlDict, #data, env)
  fun envSetGDict (env, gDict) = 
      envSet (#rename, fn _ => gDict, #rhsDict, #evlDict, #data, env)
  fun envSetRhsDict (env, rhsDict) = 
      envSet (#rename, #gDict ,fn _ => rhsDict, #evlDict, #data, env)
  fun envSetEvlDict (env, evlDict) = 
      envSet (#rename, #gDict ,#rhsDict, fn _ => evlDict, #data, env)

  fun stateGet sel (S t) = sel t

  val stateGetStM = stateGet #stM

  fun stateGetSi s = I.SymbolInfo.SiManager (stateGetStM s)

  fun getConfig env = PD.getConfig (envGetData env)

  structure Chat = ChatF(struct 
                           type env = env
                           val extract = getConfig
                           val name = passname
                           val indent = 0
                         end)

  type graph = (M.variable, unit) G.t
  type node = (M.variable, unit) G.node

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil CSE pass")

  fun debugDo (env, f) = 
      if Config.debug andalso debugPass (getConfig env)
      then f()
      else ()

  fun addToRename (env, vOrig, vNew) = 
      envSetRename (env, Rename.renameTo (envGetRename env, vOrig, vNew))

  fun addAllToRename (env, vOrig, vNew) = 
      Vector.fold2 (vOrig, vNew, env, fn (a, b, env) => addToRename (env, a, b))

  fun deleteVar (state, v) = 
      IM.variableDelete (stateGetStM state, v)

  fun addGlobalToGDict (env, v, g) = 
      (case g
        of M.GCode _ => env  (* Useless to try to CSE code *)
         | _ => 
           let
             val gDict = envGetGDict env
             val gDict = GD.insert (gDict, g, v)
             val env = envSetGDict (env, gDict)
           in env
           end)

  fun addEvalToEDict (env, parms, ev) = 
       let
         val gDict = envGetEvlDict env
         val gDict = ED.insert (gDict, ev, parms)
         val env = envSetEvlDict (env, gDict)
       in env
       end

  fun computeSCC (state, env, gd) = 
      let
        val config = envGetConfig env
        fun depsOf (v, g) = FV.global (config, v, g)
        val scc = I.variableToposort (VD.toList gd, depsOf)
        val () =
            debugDo (env, 
                 fn () => 
                    let
                      val () = print "SCC are\n"
                      fun layoutOne (v, _) = MilLayout.layoutVariable (config, stateGetSi state, v)
                      val l = List.layout (List.layout layoutOne) scc
                      val () = LayoutUtils.printLayout l
                      val () = print "\n"
                    in ()
                    end)
      in scc
      end

  local
    structure E = Effect
    val cseFx = E.union (E.Control, E.InitReadS)
  in
  fun canCse (env, i) =
      E.subset (MU.Instruction.fx (envGetConfig env, i), cseFx)
  end

  (* extract rename dict from environment
   * if instructions' rhs not present, add to dictionary creating new env 
   * if present, remove instruction and add 'variable' to rename list
   *)
  fun rewriteInstruction (i, env) =
      let
        val config = envGetConfig env
        val rename = envGetRename env 
        val i as M.I {dests, n, rhs} = RNV.instruction (config, rename, i)
        val keepWith = fn env => (SOME i, env)
        val deleteWith = fn env => (NONE, env)
        val rhsD = envGetRhsDict env 
        val res = 
            if canCse (env, i) then
              let
                val res =
                    case (Vector.length dests, RD.lookup (rhsD, rhs))
                     of (1, SOME v') => 
                        let
                          val vv = Vector.sub (dests, 0)
                        in deleteWith (addToRename (env, vv, v'))
                        end
                      | (0, SOME v') => deleteWith env
                      | (1, NONE) => 
                        let
                          val vv = Vector.sub (dests, 0)
                          val rhsD = RD.insert (rhsD, rhs, vv) 
                          val env = envSetRhsDict (env, rhsD)
                        in 
                          keepWith env
                        end
                      | (_, _) => keepWith env
              in
                res
              end
            else 
              keepWith env
     in res
     end

  fun rewriteInstructions (state, env, is) =
      let
        val (iopts, env) = Vector.mapAndFold (is, env, rewriteInstruction)
        val is = Vector.keepAllSome iopts
      in (env, is)
      end

  fun rewriteTransfer (state, env, transfer) = 
      let
        val rename = envGetRename env 
        val transfer = RNV.transfer (envGetConfig env, rename, transfer) 
        fun keep () = (transfer, env, NONE)
      in
        case transfer
         of M.TInterProc {callee = M.IpEval {typ, eval}, ret, fx} =>
            (case ED.lookup (envGetEvlDict env, eval)
              of NONE =>
                 (case ret
                   of M.RNormal {rets, block, ...} => (transfer, env, SOME (eval, rets, block))
                    | M.RTail => keep ())
               | SOME vs =>
                 (case ret
                   of M.RNormal {rets, block, ...} =>
                      (M.TGoto (M.T {block = block, arguments = Vector.new0 ()}),
                       addAllToRename (env, rets, vs),
                       NONE)
                    | M.RTail =>
                      (M.TReturn (Vector.map (vs, M.SVariable)), env, NONE)))
          | _ => keep ()
      end

  fun rewriteBlock (state, env, cfg, label) =
      let
        val block = Cfg.labelGetBlock (cfg, label)
        val M.B {parameters, instructions, transfer} = block
        val (env, is) = rewriteInstructions (state, env, instructions)
        val (t, env, out) = rewriteTransfer (state, env, transfer)
        val res = M.B {parameters = parameters, instructions = is, transfer = t}
      in
        (env, out, (label, res))
      end

  fun rewriteTree (state, env, cfg, edgeIn, domtree) =
      let
        val Tree.T (root, children) = domtree
        val env =
            case edgeIn
             of NONE => env
              | SOME (eval, rets, l) =>
                if root = l then
                  (* We must check that rets are in scope for root, this is true if there is only one edge into root *)
                  let
                    val n = Cfg.labelGetNode (cfg, root)
                    val numInEdges = Cfg.numInEdges (cfg, n)
                  in
                    if numInEdges = 1 then addEvalToEDict (env, rets, eval) else env
                  end
                else
                  env
        val (env, edgeOut, root) = rewriteBlock (state, env, cfg, root)
        fun help t = rewriteTree (state, env, cfg, edgeOut, t)
        val children = Vector.map (children, help)
      in
        Tree.T (root, children)
      end
      
  (* CSE body of a global function *)
  fun cseCode (state, env, f) =
      let
        val M.F {fx, escapes, recursive, cc, args, rtyps, body} = f
        val cfg = Cfg.build (envGetConfig env, stateGetSi state, body)
        val domtree = Cfg.getLabelDomTree cfg
        val domtree = rewriteTree (state, env, cfg, NONE, domtree)
        val insert = fn ((l, b), d) => LD.insert (d, l, b)
        val dict = Tree.foldPre (domtree, LD.empty, insert)
        val body = M.CB {entry = Cfg.startLabel cfg, blocks = dict}
        val res =
            M.F {fx = fx, escapes = escapes, recursive = recursive, cc = cc, args = args, rtyps = rtyps, body = body}
      in
        res
      end

  fun doGlobals (state, env, l) = 
      let
        val (keep, get) = 
            let
              val globals = ref []
              fun keep (v, g) = globals := (v, g):: !globals
              fun get () = VD.fromList (!globals)
            in (keep, get)
            end

        fun canonizeGlobal (env, v, g) =
            case g
             of M.GCode code =>  (*  CSE of code  *)
                let
                  val ncode = cseCode (state, env, code)
                  val res2 = M.GCode ncode
                in (v, res2)
                end
              | _ =>  (* global *)
                let
                  val config = envGetConfig env
                  val rename = envGetRename env
                  val res = RNV.global (config, rename, v, g)
                in res
                end

        fun processGlobal (env, v, g) = 
            case GD.lookup (envGetGDict env, g)
             of SOME v' => 
                let
                  val () = PD.click (envGetData env, "GlobalCse")
                  val env = addToRename (env, v, v')
                  val () = deleteVar (state, v)
                in env
                end
              | NONE => 
                let
                  val () = keep (v, g)
                  val env = addGlobalToGDict (env, v, g)
                in env
                end

        fun doComponent (l, env) = 
            let
              val l = List.map (l, fn (v, g) => canonizeGlobal (env, v, g))
              val env = 
                  (case l
                    of [(v, g)] => processGlobal(env, v, g)
                     | _ => 
                       let
                         val () = List.foreach (l, keep)
                       in env
                       end)
            in env
            end

        val env = List.fold (l, env, doComponent)
        val gs = get ()
      in gs
      end

  fun globals (state, env, gs) = 
      let
        (* Compute a good ordering of the globals *)
        val () = Chat.log3 (env, "Topo sorting globals")
        val l = computeSCC (state, env, gs)
        (* Rewrite *)
        val () = Chat.log3 (env, "Rewriting the globals")
        val gs = doGlobals (state, env, l)
      in gs
      end

  fun rewrite (d, mil) = 
      let
        val M.P {globals = gs, symbolTable, entry} = mil
        val stM = IM.fromExistingAll symbolTable
        val env = E {rename = Rename.none,
                     gDict  = GD.empty,
                     rhsDict  = RD.empty,
                     evlDict = ED.empty,
                     data   = d}
        val state = S {stM = stM}
        val gs = globals (state, env, gs)
        val st = IM.finish stM
        val mil = M.P {globals = gs, symbolTable = st, entry = entry}
      in mil
      end

  fun program (mil, d) = 
      let
        val mil = rewrite (d, mil)
        val () = PD.report (d, passname)
      in mil
      end

  val description = {name        = passname,
                     description = "Common-subexpression elimination",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = [debugPassD],
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end

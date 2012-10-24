(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* This pass contifies functions into other functions.
 * It is based on Mathew Fluett and Stephen Weeks "Contification using
 * Dominators" in ICFP 2001 pg 2-13.
 * It also does self tail call optimisation by changing this to a goto to
 * a new label that merges phis the functions parameters and these self tail
 * calls.
 *)

signature MIL_CONTIFY =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end;

structure MilContify :> MIL_CONTIFY =
struct

  structure L = Layout
  structure LU = LayoutUtils
  structure I = Identifier
  structure IM = I.Manager
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure M = Mil
  structure MU = MilUtils
  structure MSTM = MU.SymbolTableManager
  structure MCG = MilCallGraph
  structure MFV = MilFreeVars
  structure PD = PassData

  val passname = "MilContify"

  val stats =
      [("inlines",     "functions contified"  ),
       ("closures",    "closures contified"   ),
       ("dummyCode",   "dummy code closures"  ),
       ("retJump",     "return -> jump"       ),
       ("callJump",    "call -> jump"         ),
       ("untail",      "tailcall -> call"     ),
       ("tailJump",    "tailcall -> jump"     ),
       ("selfTail",    "self tailcall -> jump"),
       ("cutRewrites", "cuts rewrites"        )]

  structure A =
  struct

    (* Analysis:
     *   The purpose of this pass is to determine where each global code
     *   returns to.  This can be one of four things: uncalled, meaning that
     *   the code is unreachable from the enry point; unknown, meaning that no
     *   single return point for the code can be determinied; a label,
     *   meaning that the code always returns to that label; or a code,
     *   meaning that the code always returns to where that code returns to,
     *   so can be placed in it.
     *
     *   Additionally the analysis provides the FMil.
     *)

    datatype env = E of {config : Config.t, si : M.symbolInfo, fmil : FMil.t, rLbls : M.label LD.t}

    fun envMk (config, si, fmil, rLbls) = E {config = config, si = si, fmil = fmil, rLbls = rLbls}

    fun getConfig (E {config, ...}) = config

    fun getSi (E {si, ...}) = si

    fun getFMil (E {fmil, ...}) = fmil

    fun getRLbls (E {rLbls, ...}) = rLbls

    fun layoutVariable (env, v) = MilLayout.layoutVariable (getConfig env, getSi env, v)

    (* Compute the globals codes that are reachable from the entry point *)

    fun computeReachable (env, p as M.P {entry, globals, ...}) =
        let
          fun loop (wl, r, p) =
              case wl
               of [] => r
                | v::wl =>
                  (case FMil.getVariable (getFMil env, v)
                    of FMil.VdGlobal g => 
                       let
                         val r =
                             case g
                              of M.GCode _ => VS.insert (r, v)
                               | _ => r
                         val fvs = MFV.global (getConfig env, v, g)
                         fun doOne (v, (wl, p)) =
                             if VS.member (p, v) then
                               (wl, p)
                             else
                               (v::wl, VS.insert (p, v))
                         val (wl, p) = VS.fold (fvs, (wl, p), doOne)
                       in loop (wl, r, p)
                       end
                     | _ => loop (wl, r, p))

        in loop ([entry], VS.empty, VS.singleton entry)
        end

    fun printReachable r =
        let
          val l = VS.layout (r, I.layoutVariable')
          val l = L.align [L.str "Reachable:", LU.indent l]
          val () = LU.printLayout l
        in ()
        end

    (* The output of the analysis *)

    datatype return =
        RUncalled
      | RUnknown
      | RCont of M.label * M.cuts
                 (* The returned to label
                  * An upperbound on the cuts the calls can make
                  *)
      | RFunc of M.variable

    datatype analysis = A of {funs : return VD.t, fmil : FMil.t}

    fun layoutReturn r =
        case r
         of RUncalled => L.str "uncalled"
          | RUnknown => L.str "unknown"
          | RCont (l, _) => L.str ("cont:" ^ I.labelString l)
          | RFunc v => L.str ("func:" ^ I.variableString' v)

    fun layoutAnalysis (A {funs = rs, ...}) =
        let
          fun layout (f, r) = L.seq [L.str (I.variableString' f),
                                     L.str ": ",
                                     layoutReturn r]
          val l = VD.layout (rs, layout)
        in l
        end

    fun getReturn (env, c) =
        case FMil.getTransfer (getFMil env, c)
         of M.TInterProc {ret, ...} => ret
          | _                       => Fail.fail ("MilContify.A", "getReturn", "bad call graph")

    fun getReturnLabel (env, c) = LD.lookup (getRLbls env, c)

    (* Compute the cuts for each return label *)
    fun computeCuts (env, MCG.CG {calls, ...}) =
        let
          fun doOne (c, _, map) =
              case (getReturnLabel (env, c), getReturn (env, c))
               of (SOME l, M.RNormal {rets, block, cuts, ...}) =>
                  let
                    val cuts =
                        case LD.lookup (map, l)
                         of NONE            => cuts
                          | SOME cuts'      => MU.Cuts.union (cuts', cuts)
                    val map = LD.insert (map, l, cuts)
                  in map
                  end
                | _ => map
          val map = LD.fold (calls, LD.empty, doOne)
        in map
        end

    (* The graph used in the dominator algorithm of Fluett and Sweeks
     *
     * Nodes include a root, a node for each global code, and a node for each
     * return point.
     * Edges represent calls: nontail, tail, or unknown.
     *)

    datatype nodeLabel = NlRoot | NlFun of M.variable | NlRet of M.label

    structure G = PolyLabeledGraph
    type graph = (nodeLabel, unit) G.t
    type node = (nodeLabel, unit) G.node

    (* Build the graph used in the Fluett and Sweeks algorithm from the
     * call graph, reachable code globals set, and the entry point.
     *)

    fun buildGraph (env, entry, cg, r) =
        let
          val MCG.CG {funs, calls, callMap} = cg
          fun escapesOrNotInlinable v =
              case Option.valOf (VD.lookup (funs, v))
               of MCG.FI {unknownCallers, ...} =>
                  unknownCallers orelse
                  (case MU.Code.cc (FMil.getCode (getFMil env, v))
                    of M.CcCode => false
                     | M.CcUnmanaged _ => true
                     | M.CcClosure _ => false
                     | M.CcThunk _ => true)
          (* Nodes *)
          val funNodes = List.map (VD.domain funs, NlFun)
          fun doOne (l, _, rets) = Option.fold (getReturnLabel (env, l), rets, LS.insert o Utils.flip2)
          val retLabs = LD.fold (calls, LS.empty, doOne)
          val retNodes = List.map (LS.toList retLabs, NlRet)
          val nodes = NlRoot::(funNodes @ retNodes)
          (* Reverse node maps *)
          fun node (nl, n, (root, funs, rets)) =
              case nl
               of NlRoot  => (SOME n, funs,                   rets                  )
                | NlFun f => (root,   VD.insert (funs, f, n), rets                  )
                | NlRet l => (root,   funs,                   LD.insert (rets, l, n))
          (* Edges *)
          fun edges (root, funMap, retMap) =
              let
                val root = Option.valOf root
                fun funNode f = Option.valOf (VD.lookup (funMap, f))
                fun retNode l = Option.valOf (LD.lookup (retMap, l))
                (* root -> entry *)
                val e1 = (root, funNode entry, ())
                (* root -> unreachable code *)
                (* root -> escaping or non-inlinable code *)
                fun doOne (f, n, es) =
                    if VS.member (r, f) andalso not (escapesOrNotInlinable f) then es else (root, n, ())::es
                val es2 = VD.fold (funMap, [], doOne)
                (* root -> return label *)
                val es3 = List.map (LD.toList retMap, fn (_, n) => (root, n, ()))
                (* f -> g if f tailcalls g and f reachable *)
                (* r -> g if f calls g with return r and f reachable *)
                fun doDirectCall (caller, callee, rl, es) =
                    if VS.member (r, caller) then
                      case rl
                       of NONE   => (funNode caller, funNode callee, ())::es
                        | SOME l => (retNode l,      funNode callee, ())::es
                    else
                      es
                fun doIndirectCall (caller, callee, rl, es) = (root, funNode callee, ())::es
                fun callIsInlineable (c, MCG.CI {knownCallees, unknownCallees}) =
                    let
                      val indirect = VS.size knownCallees <> 1 orelse unknownCallees 
                      val closure = 
                          (case MU.Transfer.Dec.tInterProc (FMil.getTransfer (getFMil env, c))
                            of SOME {callee = M.IpCall {call = M.CClosure _, ...}, ...} => true
                             | _ => false)
                    in not (indirect orelse closure)
                    end
                fun doCall1 (c, call as MCG.CI {knownCallees, unknownCallees}, es) =
                    let
                      val doCall2 = if callIsInlineable (c, call) then doDirectCall else doIndirectCall 
                      val f = FMil.getLabelFun (getFMil env, c)
                      val rl = getReturnLabel (env, c)
                      val es = VS.fold (knownCallees, es, fn (g, es) => doCall2 (f, g, rl, es))
                    in es
                    end
                val es4 = LD.fold (calls, [], doCall1)
              in e1::(es2 @ es3 @ es4)
              end
          val (graph, (root, _, _)) =
              G.new {nodes = nodes, init = (NONE, VD.empty, LD.empty), node = node, edges = edges}
          val root = Option.valOf root
        in (graph, root)
        end

    (* Compute the analysis result:
     * Given the dominator tree of the graph above, the root node should be
     * the root of the tree, all codes that are children of the root are either
     * uncalled or unknown depending upon whether they are reachable or not.
     * All other nodes return the the child of the root of which they are
     * descended.
     *)

    fun computeAnalysis (env, graph, dt, MCG.CG {calls, ...}, r, rCuts) =
        let
          val Tree.T (_, tops) = dt
          fun doRest r  (Tree.T (n, children), a) =
              let
                val a =
                    case G.Node.getLabel n
                     of NlRoot => Fail.fail ("MilContify.A", "compteAnalysis", "bad dominator tree")
                      | NlFun f => VD.insert (a, f, r)
                      | NlRet _ => Fail.fail ("MilContify.A", "compteAnalysis", "bad dominator tree")
                val a = Vector.fold (children, a, doRest r)
              in a
              end
          fun doTop (Tree.T (n, children), a) =
              let
                val (r, a) =
                    case G.Node.getLabel n
                     of NlRoot => Fail.fail ("MilContify", "compteAnalysis", "bad dominator tree")
                      | NlFun f => (RFunc f, VD.insert (a, f, if VS.member (r, f) then RUnknown else RUncalled))
                      | NlRet r =>
                        let
                          val cuts = Option.valOf (LD.lookup (rCuts, r))
                        in (RCont (r, cuts), a)
                        end
                val a = Vector.fold (children, a, doRest r)
              in a
              end
          val funs = Vector.fold (tops, VD.empty, doTop)
          val a = A {funs = funs, fmil = getFMil env}
        in a
        end

    (* The whole analysis *)

    val (prnCallGraphD, prnCallGraph) =
        Config.Debug.mk (passname ^ ":call-graph", "print call graph in Mil contifier")
    val (prnReachableD, prnReachable) =
        Config.Debug.mk (passname ^ ":reachable", "print reachability analysis in Mil contifier")
    val (prnAnalyseD, prnAnalyse) =
        Config.Debug.mk (passname ^ ":analyse", "print analysis in Mil contifier")

    fun analyseProgram (config, p, rLbls) =
        let
          val (M.P {globals, symbolTable, entry, ...}, fmil) = FMil.program (config, p)
          val si = I.SymbolInfo.SiTable symbolTable
          val env = envMk (config, si, fmil, rLbls)
          val cg = MCG.program (config, si, p)
          val () =
              if Config.debug andalso prnCallGraph config
              then LU.printLayout (MCG.layout (config, si, cg))
              else ()
          val r = computeReachable (env, p)
          val () =
              if Config.debug andalso prnReachable config
              then printReachable r
              else ()
          val (graph, root) = buildGraph (env, entry, cg, r)
          val dt = G.domTree (graph, root)
          val rCuts = computeCuts (env, cg)
          val a = computeAnalysis (env, graph, dt, cg, r, rCuts)
          val () =
              if Config.debug andalso prnAnalyse config
              then LU.printLayout (L.align [L.str "Contification analysis:",
                                            LU.indent (layoutAnalysis a)])
              else ()
        in a
        end

  end (* structure A *)

  structure T =
  struct

    (* The transformation:
     *   For each code that is not unknown, remove it from the globals.
     *   For each code f that is unknown, transform it as follows:
     *    First, add the transitive closure of the following to the cb
     *    the cb of each code that returns to f or another included code or
     *    a return label in f or another included code.
     *    Second, in parts originally from a code that returns to label l,
     *    change return(os) to jump l'(os) and tailcall f(os) to
     *    call f(os) -> rvs'.l'' where l' is a newly created block of the
     *    form l'(rvs): goto l(), rvs are the return vars of l, and rvs' are
     *    copies of rvs.
     *    Third, rewrite the cuts to annotations on any included cb.  This
     *    is similar to inlining.  Anything that exits must have the labels
     *    of f that are used in RhsCont added to the target sets.
     *    Fourth, for each call to an inlined cb, change it to a jump to the
     *    cb's entry label, which must be transformed for closures and thunks
     *    to include the implicit argument and to load the free variables.
     *   For each GClosure and RhsClosureInit, if the code is not unknown
     *   then replace it with a dummy code pointer.
     *)

    (* For self tail calls in functions keep in the globals, we try to
     * optimise them to gotos.  This requires making a label whose parameters
     * are the arguments to the function.  We lookup for such a label before
     * transforming the CB, but if it does not exist, we create it lazily.
     * Thus the state records is there is no such label known, whether there is
     * and it already exists in the CB, or if we have generated the label for
     * this entry, but not made the blocks (and adjusted the entry code).
     *)
    datatype selfEntry = SeNone | SeExists of M.label | SeCreate of M.label

    datatype state = S of {
      pd   : PassData.t,
      stm  : M.symbolTableManager,
      self : (M.variable * selfEntry * M.variable M.callConv) ref,
      blks : M.block LD.t ref
    }

    fun stateMk (pd, stm, entry) = S {pd = pd, stm = stm, 
                                      self = ref (entry, SeNone, M.CcCode), (* entry, entry label, cc *)
                                      blks = ref LD.empty}

    fun getStm (S {stm, ...}) = stm

    fun click (S {pd, ...}, s) = PassData.click (pd, s)

    fun newLabel s                 = MSTM.labelFresh (getStm s)
    fun cloneVar (s, x)            = MSTM.variableClone (getStm s, x)
    fun variableFresh (s, h, t, g) = MSTM.variableFresh (getStm s, "m" ^ h ^ "_#", t, g)

    fun getSelfEntry (S {self, ...}) = #2 (!self)

    (* Is f the function being processed?  If so, return the self entry label
     * creating it if necessary.
     *)
    fun isSelf (s as S {self, ...}, f) =
        let
          val (f', se, cc) = !self
        in
          if f = f' then
            case se
             of SeNone =>
                let
                  val l = newLabel s
                  val () = self := (f', SeCreate l, cc)
                in
                  SOME (l, cc)
                end
              | SeExists l => SOME (l, cc)
              | SeCreate l => SOME (l, cc)
          else
            NONE
        end

    fun setSelf (S {self, ...}, f, se, cc) = self := (f, se, cc)

    fun getBlock (S {blks, ...}, l) = Option.valOf (LD.lookup (!blks, l))

    fun addBlock (S {blks, ...}, l, b) = blks := LD.insert (!blks, l, b)

    fun getBlocks (S {blks, ...}) =
        let
          val blocks = !blks
          val () = blks := LD.empty
        in blocks
        end

    (* The global code should directly include the listed variables, which
     * return to the label or return from the code.  The codes to include
     * is the transitive closure of this information.
     *)
    datatype funInfo = FI of (M.variable * M.cuts * (M.label option)) list

    (* The environment records:
     *   config:     The configuration.
     *   fmail:      The FMil for the original program.
     *   funs:       The codes to directly include in each code global.
     *   curReturn:  The label the current cb fragments should return to,
     *               NONE means they should return.
     *   curCuts:    The conservative set of cut targets to add to inlined
     *               code.
     *   keep:       The code globals that are unknown thus are being kept.
     *   funEntries: The label to jump to to call a given code global being
     *               inlined.
     *)
    datatype env = E of {
      config     : Config.t,
      fmil       : FMil.t,
      funs       : funInfo VD.t,
      curReturn  : M.label option,
      curCuts    : M.cuts,
      keep       : VS.t,
      funEntries : M.label VD.t
    }

    (* Given the analysis result compute the initial environment.
     * This computes the global environemnt: config, keep, funs, codes,
     * and entry.
     *)
    fun envMk (config, A.A {funs, fmil}, globals, entry) =
        let
          fun addEntry (funs, f, e) =
              let
                val fi =
                    case VD.lookup (funs, f)
                     of NONE         => FI [e]
                      | SOME (FI fs) => FI (e::fs)
              in
                VD.insert (funs, f, fi)
              end
          fun getFun r = FMil.getLabelFun (fmil, r)
          fun doOne (f, r, (keep, funs)) =
              case r
               of A.RUncalled       => (keep,                funs                                               )
                | A.RUnknown        => (VS.insert (keep, f), funs                                               )
                | A.RFunc c         => (keep,                addEntry (funs, c, (f, MU.Cuts.justExits, NONE))   )
                | A.RCont (r, cuts) => (keep,                addEntry (funs, getFun r, (f, cuts, SOME r)))
          val (keep, funs) = VD.fold (funs, (VS.empty, VD.empty), doOne)
        in
          E {config = config, fmil = fmil, funs = funs, curReturn = NONE,
             curCuts = MU.Cuts.none, keep = keep, funEntries = VD.empty}
        end

    fun getConfig (E {config, ...}) = config
    fun getFMil (E {fmil, ...}) = fmil
    fun getFun (E {funs, ...}, f) = VD.lookup (funs, f)
    fun getReturn (E {curReturn, ...}) = curReturn
    fun getCuts (E {curCuts, ...}) = curCuts
    fun keep (E {keep, ...}, f) = VS.member (keep, f)
    fun getFunEntry (E {funEntries, ...}, f) = VD.lookup (funEntries, f)

    (* Build the environment for transforming the cb of
     * a kept code global.  The main thing is to add in funEntries and
     * reset curReturn and curCuts.
     *)
    fun envReset (E {config, fmil, keep, funs, ...}, fes) =
        E {config = config, fmil = fmil, keep = keep, funs = funs, curReturn = NONE,
           curCuts = MU.Cuts.justExits, funEntries = fes}

    (* Build the environment for transformaing an inlined cb.  Here we
     * add in curReturn and curCuts.
     *)
    fun envPush (E {config, fmil, keep, funs, funEntries, ...}, ro, cuts) =
        E {config = config, fmil = fmil, keep = keep, funs = funs, curReturn = ro,
           curCuts = cuts, funEntries = funEntries}

    (* Given a Mil call and args, compute an optional label to jump to and the
     * args to pass that label, or NONE if this should remain a call.
     *)
    fun getCallEntryAndArgs (state, env, c, args) =
        case c
         of M.CCode {ptr, ...} =>
            (case getFunEntry (env, ptr)
              of NONE => NONE
               | SOME l => SOME (l, args))
          | M.CClosure {cls, ...} => NONE
          | M.CDirectClosure {cls, code, ...} =>
            (case getFunEntry (env, code)
              of NONE => NONE
               | SOME l => SOME (l, Utils.Vector.cons (M.SVariable cls, args)))

    (* Given a Mil call that is a tail call, determine if it is
     * a self tail call and compute an optional label to jump to, or
     * NONE if this call is not a self tailcall
     *)
    fun isSelfTailcall (state, env, c) =
        case c
         of M.CCode {ptr, ...}           => 
            (case isSelf (state, ptr)
              of SOME (l, M.CcCode) => SOME (l, Vector.new0 (), Vector.new0 ())
               | SOME _             => Fail.fail ("MilContify", "isSelfTailCall", "Code: mismatched calling convention")
               | NONE               => NONE)
          | M.CClosure _                 => NONE
          | M.CDirectClosure {code, cls} => 
            (case isSelf (state, code)
              of SOME (l, M.CcClosure {cls = _, fvs}) => 
                 let
                   val fvs = Vector.map (fvs, fn v => cloneVar (state, v))
                   val ts = Vector.map (fvs, fn v => MU.SymbolTableManager.variableTyp (getStm state, v))
                   val fks = Vector.map (ts, fn t => MU.FieldKind.fromTyp (getConfig env, t))
                   val get = fn (i, v) => MU.Instruction.new (v, M.RhsClosureGetFv {fvs = fks, cls = cls, idx = i})
                   val instrs = Vector.mapi (fvs, get)
                   val args = Vector.map (Utils.Vector.cons (cls, fvs), M.SVariable)
                 in SOME (l, instrs, args)
                 end
               | SOME _                         => 
                 Fail.fail ("MilContify", "isSelfTailCall", "Closure: mismatched calling convention")
               | NONE                           => NONE)


    (* Given a Mil inter proc that is a tail inter proc, determine if it is
     * a self tail inter proc and compute an optional label and arguments to jump to,
     * or NONE if this inter proc is not a self tail inter proc.
     *)
    fun isSelfTailInterProc (state, env, ip) =
        case ip
         of M.IpCall {call, args, ...} => 
            Option.map (isSelfTailcall (state, env, call), fn (l, is, vs) => (l, is, Vector.concat [vs, args]))
          | M.IpEval _ => NONE

    (* In inlined code rewrite the cuts annotation *)
    fun rewriteCuts (state, env, cuts) = MU.Cuts.inlineCall (cuts, getCuts env)

    (*** CB transformation stuff ***)

    (* Is the callee inlined, if so return label and args otherwise return NONE *)
    fun doInterProc (state, env, callee, ret) =
        case callee
         of M.IpCall {call, args} =>
            (case getCallEntryAndArgs (state, env, call, args)
              of NONE => NONE
               | x as SOME _ =>
                 let
                   val () = click (state, case ret of M.RNormal _ => "callJump" | M.RTail _ => "tailJump")
                 in x
                 end)
          | M.IpEval _ => NONE

    fun doTransfer (state, env, t) =
        let
          val just = fn t => (Vector.new0 (), t)
          val nochange = just t
        in
          case t
           of M.TGoto _ => nochange
            | M.TCase _ => nochange
            | M.TInterProc {callee, ret, fx} =>
              (case doInterProc (state, env, callee, ret)
                of NONE =>
                   (* Callee is not inlined *)
                     (case ret
                       of M.RNormal {rets, block, cuts} =>
                          (* Normal inter proc, rewrite the cuts *)
                            let
                              val ret = M.RNormal {rets = rets, block = block, cuts = rewriteCuts (state, env, cuts)}
                              val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                            in just t
                            end
                            (* Tail inter proc, check if this CB is inlined and if it is a self tail inter proc *)
                        | M.RTail {exits} =>
                          case getReturn env
                           of NONE =>
                              (* This CB does not return to a label in the final CB, check for self tail inter proc *)
                                (case isSelfTailInterProc (state, env, callee)
                                  of NONE => (* Not a self tail inter proc, no changes *) just t
                                   | SOME (l, is, args) =>
                                     (* Self tail inter proc, jump to reentry label *)
                                       let
                                         val () = click (state, "selfTail")
                                       in
                                         (is, M.TGoto (M.T {block = l, arguments = args}))
                                       end)
                                (* This CB is inlined and returns to r, must convert tail back to normal *)
                            | SOME r => 
                              let
                                val () = click (state, "untail")
                                (* Generate return vars and a label that jumps to r with those return vars as args *)
                                val vs = MU.Block.parameters (getBlock (state, r))
                                val rvs = Vector.map (vs, fn v => cloneVar (state, v))
                                val l = newLabel state
                                val t = M.TGoto (M.T {block = r, arguments = Vector.map (rvs, M.SVariable)})
                                val blk = M.B {parameters = Vector.new0 (), instructions = Vector.new0 (), transfer = t}
                                val () = addBlock (state, l, blk)
                                (* Rewrite cuts and make normal inter proc *)
                                val cuts = rewriteCuts (state, env, M.C {exits = exits, targets = LS.empty})
                                val ret = M.RNormal {rets = rvs, block = l, cuts = cuts}
                                val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                              in just t
                              end)
                     (* Callee is inlined, jump to its entry label *)
                 | SOME (l, os) => just (M.TGoto (M.T {block = l, arguments = os})))
            | M.TReturn os =>
              (* If we have a return label for this fragment, then jump to it
               * instead of returning.
               *)
                (case getReturn env
                  of NONE => just (M.TReturn os)
                   | SOME r =>
                     let
                       val () = click (state, "retJump")
                     in
                       just (M.TGoto (M.T {block = r, arguments = os}))
                     end)
            | M.TCut {cont, args, cuts} => 
              just (M.TCut {cont = cont, args = args, cuts = rewriteCuts (state, env, cuts)})
            | M.THalt _ => nochange
        end

    fun doInstr (state, env, i as M.I {dests, n, rhs}) =
        case rhs
         of M.RhsClosureInit {cls, code = SOME codeVar, fvs} =>
            if keep (env, codeVar) then 
              i
            else
              let
                val () = click (state, "dummyCode")
                val rhs = M.RhsClosureInit {cls = cls, code = NONE, fvs = fvs}
                val i = M.I {dests = dests, n = n, rhs = rhs}
              in i
              end
          | _ => i

    fun doBlock (state, env, l, b) =
        let
          val M.B {parameters, instructions, transfer} = b
          val instructions = Vector.map (instructions, fn i => doInstr (state, env, i))
          val (instructions', t) = doTransfer (state, env, transfer)
          val instructions = Vector.concat [instructions, instructions']
          val b = M.B {parameters = parameters, instructions = instructions, transfer = t}
          val () = addBlock (state, l, b)
        in ()
        end

    fun transformCB (state, env, M.CB {blocks, ...}) = LD.foreach (blocks, fn (l, b) => doBlock (state, env, l, b))

    (* Inline another code global into the current cb.
     * cuts is the conservative set of cuts for the current cb;
     * f is the variable for the global;
     * ro is the return point (NONE means return from the current cb)
     *)
    fun addCode (state, env, (f, cuts, ro)) =
        let
          val c = FMil.getCode (getFMil env, f)
          val M.F {cc, args, body, ...} = c
          val M.CB {entry, ...} = body
          val (args, is) =
              case cc
               of M.CcCode => (args, Vector.new0 ())
                | M.CcClosure {cls, fvs} =>
                  let
                    val () = click (state, "closures")
                    val args = Vector.concat [Vector.new1 cls, args]
                    val fvts = Vector.map (fvs, fn v => MU.SymbolTableManager.variableTyp (getStm state, v))
                    val fvfks = Vector.map (fvts, fn t => MU.FieldKind.fromTyp (getConfig env, t))
                    fun doOne (i, fv) =
                        M.I {dests = Vector.new1 fv, n = 0, rhs = M.RhsClosureGetFv {fvs = fvfks, cls = cls, idx = i}}
                    val is = Vector.mapi (fvs, doOne)
                  in (args, is)
                  end
                | _ => Fail.fail ("MilContify", "addCode", "bad calling convention: " ^ I.variableString' f)
          val l = Option.valOf (getFunEntry (env, f))
          val et = M.TGoto (M.T {block = entry, arguments = Vector.new0 ()})
          val eb = M.B {parameters = args, instructions = is, transfer = et}
          val () = addBlock (state, l, eb)
          val env = envPush (env, ro, cuts)
          val () = transformCB (state, env, body)
        in ()
        end

    (* Figure out all the labels that might get cut to in the given cb *)
    fun computeCuts blocks =
        let
          fun doOneI (M.I {rhs, ...}, cuts) =
              case rhs
               of M.RhsCont l => LS.insert (cuts, l)
                | _           => cuts
          fun doOneB (l, M.B {instructions = is, ...}, cuts) = Vector.fold (is, cuts, doOneI)
          val cuts = LD.fold (blocks, LS.empty, doOneB)
        in cuts
        end

    (* Find a self entry in the code body using a heuristic *)
    fun findSelfEntry (state, env, cc, args, entry, blocks) =
        let
          fun checkSame (v, opnd) =
              case opnd
               of M.SVariable v' => v = v'
                | _              => false
          val args = 
              case cc
               of M.CcClosure {cls, fvs} => Utils.Vector.cons (cls, Vector.concat [fvs, args])
                | _                      => args
        in
          case LD.lookup (blocks, entry)
           of SOME (M.B {instructions, transfer = M.TGoto (M.T {block, arguments, ...}), ...}) =>
              if Vector.length instructions = 0 andalso
                 Vector.length args = Vector.length arguments andalso
                 Vector.forall2 (args, arguments, checkSame) then
                SOME block
              else
                NONE
            | _ => NONE
        end

    (* We decided to make a self entry label for optimising self tail calls.
     * Now build the block for this label and adjust the entry code to use it.
     *)
    fun makeSelfEntry (state, env, cc, args, entry, selfEntry) =
        let
          val nentry = newLabel state
          val nargs = Vector.map (args, fn x => cloneVar (state, x))
          val (cc, nformals, nactuals, args) = 
              case cc
               of M.CcClosure {cls, fvs} => 
                  let
                    val ncls = cloneVar (state, cls)
                    val nfvs = Vector.map (fvs, fn v => cloneVar (state, v))
                    val cc = M.CcClosure {cls = ncls, fvs = nfvs}
                    val args = Utils.Vector.cons (cls, Vector.concat [fvs, args])
                    val nactuals = Utils.Vector.cons (ncls, Vector.concat [nfvs, nargs])
                  in (cc, nargs, nactuals, args)
                  end
                | _                      => (cc, nargs, nargs, args)
          val nactualso = Vector.map (nactuals, M.SVariable)
          val neb = M.B {parameters = Vector.new0 (),
                         instructions = Vector.new0 (),
                         transfer = M.TGoto (M.T {block = selfEntry, arguments = nactualso})}
          val () = addBlock (state, nentry, neb)
          val seb = M.B {parameters = args,
                         instructions = Vector.new0 (),
                         transfer = M.TGoto (M.T {block = entry, arguments = Vector.new0 ()})}
          val () = addBlock (state, selfEntry, seb)
        in (cc, nformals, nentry)
        end

    (* Transform a code global that is kept *)
    fun transformCode (state, env, x, f) =
        let
          (* Compute the code globals to include *)
          (* For each one make a label to jump to to call it,
           * and if it returns to a return label (which takes no parameters) make a new return label that takes
           * parameters for it to jump to.  If that return label had unique return variables then make a single
           * label using those return variables as parameters; otherwise make a return label per function using
           * the return types of the function to generate fresh parameters.
           *)
          (* Accumulate all the newly generated return labels so that they can be added into the code body. *)
          (* Note that here we use the fact that if A(f) = Func g then
           * A(g) = Unknown/Uncalled and never Ret l.  This is true of the
           * dominator analysis but not of an arbitrary safe analysis (as
           * defined in the Fluet et al paper).  This means that we do not
           * have to adjust the return labels when computing the transitive
           * closure.
           *)
          fun doOneA cuts ((f, cuts', ro), (fs, fes)) =
              let
                val () = click (state, "inlines")
                val l = newLabel state
                val fes = VD.insert (fes, f, l)
                val cuts = MU.Cuts.inlineCall (cuts', cuts)
                val x = doOneB (f, cuts, ((f, cuts, ro)::fs, fes))
              in x
              end
          and doOneB (f, cuts, x) =
              case getFun (env, f)
               of NONE => x
                | SOME (FI fs) => List.fold (fs, x, doOneA cuts)
          (* justExits is the identity for inlineCall *)
          val (fs, funEntries) = doOneB (x, MU.Cuts.justExits, ([], VD.empty))
          (* Deconstruct the code *)
          val M.F {fx, escapes, recursive, cc, args, rtyps, body} = f
          val M.CB {entry, blocks} = body
          val cuts = computeCuts blocks
          (* Set up the new environment *)
          val env = envReset (env, funEntries)
          val se =
              case findSelfEntry (state, env, cc, args, entry, blocks)
               of NONE   => SeNone
                | SOME l => SeExists l
          val () = setSelf (state, x, se, cc)
          (* Transform this global's cb *)
          val () = transformCB (state, env, body)
          (* Inline and transform the included cbs *)
          val () = List.foreach (List.rev fs, fn x => addCode (state, env, x))
          (* Create the self entry if necessary *)
          val (cc, args, entry) =
              case getSelfEntry state
               of SeCreate l => makeSelfEntry (state, env, cc, args, entry, l)
                | _          => (cc, args, entry)
          (* Build result *)
          val body = M.CB {entry = entry, blocks = getBlocks state}
          val f =
              M.F {fx = fx, escapes = escapes, recursive = recursive, cc = cc, args = args, rtyps = rtyps, body = body}
        in f
        end

    fun transformGlobal (state, env, x, g, globals) =
        case g
         of M.GCode f =>
            if keep (env, x) then
              VD.insert (globals, x, M.GCode (transformCode (state, env, x, f)))
            else
              globals
          | M.GClosure {code = SOME c, fvs} =>
            let
              val c = if keep (env, c) then SOME c else NONE
            in
              VD.insert (globals, x, M.GClosure {code = c, fvs = fvs})
            end
          | _ => VD.insert (globals, x, g)

    fun transformProgram (pd, p, a) =
        let
          val config = PassData.getConfig pd
          val M.P {includes, externs, entry, globals, symbolTable} = p
          val stm = IM.fromExistingAll symbolTable
          val state = stateMk (pd, stm, entry)
          val env = envMk (config, a, globals, entry)
          fun doOne (x, g, globals) = transformGlobal (state, env, x, g, globals)
          val globals = VD.fold (globals, VD.empty, doOne)
          val st = MU.SymbolTableManager.finish stm
          val p = M.P {includes = includes, externs = externs, entry = entry, globals = globals, symbolTable = st}
          val () = PassData.report (pd, passname)
        in p
        end

  end (* structure T *)

  structure Prep = 
  struct
    (* We make sure that every call of the form f(x) -> (rvars) Lret 
     * is in a canonical form:
     *  f(x) -> (rvars) Lret'
     *  Lret'():
     *    goto Lret(rvars)
     * whenever rvars is non-empty.
     * We treat Lret as the return label of the call.
     *)

    datatype state = S of {blocks : M.block LD.t ref, rLbls : M.label LD.t ref, stm : MSTM.t}

    datatype env = E of {pd : PD.t}

    val ((stateSetBlocks, stateGetBlocks),
         (stateSetRLbls, stateGetRLbls),
         (stateSetStm, stateGetStm)) = 
        let
          val r2t = fn S {blocks, rLbls, stm} => (blocks, rLbls, stm)
          val t2r = fn (blocks, rLbls, stm) => 
                       S {blocks = blocks, rLbls = rLbls, stm = stm}
        in FunctionalUpdate.mk3 (r2t, t2r)
        end

    val ((envSetPd, envGetPd)
        ) = 
        let
          val r2t = fn E {pd} => (pd)
          val t2r = fn (pd) => E {pd = pd}
        in FunctionalUpdate.mk1 (r2t, t2r)
        end

    val getBlock : state * env * M.label -> M.block = 
     fn (state, env, l) => valOf (LD.lookup (!(stateGetBlocks state), l))

    val addBlock : state * M.label * M.block -> unit = 
     fn (state, l, b) => 
        let
          val br = stateGetBlocks state
          val () = br := LD.insert (!br, l, b)
        in ()
        end

    val addRLbl : state * M.label * M.label -> unit = 
     fn (state, l, rl) =>
        let
          val rlr = stateGetRLbls state
          val () = rlr := LD.insert (!rlr, l, rl)
        in ()
        end

    val newLabel = fn state => MSTM.labelFresh (stateGetStm state)

    val cloneVariables = fn (state, vs) => Vector.map (vs, fn v => MSTM.variableClone (stateGetStm state, v))
   
    val doTransfer : state * env * M.label * M.transfer -> M.transfer = 
     fn (state, env, blockL, t) => 
        let
          val getPassthru' = 
              Try.lift 
                (fn (M.B {parameters, instructions, transfer}, rvs) => 
                    let
                      val () = Try.require (Vector.isEmpty instructions)
                      val () = Try.require (Vector.isEmpty parameters)
                      val M.T {block, arguments} = Try.<@ MU.Transfer.Dec.tGoto transfer
                      val () = Try.require (Vector.length rvs = Vector.length arguments)
                      val eq1 = fn (v, oper) => 
                                   (case oper 
                                     of M.SVariable v' => v = v'
                                      | M.SConstant _  => false)
                      val () = Try.require (Vector.forall2 (rvs, arguments, eq1))
                    in block
                    end)

          val getPassthru = 
           fn (block, rvs) => if Vector.isEmpty rvs then SOME block
                              else getPassthru' (getBlock (state, env, block), rvs)

          val t = 
              case t
               of M.TInterProc {callee = callee as M.IpCall _, ret = M.RNormal {rets, block, cuts}, fx} => 
                  (case getPassthru (block, rets)
                    of SOME retL => 
                       let
                         val () = addRLbl (state, blockL, retL)
                       in t
                       end
                     | NONE => 
                       let
                         val retL = newLabel state
                         val mergeL = newLabel state
                         val rets' = cloneVariables (state, rets)
                         val retB = M.B {parameters = Vector.new0 (),
                                         instructions = Vector.new0 (),
                                         transfer = M.TGoto (M.T {block = mergeL, 
                                                                  arguments = Vector.map (rets', M.SVariable)})}
                         val mergeB = M.B {parameters = rets,
                                           instructions = Vector.new0 (),
                                           transfer = M.TGoto (M.T {block = block, arguments = Vector.new0 ()})}
                         val () = addBlock (state, retL, retB)
                         val () = addBlock (state, mergeL, mergeB)
                         val () = addRLbl (state, blockL, mergeL)
                         val ret = M.RNormal {rets = rets', block = retL, cuts = cuts}
                         val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                       in t
                       end)
                | _ => t
        in t
        end

    val doCodeBody : state * env * M.codeBody -> M.codeBody = 
     fn (state, env, cb) => 
        let
          val M.CB {entry, blocks} = cb
          val br = ref blocks
          val state = stateSetBlocks (state, br)
          val doT = fn l => fn t => doTransfer (state, env, l, t)
          val doBlock =
           fn (l, b) => addBlock (state, l, MU.Block.Map.transfers (b, doT l))
          val blocks = LD.foreach (blocks, doBlock)
          val blocks = !br
        in M.CB {entry = entry, blocks = blocks}
        end

    val rewrite = 
     fn (p, pd) =>
        let
          val M.P {includes, externs, entry, globals, symbolTable} = p
          val stm = IM.fromExistingAll symbolTable
          val rlr = ref LD.empty
          val state = S {blocks = ref LD.empty, rLbls = rlr, stm = stm}
          val env = E {pd = pd}
          val globals = MU.Globals.Map.codeBodies (globals, fn g => doCodeBody (state, env, g))
          val symbolTable = IM.finish stm
          val rLbls = !rlr
          val p = M.P {includes = includes, externs = externs, entry = entry, 
                       globals = globals, symbolTable = symbolTable}
        in (p, rLbls)
        end
  end (* structure prep *)

  (*** The pass ***)

  fun program (p, pd) =
      let
        val config = PassData.getConfig pd
        fun doP () = Prep.rewrite (p, pd)
        val (p, rLbls) = Pass.doPassPart (config, "Contify Prep", doP)
        fun doA () = A.analyseProgram (config, p, rLbls)
        val a = Pass.doPassPart (config, "Contify Analysis", doA)
        fun doT () = T.transformProgram (pd, p, a)
        val p = Pass.doPassPart (config, "Contify Transformation", doT)
      in p
      end

  val description = {name        = passname,
                     description = "Contify Mil",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val debugs = [A.prnCallGraphD, A.prnReachableD, A.prnAnalyseD]

  val associates = {controls  = [],
                    debugs    = debugs,
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end;

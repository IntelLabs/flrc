(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, July 2008 *)

signature MIL_DATAFLOW_ANALYSIS =
sig
  type info
  type env

  val debugs : Config.Debug.debug list

  (* requires entry to dominate all blocks in the set of blocks
     to operate on. The fifth argument is a function that computes
     the initial information for the start block. This might be 
     required as no transfer to the block is followed. *)
  val blocks : env
               * Mil.t
               * Mil.code
               * Identifier.label      (* entry *)
               * (env * (Mil.variable -> info) * Mil.label -> info Vector.t)
               * Identifier.LabelSet.t (* blocks to operate on *)
               -> info Identifier.VariableDict.t

  val blocks' : env
                * Mil.t
                * Mil.code
                * Identifier.label      (* entry *)
                * (env * (Mil.variable -> info) * Mil.label -> info Vector.t)
                * Identifier.LabelSet.t (* blocks to operate on *)
                * MilCfg.LabelDominance.t
                -> info Identifier.VariableDict.t

  val function : env 
                 * Mil.t 
                 * Mil.code
                 -> info Identifier.VariableDict.t

  val function' : env 
                  * Mil.t 
                  * Mil.code
                  * MilCfg.LabelDominance.t
                  -> info Identifier.VariableDict.t

  val program : env * Mil.t -> info Identifier.VariableDict.t
end
                           
functor MilDataFlowAnalysisF (
  type env
  type info

  val getConfig : env -> Config.t
  val passname : string
  val indent : int

  val deriveConstant : env * Mil.constant -> info
  val deriveInstr : env
                    * (Mil.variable -> info)
                    * Mil.variable vector
                    * Mil.rhs 
                    -> (Mil.variable * info) vector
  val deriveGlobal : env
                     * (Mil.variable -> info)
                     * Mil.global
                     -> info
  val deriveFunction : env
                       * (Mil.variable -> info)
                       * Mil.operand Vector.t (* arguments *)
                       * int                  (* no of target block args *)
                       * Mil.cuts
                       * Effect.set
                       -> info Vector.t
  val deriveBlock : env
                    * (Mil.variable -> info)
                    * Identifier.label      (* target *)
                    * Mil.operand Vector.t  (* parameters *)
                    -> info Vector.t

  val emptyInfo  : env * Mil.variable -> info
  val mergeInfo  : env * info * info -> info
  val coerceInfo : env * Mil.typ * info -> info
  val equalInfo  : env * info * info -> bool
  val layoutInfo : env * info -> Layout.t
) :> MIL_DATAFLOW_ANALYSIS where type env = env 
                             and type info = info 

= struct
  val mypassname = passname ^ ":DFA"

  val (debugPassD, debugPass) = 
    Config.Debug.mk (mypassname, "debug the dataflow analysis module")

  val debugs = [debugPassD]

  type env = env
  type info = info

  structure L = Layout
  structure LU = LayoutUtils
  structure M = Mil
  structure DomInfo = MilCfg.LabelDominance
  structure I = Identifier
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict
  structure VD = Identifier.VariableDict
  structure P = Prims
 
  (*
   * local environment structure
   *)

  (* TODO WL: split between int env and state *)  
  (*   env: immutable *)
  datatype 'a localenv = E of {
           env         : env,
           dominfo     : DomInfo.t, 
           blocks      : M.block LD.t,  (* blocks it works on *)
           current     : I.label list ref, (* a stack of blocks currently 
                                            * working on *)
           todo        : LS.t ref,      (* todo blocks *)
           fixinfo     : info Vector.t LD.t ref, (* fixinfo for current blocks'
                                                  *)
           restriction : LS.t option    (* blocks that DFA can work on *)
  }

  (*
   * debugging functions
   *)
  fun dbgPrint (E env, msg) = 
      if Config.debug andalso (debugPass (getConfig (#env env))) then 
        print msg
      else ()

  fun dbgLayout' (msg) = if Config.debug then LU.printLayout msg else ()

  structure Debug =
  struct
    fun trace s = print ("DataFLowAnalyse tracing function: " ^ s ^ "\n")
  end

  (*
   * state manipulation functions
   *)

  fun getStateInfo (env, st, v) = VD.lookup (!st, v)

  fun getStateInfoDef (env, st, v) = Utils.Option.get (VD.lookup (!st, v), emptyInfo (env, v))

  fun setStateInfo (st, v, i) = st := VD.insert (!st, v, i)

  fun updateStateInfo (E env, st, v, i) = 
      let
        fun dbgString (prefix, var, info) =
            (prefix ^ " " ^
             LU.toString (I.layoutVariable' var) ^ ": " ^
             LU.toString (layoutInfo (#env env, info)) ^ "\n")
        val new = 
          case getStateInfo (#env env, st, v)
           of SOME old => 
              let
                val () = dbgPrint (E env, dbgString ("PRE-UPDATE", v, old))
                val () = dbgPrint (E env, dbgString ("NEW-INFO", v, i))
              in
                mergeInfo (#env env, old, i)
              end
            | NONE => i
        val () = dbgPrint (E env, dbgString ("UPDATE", v, new))
      in 
        setStateInfo (st, v, new)
      end

  fun stateDict (env, st) (v) = getStateInfoDef (env, st, v)

  fun layoutStateInfo (env, info) =
      let
      in ()
      end

  fun layoutStateDict (env, st) =
      let
(*	  val () = dbgLayout' ( *)
      in ()
      end      
  (* XXX WL: Code review here ! *)    

  (*
   * local environment functions
   *)
  fun mkLocalEnv' (env : env, 
                   M.CB {entry, blocks} : M.codeBody,
                   restr : LS.t option, 
                   dominfo : DomInfo.t) =
      E {env = env, 
         dominfo = dominfo, 
         blocks = blocks,
         current = ref [],
         todo = ref LS.empty,
         fixinfo = ref LD.empty,
         restriction = restr}

  fun mkLocalEnv (env : env, 
                  funbody : M.codeBody, 
                  restr : LS.t option,
                  m as M.P {globals, symbolTable, ...}) =
      let
        val config = getConfig env
        val si = I.SymbolInfo.SiTable symbolTable
        val cfg = MilCfg.build (config, si, funbody)
        val domtree = MilCfg.getLabelDomTree cfg
        val dominfo = MilCfg.LabelDominance.new domtree
      in
        mkLocalEnv' (env, funbody, restr, dominfo)
      end

  fun envGetBlock (E env, label) = Option.valOf (LD.lookup (#blocks env, label))

  (* pre-process before getting into a block. It will process the block's 
     successors as well, so it enters an SCC. *)
  fun envEnterBlock (E env, st, label) =
      let
        val () = dbgPrint (E env, "ENTER: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val M.B {parameters, ...} = envGetBlock (E env, label)
        val vect = Vector.map (parameters, fn (s) => getStateInfoDef (#env env, st, s))
        val () = #fixinfo env := LD.insert (!(#fixinfo env), label, vect)
        val () = List.push (#current env, label)
      in ()
      end

  (* post-process after leaving a block or SCC *)
  fun envExitBlock (E env, label) =
      let
        val () = dbgPrint (E env, "EXIT: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val () = #fixinfo env := LD.remove (!(#fixinfo env), label)
        val () = #todo env := LS.remove (!(#todo env), label)
        val _ = List.pop (#current env)
      in ()
      end
    
  fun envGetDeferred (E env, label) = LD.lookup (!(#fixinfo env), label)

  (* process deferred block *)
  fun envDefer (E env, label, args) =
      let
        val () = dbgPrint (E env, "DEFER: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val new = case envGetDeferred (E env, label) 
                   of SOME old => Vector.map2 (old, args, fn (a, b) => mergeInfo (#env env, a, b))
                    | NONE => args
      in
        #fixinfo env := LD.insert (!(#fixinfo env), label, new)
      end

  (* push a block into todo list *)
  fun envRememberJump (E env, label) = 
      #todo env := LS.insert (!(#todo env), label)

  (* XXX WL: code review: is the comment consistent with the code? *)
  (* target is not in restriction set and dominates current list *) 
  fun envValidTarget (E env, target) =
      (Option.fold (#restriction env, 
                    true, 
                    fn (d, _) => LS.member (d, target))) andalso
      ((List.isEmpty (!(#current env))) orelse
       ((List.first (!(#current env)) <> target) andalso
        (DomInfo.dominates (#dominfo env, 
                            List.first (!(#current env)), target))))

  (* get ready blocks from todo set *)
  fun envGetTodos (E env) =
      let
        val {no, yes} = LS.partition (!(#todo env), fn (l) => envValidTarget (E env, l))
        val () = #todo env := no
      in yes
      end

  (*
   * collect information for global values
   *)
  fun inferGlobalInfo (genv, st, v, g) = setStateInfo (st, v, deriveGlobal (genv, stateDict (genv, st), g))

  (*
   * Traversal functions to guide the inference.
   *
   * To propagate the information in cycles, I perform a fixpoint
   * iteration there. This of course only works if the
   * compute functions are monotonic.
   *
   *)
  fun projectArgs (E env, 
                   st : info VD.t ref, 
                   l : Identifier.label, 
                   info : info vector,
                   m : Mil.t) =
      let
        val M.B {parameters, ...} = envGetBlock (E env, l)

        val paramlen = Vector.length parameters
      in
        if paramlen > 0 then
          Vector.foreach2 (parameters, 
                           info,
                        fn (p, i) => updateStateInfo (E env, st, p, i))
        else ()
      end

  fun goBlock (E env, st, l, m) =
      let
	  fun fix (l, b as M.B {parameters, ...}) = 
            let
              val i = Vector.map (parameters, 
                                  fn (p) => getStateInfoDef (#env env, st, p))
              val () = inferBlock (E env, st, b, m)
              fun processTodos (todos : LS.t) =
                  let 
                    fun doOne (l : I.label) = 
                        let
                          val () = 
                              Option.app (envGetDeferred (E env, l),
                                          fn x => projectArgs (E env, st, l, x, m))
                        in
                          goLabel (E env, st, l, m)
                        end
                    val () = LS.foreach (todos, doOne)
                    val next = envGetTodos (E env)
                  in 
                    if LS.isEmpty next then () 
                    else processTodos (next)
                  end
              val () = processTodos (envGetTodos (E env))
              val nt = envGetDeferred (E env, l)
              val () = Option.app (nt, fn (x) => projectArgs (E env, st, l, x, m))
              val p = 
                case nt
                 of SOME ni => 
                    Vector.fold2 (i, ni, true,
                                  fn (a, b, r) => 
                                     (equalInfo (#env env, a, b)) andalso r)
                  | NONE => true
            in 
              if not p then 
                let
                  val () = dbgPrint (E env, "DFA goBlock ITERATE: " ^ (LU.toString (I.layoutLabel l)) ^ "\n")
                in
                  fix (l, b)
                end
              else 
                dbgPrint (E env, "DFA goBlock FIXED: " ^ (LU.toString (I.layoutLabel l)) ^ "\n")
            end
        val () = envEnterBlock (E env, st, l)
        val () = fix (l, envGetBlock (E env, l))
        val () = envExitBlock (E env, l)
      in ()
      end

  and goLabel (E env, st, l, m) = 
      if envValidTarget (E env, l) then 
        goBlock (E env, st, l, m)
      else 
        envRememberJump (E env, l)

  and inferTransfer (E env, st, transfer : M.transfer, m) = 
      let
        val config = getConfig (#env env)
        val mp as Mil.P {symbolTable, ...} = m
        val si = I.SymbolInfo.SiTable symbolTable
        val tl = MilLayout.layoutTransfer (config, si, transfer)
        val () = LU.printLayout tl

        fun goTarget (E env, st, M.T {block, arguments}) =
            let
              val argnfo = deriveBlock (#env env, stateDict (#env env, st), block, arguments)
              val () = if envValidTarget (E env, block) then 
                         let
                           val () = projectArgs (E env, st, block, argnfo, m)
                         in
                           goBlock (E env, st, block, m)
                         end
                       else 
                         let
                           val () = envDefer (E env, block, argnfo)
                         in 
                           envRememberJump (E env, block)
                         end
            in ()
            end 
        fun goSwitch (E env, st, {on, cases, default}) =
            let
              val () = Vector.foreach (cases, fn (_, t) => goTarget (E env, st, t))
              val () = Option.app (default, fn (t) => goTarget (E env, st, t))
            in ()
            end
      in
        case transfer
         of M.TGoto t => goTarget (E env, st, t)
          | M.TCase s => goSwitch (E env, st, s)
          | M.TPSumCase s => goSwitch (E env, st, s)
          | M.TInterProc {callee, ret, fx} => 
            (case callee
              of M.IpCall {call, args} => (
                 case ret
                  of M.RNormal {rets, block, cuts} =>
                     let
                       val M.B {parameters, ...} = envGetBlock (E env, block)
                       val a = deriveFunction (#env env, stateDict (#env env, st),
                                               args, Vector.length (rets), cuts, fx)
                       val () = if envValidTarget (E env, block) then
                                  projectArgs (E env, st, block, a, m)
                                else
                                  envDefer (E env, block, a)
                     in 
                       goLabel (E env, st, block, m)
                     end
                   | M.RTail => ())
               | M.IpEval _ => ())
          | _ => () (* all others terminate the function *)
      end
  
  and inferInstruction (E env, st, M.I {dests, n, rhs}) = 
      let
        val vis = deriveInstr (#env env, stateDict (#env env, st), dests, rhs)
        val () = Vector.foreach (vis, fn (v, i) => updateStateInfo (E env, st, v, i))
      in ()
      end
    
  and inferBlock (env, st, M.B {instructions, transfer, ...}, m) = 
      let
        val () = Vector.foreach (instructions, fn i => inferInstruction (env, st, i))
        val () = inferTransfer (env, st, transfer, m)
      in () 
      end

  fun doGlobals (env, st, M.P {globals, ...}) =
      VD.foreach (globals, fn (v, g) => inferGlobalInfo (env, st, v, g))

  fun blocks (env, 
              m as M.P {globals, ...}, 
              M.F {body, ...},
              e,
              initf,
              blocks) =
      let
        val st = ref VD.empty
        val () = doGlobals (env, st, m)
        val le = mkLocalEnv (env, body, SOME blocks, m)
        val infos = initf (env, stateDict (env, st), e)
        val () = projectArgs (le, st, e, infos, m)
        val () = goLabel (le, st, e, m)
      in !st
      end

  fun blocks' (env, 
               m as M.P {globals, symbolTable, entry}, 
               M.F {body, ...},
               e,
               initf,
               blocks,
               dominfo) =
      let
        val st = ref VD.empty
        val () = doGlobals (env, st, m)

        val le = mkLocalEnv' (env, body, SOME blocks, dominfo)
        val infos = initf (env, stateDict (env, st), e)
        val () = projectArgs (le, st, e, infos, m)
        val () = goLabel (le, st, e, m)
      in !st
      end

  fun function (env, 
                m as M.P {globals, ...}, 
                M.F {body, ...}) =
      let
        val st = ref VD.empty
        val M.CB {entry, blocks} = body
        val () = doGlobals (env, st, m)
        val le = mkLocalEnv (env, body, NONE, m)
        val () = goLabel (le, st, entry, m)
      in !st
      end
      
  fun function' (env, 
                m as M.P {globals, ...}, 
                M.F {body, ...},
                dominfo) =
      let
        val st = ref VD.empty
        val M.CB {entry, blocks} = body
        val () = doGlobals (env, st, m)
        val le = mkLocalEnv' (env, body, NONE, dominfo)
        val () = goLabel (le, st, entry, m)
      in !st
      end
      
  fun program (env, m as M.P {globals, symbolTable, ...}) = 
      let
        val st = ref VD.empty 
        val () = doGlobals (env, st, m)
        fun inferFunction (env, st, v, g) =
            case g
             of M.GCode (M.F {body, ...}) => 
                let
                  val M.CB {entry, blocks} = body

                  val le = mkLocalEnv (env, body, NONE, m)
                  val () = goLabel (le, st, entry, m)
                in ()
                end
              | _ => ()
        val () = VD.foreach (globals, fn (v, g) => inferFunction (env, st, v, g))
      in !st
      end
end 

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


(* Comment by Leaf, after reviewing code.
 * 
 * This seems to be basically a standard intra-procedural forward
 * dataflow, with tricks to make the computation more efficient.  The
 * functor parameters essentially specify the lattice type, with
 * associated bottom, equality, and join operations; along with
 * transfer functions for instructions, gotos, globals, and calls.
 * 
 * The globals are dealt with in a single un-ordered pass, which is
 * probably not generally correct.  As long as nothing too interesting
 * is being extracted from the globals though, this will be fine.
 *
 * My rough understanding of the algorithm being used here is as follows.
 * For a given block (starting at the entry), compute the forward dataflow
 * information for the block.  From the transfer, recursively analyze
 * any successors which are dominated by the current block.  Anything 
 * which is reachable, but not dominated, gets put on a todo list,
 * and accumulates dataflow information.  When everything dominated
 * by the block has been analyzed (recursively), there will be a todo 
 * list of things leftover.  These are then analyzed (possibly producing 
 * more things to do) until nothing is left.  If a fixed point has been 
 * reached (that is, the initial information that we had for the block
 * is the same as what we have just computed), then we are done.  Otherwise
 * we iterate.
 *
 * Limitations:
 *   - This doesn't provide a distinction between initial values for local variables
 *     and initial values for inputs.
 *)
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
        print (msg ())
      else ()

  fun dbgLayout' (msg) = if Config.debug then LU.printLayout msg else ()

  val fail = 
   fn (f, m) => Fail.fail ("dataflow-analysis.sml", f, m)

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
                val () = dbgPrint (E env, fn () => dbgString ("PRE-UPDATE", v, old))
                val () = dbgPrint (E env, fn () => dbgString ("NEW-INFO", v, i))
              in
                mergeInfo (#env env, old, i)
              end
            | NONE => i
        val () = dbgPrint (E env, fn () => dbgString ("UPDATE", v, new))
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

  val layoutInfoVector = 
   fn (E env, vect) => Vector.layout (fn info => layoutInfo (#env env, info)) vect 

  fun envGetFixInfo (E env, label) = LD.lookup (!(#fixinfo env), label)

  val initializeFixInfo = 
   fn (e as E env, label, vect) =>
      let
        val () = dbgPrint (e, fn () => "FIX_INIT: " ^ LU.toString (I.layoutLabel (label)) ^ " : ")
        val () = dbgPrint (e, fn () => LU.toString (layoutInfoVector (e, vect)) ^ "\n")
        val fref = #fixinfo env
        val fd = !fref
        val () = fref := LD.insert (fd, label, vect)
      in ()
      end

  val clearFixInfo = 
   fn (e as E env, label) =>
      let
        val () = dbgPrint (e, fn () => "FIX_CLEAR: " ^ LU.toString (I.layoutLabel (label)) ^ " : ")
        val () = #fixinfo env := LD.remove (!(#fixinfo env), label)
      in ()
      end

  val mergeFixInfo = 
   fn (e as E env, label, args) => 
      let
        val () = dbgPrint (e, fn () => "FIX_MERGE: " ^ LU.toString (I.layoutLabel (label)) ^ " => ")
        val new = 
            case envGetFixInfo (e, label) 
             of SOME old => Vector.map2 (old, args, fn (a, b) => mergeInfo (#env env, a, b))
              | NONE => args
        val () = dbgPrint (e, fn () => LU.toString (layoutInfoVector (e, new)) ^ "\n")
        val () = #fixinfo env := LD.insert (!(#fixinfo env), label, new)
      in ()
      end

  (* pre-process before getting into a block. It will process the block's 
     successors as well, so it enters an SCC. *)
  fun envEnterBlock (E env, st, label) =
      let
        val () = dbgPrint (E env, fn () => "ENTER: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val M.B {parameters, ...} = envGetBlock (E env, label)
        val vect = Vector.map (parameters, fn (s) => getStateInfoDef (#env env, st, s))
        val () = initializeFixInfo (E env, label, vect)
        val () = List.push (#current env, label)
      in ()
      end

  (* post-process after leaving a block or SCC *)
  fun envExitBlock (E env, label) =
      let
        val () = dbgPrint (E env, fn () => "EXIT: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val () = clearFixInfo (E env, label)
        val () = #todo env := LS.remove (!(#todo env), label)
        val _ = List.pop (#current env)
      in ()
      end
    
  (* process deferred block *)
  fun envDefer (E env, label, args) =
      let
        val () = dbgPrint (E env, fn () => "DEFER: " ^ LU.toString (I.layoutLabel (label)) ^ "\n")
        val () = mergeFixInfo (E env, label, args)
      in ()
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

  (* XXX - why is this safe to do in a single pass?  Why no iterate around cycles?  -leaf *)
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
                              Option.app (envGetFixInfo (E env, l),
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
              val nt = envGetFixInfo (E env, l)
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
                  val () = dbgPrint (E env, fn () => "DFA goBlock ITERATE: " ^ (LU.toString (I.layoutLabel l)) ^ "\n")
                in
                  fix (l, b)
                end
              else 
                dbgPrint (E env, fn () => "DFA goBlock FIXED: " ^ (LU.toString (I.layoutLabel l)) ^ "\n")
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
        fun goSwitch (E env, st, {select, on, cases, default}) =
            let
              val () = Vector.foreach (cases, fn (_, t) => goTarget (E env, st, t))
              val () = Option.app (default, fn (t) => goTarget (E env, st, t))
            in ()
            end
      in
        case transfer
         of M.TGoto t => goTarget (E env, st, t)
          | M.TCase s => goSwitch (E env, st, s)
          | M.TInterProc {callee, ret, fx} => 
            (case callee
              of M.IpCall {call, args} => (
                 case ret
                  of M.RNormal {rets, block, cuts} =>
                     let
                       val M.B {parameters, ...} = envGetBlock (E env, block)
                       val a =
                           deriveFunction (#env env, stateDict (#env env, st), args, Vector.length (rets), cuts, fx)
                       val () = if envValidTarget (E env, block) then
                                  projectArgs (E env, st, block, a, m)
                                else
                                  envDefer (E env, block, a)
                     in 
                       goLabel (E env, st, block, m)
                     end
                   | M.RTail {exits} => ())
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

  fun const f = (fn _ => f)

  fun blocks' (env, m as M.P {globals, ...}, M.F {body, ...}, entry:Identifier.label,
               initf, blocks, dominfo) =
      let
        val st = ref VD.empty
        val () = doGlobals (env, st, m)
        val le = mkLocalEnv' (env, body, SOME blocks, dominfo)
        val () = dbgPrint (le, const "Made local env\n")
        val infos = initf (env, stateDict (env, st), entry)
        val () = dbgPrint (le, const "initf'd\n")
        val () = projectArgs (le, st, entry, infos, m)
        val () = dbgPrint (le, const "project args ok")
        val () = goLabel (le, st, entry, m)
        val () = dbgPrint (le, const "done\n")
      in !st
      end

  fun function (env, m as M.P {globals, ...}, M.F {body, ...}) =
      let
        val st = ref VD.empty
        val M.CB {entry, blocks} = body
        val () = doGlobals (env, st, m)
        val le = mkLocalEnv (env, body, NONE, m)
        val () = goLabel (le, st, entry, m)
      in !st
      end
      
  fun function' (env, m as M.P {globals, ...}, M.F {body, ...}, dominfo) =
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

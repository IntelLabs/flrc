(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

structure MilRewriterClient = 
struct
  datatype 'a change = Stop | Continue | StopWith of 'a | ContinueWith of 'a 
  type ('s, 'e, 'o) rewriter = 's * 'e * 'o -> ('e * 'o) change
  type ('s, 'e, 'o) binder = 's * 'e * 'o  -> ('e * 'o option)
end;

signature MIL_REWRITER = 
sig

  type state
  type env

  type 'a rewriter  = state * env * 'a -> 'a
  type 'a rewriterE = state * env * 'a -> env * 'a

  val variable    : Mil.variable                rewriter
  val simple      : Mil.simple                  rewriter
  val operand     : Mil.operand                 rewriter
  val instruction : Mil.instruction             rewriterE
  val transfer    : Mil.transfer                rewriter
  val codeBody    : Mil.codeBody                rewriter
  val code        : Mil.code                    rewriter
  val global      : (Mil.variable * Mil.global) rewriter
  val globals     : Mil.globals                 rewriterE
  val program     : Mil.t                       rewriter

end;

functor MilRewriterF (
  type state
  type env
  val config      : env -> Config.t
  val label       : (state, env, Mil.label      ) MilRewriterClient.rewriter
  val variable    : (state, env, Mil.variable   ) MilRewriterClient.rewriter
  val operand     : (state, env, Mil.operand    ) MilRewriterClient.rewriter
  val instruction : (state, env, Mil.instruction) MilRewriterClient.rewriter
  val transfer    : (state, env, Mil.transfer   ) MilRewriterClient.rewriter
  val global      : (state, env, Mil.variable*Mil.global) MilRewriterClient.rewriter
  val bind        : (state, env, Mil.variable   ) MilRewriterClient.binder
  val bindLabel   : (state, env, Mil.label      ) MilRewriterClient.binder
  val cfgEnum     : state * env * Mil.codeBody
                    -> (Mil.label * Mil.block) Tree.t Vector.t
  val indent      : int
) :> MIL_REWRITER where type state = state and type env = env = 
struct

  structure M = Mil
  structure VS = Identifier.VariableSet
  structure VD = Identifier.VariableDict
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict
  open MilRewriterClient

  type state = state
  type env = env
  type 'a rewriter  = state * env * 'a -> 'a
  type 'a rewriterE = state * env * 'a -> env * 'a

  val getConfig         = config
  val clientLabel       = label
  val clientVariable    = variable
  val clientOperand     = operand
  val clientTransfer    = transfer
  val clientGlobal      = global
  val clientInstruction = instruction
  val clientBind        = bind
  val clientBindLabel   = bindLabel

  fun bindVar (state, env, v) = 
      let
	val (env, vo) = clientBind (state, env, v)
	val v = Pervasive.Option.getOpt (vo, v)
      in (env, v)
      end

  fun bindLabel (state, env, l) = 
      let
	val (env, lo) = clientBindLabel (state, env, l)
	val l = Pervasive.Option.getOpt (lo, l)
      in (env, l)
      end

  fun bindVars (state, env, vs) =
      let
        fun doOne (v, env) = Utils.Function.flipOut bindVar (state, env, v)
        val (vs, env) = Vector.mapAndFold (vs, env, doOne)
      in (env, vs)
      end

  fun callClientCode (itemhandler, doitem, state, env, item) = 
      case itemhandler (state, env, item)
	of StopWith     (env, i) => i
	 | ContinueWith (env, i) => doitem (state, env, i)
	 | Continue              => doitem (state, env, item)     
	 | Stop                  => item

  fun label (state, env, l) =
      let
        fun dolabel (state, env, l) = l
      in
        callClientCode (clientLabel, dolabel, state, env, l)
      end

  fun variable (state, env, v) = 
      let
        fun dovariable (state, env, v) = v
      in
        callClientCode (clientVariable, dovariable, state, env, v)
      end

  fun simple (state, env, s) = 
      let
        fun doSimple (state, env, s) =
            case s 
             of M.SVariable v => M.SVariable (variable (state, env, v))
              | M.SConstant _ => s
      in
        callClientCode (clientOperand, doSimple, state, env, s)
      end

  val operand = simple

  fun operands (state, env, os) =
      Vector.map (os, fn opnd => operand (state, env, opnd))

  fun doRhs (state, env, rhs) =
      let
        fun doOp opnd = operand (state, env, opnd)
        fun doOps os = Vector.map (os, doOp)
        fun doVar v = variable (state, env, v)
        fun doVarO vo = Option.map (vo, doVar)
        fun doFi fi =
            case fi
             of M.FiFixed _ => fi
              | M.FiVariable opnd => M.FiVariable (doOp opnd)
              | M.FiViFixed _ => fi
              | M.FiViVariable {typ, idx} =>
                M.FiViVariable {typ = typ, idx = doOp idx}
              | M.FiViIndexed {typ, idx} =>
                M.FiViIndexed {typ = typ, idx = doOp idx}
        fun doTf (M.TF {tupDesc, tup, field}) =
            M.TF {tupDesc = tupDesc, tup = doVar tup, field = doFi field}
        fun doFkOps fkos = Vector.map (fkos, fn (fk, opnd) => (fk, doOp opnd))
      in
        case rhs 
         of M.RhsSimple s => M.RhsSimple (simple (state, env, s))
          | M.RhsPrim {prim, createThunks, args} =>
            M.RhsPrim {prim = prim, createThunks = createThunks, args = doOps args}
          | M.RhsTuple {mdDesc, inits} =>
            M.RhsTuple {mdDesc = mdDesc, inits = doOps inits}
          | M.RhsTupleSub tf => M.RhsTupleSub (doTf tf)
          | M.RhsTupleSet {tupField, ofVal} =>
            M.RhsTupleSet {tupField = doTf tupField, ofVal = doOp ofVal}
          | M.RhsTupleInited {mdDesc, tup} =>
            M.RhsTupleInited {mdDesc = mdDesc, tup = doVar tup}
          | M.RhsIdxGet {idx, ofVal} =>
            M.RhsIdxGet {idx = doVar idx, ofVal = doOp ofVal}
          | M.RhsCont l => M.RhsCont (label (state, env, l))
          | M.RhsObjectGetKind v => M.RhsObjectGetKind (doVar v)
          | M.RhsThunkMk {typ, fvs} => rhs
          | M.RhsThunkInit {typ, thunk, fx, code, fvs} =>
            M.RhsThunkInit {typ   = typ, thunk = doVarO thunk, fx    = fx, code  = doVarO code, fvs   = doFkOps fvs}
          | M.RhsThunkGetFv {typ, fvs, thunk, idx} =>
            M.RhsThunkGetFv {typ = typ, fvs = fvs, thunk = doVar thunk, idx = idx}
          | M.RhsThunkValue {typ, thunk, ofVal} =>
            M.RhsThunkValue {typ = typ, thunk = doVarO thunk, ofVal = doOp ofVal}
          | M.RhsThunkGetValue {typ, thunk} =>
            M.RhsThunkGetValue {typ = typ, thunk = doVar thunk}
          | M.RhsThunkSpawn {typ, thunk, fx} =>
            M.RhsThunkSpawn {typ = typ, thunk = doVar thunk, fx = fx}
          | M.RhsClosureMk {fvs} => rhs
          | M.RhsClosureInit {cls, code, fvs} =>
            M.RhsClosureInit {cls  = doVarO cls, code = doVarO code, fvs  = doFkOps fvs}
          | M.RhsClosureGetFv {fvs, cls, idx} =>
            M.RhsClosureGetFv {fvs = fvs, cls = doVar cls, idx = idx}
          | M.RhsPSetNew opnd => M.RhsPSetNew (doOp opnd)
          | M.RhsPSetGet v => M.RhsPSetGet (doVar v)
          | M.RhsPSetCond {bool, ofVal} =>
            M.RhsPSetCond {bool = doOp bool, ofVal = doOp ofVal}
          | M.RhsPSetQuery oper => M.RhsPSetQuery (doOp oper)
          | M.RhsPSum {tag, typ, ofVal} =>
            M.RhsPSum {tag = tag, typ = typ, ofVal = doOp ofVal}
          | M.RhsPSumProj {typ, sum, tag} =>
            M.RhsPSumProj {typ = typ, sum = doVar sum, tag = tag}
      end

  fun instruction (state, env, i) = 
      let
        fun bindInstr (env, M.I {dests, n, rhs}) = 
            let
              val (env, dests) = bindVars (state, env, dests)
            in (env, M.I {dests = dests, n = n, rhs = rhs})
            end
        fun doInstr (env, M.I {dests, n, rhs}) =
            let
              val rhs = doRhs (state, env, rhs)
              val (env, dests) = bindVars (state, env, dests)
              val i = M.I {dests = dests, n = n, rhs = rhs}
            in (env, i)
            end
        fun doInstrs (env, instrs) = 
            Utils.Function.flipOut Vector.mapAndFold (instrs, env, Utils.Function.flip doInstr)
      in
	case clientInstruction (state, env, i)
	 of StopWith     (env, i) => bindInstr (env, i)
	  | ContinueWith (env, i) => doInstr   (env, i)
	  | Continue              => doInstr   (env, i)
	  | Stop                  => bindInstr (env, i)
      end

  fun instructions (state, env, is) = 
      let
        fun doOne (i, env) = Utils.Function.flipOut instruction (state, env, i)
        val (is, env) = Vector.mapAndFold (is, env, doOne)
      in (env, is)
      end       

  fun target (state, env, M.T {block, arguments}) = 
      let
        val block = label (state, env, block)
        val arguments = operands (state, env, arguments)
        val t = M.T {block = block, arguments = arguments}
      in t
      end

  fun switch (state, env, {on, cases, default} : 'a Mil.switch) =
      let
        val on = operand (state, env, on)
        fun doOne (k, t) = (k, target (state, env, t))
        val cases = Vector.map (cases, doOne)
        val default = Option.map (default, fn t => target (state, env, t))
        val s = {on = on , cases = cases, default = default}
      in s
      end

  fun codes (state, env, {possible, exhaustive} : M.codes) =
      let
        fun doOne (v, vs) = VS.insert (vs, variable (state, env, v))
        val possible = VS.fold (possible, VS.empty, doOne)
        val codes = {possible = possible, exhaustive = exhaustive}
      in codes
      end

  fun call (state, env, call) = 
      case call 
       of M.CCode {ptr, code} => M.CCode {ptr = variable (state, env, ptr), code = codes (state, env, code)}
        | M.CClosure {cls, code} => M.CClosure {cls = variable (state, env, cls), code = codes (state, env, code)}
        | M.CDirectClosure {cls, code} =>
          M.CDirectClosure {cls = variable (state, env, cls), code = variable (state, env, code)}

  fun eval (state, env, eval) = 
      case eval
       of M.EThunk {thunk, code} =>
          M.EThunk {thunk = variable (state, env, thunk),
                    code = codes (state, env, code)}
        | M.EDirectThunk {thunk, code} =>
          M.EDirectThunk {thunk = variable (state, env, thunk),
                          code = variable (state, env, code)}

  fun interProc (state, env, ip) =
      case ip
       of M.IpCall {call = c, args} =>
          M.IpCall {call = call (state, env, c),
                    args = operands (state, env, args)}
        | M.IpEval {typ, eval = e} =>
          M.IpEval {typ = typ, eval = eval (state, env, e)}

  fun cuts (state, env, M.C {exits, targets}) = 
      let
        fun doOne (l, ls) = LS.insert (ls, label (state, env, l))
        val targets = LS.fold (targets, LS.empty, doOne)
        val cuts = M.C {exits = exits, targets = targets}
      in cuts
      end

  fun return (state, env, r) =
      case r
       of M.RNormal {rets, block, cuts = cs} =>
          let
            val (env', rets) = bindVars (state, env, rets)
            val block = label (state, env', block)
            val cs = cuts (state, env, cs)
            val r = M.RNormal {rets = rets, block = block, cuts = cs}
          in r
          end
        | M.RTail {exits} => r
                                       
  fun transfer (state, env, transfer) = 
      let
        fun doTransfer (state, env, transfer) = 
            case transfer
             of M.TGoto t => M.TGoto (target (state, env, t))
              | M.TCase s => M.TCase (switch (state, env, s))
              | M.TInterProc {callee, ret, fx} =>
                M.TInterProc {callee = interProc (state, env, callee),
                              ret = return (state, env, ret),
                              fx = fx}
              | M.TReturn os => M.TReturn (operands (state, env, os))
              | M.TCut {cont, args, cuts = cs} =>
                M.TCut {cont = variable (state, env, cont),
                        args = operands (state, env, args),
                        cuts = cuts (state, env, cs)}
              | M.THalt opnd => M.THalt (operand (state, env, opnd))
              | M.TPSumCase s => M.TPSumCase (switch (state, env, s))
      in
        callClientCode (clientTransfer, doTransfer, state, env, transfer)
      end

  fun block (state, env, M.B {parameters, instructions = is , transfer = t}) =
      let
        val (env, ps) = bindVars (state, env, parameters)
        val (env, is) = instructions (state, env, is)
        val t = transfer (state, env, t)
        val blk = M.B {parameters = ps, instructions = is, transfer = t}
      in (env, blk)
      end
      
  fun codeBody (state, env, cb) =
      let
        val entry = MilUtils.CodeBody.entry cb
        val lbts = cfgEnum (state, env, cb)
        fun bind (Tree.T ((l, blk), children), env) =
            let
              val (env, l) = bindLabel (state, env, l)
              val (children, env) = Vector.mapAndFold (children, env, bind)
            in
              (Tree.T ((l, blk), children), env)
            end 
        and binds (children, env) = Vector.mapAndFold (children, env, bind)
        val (lbts, env) = binds (lbts, env)
        val entry = label (state, env, entry)
        fun doBlocks (Tree.T ((l, blk), children), env, blks) =
            let
              val (env, blk) = block (state, env, blk)
              val blks = doBlockss (children, env, blks)
              val blks = LD.insert (blks, l, blk)
            in blks
            end
        and doBlockss (children, env, blks) =
            let
              fun doOne (c, blks) = doBlocks (c, env, blks)
              val blks = Vector.fold (children, blks, doOne)
            in blks
            end
        val blks = doBlockss (lbts, env, LD.empty)
        val cb = M.CB {entry = entry, blocks = blks}
      in cb
      end

  fun callConv (state, env, cc) = 
      case cc
       of M.CcCode => (env, M.CcCode)
        | M.CcClosure {cls, fvs} =>
          let
            val (env, cls) = bindVar (state, env, cls)
            val (env, fvs) = bindVars (state, env, fvs)
          in
            (env, M.CcClosure {cls = cls, fvs = fvs})
          end
        | M.CcThunk {thunk, fvs} =>
          let
            val (env, thnk) = bindVar (state, env, thunk)
            val (env, fvs) = bindVars (state, env, fvs)
          in
            (env, M.CcThunk {thunk = thnk, fvs = fvs})
          end

  fun code (state, env, f) =
      let
        val M.F {fx, escapes, recursive, cc, args, rtyps, body} = f
        val (env, cc) = callConv (state, env, cc)
        val (env, args) = bindVars (state, env, args)
        val body = codeBody (state, env, body)
      in
        M.F {fx        = fx,
             escapes   = escapes,
             recursive = recursive,
             cc        = cc,
             args      = args,
             rtyps     = rtyps,
             body      = body}
      end

  (* Variables already bound *)
  fun global (state, env, xg) =
      let
        fun doGlobal (state, env, (x, global)) = 
            let
              fun doOp opnd = operand (state, env, opnd)
              fun doVarO vo = Option.map (vo, fn v => variable (state, env, v))
              fun doFkOps fkos = Vector.map (fkos, fn (fk, opnd) => (fk, doOp opnd))
              val global = 
                  case global
                   of M.GCode f => M.GCode (code (state, env, f))
                    | M.GErrorVal _ => global
                    | M.GIdx _ => global
                    | M.GTuple {mdDesc, inits} =>
                      M.GTuple {mdDesc = mdDesc, inits = operands (state, env, inits)}
                    | M.GRat _ => global
                    | M.GInteger _ => global
                    | M.GCString _ => global
                    | M.GThunkValue {typ, ofVal} =>
                      M.GThunkValue {typ = typ, ofVal = simple (state, env, ofVal)}
                    | M.GSimple s => M.GSimple (simple (state, env, s))
                    | M.GClosure {code, fvs} => M.GClosure {code = doVarO code, fvs  = doFkOps fvs}
                    | M.GPSum {tag, typ, ofVal} =>
                      M.GPSum {tag = tag, typ = typ, ofVal = simple (state, env, ofVal)}
                    | M.GPSet s => M.GPSet (simple (state, env, s))
            in (x, global)
            end
      in
        callClientCode (clientGlobal, doGlobal, state, env, xg)
      end

  fun globals (state, env, gs) = 
      let
        fun bindGlobal (x, g, (gs, env)) = 
            let
              val (env, x) = bindVar (state, env, x)
            in ((x, g)::gs, env)
            end
        val (gs, env) = VD.fold (gs, ([], env), bindGlobal)
        fun doGlobal ((x, g), gs) =
            let
              val (x, g) = global (state, env, (x, g))
              val gs = VD.insert (gs, x, g)
            in gs
            end
        val gs = List.fold (gs, VD.empty, doGlobal)
      in
        (env, gs)
      end

  fun program (state, env, M.P {includes, externs, globals = gs, symbolTable, entry})  = 
      let
        val (env, gs) = globals (state, env, gs)
        val entry = variable (state, env, entry)
        val p = M.P {includes = includes, externs = externs, globals = gs, symbolTable = symbolTable, entry = entry}
      in p
      end

end; (* Functor MilRewriterF*)

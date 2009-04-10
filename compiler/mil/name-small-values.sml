(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation *)

signature MIL_NAME_SMALL_VALUES = 
sig
  val program : Config.t 
                * (Mil.operand -> bool)
                * Mil.t 
                -> Mil.t
end

structure MilNameSmallValues :> MIL_NAME_SMALL_VALUES =
struct
  structure I = Identifier
  structure IM = Identifier.Manager
  structure VD = I.VariableDict
  structure M = Mil
  structure MSTM = MilUtils.SymbolTableManager

  datatype state = S of {stm : M.symbolTableManager}

  datatype env = E of {name : M.operand -> bool, config : Config.t}
                         
  local
  val getS = fn g => fn (S t) => g t
  val getE = fn g => fn (E t) => g t
  in
  val stateGetStm = getS #stm
  val envGetConfig = getE #config
  val envGetName = getE #name
  end

  val stateGetFreshVariable = 
   fn (state, hint, t, global) =>
      MSTM.variableFresh (stateGetStm state, hint, t, global)

  val nameOperand = 
   fn (env, opnd) => envGetName env opnd

  structure TO = MilType.Typer

  structure MSS =
  MilStreamF (
  struct
    type state = state
    type env = env
    val toConfig = envGetConfig
    val getStm = stateGetStm
    val indent = 2
  end
  )
  structure Stream = MSS.Stream

  val bind = 
   fn (global, mk) => 
   fn (state, env, oper) => 
      let
        val c = envGetConfig env
        val si = I.SymbolInfo.SiManager (stateGetStm state)
        val t = TO.operand (c, si, oper)
        val v = stateGetFreshVariable (state, "mnm", t, global)
        val r = mk (v, oper)
      in ([r], M.SVariable v)
      end

  val bindGlobal = bind (true, fn (v, oper) => 
                                  let
                                    val g = M.GSimple oper
                                  in (v, g)
                                  end)

  val bindLocal = fn mk => bind (false, mk)

  val rec doOperand = 
   fn bind => 
   fn (state, env, oper) => 
      let
        val (is, oper) = 
            if nameOperand (env, oper) then
              bind (state, env, oper)
            else 
              ([], oper)
      in (is, oper)
      end

  val doOperands = 
   fn bind => 
      fn (state, env, opers) => 
         let
           val help = 
            fn (oper, bs) => 
               let
                 val (bs', oper) = doOperand bind (state, env, oper)
                 val bs = bs' @ bs
               in (oper, bs)
               end
           val (opers, bs) = Vector.mapAndFold (opers, [], help)
         in (bs, opers)
         end

  val label = 
   fn (state, env, _) => (env, NONE)

  val instr = 
   fn (state, env, M.I {dest, rhs}) => 
      let
        val bind = bindLocal (fn (v, oper) => 
                                 let
                                   val rhs = M.RhsSimple oper
                                   val s = Stream.bindRhs (state, env, v, rhs)
                                 in s
                                 end)
        val lift = 
         fn doOp => 
         fn (state, env, obj) => 
            let
              val (sL, obj) = doOp (state, env, obj)
            in (Stream.seqn (state, env, sL), obj)
            end

        val doOperand = lift (doOperand bind)
        val doOperands = lift (doOperands bind)

        val assignDest = 
            case dest
             of SOME v => (fn rhs => Stream.bindRhs (state, env, v, rhs))
              | NONE   => (fn rhs => Stream.doRhs (state, env, rhs))

        val vector = 
         fn mk => 
         fn opers => 
            let
              val (s, opers) = doOperands (state, env, opers)
              val s' = assignDest (mk opers)
              val s = Stream.seq (state, env, s, s')
            in SOME s
            end

        val vectorFk = 
         fn mk => 
         fn fkos => 
            let
              fun doOne ((fk, opnd), s) =
                  let
                    val (s', opnd) = doOperand (state, env, opnd)
                  in
                    ((fk, opnd), Stream.seq (state, env, s, s'))
                  end
              val (fkos, s) =
                  Vector.mapAndFold (fkos, Stream.new (state, env), doOne)
              val s' = assignDest (mk fkos)
              val s = Stream.seq (state, env, s, s')
            in SOME s
            end

        val unary = 
         fn mk => 
         fn oper => 
            let
              val (s, oper) = doOperand (state, env, oper)
              val s' = assignDest (mk oper)
              val s = Stream.seq (state, env, s, s')
            in SOME s
            end

        val pair = 
         fn mk => 
         fn (op1, op2) => 
            let
              val (s1, op1) = doOperand (state, env, op1)
              val (s2, op2) = doOperand (state, env, op2)
              val s3 = assignDest (mk (op1, op2))
              val s = Stream.seqn (state, env, [s1, s2, s3])
            in SOME s
            end

        val trinary =
         fn mk =>
         fn (op1, op2, op3) =>
            let
              val (s1, op1) = doOperand (state, env, op1)
              val (s2, op2) = doOperand (state, env, op2)
              val (s3, op3) = doOperand (state, env, op3)
              val s4 = assignDest (mk (op1, op2, op3))
              val s = Stream.seqn (state, env, [s1, s2, s3, s4])
            in SOME s
            end

        val option = 
         fn mk => 
         fn opt => 
            (case opt
              of SOME oper => unary (mk o SOME) oper
               | NONE => SOME (assignDest (mk NONE)))

        fun fieldIdentifier (state, env, fi) =
            case fi
             of M.FiFixed _ => (Stream.new (state, env), fi)
              | M.FiVariable opnd =>
                let
                  val (s, opnd) = doOperand (state, env, opnd)
                in (s, M.FiVariable opnd)
                end
              | M.FiViFixed _ => (Stream.new (state, env), fi)
              | M.FiViVariable {typ, idx} =>
                let
                  val (s, idx) = doOperand (state, env, idx)
                in (s, M.FiViVariable {typ = typ, idx = idx})
                end
              | M.FiViIndexed {typ, idx} =>
                let
                  val (s, idx) = doOperand (state, env, idx)
                in (s, M.FiViIndexed {typ = typ, idx = idx})
                end

        fun tupleField (state, env, M.TF {tupDesc, tup, field}) =
            let
              val (s, field) = fieldIdentifier (state, env, field)
              val tf = M.TF {tupDesc = tupDesc, tup = tup, field = field}
            in (s, tf)
            end

        val s = 
            case rhs
             of M.RhsSimple _ => NONE
              | M.RhsPrim {prim, createThunks, args} =>
                vector (fn args => M.RhsPrim {prim = prim,
                                              createThunks = createThunks,
                                              args = args})
                       args
              | M.RhsTuple {vtDesc, inits} =>
                vector (fn inits =>
                           M.RhsTuple {vtDesc = vtDesc, inits = inits})
                       inits
              | M.RhsTupleSub tf =>
                let
                  val (s1, tf) = tupleField (state, env, tf)
                  val s2 = assignDest (M.RhsTupleSub tf)
                  val s = Stream.seq (state, env, s1, s2)
                in SOME s
                end
              | M.RhsTupleSet {tupField, ofVal} =>
                let
                  val (s1, tf) = tupleField (state, env, tupField)
                  val (s2, ofVal) = doOperand (state, env, ofVal)
                  val rhs = M.RhsTupleSet {tupField = tf, ofVal = ofVal}
                  val s3 = assignDest rhs
                  val s = Stream.seqn (state, env, [s1, s2, s3])
                in SOME s
                end
              | M.RhsTupleInited _ => NONE
              | M.RhsIdxGet {idx, ofVal} =>
                unary (fn opnd => M.RhsIdxGet {idx = idx, ofVal = opnd}) ofVal
              | M.RhsCont _ => NONE
              | M.RhsObjectGetKind _ => NONE
              | M.RhsThunkMk _ => NONE
              | M.RhsThunkInit {typ, thunk, fx, code, fvs} => 
                vectorFk (fn fkos => 
                             M.RhsThunkInit {typ = typ,
                                             thunk = thunk, 
                                             fx = fx, 
                                             code = code,
                                             fvs = fkos}) 
                         fvs
              | M.RhsThunkGetFv _ => NONE
              | M.RhsThunkValue {typ, thunk, ofVal} =>
                unary (fn oper =>
                          M.RhsThunkValue {typ = typ,
                                           thunk = thunk,
                                           ofVal = oper})
                      ofVal
              | M.RhsThunkGetValue _ => NONE
              | M.RhsThunkSpawn _ => NONE
              | M.RhsPFunctionMk _ => NONE
              | M.RhsPFunctionInit {cls, code, fvs} =>
                vectorFk (fn fkos => M.RhsPFunctionInit {cls = cls,
                                                         code = code,
                                                         fvs = fkos})
                         fvs
              | M.RhsPFunctionGetFv _ => NONE
              | M.RhsPSetNew oper => 
                unary M.RhsPSetNew oper
              | M.RhsPSetGet _ => NONE
              | M.RhsPSetCond {bool, ofVal} =>
                pair (fn (o1, o2) => M.RhsPSetCond {bool = o1, ofVal = o2})
                     (bool, ofVal)
              | M.RhsPSetQuery _ => NONE
              | M.RhsPSum {tag, typ, ofVal} =>
                unary (fn ofVal => M.RhsPSum {tag = tag, typ = typ, ofVal = ofVal})
                      ofVal
              | M.RhsPSumProj _ => NONE
      in (env, s)
      end

  val transfer = 
   fn (state, env, t) => 
      let
        val bind = bindLocal (fn (v, oper) => 
                                 let
                                   val rhs = M.RhsSimple oper
                                   val s = Stream.bindRhs (state, env, v, rhs)
                                 in s
                                 end)
        val lift = 
         fn doOp => 
         fn (state, env, obj) => 
            let
              val (sL, obj) = doOp (state, env, obj)
            in (Stream.seqn (state, env, sL), obj)
            end

        val doOperand = lift (doOperand bind)
        val doOperands = lift (doOperands bind)

        val vector = 
         fn mk => 
         fn opers => 
            let
              val (s, opers) = doOperands (state, env, opers)
              val s' = Stream.transfer (state, env, mk opers)
              val s = Stream.seq (state, env, s, s')
            in SOME s
            end

        val target = 
         fn M.T {block, arguments} => 
            let
              val (s, arguments) = doOperands (state, env, arguments)
            in (s, M.T {block = block, arguments = arguments})
            end

        val switch =
         fn mk => 
         fn ({on, cases, default} : 'a M.switch) =>
            let
              val (s1, op1) = doOperand (state, env, on)
              val help = 
               fn ((a, tg), s) => 
                  let
                    val (st, tg) = target tg
                  in ((a, tg), Stream.seq (state, env, s, st))
                  end
              val (tgs, s) = Vector.mapAndFold (cases, s1, help)
              val (s, dflt) =
                  case default
                   of SOME tg => 
                      let
                        val (sd, tg) = target tg
                      in (Stream.seq (state, env, s, sd), SOME tg)
                      end
                    | NONE => (s, NONE)
              val t = mk {on = op1, cases = tgs, default = dflt}
              val s' = Stream.transfer (state, env, t)
              val s = Stream.seq (state, env, s, s')
            in SOME s
            end

        fun interProc ip =
            case ip
             of M.IpCall {call, args} =>
                let
                  val (s, args) = doOperands (state, env, args)
                in (s, M.IpCall {call = call, args = args})
                end
              | M.IpEval _ => (Stream.new (state, env), ip)

        val s = 
            case t
             of M.TGoto tg => 
                let
                  val (s, tg) = target tg
                  val s' = Stream.transfer (state, env, M.TGoto tg)
                  val s = Stream.seq (state, env, s, s')
                in SOME s
                end
              | M.TCase sw => switch M.TCase sw
              | M.TInterProc {callee, ret, fx} =>
                let
                  val (s1, callee) = interProc callee
                  val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                  val s2 = Stream.transfer (state, env, t)
                  val s = Stream.seq (state, env, s1, s2)
                in SOME s
                end
              | M.TReturn opers => 
                vector M.TReturn opers
              | M.TCut {cont, args, cuts} =>
                vector (fn opers => M.TCut {cont = cont,
                                            args = opers,
                                            cuts = cuts})
                        args
              | M.TPSumCase sw => switch M.TPSumCase sw

      in (env, s)
      end

  val global = 
   fn (state, env, (v, g)) => 
      let
        val single = 
         fn mk => 
         fn opers => 
            let
              val (gs, oper) = doOperand bindGlobal (state, env, opers)
              val g = mk oper
            in (v, g) :: gs
            end

        val nary = 
         fn mk => 
         fn opers => 
            let
              val (gs, opers) = doOperands bindGlobal (state, env, opers)
              val g = mk opers
            in (v, g) :: gs
            end

        val l = 
            case g
             of M.GTuple {vtDesc, inits} =>
                nary (fn opers => M.GTuple {vtDesc = vtDesc, inits = opers})
                     inits
              | M.GThunkValue {typ, ofVal} =>
                single (fn oper => M.GThunkValue {typ = typ, ofVal = oper})
                       ofVal
              | M.GPSum {tag, typ, ofVal} =>
                single (fn opr => M.GPSum {tag = tag, typ = typ, ofVal = opr})
                       ofVal
              | M.GPSet s => single M.GPSet s
              | _ => [(v, g)]
      in (env, SOME l)
      end

  structure Transform = 
  MilTransformF (
    type state = state
    type env = env
    structure MSS = MSS
    val config = envGetConfig
    val label = label
    val instr = instr
    val transfer = transfer
    val global = global
    val indent = 2
  )

  val program = 
   fn (config, name, p) => 
      let
        val M.P {symbolTable, ...} = p
        val stm = IM.fromExistingAll symbolTable
        val state = S {stm = stm}
        val env = E {name = name, config = config}
        val M.P {symbolTable, globals, entry} =
            Transform.program (state, env, Transform.OAny, p)
        val st = IM.finish stm
        val p = M.P {symbolTable = st, globals = globals, entry = entry}
      in p
      end

end;

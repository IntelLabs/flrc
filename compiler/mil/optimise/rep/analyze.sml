(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_ANALYZE = 
sig
  val program : PassData.t * Mil.t -> MilRepSummary.summary
end

structure MilRepAnalyze :> MIL_REP_ANALYZE = 
struct

  val passname = "MilRepAnalyze"

  structure M = Mil
  structure P = Mil.Prims
  structure I = Identifier
  structure MU = MilUtils
  structure MUP = MU.Prims
  structure PU = MUP.Utils
  structure MT = MilType
  structure PD = PassData

  structure MRB = MilRepBase
  structure MRO = MilRepObject
  structure Node = MilRepNode
  structure Unify = Node.Unify
  structure Shape = MRO.Shape
  structure Build = Shape.Build

  structure Summary = MilRepSummary

  structure ID = IntDict
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure ND = Mil.ND
  structure VS = Mil.VS
  structure LS = Mil.LS
  structure IIdD = MU.Id.ImpDict

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 4
                          end)

  datatype edge = datatype MRB.edge

  val @@ = Utils.Function.@@
  val <\ = Utils.Function.<\
  val \> = Utils.Function.\>
  val /> = Utils.Function./>
  val </ = Utils.Function.</
  val >| = Utils.Function.>|
  val |< = Utils.Function.|<
  val ` = Utils.Function.`

  infix 3 @@ 
  infix 3 <\ \> 
  infixr 3 /> </
  infix 1 >| 
  infixr 1 |< 

  infix 4 <-- --> <== == ^|^

 (* Reports a fail message and exit the program.
  * param f: The function name.
  * param s: the messagse. *)
  val fail = 
   fn (f, m) => Fail.fail ("analyze.sml", f, m)

  val stringFromVar = Layout.toString o MilLayout.layoutVariable
  val stringFromLabel = Layout.toString o MilLayout.layoutLabel

  val useShallowTypes = MilRepBase.useShallowTypes

  structure Env = 
  struct
    datatype t = E of {pd : PD.t,
                       si : M.symbolInfo,
                       varNodes : Node.node VD.t,
                       labelNodes : Node.node LD.t,
                       names  : M.variable ID.t,
                       conts : Node.node Vector.t LD.t,
                       returns : Node.node Vector.t}

    val new = 
     fn (pd, si) => E {pd = pd, 
                       si = si, 
                       varNodes = VD.empty,
                       labelNodes = LD.empty,
                       names = ID.empty,
                       conts = LD.empty,
                       returns = Vector.new0()}

    val ((envSetPd, envGetPd),
         (envSetSi, envGetSi),
         (envSetVarNodes, envGetVarNodes),
         (envSetLabelNodes, envGetLabelNodes),
         (envSetNames, envGetNames),
         (envSetConts, envGetConts),
         (envSetReturns, envGetReturns)) = 
        let
          val r2t = 
           fn (E {pd, si, varNodes, labelNodes, names, conts, returns}) => 
              (pd, si, varNodes, labelNodes, names, conts, returns)
          val t2r = 
           fn (pd, si, varNodes, labelNodes, names, conts, returns) => 
              E {pd = pd, si = si, varNodes = varNodes, labelNodes = labelNodes, 
                 names = names, conts = conts, returns = returns}
        in
          FunctionalUpdate.mk7 (r2t, t2r)
        end

    val getPD = envGetPd 

    val getConfig = 
     fn env => PD.getConfig (getPD env)

    val getSi = envGetSi

    val stringFromVar = fn (env, v) => stringFromVar (getConfig env, envGetSi env, v)

    val stringFromLabel = fn (env, l) => stringFromLabel (getConfig env, envGetSi env, l)

    val getVarNode' = 
     fn (env, v) => VD.lookup (envGetVarNodes env, v)

    val getVarNode = 
     fn (env, v) => 
        (case getVarNode' (env, v)
          of SOME n => n
           | NONE => fail ("getVarNode", "Unknown variable: "^stringFromVar (env, v)))

    val getLabelNode' = 
     fn (env, l) => LD.lookup (envGetLabelNodes env, l)

    val getLabelNode = 
     fn (env, l) => 
        (case LD.lookup (envGetLabelNodes env, l)
          of SOME n => n
           | NONE => fail ("getLabelNode", "Unknown label: "^stringFromLabel (env, l)))

    val getCurrentReturns = envGetReturns

    val setCurrentReturns = envSetReturns

    val addVarNode = 
     fn (env, v, n) => 
        let
          val env = envSetVarNodes (env, VD.insert (envGetVarNodes env, v, n))
          val id = Node.id n
          val env = envSetNames (env, ID.insert (envGetNames env, id, v))
        in env
        end

    val addLabelNode = 
     fn (env, l, n) => envSetLabelNodes (env, LD.insert (envGetLabelNodes env, l, n))

    val getCont = 
     fn (env, l) => 
        (case LD.lookup (envGetConts env, l)
          of SOME ns => ns
           | NONE => fail ("getCont", "Unknown label: "^stringFromLabel (env, l)))

    val addCont = 
     fn (env, l, ns) => envSetConts (env, LD.insert (envGetConts env, l, ns))


  end (* structure Env *)

  structure State = 
  struct
    datatype t = S of {nextId : int ref,
                       nodes : Node.node List.t ref,
                       iInfo : Node.node MRB.iInfo IIdD.t}

    val new = 
     fn () => S {nextId = ref 0,
                 nodes = ref [],
                 iInfo = IIdD.empty ()}

    val ((stateSetNextId, stateGetNextId),
         (stateSetNodes, stateGetNodes),
         (stateSetIInfo, stateGetIInfo)) = 
        let
          val r2t = 
           fn (S {nextId, nodes, iInfo}) => (nextId, nodes, iInfo)
          val t2r = 
           fn (nextId, nodes, iInfo) => S {nextId = nextId, nodes = nodes, iInfo = iInfo}
        in
          FunctionalUpdate.mk3 (r2t, t2r)
        end

    val addNode = 
     fn (state, n) => 
        let
          val r as ref l = stateGetNodes state
          val () = r := n :: l
        in ()
        end

    val addIInfo = 
     fn (state, id, i) => 
        IIdD.insert (stateGetIInfo state, id, i)

    val getIInfo = 
     fn (state, id) => 
        IIdD.lookup (stateGetIInfo state, id)

    val nextId = 
     fn state => 
        let
          val nextId = stateGetNextId state
          val id = !nextId
          val () = nextId := !nextId + 1
        in id
        end
  end (* structure State *)

  val stringFromVar = 
   fn ((state, env), v) => Env.stringFromVar (env, v)

  val getConfig = 
   fn (state, env) => Env.getConfig env

  val getPD = 
   fn (state, env) => Env.getPD env

  val getSi = 
   fn (state, env) => Env.getSi env

  val typOfPrimPrimResult = 
   fn (se, p) => #2 (MT.PrimsTyper.prim (getConfig se, p))

  val typOfPrimPrimResult = 
   fn (se, p) => #2 (MT.PrimsTyper.prim (getConfig se, p))

  val typOfPrimRuntimeResult = 
   fn (se, runtime) => #2 (MT.PrimsTyper.runtime (getConfig se, getSi se, runtime))

  val typOfPrimViResult = 
   fn (se, vi, typs) => #2 (MT.PrimsTyper.vector (getConfig se, vi, typs))

  val typOfVariable =
   fn (se, v) => MU.SymbolInfo.variableTyp (getSi se, v)

  val getVarNode = 
   fn ((state, env), v) => Env.getVarNode (env, v)

  val getLabelNode = 
   fn ((state, env), l) => Env.getLabelNode (env, l)

  val getLabelNode' = 
   fn ((state, env), l) => Env.getLabelNode' (env, l)

  val getCont = 
   fn ((state, env), l) => Env.getCont (env, l)

  val getFunInfo = 
   fn ((state, env), f) => 
      (case State.getIInfo (state, MU.Id.G f)
        of SOME (MRB.IiCode r) => SOME r
         | _ => NONE)

  val getCurrentReturns = 
   fn (state, env) => Env.getCurrentReturns env

  val requireFunInfo = 
   fn (se as (state, env), v) => 
      (case State.getIInfo (state, MU.Id.G v)
        of SOME (MRB.IiCode r) => r
         | SOME _ => fail ("requireFunInfo", "Not a function name: "^stringFromVar (se, v))
         | NONE => fail ("requireFunInfo", "Not a known variable: "^stringFromVar (se, v)))

  val enterFunction = 
   fn (se as (state, env), fname) =>
      let
        val {returns, ...} = requireFunInfo (se, fname)
        val env = Env.setCurrentReturns (env, returns)
      in (env, returns)
      end

  val nextId = 
   fn (state, env) => State.nextId state

  val addNode = 
   fn ((state, env), n) => State.addNode (state, n)

  val addIInfo = 
   fn ((state, env), n, i) => State.addIInfo (state, n, i)

  val addMetaData = 
   fn (se, n, pok, fixed, array) => addIInfo (se, n, MRB.IiMetaData {pok = pok, fixed = fixed, array = array})

  val addTupleDescriptor = 
   fn (se, n, fixed, array) => addIInfo (se, n, MRB.IiTupleDescriptor {fixed = fixed, array = array})

  val addThunkDescriptor = 
   fn (se, n, typ, fvs) => addIInfo (se, n, MRB.IiThunk {typ = typ, fvs = fvs})

  val addClosureDescriptor = 
   fn (se, n, fvs) => addIInfo (se, n, MRB.IiClosure fvs)

  val addPSumDescriptor = 
   fn (se, n, typ) => addIInfo (se, n, MRB.IiPSum typ)

  val symbols =
   fn se => 
      let
        val config = getConfig se
        val unify = fn (n1, n2) => Unify.node (config, n1, n2)
        val op <== = unify
        val op == = unify
        val op <-- = fn (node, shape) => Unify.nodeWithShape (config, node, shape)
        val op --> = fn (node, shape) => Unify.shapeWithNode (config, shape, node)
      in (op ==, op <==, op <--, op -->)
      end

  val mkBottomNode = 
   fn (se, fk, variance, shape) => 
      let
        val n = Node.mkBottom (nextId se, fk, variance, shape)
        val () = addNode (se, n)
      in n
      end

  val mkShapedNode = 
   fn (se, fk, variance, shape) => 
      let
        val n = Node.mkShaped (nextId se, fk, variance, shape)
        val () = addNode (se, n)
      in n
      end

  val newBottomNode = 
   fn (se, fk) => mkBottomNode (se, SOME fk, NONE, NONE)

  val newShallowTypedBottomNode = 
   fn (se, t) => 
      let
        val fk = MU.FieldKind.fromTyp' (getConfig se, t)
        val shape = Build.unknown (MU.FlatTyp.fromTyp (getConfig se, t))
      in mkBottomNode (se, fk, NONE, SOME shape)
      end

  val newShapedNode = 
   fn (se, shape) => mkShapedNode (se, Shape.fieldKind (getConfig se, shape), NONE, shape)

  val newFieldBottomNode =
   fn (se, fk, variance) => mkBottomNode (se, SOME fk, SOME variance, NONE)

  val newFieldShapedNode = 
   fn (se, shape, variance) => mkShapedNode (se, Shape.fieldKind (getConfig se, shape), SOME variance, shape)

  val escapes = fn (se, node) => Node.markNodeEscaping node
  val unknown = fn (se, node) => Node.markNodeUnknownDefs node

  val typOfCode = 
   fn se => M.TBits (MU.ValueSize.ptrSize (getConfig se))

  val rec typ =
   fn (se, t) =>
      let
        val n = 
            (case typShape (se, t)
              of SOME s => newShapedNode (se, s)
               | NONE => newShallowTypedBottomNode (se, t))
      in n
      end
  and rec typShape =
   fn (se, t) =>
      let

        val typ = 
            if useShallowTypes (getPD se) then 
              newShallowTypedBottomNode
            else
              typ

        val typField = 
         fn (se, (t, fv)) => 
            let
              val s = case typShape (se, t)
                       of SOME s => s
                        | NONE => Build.unknown (MU.FlatTyp.fromTyp (getConfig se, t))
              val n = newFieldShapedNode (se, s, fv)
            in n
            end

        val typs = 
         fn (se, ts) => Vector.map (ts, fn t => typ (se, t))

        val abstract =
         fn t => NONE

        val shaped = SOME

        val n = 
            (case t
              of M.TAny => abstract t
               | M.TAnyS vs => abstract t
               | M.TNonRefPtr => abstract t
               | M.TRef => abstract t
               | M.TBits vs => abstract t
               | M.TNone => abstract t
               | M.TNumeric _ => shaped (Build.base t)
               | M.TBoolean => shaped (Build.base t)
               | M.TName => shaped (Build.base t)
               | M.TViVector _ => shaped (Build.base t)
               | M.TViMask _ => shaped (Build.base t)
               | M.TCode {cc, args, ress} => abstract (typOfCode se)
               | M.TTuple {pok, fixed, array} => 
                 let
                   val help = fn tfv => typField (se, tfv)
                   val array = 
                       (case array
                         of (M.TNone, _) => NONE
                          | _ => SOME (help array))
                   val shape =
                       Build.tuple {pok = SOME pok, fields = Vector.map (fixed, help), array = array}
                 in shaped shape
                 end
               | M.TCString => shaped (Build.base t)
               | M.TIdx => shaped (Build.base t)
               | M.TContinuation ts => shaped (Build.cont {label = NONE, args = typs (se, ts)})
               | M.TThunk t => 
                 let
                   val shape = Build.thunkValue {code = newShallowTypedBottomNode (se, typOfCode se), 
                                                 result = typ (se, t)}
                 in shaped shape
                 end
               | M.TPAny => abstract t
               | M.TClosure {args, ress} => 
                 let
                   val shape = Build.closure {name = NONE, 
                                              code = newShallowTypedBottomNode (se, typOfCode se),
                                              fvs = Vector.new0 ()}
                 in shaped shape
                 end
               | M.TPSum nd => 
                 let
                   val nd = ND.map (nd, fn (nm, t) => typ (se, t))
                 in shaped (Build.pSum' nd)
                 end
               | M.TPType {kind = M.TkE, over} => shaped (Build.pSet (typ (se, over)))
               | M.TPType {kind = M.TkI, over} => shaped (Build.base t)
               | M.TPRef _ => shaped (Build.base t))
      in n
      end

  val variable = getVarNode

  val variables = 
   fn (se, vs) => Vector.map (vs, fn v => variable (se, v))

  val label = getLabelNode

  val label' = getLabelNode'

  val constant = 
   fn (se, c) =>
      (case c
        of M.COptionSetEmpty => newShapedNode (se, Build.pSetEmpty ())
         | _ => 
           let
             val t = MT.Typer.constant (getConfig se, getSi se, c)
             val shape = Build.base t
             val node = newShapedNode (se, shape)
           in node
           end)

  val operand = 
   fn (se, oper) => 
      (case oper
        of M.SVariable v => variable (se, v)
         | M.SConstant c => constant (se, c))

  val operands = 
      fn (se, v) => Vector.map (v, fn oper => operand (se, oper))

  val fieldKind = 
   fn (se, kind) => newBottomNode (se, kind)

  val fieldKindOperand = 
   fn (se, (kind, oper)) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val oper = operand (se, oper)
        val kind = fieldKind (se, kind)
        val () = oper == kind
      in oper
      end

  val fieldKindOperands =
      fn (se, fkos) => Vector.map (fkos, fn fko => fieldKindOperand (se, fko))

  val fieldKinds = 
      fn (se, fks) => Vector.map (fks, fn fk => fieldKind (se, fk))

  val fieldDescriptor = 
   fn (se, M.FD {kind, var}) => newFieldBottomNode (se, kind, var)

  val fieldDescriptors = 
   fn (se, fds) => Vector.map (fds, fn fd => fieldDescriptor (se, fd))

  val vTable = 
   fn (se, M.MDD {pok, fixed, array}) => 
      let
        val fixed = fieldDescriptors (se, fixed)
        val array = 
            (case array
              of SOME (i, fd) => SOME (i, fieldDescriptor (se, fd))
               | NONE => NONE)
      in (pok, fixed, array)
      end

  val tupleDescriptor = 
   fn (se, M.TD {fixed, array}) => 
      let
        val fixed = fieldDescriptors (se, fixed)
        val array = 
            (case array
              of SOME fd => SOME (fieldDescriptor (se, fd))
               | NONE => NONE)
      in (fixed, array)
      end
      
  val tupleMk =
   fn (se, id, mdDesc, inits) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val (pok, nodes, array) = vTable (se, mdDesc)
        val inits = operands (se, inits)
        val () = 
            let
              val limit = Int.min (Vector.length nodes, Vector.length inits)
              val nodesP = Vector.prefix (nodes, limit)
              val initsP = Vector.prefix (inits, limit)
              val () = Vector.foreach2 (nodesP, initsP, op ==)
              val initsS = Vector.dropPrefix (inits, limit)
              val doVariable = 
               fn n1 => 
                  (case array 
                    of SOME (_, n2) => n1 == n2
                     | NONE    => fail ("tupleMk", "Bad tuple descriptor in instruction: "
                                                   ^ Layout.toString (MU.Id.layout (getSi se, id))))
              val () = Vector.foreach (initsS, doVariable)
            in ()
            end
        val object = Build.tuple {pok = SOME pok, fields = nodes, array = Option.map (array, fn (_, n) => n)}
        val () = addMetaData (se, id, pok, nodes, array)
      in object
      end

  val tupleOp =
   fn (se, id, M.TF {tupDesc, tup, field}, element) =>
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val tup = variable (se, tup)
        val (nodes, array) = tupleDescriptor (se, tupDesc)
        val () = addTupleDescriptor (se, id, nodes, array)
        val () = 
            (case (field, array)
              of (M.FiFixed i, _) => 
                 let
                   val () = 
                       case (i < Vector.length nodes, array)
                        of (true, _)          => element == (Vector.sub (nodes, i))
                         | (false, SOME node) => element == node
                         | (false, NONE)      => 
                           fail ("tupleOp", "Bad tuple descriptor in instruction: "
                                            ^ Layout.toString (MU.Id.layout (getSi se, id)))
                 in ()
                 end
               | (M.FiVariable oper, SOME node) => element == node
               | _ => 
                 let
                   val () = escapes (se, tup)
                   val () = unknown (se, element)
                 in ()
                 end)
        val object = Build.tuple {pok = NONE, fields = nodes, array = array}
      in (tup, object)
      end

  val pFunctionInit = 
   fn (se, name, id, destNode, code, fvs) =>
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val fvs = fieldKindOperands (se, fvs)
        val () = addClosureDescriptor (se, id, fvs)
        val cNode = 
            (case code
              of SOME f => 
                 let
                   val {cargs, args, returns} = requireFunInfo (se, f)
                   val (clsFormal, fvFormals) =
                       (case cargs
                         of M.CcClosure {cls, fvs} => (cls, fvs)
                          | _ => fail ("instruction", "Bad fun info for function init"))
                   val () = Vector.foreach2 (fvFormals, fvs, op <==)
                   val () = clsFormal <== destNode
                 in variable (se, f)
                 end
               | NONE => newShallowTypedBottomNode (se, typOfCode se))
        val shape = 
            Build.closure {name = SOME name,
                           code = cNode,
                           fvs = fvs}
      in shape
      end

  val doPrimPrim = 
   fn (se, dests, prim, createThunks, args) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val args = operands (se, args)
        val rts = typOfPrimPrimResult (se, prim)
        val () = Vector.foreach2 (dests, rts, fn (v, rt) => variable (se, v) <-- Build.base rt)
      in ()
      end

  (* Un-interpreted primitives *)
  val doRuntimePrim = 
   fn (se, dests, rt, createThunks, args) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val () = Vector.foreach (args, fn arg => escapes (se, operand (se, arg)))
        val () = 
            let
              val doIt = 
               fn (v, t) => 
                 let
                   val node = variable  (se, v)
                   val shape = Build.unknown t
                   val () = node <-- shape
                   val () = unknown (se, node)
                 in ()
                 end
              val rts = typOfPrimRuntimeResult (se, rt)
            in Vector.foreach2 (dests, rts, doIt)
            end
      in ()
      end

  val doViPrim = 
   fn (se, dests, vi, createThunks, typs, args) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val args = operands (se, args)
        val () = Vector.foreach (args, fn arg => escapes (se, arg))
        val rts = typOfPrimViResult (se, vi, typs)
        val dests = variables (se, dests)
        val () = Vector.foreach2 (dests, rts, fn (n, rt) => n <-- Build.base rt)
        val () = Vector.foreach (dests, fn n => unknown (se, n))
      in ()
      end

  val doPrim =
   fn (se, dests, prim, createThunks, typs, args) => 
      let
        val () = 
            (case prim
              of P.Prim p => doPrimPrim (se, dests, p, createThunks, args)
               | P.Runtime rt => doRuntimePrim (se, dests, rt, createThunks, args)
               | P.Vector vi => doViPrim (se, dests, vi, createThunks, typs, args))
      in ()
      end

  val instruction =
   fn (se, M.I {dests, n, rhs}) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val id = MU.Id.I n
        val dest = 
         fn () => 
            (case Utils.Option.fromVector dests
              of NONE => fail ("instruction", "Too many destinations")
               | SOME NONE => fail ("instruction", "Destination required, none found")
               | SOME (SOME v) => v)
        val node = 
         fn () => variable (se, dest ())
        val () = 
            (case rhs
              of M.RhsSimple s => node () <== operand (se, s)
               | M.RhsPrim {prim, createThunks, typs, args} => doPrim (se, dests, prim, createThunks, typs, args)
               | M.RhsTuple {mdDesc, inits} => node () <-- tupleMk (se, id, mdDesc, inits)
               | M.RhsTupleSub tf => op --> (tupleOp (se, id, tf, node ()))
               | M.RhsTupleSet {tupField, ofVal} => op <-- (tupleOp (se, id, tupField, operand (se, ofVal)))  
               | M.RhsTupleInited {mdDesc, tup} => variable (se, tup) <-- tupleMk (se, id, mdDesc, Vector.new0 ())
               | M.RhsIdxGet {idx, ofVal} => node () <-- Build.base (MU.Uintp.t (getConfig se))
               | M.RhsCont l => 
                 (case label' (se, l)
                   of SOME n => node () <== n
                    | NONE   => ())
               | M.RhsObjectGetKind v => node () <-- Build.base (MU.Uintp.t (getConfig se))
               | M.RhsThunkMk {typ, fvs} => 
                 let
                   val res = fieldKind (se, typ)
                   val fvs = fieldKinds (se, fvs)
                   val () = addThunkDescriptor (se, MU.Id.I n, res, fvs)
                   val cNode = newShallowTypedBottomNode (se, typOfCode se)
                   val object = 
                       Build.thunk {name = SOME (dest ()),
                                    code = cNode,
                                    result = res, 
                                    fvs = fvs}
                   val () = node () <-- object
                 in ()
                 end
               | M.RhsThunkInit {typ, thunk, fx, code, fvs} => 
                 let
                   val (name, node) = 
                       (case thunk
                         of SOME t => (t, variable (se, t))
                          | NONE => (dest (), node ()))
                   val res = fieldKind (se, typ)
                   val fvs = fieldKindOperands (se, fvs)
                   val () = addThunkDescriptor (se, MU.Id.I n, res, fvs)
                   val cNode = 
                       (case code
                         of SOME f => 
                            let
                              val {cargs, args, returns} = requireFunInfo (se, f)
                              val (clsFormal, fvFormals) =
                                  (case cargs
                                    of M.CcThunk {thunk, fvs} => (thunk, fvs)
                                     | _ => fail ("instruction", "Bad fun info for thunk init"))
                              val () = Vector.foreach2 (fvFormals, fvs, op <==)
                              val () = res <== Vector.sub (returns, 0)
                              val () = clsFormal <== node
                            in variable (se, f)
                            end
                          | NONE => newShallowTypedBottomNode (se, typOfCode se))
                   val object = 
                       Build.thunk {name = SOME name,
                                    code = cNode,
                                    result = res, 
                                    fvs = fvs}
                   val () = node <-- object
                 in ()
                 end
               | M.RhsThunkGetFv {typ, fvs, thunk, idx} => 
                 let
                   val thunk = variable (se, thunk)
                   val fvs = fieldKinds (se, fvs)
                   val result = fieldKind (se, typ)
                   val () = addThunkDescriptor (se, MU.Id.I n, result, fvs)
                   val () = node () == (Vector.sub (fvs, idx))
                   val shape = Build.thunk {name = NONE,
                                            code = newShallowTypedBottomNode (se, typOfCode se),
                                            result = result,
                                            fvs = fvs}
                   val () = thunk --> shape
                 in ()
                 end
               | M.RhsThunkValue {typ, thunk, ofVal} => 
                 let
                   val node = 
                       (case thunk
                         of SOME t => variable (se, t)
                          | NONE => node ())
                   val contents = operand (se, ofVal)
                   val () = addThunkDescriptor (se, MU.Id.I n, contents, Vector.new0 ())
                   val code = newShallowTypedBottomNode (se, typOfCode se)
                   val object = Build.thunkValue {result = contents, code = code}
                 in node <-- object
                 end
               | M.RhsThunkGetValue {typ, thunk} => 
                 let
                   val result = fieldKind (se, typ)
                   val () = node () <== result
                   val () = addThunkDescriptor (se, MU.Id.I n, result, Vector.new0 ())
                   val shape = Build.thunkValue {result = result,
                                                 code = newShallowTypedBottomNode (se, typOfCode se)}
                   val () = variable (se, thunk) --> shape
                 in ()
                 end
               | M.RhsThunkSpawn {typ, thunk, fx} => 
                 let
                   val result = fieldKind (se, typ)
                   val thunk = variable (se, thunk)
                   val () = addThunkDescriptor (se, MU.Id.I n, result, Vector.new0 ())
                   val shape = Build.thunkValue {result = result,
                                                 code = newShallowTypedBottomNode (se, typOfCode se)}
                   val () = thunk --> shape
                   val () = escapes (se, thunk)
                 in ()
                 end
               | M.RhsClosureMk {fvs} => 
                 let
                   val fvs = fieldKinds (se, fvs)
                   val () = addClosureDescriptor (se, id, fvs)
                   val cNode = newShallowTypedBottomNode (se, typOfCode se)
                   val shape = 
                       Build.closure {name = SOME (dest ()),
                                      code = cNode,
                                      fvs = fvs}
                   val () = node () <-- shape
                 in ()
                 end
               | M.RhsClosureInit {cls, code, fvs} => 
                 let
                   val (dest, node) = 
                       case cls
                        of SOME c => (c, variable (se, c))
                         | NONE => (dest (), node ())
                   val () = node <-- pFunctionInit (se, dest, id, node, code, fvs)
                 in ()
                 end
               | M.RhsClosureGetFv {fvs, cls, idx} => 
                 let
                   val cls = variable (se, cls)
                   val fvs = fieldKinds (se, fvs)
                   val () = addClosureDescriptor (se, MU.Id.I n, fvs)
                   val () = node () == (Vector.sub (fvs, idx))
                   val shape = Build.closure {name = NONE, 
                                              code = newShallowTypedBottomNode (se, typOfCode se),
                                              fvs = fvs}
                   val () = cls --> shape
                 in ()
                 end
               | M.RhsPSetNew oper => node () <-- Build.pSet (operand (se, oper))
               | M.RhsPSetGet v => variable (se, v) --> Build.pSet (node ())
               | M.RhsPSetCond {bool, ofVal} => 
                 let
                   val () = node () <-- Build.pSet (operand (se, ofVal))
                   val () = node () <-- Build.pSetEmpty ()
                 in ()
                 end
               | M.RhsPSetQuery oper => node () <-- Build.base (MU.Bool.t (getConfig se))
               | M.RhsPSum {tag, typ, ofVal} => 
                 let
                   val field = operand (se, ofVal)
                   val () = addPSumDescriptor (se, MU.Id.I n, field)
                   val () = node () <-- Build.pSum {tag = tag, field = field}
                 in ()
                 end
               | M.RhsPSumProj {typ, sum, tag} => 
                 let
                   val node = node ()
                   val sum = variable (se, sum) 
                   val () = addPSumDescriptor (se, MU.Id.I n, node)
                   val () = sum --> Build.pSum {tag = tag, field = node}
                 in ()
                 end)
      in ()
      end

  val instructions = 
   fn (se, is) => Vector.foreach (is, fn i => instruction (se, i))

  val target = 
   fn (se as (state, env), M.T {block, arguments}) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val arguments = operands (se, arguments)
        val formals = getCont (se, block)
        val () = Vector.foreach2 (formals, arguments, op <==)
      in ()
      end

  val switch = 
   fn (se as (state, env), {on, cases, default}) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val () = Vector.foreach (cases, fn (_, t) => target (se, t))
        val () = Option.foreach (default, fn t => target (se, t))
      in ()
      end

  val interProc = 
   fn (se, (l, {callee, ret, fx})) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se

        val rets = 
            (case ret
              of M.RNormal {rets, ...} => variables (se, rets)
               | M.RTail _ => getCurrentReturns se)

        val directCall = 
         fn (f, args) => 
            let
              val actuals = operands (se, args)
              val () = 
                  (case getFunInfo (se, f)
                    of SOME {cargs, args, returns} => 
                       let
                         val () = Vector.foreach2 (args, actuals, op <==)
                         val () = Vector.foreach2 (rets, returns, op <==)
                       in ()
                       end
                     | NONE => variable (se, f) --> Build.code {name = NONE, args = actuals, ress = rets})
            in ()
            end

        val () = 
            (case callee
              of M.IpCall {call, args} => 
                 (case call
                   of M.CCode {ptr, ...} => directCall (ptr, args)
                    | M.CClosure {cls, code} => 
                      let
                        val args = operands (se, args)
                        val filter = if MU.Codes.exhaustive code then
                                       SOME (MU.Codes.possible code)
                                     else 
                                       NONE
                        val codeShape = Build.call {filter = filter, args = args, ress = rets}
                        val code = newShapedNode (se, codeShape)
                        val shape = Build.callClosure code
                        val () = variable (se, cls) --> shape
                      in ()
                      end
                    | M.CDirectClosure {cls, code} => directCall (code, args))
               | M.IpEval {typ, eval} => 
                 let
                   val typ = fieldKind (se, typ)
                   val () = typ == Vector.sub (rets, 0)
                   val () = addThunkDescriptor (se, MU.Id.T l, typ, Vector.new0 ())
                   val () = 
                       (case eval
                         of M.EThunk {thunk, code} => 
                            let
                              val filter = if MU.Codes.exhaustive code then
                                             SOME (MU.Codes.possible code)
                                           else 
                                             NONE
                              val evalShape = Build.eval {filter = filter, ress = rets}
                              val eval = newShapedNode (se, evalShape)
                              val shape = Build.evalThunk {result = Vector.sub (rets, 0), 
                                                           code = eval}
                              val () = variable (se, thunk) --> shape
                            in ()
                            end
                          | M.EDirectThunk {thunk, code} => 
                            let
                              val () = directCall (code, Vector.new0 ())
                              val shape = Build.thunkValue {result = Vector.sub (rets, 0),
                                                            code = variable (se, code)}
                              val () = variable (se, thunk) --> shape
                            in ()
                            end)
                 in ()
                 end)
      in ()
      end

  val transfer = 
   fn (se as (state, env), (l, t)) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
            
        val () = 
            (case t
              of M.TGoto tg => target (se, tg)
               | M.TCase sw => switch (se, sw)
               | M.TInterProc r => interProc (se, (l, r))
               | M.TReturn args => 
                 let
                   val rets = getCurrentReturns se
                   val args = operands (se, args)
                   val () = Vector.foreach2 (rets, args, op <==)
                 in ()
                 end
               | M.TCut {cont, args, cuts} => 
                 let
                   val cont = variable (se, cont)
                   val args = operands (se, args)
                   val targets = MU.Cuts.targets cuts
                   val filter = 
                       if MU.Cuts.exits cuts then
                         NONE
                       else
                         SOME targets
                   val () = cont --> Build.cut {filter = filter, args = args}
                 in ()
                 end
               | M.THalt opnd =>
                 let
                   val opnd = operand (se, opnd)
                   val () = escapes (se, opnd)
                 in ()
                 end
               | M.TPSumCase sw => switch (se, sw))
      in ()
      end 

  val block = 
   fn (se, (l, M.B {parameters, instructions = is, transfer = t})) => 
      let
        val (op ==, op <==, op <--, op -->) = symbols se
        val parameters = variables (se, parameters)
        val label = label (se, l)
        val cont = Build.cont {label = SOME l, args = parameters}
        val () = label <-- cont
        val () = instructions (se, is) 
        val () = transfer (se, (l, t))
      in ()
      end

  val codeBody = 
   fn (se, M.CB {entry, blocks}) => LD.foreach (blocks, fn a => block (se, a))

  val global = 
   fn (se as (state, env), (v, g)) =>
      let 
        val (op ==, op <==, op <--, op -->) = symbols se
        val destNode = variable (se, v)
        val () = 
            (case g
              of M.GCode (M.F {cc, args, rtyps, body, ...}) =>
                 let
                   val (env, returns) = enterFunction (se, v)
                   val se = (state, env)
                   val args = variables (se, args)
                   val shape = Build.code {name = SOME v,
                                           args = args,
                                           ress = returns}
                   val () = destNode <-- shape
                   val () = codeBody (se, body)
                 in ()
                 end
               | M.GErrorVal t => destNode <== newShallowTypedBottomNode (se, t)
               | M.GIdx d => destNode <-- Build.base M.TIdx
               | M.GTuple {mdDesc, inits} => destNode <-- tupleMk (se, MU.Id.G v, mdDesc, inits)
               | M.GRat _ => destNode <-- Build.base MUP.NumericTyp.tRat
               | M.GInteger _ => destNode <-- Build.base MUP.NumericTyp.tIntegerArbitrary
               | M.GCString _ => destNode <-- Build.base M.TCString
               | M.GThunkValue {typ, ofVal} => 
                 let
                   val contents = operand (se, ofVal)
                   val code = newShallowTypedBottomNode (se, typOfCode se)
                   val () = addThunkDescriptor (se, MU.Id.G v, contents, Vector.new0 ())
                   val shape = Build.thunkValue {result = contents, code = code}
                 in destNode <-- shape
                 end
               | M.GSimple s => destNode <== operand (se, s)
               | M.GClosure {code, fvs} => destNode <-- pFunctionInit (se, v, MU.Id.G v, destNode, code, fvs)
               | M.GPSum {tag, typ, ofVal} => 
                 let
                   val field = operand (se, ofVal)
                   val () = addPSumDescriptor (se, MU.Id.G v, field)
                 in destNode <-- Build.pSum {tag = tag, field = field}
                 end
               | M.GPSet s => destNode <-- Build.pSet (operand (se, s)))
      in ()
      end


  val globals = 
   fn (se, globals) => VD.foreach (globals, fn a => global (se, a))

  structure Initialize = 
  struct
    val initializeFunInfo = 
     fn (se as (state, env), p) => 
        let
          val M.P {globals, ...} = p
          val help = 
           fn (v, g) => 
              let
                val () = 
                    (case g
                      of M.GCode (M.F {cc, args, rtyps, ...}) => 
                         let
                           val cargs = MU.CallConv.map (cc, fn v => variable (se, v))
                           val args = variables (se, args)
                           val returns = Vector.map (rtyps, fn t => typ (se, t))
                           val info = MRB.IiCode {cargs = cargs,
                                                  args = args,
                                                  returns = returns}
                           val () = State.addIInfo (state, MU.Id.G v, info)
                         in ()
                         end
                       | _ => ())
              in ()
              end
          val () = VD.foreach (globals, help)
        in ()
        end

    structure BindVars = 
    MilAnalyseF(struct
                  type state = (State.t * Env.t) ref
                  type env = Config.t
                  val config = fn c => c
                  val indent = 2
                  val allocateVariable = 
                   fn (ser as (ref (se as (state, env))), v) => 
                      let
                        val si = getSi se
                        val node = 
                            case Env.getVarNode' (env, v)
                             of SOME node => node
                              | NONE => 
                                (case MU.SymbolInfo.variableKind (si, v)
                                  of M.VkExtern => 
                                     let
                                       val t = MU.SymbolInfo.variableTyp (si, v)
                                       val shape = Build.unknown t
                                       val node = newShapedNode (se, shape)
                                       val env = Env.addVarNode (env, v, node)
                                       val () = ser := (state, env)
                                     in node
                                     end
                                   | _ => 
                                     let
                                       val t = MU.SymbolInfo.variableTyp (si, v)
                                       val node = typ (se, t)
                                       val env = Env.addVarNode (env, v, node)
                                       val () = ser := (state, env)
                                     in node
                                     end)
                      in node
                      end
                  val variableBind = 
                   fn (ser, c, v) => 
                      let
                        val _ = allocateVariable (ser, v)
                      in c
                      end
                  val variableBind = SOME variableBind
                  val labelBind = 
                   fn (ser as ref (se as (state, env)), c, l) => 
                      let
                        val node = newShallowTypedBottomNode (se, typOfCode se)
                        val env = Env.addLabelNode (env, l, node)
                        val () = ser := (state, env)
                      in c
                      end
                  val labelBind = SOME labelBind
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction = NONE
                  val analyseTransfer = NONE
                  val analyseBlock = 
                   fn (ser, c, l, b) => 
                      let 
                        val parameters = MU.Block.parameters b
                        val nodes = Vector.map (parameters, fn v => allocateVariable (ser, v))
                        val ref (state, env) = ser
                        val env = Env.addCont (env, l, nodes)
                        val () = ser := (state, env)
                      in c
                      end
                  val analyseBlock = SOME analyseBlock
                  val analyseGlobal = NONE
                end)

    val bindVars = BindVars.analyseProgram 

    val initialize = 
     fn (pd, st, p) => 
        let
          val state = State.new ()
          val env = Env.new (pd, I.SymbolInfo.SiTable st)
          val ser = ref (state, env)
          val () = bindVars (ser, getConfig (state, env), p)
          val se = !ser
          val () = initializeFunInfo (se, p)
        in se
        end

  end (* structure Initialize *)


  val summarize = 
   fn ((state, env), st) => 
      let
        val pd = Env.getPD env
        val varNodes = Env.envGetVarNodes env
        val labelNodes = Env.envGetLabelNodes env
        val names = Env.envGetNames env
        val iInfo = State.stateGetIInfo state
        val nodes = !(State.stateGetNodes state)
        val summary = MilRepSummary.summarize (pd, st, {varNodes = varNodes,
                                                        iInfo = iInfo,
                                                        nodes = nodes,
                                                        labelNodes = labelNodes,
                                                        names = names})
      in summary
      end

  val propagate = 
   fn (se as (state, env)) => 
      let
        val nodes = !(State.stateGetNodes state)
        val () = List.foreach (nodes, Node.propagate)
      in ()
      end

  val program = 
   fn (pd, p) => 
      let
        val () = Chat.log2 (pd, "Starting analysis")
        val M.P {globals = gs, symbolTable = st, entry, externs, includes, ...} = p
        val () = Chat.log2 (pd, "Initializing")
        val se = Initialize.initialize (pd, st, p)
        val () = Chat.log2 (pd, "Analyzing")
        fun doExtern v = unknown (se, variable (se, v))
        val () = VS.foreach (externs, doExtern)
        val () = Vector.foreach (includes, fn (M.IF {externs, ...}) => VS.foreach (externs, doExtern))
        val () = globals (se, gs)
        val () = escapes (se, variable (se, entry))
        val () = Chat.log2 (pd, "Propagating")
        val () = propagate se
        val () = Chat.log2 (pd, "Summarizing")
        val summary = summarize (se, st)
        val () = Chat.log2 (pd, "Analysis done")
      in summary
      end


end (* structure MilRepAnalyze *)

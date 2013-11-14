(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_LOWER_VECTOR = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilLowerVector :> MIL_LOWER_VECTOR = 
struct

  val passname = "MilLowerVector"

  val fail = fn (fname, msg) => Fail.fail (passname, fname, msg)

  structure I = Identifier
  structure IM = I.Manager
  structure VS = I.VariableSet
  structure VD = I.VariableDict
  structure ND = I.NameDict
  structure LD = I.LabelDict
  structure M = Mil
  structure MP = Mil.Prims
  structure MU = MilUtils
  structure MPU = MU.Prims.Utils
  structure MSTM = MU.SymbolTableManager
  structure MTT = MilType.Typer
  structure MS = MilStream
  structure POM = PObjectModelLow
  structure PD = PassData
  structure MCFG = MilCfg

  datatype state = S of {stm : M.symbolTableManager}

  datatype env = E of {pd : PD.t,
                       targetSize : MP.vectorSize,
                       vars : M.variable List.t VD.t}


  val ((stateSetStm, stateGetStm))=
      let
        val r2t = fn S {stm} => (stm)
        val t2r = fn (stm) => 
                     S {stm = stm}
      in FunctionalUpdate.mk1 (r2t, t2r)
      end
        
  val ((envSetPd, envGetPd),
       (envSetTargetSize, envGetTargetSize),
       (envSetVars, envGetVars)
      ) = 
      let
        val r2t = fn E {pd, targetSize, vars} => (pd, targetSize, vars)
        val t2r = fn (pd, targetSize, vars) => E {pd = pd, targetSize = targetSize, vars = vars}
      in FunctionalUpdate.mk3 (r2t, t2r)
      end
        
  val envGetConfig = PD.getConfig o envGetPd

  fun stateGetSymbolInfo s = I.SymbolInfo.SiManager (stateGetStm s)

  fun getVarTyp (s, v) = MSTM.variableTyp (stateGetStm s, v)

  val relatedSplitVar = 
   fn (state, vo, t)  => 
      let
        val stm = stateGetStm state
        val M.VI {typ, kind} = MSTM.variableInfo (stm, vo)
        val v = MSTM.variableRelated (stm, vo, "vlwr", t, kind)
      in v
      end

  val splitVectorDescriptor = 
   fn (s, e, MP.Vd {vectorSize, elementSize}) =>
      let
        val targetBits = MPU.VectorSize.numBits (envGetTargetSize e)
        val () = if targetBits < MU.FieldSize.numBits elementSize then
                   fail ("splitVectorDescriptor", "Target vector size smaller than element size")
                 else
                   ()
        val rec split = 
         fn vectorSize => 
             let 
               val bits = MPU.VectorSize.numBits vectorSize
             in 
               if bits <= targetBits then 
                 vectorSize
               else 
                 case MPU.VectorSize.halfSize vectorSize
                  of SOME vs => split vs
                   | NONE    => fail ("splitVectorDescriptor", "Can't divide vectorSize")
             end
      in MP.Vd {vectorSize = split vectorSize, elementSize = elementSize}
      end

  val bindSplitVar = 
   fn (s, e, v) =>
      let
        val targetBits = MPU.VectorSize.numBits (envGetTargetSize e)
        val t = getVarTyp (s, v)
        val mkVars = 
         fn (vectorSize, count, mker) => 
            let
              val typs = 
                  List.tabulate (count, fn _ => mker vectorSize)
              val vars = List.map (typs, fn t => relatedSplitVar (s, v, t))
              val d = envGetVars e
              val d = VD.insert (d, v, vars)
              val en = envSetVars (e, d)
            in (en, vars)
            end
        val rec split = 
         fn (vectorSize, count, mker) => 
             let 
               val bits = MPU.VectorSize.numBits vectorSize
             in 
               if bits <= targetBits then 
                 mkVars (vectorSize, count, mker) 
               else 
                 case MPU.VectorSize.halfSize vectorSize
                  of SOME vs => split (vs, 2*count, mker)
                   | NONE    => fail ("bindSplitVar", "Can't divide vectorSize")
             end
      in case MU.Typ.Dec.tViVector t
          of SOME {vectorSize, elementTyp} => 
             let val bits = MPU.VectorSize.numBits vectorSize
                 val mker  = fn vectorSize => M.TViVector {vectorSize = vectorSize, elementTyp = elementTyp}
             in if bits <= targetBits then (e, [v]) else split (vectorSize, 1, mker)
             end
           | NONE => 
             (case MU.Typ.Dec.tViMask t
               of SOME (MP.Vd {vectorSize, elementSize}) => 
                  let val bits = MPU.VectorSize.numBits vectorSize
                      val mker = 
                       fn vectorSize => M.TViMask (MP.Vd {vectorSize = vectorSize, elementSize = elementSize})
                  in if bits <= targetBits then (e, [v]) else split (vectorSize, 1, mker)
                  end
                | NONE  => (e, [v]))
      end

        
  val splitVar = 
   fn (state, env, v) => 
      (case VD.lookup (envGetVars env, v)
        of SOME l => l
         | NONE   => [v])


  structure Chat = ChatF(struct type env = env
                                val extract = envGetConfig
                                val name = passname
                                val indent = 2 
                         end)

  val rec doTyps =
   fn (state, env, ts) => Vector.map (ts, fn t => doTyp (state, env, t))
  and rec doTyp = 
   fn (state, env, t) => 
      let 
        val typs = fn ts => doTyps (state, env, ts)
        val typ = fn t => doTyp (state, env, t)
        val t = 
            case t
             of M.TAny => t
              | M.TAnyS _ => t
              | M.TNonRefPtr => t
              | M.TRef => t
              | M.TBits _ => t
              | M.TNone => t
              | M.TNumeric _ => t
              | M.TBoolean => t
              | M.TName => t
              | M.TViVector _ => t
              | M.TViMask _ => t
              | M.TCode {cc, args, ress} => 
                let
                  val cc = MU.CallConv.map (cc, typ)
                  val aTyps = typs args
                  val rTyps = typs ress
                  val t = M.TCode {cc = cc, args = aTyps, ress = rTyps}
                in t
                end
              | M.TTuple {pok, fixed, array} =>
                let
                  fun typVar (t, ag, v) = (typ t, ag, v)
                  val tvs = Vector.map (fixed, typVar)
                  val tv = typVar array
                  val t = M.TTuple {pok = pok, fixed = tvs, array = tv}
                in t
                end
              | M.TCString => t
              | M.TIdx => t
              | M.TContinuation ts => M.TContinuation (typs ts)
              | M.TThunk t => M.TThunk (typ t)
              | M.TPAny => t
              | M.TClosure {args, ress} =>
                let
                  val args = typs args
                  val ress = typs ress
                  val t = M.TClosure {args = args, ress = ress}
                in t
                end
              | M.TSum {tag, arms} =>
                let
                  val tt = typ tag
                  val arms = Vector.map (arms, fn (k, v) => (k, Vector.map (v, typ)))
                  val t = M.TSum {tag = tt, arms = arms}
                in t
                end
              | M.TPType {kind, over} =>
                let
                  val over = typ over
                  val t = M.TPType {kind = kind, over = over}
                in t
                end
              | M.TPRef t =>
                let
                  val t = typ t
                  val t = M.TPRef (typ t)
                in t
                end
      in t
      end

  val doOperand : state * env * M.operand -> M.operand List.t = 
   fn (state, env, operand) => 
      (case operand
        of M.SVariable v => List.map (splitVar (state, env, v), M.SVariable)
         | M.SConstant _ => [operand])

  val doOperands' : state * env * (M.operand Vector.t) -> M.operand List.t Vector.t = 
   fn (state, env, operands) => Vector.map (operands, fn operand => doOperand (state, env, operand))

  val doOperands : state * env * (M.operand Vector.t) -> M.operand Vector.t = 
   fn (state, env, operands) => 
      Vector.concatV (Vector.map (doOperands' (state, env, operands), Vector.fromList))

  val doOperandsPointwise : state * env * (M.operand Vector.t) -> M.operand Vector.t List.t = 
   fn (state, env, operands) => 
      let
        val opers = doOperands' (state, env, operands)
        val rec doIt =
         fn opers =>
            if List.isEmpty (Vector.sub (opers, 0)) then
              []
            else
              Vector.map (opers, hd) :: doIt (Vector.map (opers, tl))
      in if Vector.length opers = 0 then
           []
         else
           doIt opers
      end

  val doVariableBinders = 
   fn (state, env, vs) => 
      let
        val folder = fn (v, env) => 
                        let
                          val (env, vs) = bindSplitVar (state, env, v)
                          val vs = Vector.fromList vs
                        in (vs, env)
                        end
        val (vs, env) = Vector.mapAndFold (vs, env, folder)
      in (env, Vector.concatV vs)
      end

  val doInstruction =
   fn (state, env, i as M.I {dests, n, rhs}) => 
      let
        val checkDesc =
         fn (desc, s, a) => 
            let
              val targetBits = MPU.VectorSize.numBits (envGetTargetSize env)
              val bits = MPU.VectorSize.numBits (MPU.VectorDescriptor.vectorSize desc)
            in if bits <= targetBits then a
               else fail ("doInstruction:"^s, "Unimplemented")
            end
        val (env, is) = 
            case rhs
             of M.RhsPrim {prim, createThunks, typs, args} =>
                let
                  val typs = doTyps (state, env, typs)
                in
                  case prim
                   of MP.Vector v => 
                      (case v 
                        of MP.ViPointwise {descriptor, masked, operator} => 
                           let
                             val () = if masked then fail ("doInstruction:ViPointwise", "Masked unimplemented") 
                                      else ()
                             val desc = splitVectorDescriptor (state, env, descriptor)
                             val dest = Vector.sub (dests, 0)
                             val argsL = doOperandsPointwise (state, env, args)
                             val (env, dests) = bindSplitVar (state, env, dest)
                             val pairs = List.zip (dests, argsL)
                             val p = MP.ViPointwise {descriptor = desc, masked = false, operator = operator}
                             val mkRhs = fn args => M.RhsPrim {prim = MP.Vector p, 
                                                               createThunks = createThunks, 
                                                               typs = typs, 
                                                               args = args}
                             val mk1 = fn (v, args) => M.I {dests = Vector.new1 v,
                                                            n = 0,
                                                            rhs = mkRhs args}
                             val is = List.map (pairs, mk1)
                           in (env, is)
                           end
                         | MP.ViConvert {to, from} => checkDesc(#descriptor to, "ViConvert", 
                                                                checkDesc (#descriptor from, "ViConvert", (env, [i])))
                         | MP.ViCast {to, from} => checkDesc(#descriptor to, "ViCast", 
                                                             checkDesc (#descriptor from, "ViCast", (env, [i])))
                         | MP.ViCompare {descriptor, typ, operator} => 
                           let
                             val desc = splitVectorDescriptor (state, env, descriptor)
                             val dest = Vector.sub (dests, 0)
                             val argsL = doOperandsPointwise (state, env, args)
                             val (env, dests) = bindSplitVar (state, env, dest)
                             val pairs = List.zip (dests, argsL)
                             val p = MP.ViCompare {descriptor = desc, typ = typ, operator = operator}
                             val mkRhs = fn args => M.RhsPrim {prim = MP.Vector p, 
                                                               createThunks = createThunks, 
                                                               typs = typs, 
                                                               args = args}
                             val mk1 = fn (v, args) => M.I {dests = Vector.new1 v,
                                                            n = 0,
                                                            rhs = mkRhs args}
                             val is = List.map (pairs, mk1)
                           in (env, is)
                           end

                         | MP.ViReduction r => checkDesc(#descriptor r, "ViReduction", (env, [i]))
                         | MP.ViData {descriptor, operator} => 
                           let
                             val desc = splitVectorDescriptor (state, env, descriptor)
                             val dest = Vector.sub (dests, 0)
                             val (env, dests) = bindSplitVar (state, env, dest)
                           in
                             case operator
                              of MP.DBroadcast => 
                                 let
                                   val p = MP.ViData {descriptor = desc, operator = operator}
                                   val rhs = M.RhsPrim {prim = MP.Vector p, 
                                                        createThunks = createThunks, 
                                                        typs = typs, 
                                                        args = args}
                                   val mk1 = fn v => M.I {dests = Vector.new1 v,
                                                          n = 0,
                                                          rhs = rhs}
                                   val is = List.map (dests, mk1)
                                 in (env, is)
                                 end
                               | MP.DVector => 
                                 let
                                   val p = MP.ViData {descriptor = desc, operator = operator}
                                   val eltCount = 
                                       let
                                         val dl = List.length dests
                                         val al = Vector.length args
                                         val count = al div dl
                                       in if al mod dl = 0 then
                                            count
                                          else
                                            fail ("doInstruction:DVector", "Bad arg count")
                                       end
                                   val argsL = 
                                       let
                                         val rec split = 
                                          fn l => 
                                             (case l
                                               of [] => []
                                                | _  => 
                                                  let
                                                    val (elts, rest) = List.splitAt (l, eltCount)
                                                  in elts :: split rest
                                                  end)
                                       in split (Vector.toList args)
                                       end
                                   val pairs = List.zip (dests, argsL)
                                   val mkRhs = fn args => M.RhsPrim {prim = MP.Vector p, 
                                                                     createThunks = createThunks, 
                                                                     typs = typs, 
                                                                     args = args}
                                   val mk1 = fn (v, args) => M.I {dests = Vector.new1 v,
                                                                  n = 0,
                                                                  rhs = mkRhs (Vector.fromList args)}
                                   val is = List.map (pairs, mk1)
                                 in (env, is)
                                 end
                               | _ => checkDesc(descriptor, "ViData", (env, [i]))
                           end
                         | MP.ViMaskData r => checkDesc(#descriptor r, "ViMaskData", (env, [i]))
                         | MP.ViMaskBoolean r => checkDesc(#descriptor r, "ViMaskBoolean", (env, [i]))
                         | MP.ViMaskConvert {to, from} => 
                           checkDesc(to, "ViMaskConvert", 
                                     checkDesc (from, "ViMaskConvert", (env, [i])))
                      )
                    | _ => (env, [i])
                end
              | M.RhsTupleSub tf => 
                let
                  val dest = Vector.sub (dests, 0)
                  val (env, dests) = bindSplitVar (state, env, dest)
                  val v = case dests
                           of [v] => v
                            | _   => fail ("doInstruction:RhsTupleSub", "Splitting dests not supported")
                  val tfs = 
                      let
                        val M.TF {tupDesc, tup, field} = tf
                        val nochange = 
                         fn descriptor => MPU.VectorDescriptor.eq (descriptor, 
                                                                   splitVectorDescriptor (state, env, descriptor))
                        val tfs = 
                            case field
                             of M.FiFixed _    => [tf]
                              | M.FiVariable _ => [tf]
                              | M.FiVectorFixed {descriptor, mask, index} => 
                                if nochange descriptor then
                                  [tf]
                                else
                                  fail ("doTupleField", "VectorFixed not supported")
                              | M.FiVectorVariable {descriptor, base, mask, index, kind} => 
                                if nochange descriptor then
                                  case (base, kind, doOperand (state, env, index))
                                   of (_, _, [_]) => [tf]
                                    | (M.TbScalar, M.VikVector, [v0, v1]) => 
                                      let
                                        val desc = 
                                            let
                                              val MP.Vd {vectorSize, elementSize} = descriptor
                                              val vectorSize2 = valOf (MPU.VectorSize.halfSize vectorSize)
                                            in MP.Vd {vectorSize = vectorSize2, elementSize = elementSize}
                                            end
                                        val field0 = 
                                            M.FiVectorVariable {descriptor = desc, base = base, 
                                                                mask = mask, index = v0, kind = kind}
                                        val field1 = 
                                            M.FiVectorVariable {descriptor = desc, base = base, 
                                                                mask = mask, index = v1, kind = kind}
                                        val tf0 = M.TF {tupDesc = tupDesc, tup = tup, field = field0}
                                        val tf1 = M.TF {tupDesc = tupDesc, tup = tup, field = field1}
                                      in [tf0, tf1]
                                      end
                                    | _ => fail ("doTupleField", "VectorVariable unsupported pattern")
                                else
                                  fail ("doTupleField", "VectorVariable not supported")
                      in tfs
                      end
                  val is = 
                      case tfs
                       of [tf] => [M.I {dests = Vector.new1 v,
                                        n = 0,
                                        rhs = M.RhsTupleSub tf}]
                        | [tf0, tf1] => 
                          let
                            val t = getVarTyp (state, v)
                            val {vectorSize, elementTyp} = 
                                case t
                                 of M.TViVector r => r
                                  | _ => fail ("doInstruction:RhsTupleSub", "Bad typ")
                            val vectorSize2 = valOf (MPU.VectorSize.halfSize vectorSize)
                            val t2 = M.TViVector {vectorSize = vectorSize2, elementTyp = elementTyp}
                            val v0 = relatedSplitVar (state, v, t2)
                            val v1 = relatedSplitVar (state, v, t2)
                            val concat = 
                                let
                                  val config = envGetConfig env
                                  val descriptor = MP.Vd {vectorSize = vectorSize2,
                                                          elementSize = MU.Typ.fieldSize' (config, elementTyp)}
                                  val data = MP.ViData {descriptor = descriptor, operator = MP.DConcat}
                                in M.RhsPrim {prim = MP.Vector data,
                                              createThunks = false,
                                              typs = Vector.new1 elementTyp,
                                              args = Vector.new2 (M.SVariable v0, M.SVariable v1)}
                                end
                            val i0 = M.I {dests = Vector.new1 v0, 
                                          n = 0,
                                          rhs = M.RhsTupleSub tf0}
                            val i1 = M.I {dests = Vector.new1 v1, 
                                          n = 0,
                                          rhs = M.RhsTupleSub tf1}
                            val i2 = M.I {dests = Vector.new1 v,
                                          n = 0,
                                          rhs = concat}
                          in [i0, i1, i2]
                          end
                        | _ => fail ("doInstruction:RhsTupleSub", "Unsupported")
                in (env, is)
                end
              | M.RhsTupleSet {tupField as M.TF {tupDesc, tup, field}, ofVal} => 
                let
                  val tf = 
                      case field
                       of M.FiFixed _    => tupField
                        | M.FiVariable _ => tupField
                        | M.FiVectorFixed {descriptor, mask, index} => 
                          if MPU.VectorDescriptor.eq (descriptor, splitVectorDescriptor (state, env, descriptor)) then
                            tupField
                          else
                            fail ("doTupleField", "VectorFixed not supported")
                        | M.FiVectorVariable {descriptor, base, mask, index, kind} => 
                          if MPU.VectorDescriptor.eq (descriptor, splitVectorDescriptor (state, env, descriptor)) then
                            tupField
                          else
                            fail ("doTupleField", "VectorVariable unsupported pattern")

                  val arg = case doOperand (state, env, ofVal)
                             of [arg] => arg
                              | _     => fail ("doTupleField", "Split tuple set not supported")
                  val i = M.I {dests = Vector.new0 (),
                               n = 0,
                               rhs = M.RhsTupleSet {tupField = tf, 
                                                    ofVal = arg}}
                in (env, [i])
                end
              | _ => (env, [i])
                                                   
      in (env, Vector.fromList is)
      end

  val doInstructions = 
   fn (state, env, is) => 
      let
        val folder = fn (i, env) => 
                        let
                          val (env, is) = doInstruction (state, env, i)
                        in (is, env)
                        end
        val (iss, env) = Vector.mapAndFold (is, env, folder)
      in (env, Vector.concatV iss)
      end

  val doTransfer = 
   fn (state, env, transfer) => 
      let
        val doTarget = 
         fn (M.T {block, arguments}) => M.T {block = block, arguments = doOperands (state, env, arguments)}
      in MU.Transfer.mapOverTargets (transfer, doTarget)
      end

  val doBlock =
   fn (state, env, M.B {parameters, instructions, transfer}) => 
      let
        val (env, parameters) = doVariableBinders (state, env, parameters)
        val (env, instructions) = doInstructions (state, env, instructions)
        val transfer = doTransfer (state, env, transfer)
      in (env, M.B {parameters = parameters, instructions = instructions, transfer = transfer})
      end

  val rec doDomForest = 
   fn (state, env, children) => Vector.map (children, fn child => doDomTree (state, env, child))

  and rec doDomTree = 
   fn (state, env, Tree.T ((label, block), children)) => 
      let
        val (env, block) = doBlock (state, env, block)
        val children = doDomForest (state, env, children)
      in Tree.T ((label, block), children)
      end

  val doCodeBody = 
   fn (state, env, cb as M.CB {entry, blocks}) => 
      let
        val cfg = MCFG.build (envGetConfig env, stateGetSymbolInfo state, cb)
        val tree = MCFG.getLabelBlockDomTree cfg
        val tree = doDomTree (state, env, tree)
        val blocks = Tree.foldPre (tree, LD.empty, fn ((l, b), d) => LD.insert (d, l, b))
      in M.CB {entry = entry, blocks = blocks}
      end

 (* No vectors in arguments, etc *)
  val doCode = 
   fn (state, env, M.F {fx, escapes, recursive, cc, args, rtyps, body}) => 
      let
        val body = doCodeBody (state, env, body)
        val c = M.F {fx = fx, escapes = escapes, recursive = recursive, 
                     cc = cc, args = args, rtyps = rtyps, 
                     body = body} 
      in c
      end

  val doGlobal = 
   fn (state, env, v, g) => 
      case g
       of M.GCode f => M.GCode (doCode (state, env, f)) 
        | _         => g
                         
  val doGlobals = 
   fn (state, env, globals) => VD.map (globals, fn (v, g) => doGlobal (state, env, v, g))
                                      
  val (targetSizeC, findTargetSize) =
      let
        val default = fn _ => MP.Vs1024
                               
        val parser : string -> MP.vectorSize option =
         fn (s : string) =>
            case s
             of "64"   => SOME MP.Vs64
             |  "128"  => SOME MP.Vs128
             |  "256"  => SOME MP.Vs256
             |  "512"  => SOME MP.Vs512
             |  "1024" => SOME MP.Vs1024
             | _       => NONE
        val description =
         fn () =>
            Layout.str (passname ^ " target vector size to lower to (64|128|256|512|1024)")
                  
        val name = passname ^ ":target-size"
      in
        Config.Control.mk (name, description, parser, default) 
      end

  val program = 
   fn (p, pd) => 
      (case findTargetSize (PD.getConfig pd)
        of MP.Vs1024 => p
         | targetSize => 
           let 
             val M.P {symbolTable = st, ...} = p
             val stm = IM.fromExistingAll st
             val state = S {stm = stm}
             val env = E {pd = pd, targetSize = targetSize, vars = VD.empty}
             val M.P {includes, externs, symbolTable, globals, entry} = p
             val globals = doGlobals (state, env, globals)
             val st = IM.finish stm
             val p = M.P {includes = includes, externs = externs, symbolTable = st, globals = globals, entry = entry}
           in p
           end)

  val description = {name        = passname,
                     description = "Lower vector primitives",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls  = [targetSizeC], debugs = [], features = [], subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates,
                      BothMil.mkMilPass program) 

end (* structure MilLowerVector *)

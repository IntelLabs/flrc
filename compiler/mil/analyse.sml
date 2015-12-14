(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MilAnalyse =
sig
  type state
  type env
  val analyseRhs : state * env * Mil.rhs -> unit
  val analyseInstruction : state * env * Mil.instruction -> env
  val analyseCall : state * env * Mil.call ->  unit
  val analyseEval : state * env * Mil.eval ->  unit
  val analyseTransfer : state * env * Mil.label option * Mil.transfer -> unit
  val analyseBlock : state * env * Mil.label * Mil.block -> unit
  val analyseBlocks : state * env * Mil.block Mil.LD.t -> unit
  val analyseCodeBody : state * env * Mil.codeBody -> unit
  val analyseCode : state * env * Mil.code -> unit
  val analyseGlobal : state * env * Mil.variable * Mil.global -> unit
  val analyseProgram : state * env * Mil.t -> unit
end;

(* Each thing will be analyzed once.  There are no guarantees
 * about ordering.  variable/label uses may be encountered before
 * their binding sites.  Labels will be analyzed before the block
 * which they bind is analyzed.
 *)
functor MilAnalyseF (
  type state
  type env
  val config : env -> Config.t
  val indent : int
  val externBind         : (state * env * Mil.variable -> env) option
  val variableBind       : (state * env * Mil.variable -> env) option
  val labelBind          : (state * env * Mil.label -> env) option
  val variableUse        : (state * env * Mil.variable -> unit) option
  val analyseJump        : (state * env * Mil.label -> unit) option
  val analyseCut         : (state * env * Mil.label -> unit) option
  val analyseConstant    : (state * env * Mil.constant -> unit) option
  val analyseInstruction : (state * env * Mil.instruction -> env) option
  val analyseTransfer    : (state * env * Mil.label option * Mil.transfer -> env) option
  val analyseBlock       : (state * env * Mil.label * Mil.block -> env) option
  val analyseGlobal      : (state * env * Mil.variable * Mil.global -> env) option
) :> MilAnalyse where type state = state
                  and type env = env
= struct
  structure VS = Mil.VS
  structure Chat = ChatF (struct
                          type env = env
                          val extract = config
                          val name = "MilAnalyse"
                          val indent = indent
                          end)

  val clientBind = variableBind
  val clientExternBind = externBind
  val clientLabelBind = labelBind
  val clientVariable = variableUse
  val clientJump = analyseJump
  val clientCut = analyseCut
  val clientConstant = analyseConstant
  val clientInstruction = analyseInstruction
  val clientTransfer = analyseTransfer
  val clientBlock = analyseBlock
  val clientGlobal = analyseGlobal

  structure I = Identifier
  structure VD = I.VariableDict
  structure LS = I.LabelSet
  structure LD = I.LabelDict
  structure M = Mil

  type state = state
  type env = env

  fun analyseBinder (s, e, v) =
      case clientBind
       of NONE => e
        | SOME vb => vb (s, e, v)

  fun analyseExtern (s, e, v) =
      case clientExternBind
       of NONE => e
        | SOME vb => vb (s, e, v)

  fun analyseLabelBinder (s, e, l) =
      case clientLabelBind
       of NONE => e
        | SOME lb => lb (s, e, l)

  fun analyseBinders (s, e, vs) =
      Vector.fold (vs, e, fn (v, e) => analyseBinder (s, e, v))

  fun analyseVariable (s, e, v) =
      case clientVariable
       of NONE => ()
        | SOME vu => vu (s, e, v)

  fun analyseVariableO (s, e, ov) =
      case ov
       of NONE => ()
        | SOME v => analyseVariable (s, e, v)

  fun analyseJump (s, e, l) =
      case clientJump
       of NONE => ()
        | SOME aj => aj (s, e, l)

  fun analyseCut (s, e, l) =
      case clientCut
       of NONE => ()
        | SOME ac => ac (s, e, l)

  fun analyseConstant (s, e, c) =
      case clientConstant
       of NONE => ()
        | SOME ac => ac (s, e, c)

  fun analyseSimple (s, e, simple) =
      case simple
       of M.SVariable v => analyseVariable (s, e, v)
        | M.SConstant c => analyseConstant (s, e, c)

  fun analyseSimples (s, e, simps) =
      Vector.foreach (simps, fn simp => analyseSimple (s, e, simp))

  fun analyseOperand (s, e, opnd) = analyseSimple (s, e, opnd)

  fun analyseOperands (s, e, os) =
      Vector.foreach (os, fn opnd => analyseOperand (s, e, opnd))

  fun analyseOperandO (s, e, oo) =
      case oo
       of NONE => ()
        | SOME opnd => analyseOperand (s, e, opnd)

  fun analyseFieldIdentifier (s, e, fi) =
      case fi
       of M.FiFixed      idx              => ()
        | M.FiVariable   opnd             => analyseOperand (s, e, opnd)
        | M.FiVectorFixed {descriptor,
                           mask,
                           index}         => analyseOperandO (s, e, mask)
        | M.FiVectorVariable {descriptor,
                              base,
                              mask,
                              index,
                              kind}       =>
          let
            val () = analyseOperandO (s, e, mask)
            val () = analyseOperand (s, e, index)
          in ()
          end

  fun analyseTupleField (s, e, M.TF {tupDesc, tup, field}) =
      let
        val () = analyseVariable (s, e, tup)
        val () = analyseFieldIdentifier (s, e, field)
      in ()
      end

  fun analyseRhs (s, e, rhs) =
      case rhs
       of M.RhsSimple simple => analyseSimple (s, e, simple)
        | M.RhsPrim {prim, createThunks, typs, args} => analyseOperands (s, e, args)
        | M.RhsTuple {mdDesc, inits} => analyseOperands (s, e, inits)
        | M.RhsTupleSub tf => analyseTupleField (s, e, tf)
        | M.RhsTupleSet {tupField, ofVal} =>
          let
            val () = analyseTupleField (s, e, tupField)
            val () = analyseOperand (s, e, ofVal)
          in ()
          end
        | M.RhsTupleCAS {tupField, cmpVal, newVal} =>
          let
            val () = analyseTupleField (s, e, tupField)
            val () = analyseOperand (s, e, cmpVal)
            val () = analyseOperand (s, e, newVal)
          in ()
          end
        | M.RhsTupleWait {tupField, pred} => analyseTupleField (s, e, tupField)
        | M.RhsTupleInited {mdDesc, tup} =>
          let
            val () = analyseVariable (s, e, tup)
          in ()
          end
        | M.RhsIdxGet {idx, ofVal} =>
          let
            val () = analyseVariable (s, e, idx)
            val () = analyseOperand (s, e, ofVal)
          in ()
          end
        | M.RhsCont l => analyseJump (s, e, l)
        | M.RhsThunkMk {typ, fvs} => ()
        | M.RhsThunkInit {typ, thunk, fx, code, fvs} =>
          let
            val () = analyseVariableO (s, e, thunk)
            val () = analyseVariableO (s, e, code)
            fun doOne (fk, opnd) = analyseOperand (s, e, opnd)
            val () = Vector.foreach (fvs, doOne)
          in ()
          end
        | M.RhsThunkGetFv {typ, fvs, thunk, idx} =>
          analyseVariable (s, e, thunk)
        | M.RhsThunkValue {typ, thunk, ofVal} =>
          let
            val () = analyseVariableO (s, e, thunk)
            val () = analyseOperand (s, e, ofVal)
          in ()
          end
        | M.RhsThunkGetValue {typ, thunk} => analyseVariable (s, e, thunk)
        | M.RhsThunkSpawn {typ, thunk, fx} => analyseVariable (s, e, thunk)
        | M.RhsClosureMk {fvs} => ()
        | M.RhsClosureInit {cls, code, fvs} =>
          let
            val () = analyseVariableO (s, e, cls)
            val () = analyseVariableO (s, e, code)
            fun doOne (fk, opnd) = analyseOperand (s, e, opnd)
            val () = Vector.foreach (fvs, doOne)
          in ()
          end
        | M.RhsClosureGetFv {fvs, cls, idx} => analyseVariable (s, e, cls)
        | M.RhsPSetNew opnd => analyseOperand (s, e, opnd)
        | M.RhsPSetGet v => analyseVariable (s, e, v)
        | M.RhsPSetCond {bool, ofVal} =>
          let
            val () = analyseOperand (s, e, bool)
            val () = analyseOperand (s, e, ofVal)
          in ()
          end
        | M.RhsPSetQuery oper => analyseOperand (s, e, oper)
        | M.RhsEnum {tag, typ} => analyseOperand (s, e, tag)
        | M.RhsSum {tag, typs, ofVals} => analyseOperands (s, e, ofVals)
        | M.RhsSumProj {typs, sum, tag, idx} => analyseVariable (s, e, sum)
        | M.RhsSumGetTag {typ, sum} => analyseVariable (s, e, sum)

  fun analyseInstruction (s, e, i as M.I {dests, n, rhs}) =
      let
        val e =
            case clientInstruction
             of NONE => e
              | SOME ai => ai (s, e, i)
        val () = analyseRhs (s, e, rhs)
        val e = Vector.fold (dests, e, fn (v, e) => analyseBinder (s, e, v))
      in e
      end

  fun analyseTarget (s, e, M.T {block, arguments}) =
      let
        val () = analyseJump (s, e, block)
        val () = analyseOperands (s, e, arguments)
      in ()
      end

  fun analyseSwitch (s, e, {select, on, cases, default}) =
      let
        val () = analyseOperand (s, e, on)
        fun doOne (x, t) =
            let
              val () = analyseConstant (s, e, x)
              val () = analyseTarget (s, e, t)
            in ()
            end
        val () = Vector.foreach (cases, doOne)
        val () =
            case default
             of NONE => ()
              | SOME t => analyseTarget (s, e, t)
      in ()
      end

  fun analyseCodes (s, e, {possible, exhaustive}) =
      VS.foreach (possible, fn v => analyseVariable (s, e, v))

  fun analyseCall (s, e, c) =
      case c
       of M.CCode {ptr, code} =>
          let
            val () = analyseVariable (s, e, ptr)
            val () = analyseCodes (s, e, code)
          in ()
          end
        | M.CClosure {cls, code} =>
          let
            val () = analyseVariable (s, e, cls)
            val () = analyseCodes (s, e, code)
          in ()
          end
        | M.CDirectClosure {cls, code} =>
          let
            val () = analyseVariable (s, e, cls)
            val () = analyseVariable (s, e, code)
          in ()
          end

  fun analyseEval (s, e, eval) =
      case eval
       of M.EThunk {thunk, value, code} =>
          let
            val () = analyseVariable (s, e, thunk)
            val () = analyseCodes (s, e, code)
          in ()
          end
        | M.EDirectThunk {thunk, value, code} =>
          let
            val () = analyseVariable (s, e, thunk)
            val () = analyseVariable (s, e, code)
          in ()
          end

  fun analyseInterProc (s, e, ip) =
      case ip
       of M.IpCall {call, args} =>
          let
            val () = analyseCall (s, e, call)
            val () = analyseOperands (s, e, args)
          in ()
          end
        | M.IpEval {typ, eval} => analyseEval (s, e, eval)

  fun analyseCuts (s, e, M.C {exits = _, targets}) =
      let
        val () = LS.foreach (targets, fn l => analyseCut (s, e, l))
      in ()
      end

  fun analyseReturn (s, e, ret) =
      case ret
       of M.RNormal {rets, block, cuts} =>
          let
            fun doOne (r, e) = analyseBinder (s, e, r)
            val e1 = Vector.fold (rets, e, doOne)
            val () = analyseJump (s, e1, block)
            val () = analyseCuts (s, e, cuts)
          in ()
          end
        | M.RTail {exits} => ()

  fun analyseTransfer (s, e, l, t) =
      let
        val e =
            case clientTransfer
             of NONE => e
              | SOME at => at (s, e, l, t)
      in
        case t
         of M.TGoto t => analyseTarget (s, e, t)
          | M.TCase cs => analyseSwitch (s, e, cs)
          | M.TInterProc {callee, ret, fx} =>
            let
              val () = analyseInterProc (s, e, callee)
              val () = analyseReturn (s, e, ret)
            in ()
            end
          | M.TReturn os => analyseOperands (s, e, os)
          | M.TCut {cont, args, cuts} =>
            let
              val () = analyseVariable (s, e, cont)
              val () = analyseOperands (s, e, args)
              val () = analyseCuts (s, e, cuts)
            in ()
            end
          | M.THalt opnd => analyseOperand (s, e, opnd)
      end

  fun analyseBlock (s, e, l,
                    b as M.B {parameters, instructions, transfer}) =
      let
        val e = analyseLabelBinder (s, e, l)
        val e =
            case clientBlock
             of NONE => e
              | SOME ab => ab (s, e, l, b)
        val e = analyseBinders (s, e, parameters)
        fun doOne (i, e) = analyseInstruction (s, e, i)
        val e = Vector.fold (instructions, e, doOne)
        val () = analyseTransfer (s, e, SOME l, transfer)
      in ()
      end

  fun analyseBlocks (s, e, blocks) =
      LD.foreach (blocks, fn (l, b) => analyseBlock (s, e, l, b))

  fun analyseCodeBody (s, e, M.CB {entry, blocks}) =
      let
        val () = analyseJump (s, e, entry)
        val () = analyseBlocks (s, e, blocks)
      in ()
      end

  fun analyseCode (s, e, f) =
      let
        val M.F {fx, escapes, recursive, cc, args, rtyps, body} = f
        val e =
            case cc
             of M.CcCode => e
              | M.CcUnmanaged _ => e
              | M.CcClosure {cls, fvs} =>
                let
                  val e = analyseBinder (s, e, cls)
                  val e = analyseBinders (s, e, fvs)
                in e
                end
              | M.CcThunk {thunk, fvs} =>
                let
                  val e = analyseBinder (s, e, thunk)
                  val e = analyseBinders (s, e, fvs)
                in e
                end
        val e = analyseBinders (s, e, args)
        val () = analyseCodeBody (s, e, body)
      in ()
      end

  fun analyseGlobal (s, e, v, g) =
      let
        val e = analyseBinder (s, e, v)
        val e =
            case clientGlobal
             of NONE => e
              | SOME ag => ag (s, e, v, g)
      in
        case g
         of M.GCode f                  => analyseCode (s, e, f)
          | M.GErrorVal _              => ()
          | M.GIdx _                   => ()
          | M.GTuple {mdDesc, inits}   => analyseSimples (s, e, inits)
          | M.GRat _                   => ()
          | M.GInteger _               => ()
          | M.GCString _               => ()
          | M.GThunkValue {typ, ofVal} => analyseSimple (s, e, ofVal)
          | M.GSimple simp             => analyseSimple (s, e, simp)
          | M.GClosure {code, fvs}   =>
            let
              val () = analyseVariableO (s, e, code)
              fun doOne (fk, opnd) = analyseOperand (s, e, opnd)
              val () = Vector.foreach (fvs, doOne)
            in ()
            end
          | M.GSum {tag, typs, ofVals} => analyseSimples (s, e, ofVals)
          | M.GPSet simp               => analyseSimple (s, e, simp)
      end

  fun analyseProgram (s, e, p) =
      let
        val M.P {includes, externs, globals, symbolTable, entry} = p
        fun doOne (v, e) = analyseExtern (s, e, v)
        val e = VS.fold (MilUtils.Program.externVars p, e, doOne)
        fun doOne (v, g) = analyseGlobal (s, e, v, g)
        val () = VD.foreach (globals, doOne)
      in ()
      end

end;

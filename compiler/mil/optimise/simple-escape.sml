(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_ESCAPE_ANALYSIS = 
sig
  val stats : (string * string) list
  val optimize : PassData.t * IMil.t -> unit
end

functor MilSimpleEscapeF (structure Chat : CHAT where type env = PassData.t
                          val simplify : PassData.t 
                                         * IMil.t
                                         * IMil.WorkSet.ws
                                         -> unit)
        :> MIL_ESCAPE_ANALYSIS = 
struct


  val <- = Try.<-
  val <@ = Try.<@
  val <! = Try.<!
  val << = Try.<<
  val oo = Try.oo
  val om = Try.om
  val or = Try.or
  val || = Try.||
  val @@ = Utils.Function.@@

  infix 3 << @@ oo om <! <\ 
  infixr 3 />
  infix 4 or || 

  val stats = [("NonEscape",    "Functions marked non-escaping" )]

  structure M = Mil
  structure MU = MilUtils
  structure MCG = MilCallGraph
  structure PD = PassData
  structure PLG = PolyLabeledGraph
  structure IFunc = IMil.IFunc
  structure IInstr = IMil.IInstr
  structure IGlobal = IMil.IGlobal
  structure Use = IMil.Use
  structure WS = IMil.WorkSet
  structure L = Layout

  val fail = fn (f, msg) => Fail.fail ("simple-escape.sml", f, msg)

  val closureUseIsNonEscaping = 
   fn (d, imil, c, u) =>
      let
        val cOp = M.SVariable c
        val isThisClosure = 
         fn oper => MU.Operand.eq (oper, cOp)
        val closureNotIn = 
            fn ops => Vector.forall (ops, not o isThisClosure)
        val closureNotIn2nd = 
            fn ops => Vector.forall (ops, not o isThisClosure o #2)

        val doCall = 
         fn call => 
            case call
             of M.CCode _                    => false
              | M.CClosure _                 => false
              | M.CDirectClosure {cls, ... } => isThisClosure (M.SVariable cls)

        val doEval = 
         fn eval => 
            case eval
             of M.EThunk _                   => false
              | M.EDirectThunk {thunk, value, code} => isThisClosure (M.SVariable thunk)

        val doTransfer = 
         fn t => 
            (case t
              of M.TInterProc {callee, ...} => 
                 (case callee 
                   of M.IpCall {call, args} => doCall call andalso closureNotIn args 
                    | M.IpEval {eval, typ}  => doEval eval)
               | _ => false)

        val doRhs = 
         fn rhs =>
            (case rhs
              of M.RhsClosureInit {fvs, ...} => closureNotIn2nd fvs
               | M.RhsClosureGetFv _ => true
               | M.RhsThunkInit {fvs, ...} => closureNotIn2nd fvs
               | M.RhsThunkGetFv _ => true
               | M.RhsObjectGetKind _ => true
               | _ => false)

        val doIInstr =
            fn i =>
               (case IInstr.getMil (imil, i)
                 of IMil.MInstr (M.I {rhs, ...}) => doRhs rhs
                  | IMil.MTransfer t => doTransfer t
                  | IMil.MLabel _ => false
                  | IMil.MDead => false)

        val res = 
            (case u
              of IMil.UseGlobal g => false
               | IMil.UseInstr i => doIInstr i
               | IMil.Used => false)
      in res
      end 

  val closureIsNonEscaping = 
   fn (d, imil, c) =>
      let
        val uses = Use.getUses (imil, c)
        val res = Vector.forall (uses, fn u => closureUseIsNonEscaping (d, imil, c, u))
      in res
      end

  val noInternalEscapes =
   fn (d, imil, fname, iFunc) =>
      let
        val conv = IFunc.getCallConv (imil, iFunc)
        val res = 
            case conv
             of M.CcClosure {cls, ...} => closureIsNonEscaping (d, imil, cls)
              | M.CcThunk {thunk, ...} => closureIsNonEscaping (d, imil, thunk)
              | M.CcCode => true
              | M.CcUnmanaged _ => false
      in res
      end

  val codePtrUseIsNonEscaping =
   fn (d, imil, fname, u) =>
      let
        fun warn () = 
            let
              val () = Chat.warn2 (d, "Unexpected use of function pointer (unless lowered)")
            in false
            end
        val nonEscaping = 
            case u
             of IMil.UseInstr i => 
                (case IInstr.getMil (imil, i)
                  of IMil.MInstr (M.I {rhs, ...}) => 
                     (case rhs
                       of M.RhsClosureInit {cls = SOME cls, ...} => closureIsNonEscaping (d, imil, cls)
                        | M.RhsClosureInit {cls = NONE, ...}     => false
                        | M.RhsThunkInit {thunk = SOME thunk, ...} => closureIsNonEscaping (d, imil, thunk)
                        | M.RhsThunkInit {thunk = NONE, ...}       => false
                        | _ => warn ())
                   | IMil.MTransfer (M.TInterProc {callee, ...}) => 
                     (case callee
                       of M.IpCall {args, ...} => 
                          not (Vector.contains (args, M.SVariable fname, MU.Operand.eq))
                        | M.IpEval _ => true)
                   | IMil.MTransfer _ => false
                   | IMil.MLabel _ => warn ()
                   | IMil.MDead => warn ())
              | IMil.UseGlobal g =>
                (case IGlobal.toGlobal g
                  of SOME (clos, M.GClosure {code = SOME f, fvs}) => 
                     if f = fname then 
                       closureIsNonEscaping (d, imil, clos)
                     else
                       fail ("codePtrUseIsNonEscaping", "Code pointer in free vars of closure")
                   | _ => warn())
              | IMil.Used => false
      in nonEscaping
      end

  val noExternalEscapes = 
   fn (d, imil, fname) =>
      let
        val uses = Use.getUses (imil, fname)
        val ok = Vector.forall (uses, fn u => codePtrUseIsNonEscaping (d, imil, fname, u))
      in ok
      end

  val doFunction = 
   Try.lift 
     (fn ((d, imil), fname) =>
         let
           (* Function may have become dead before we get here *)
           val iFunc = <@ IFunc.getIFuncByName' (imil, fname)
           val () = Try.require (IFunc.getEscapes (imil, iFunc))
           val () = Try.require (not (IFunc.isProgramEntry (imil, iFunc)))
           val () = Try.require (noInternalEscapes (d, imil, fname, iFunc))
           val () = Try.require (noExternalEscapes (d, imil, fname))
           val () = PD.click (d, "NonEscape")
           val () = IFunc.markNonEscaping (imil, iFunc)
           val w = WS.new ()
           val () = WS.addUses (w, IMil.Use.getUses (imil, fname))
           val () = WS.addItem (w, IMil.ItemFunc iFunc)
           val () = simplify (d, imil, w)
         in ()
         end)

  val doConnectedComponent = 
   fn ((d, imil), nodes) =>
      let
        val doOne = 
         fn (node, changed) => 
            (case PLG.Node.getLabel node
              of MCG.Graph.NUnknown => changed
               | MCG.Graph.NFun f => isSome (doFunction ((d, imil), f)))

        val rec loop = 
         fn changed => 
            if List.fold (nodes, false, doOne) then 
              loop true
            else changed

        val changed = 
            (case nodes
              of [f] => doOne (f, false)
               | _ => loop false)
            
      in ()
      end

  val optimize = 
   fn (d, imil) =>
      let
        val MCG.Graph.G {unknown, known, graph} = IMil.T.callGraph imil
        (* Process callees before callers, since escape analysis may enable
         * inlining in the caller. *)
        val components = List.rev (PLG.scc graph)
        val doOne = fn comp => doConnectedComponent ((d, imil), comp)
        val () = List.foreach (components, doOne)
      in ()
      end

end

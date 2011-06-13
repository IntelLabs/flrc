(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_CFG_SIMPLIFY =
sig
  val program'  : PassData.t * IMil.t * IMil.WorkSet.ws -> unit
  val function' : PassData.t * IMil.t * IMil.WorkSet.ws * IMil.iFunc -> unit
  val function  : PassData.t * IMil.t * IMil.iFunc -> unit
  val stats : (string * string) list
end;

structure MilCfgSimplify :> MIL_CFG_SIMPLIFY =
struct

  structure PD = PassData
  structure LS = Identifier.LabelSet
  structure ILD = Identifier.ImpLabelDict
  structure M = Mil
  structure MU = MilUtils
  structure IM = IMil
  structure WS = IM.WorkSet
  structure IBlock = IM.IBlock
  structure Use = IM.Use
  structure IInstr = IM.IInstr
  structure IFunc = IM.IFunc
  structure T = IM.T
  structure SS = StringSet

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

  val passname = "MilCfgSimplify"

  val fail = 
   fn (f, m) => Fail.fail ("cfg-simplify.sml", f, m)

  structure Chat = ChatF(type env = PD.t
                         val extract = PD.getConfig
                         val name = passname
                         val indent = 2)

  structure Click = 
  struct
    val stats = []
    val {stats, click = collapseSwitch} = PD.clicker {stats = stats, passname = passname,
                                                      name = "CollapseSwitch", desc = "Cases collapsed"}
    val {stats, click = gotoShortcut} = PD.clicker {stats = stats, passname = passname,
                                                    name = "GotoShortcut", desc =   "Goto shortcuts"}
    val {stats, click = iPShortcut} = PD.clicker {stats = stats, passname = passname,
                                                  name = "IPShortcut", desc =     "InterProc shortcuts"}
    val {stats, click = returnShortcut} = PD.clicker {stats = stats, passname = passname,
                                                      name = "ReturnShortcut", desc = "Return shortcuts"}
    val {stats, click = switchTarget} = PD.clicker {stats = stats, passname = passname,
                                                    name = "SwitchTarget", desc =   "Switch shortcuts"}
    val {stats, click = tailCall} = PD.clicker {stats = stats, passname = passname,
                                                name = "TailCall", desc =       "Tail calls introduced"}
  end (* structure Click *)
                   
  datatype arg = 
           VParam of int
         | VFree of M.operand

  type pattern = arg Vector.t
  type target = M.label * pattern

  datatype cont = 
           CReturn of pattern
         | CGoto of target
(*         | CCase of {on : operand,
                     cases : target Vector.t,
                     default : target option}*)


  val layoutArg = 
   fn (imil, v) =>
      (case v
        of VParam i => Layout.seq [Layout.str "Arg_", Int.layout i]
         | VFree oper => Layout.seq [Layout.str "Oper_", 
                                     MilLayout.layoutOperand (T.getConfig imil, T.getSi imil, oper)])

  val printPattern = 
   fn (imil, args) => LayoutUtils.printLayout (Vector.layout (fn arg => layoutArg (imil, arg)) args)

  val printTarget = 
   fn (imil, (l, p)) => 
      (print (Identifier.labelString l);
       printPattern (imil, p))
      
  val printCont = 
   fn (imil, c) =>
      (case c
        of NONE => print "None"
         | SOME c => 
           (case c
             of CReturn p => 
                (print "Return ";
                 printPattern (imil, p))
              | CGoto t => 
                (print "Goto ";printTarget (imil, t))))
      
  val eqArg = 
   fn args => 
      (case args
        of (VParam i, VParam i') => i = i'
         | (VFree oper1, VFree oper2) => MU.Operand.eq (oper1, oper2)
         | (VFree _, VParam _) => false
         | (VParam _, VFree _) => false)

  val eqPattern = 
   fn (p1, p2) => Vector.equals (p1, p2, eqArg)

  val argIsVar = 
      fn (arg, v) => 
         (case arg
           of VFree oper => MU.Operand.eq (M.SVariable v, oper)
            | VParam i => false)

  val instantiate1 = 
   fn (p, outargs) =>
      (case p
        of VParam i => 
           (case Utils.Vector.lookup (outargs, i)
             of SOME oper => oper
              | NONE => fail ("instantiate1", "Mismatch between transfer/label parameter count"))
         | VFree oper => oper)

  val instantiate = 
   fn (pattern, outargs) => Vector.map (pattern, fn p => instantiate1 (p, outargs))

  val generalize1 = 
   fn (parameters, outarg) =>
      (case outarg
        of M.SConstant c => VFree outarg
         | M.SVariable v => 
           (case Vector.index (parameters, fn v' => v = v')
             of SOME i => VParam i
              | _ => VFree outarg))
      
  val generalize = 
   fn (parameters, outargs) => Vector.map (outargs, fn outarg => generalize1 (parameters, outarg))

  (* Given a block b, compute a continuation for it if it is a passthru.  
   * That is, if the block is of the form:
   *   L(x1, x2)
   *   Transfer[x1, x2]
   *
   * Compute a continuation approximating Transfer[x1, x2] if one exists.
   * The pattern for the continuation will use VParam 0 for x1 and VParam 1 for x2.
   *)
  val shortcut = 
      Try.lift 
        (fn (d, imil, b)=> 
            let
              val () = Try.require (IBlock.isEmpty (imil, b))
              val (l, parms) =  <@ IInstr.toLabel o IBlock.getLabel @@ (imil, b)
              val itfer = IBlock.getTransfer (imil, b)
              val tfer = <@ IInstr.toTransfer itfer 

              (* Every parameter must either be unused, or used only in
               * the transfer.
               *)
              val paramIsPassThru = 
               fn v =>
                  let
                    val uses = Use.getUses (imil, v)
                    val () = 
                        case Vector.length uses
                         of 0 => ()
                          | 1 => 
                            let
                              val iuse = <@ Use.toIInstr (Vector.sub (uses, 0))
                            in Try.require (iuse = itfer)
                            end
                          | _ => Try.fail()
                  in ()
                  end
              val () = Vector.foreach (parms, paramIsPassThru)

              val res = 
                  (case tfer
                    of M.TGoto (M.T {block, arguments}) => 
                       let
                         val () = Try.require (block <> l)
                         val pattern = generalize (parms, arguments)
                         val sc = CGoto (block, pattern)
                       in sc
                       end
                     | M.TCase _ => Try.fail ()
                     | M.TInterProc _ => Try.fail ()
                     | M.TReturn arguments => 
                       let 
                         val pattern = generalize (parms, arguments)
                         val sc = CReturn pattern
                       in sc
                       end 
                     | M.TCut _ => Try.fail ()
                     | M.THalt _ => Try.fail ()
                     | M.TPSumCase _ => Try.fail ())
            in res
            end)

  val rewrite =
   fn ((d, imil, ws, seen), b) =>
      Try.exec
        (fn () => 
            let
              val itfer = IBlock.getTransfer (imil, b)
              val tfer = <@ IInstr.toTransfer itfer
                  
              val doSwitch = 
               fn (construct, {on, cases, default}) =>
                  let
                    val changed = ref false
                    val doTarget = 
                     fn (t as (M.T {block, arguments})) =>
                        (case ILD.lookup (seen, block)
                          of SOME contO => 
                             (case contO
                               of SOME (CGoto (l, p)) => 
                                  if (block <> l) then
                                    let
                                      val () = changed := true
                                      val args = instantiate (p, arguments)
                                      val t = M.T {block = l,
                                                   arguments = args}
                                      val () = Click.switchTarget d
                                    in t
                                    end
                                  else t
                                | _=> t)
                           | NONE => t)
                    val cases = Vector.map (cases, fn (a, tg) => (a, doTarget tg))
                    val default = Option.map (default, doTarget)
                    val () = Try.require (!changed)
                    val t = construct {on = on, cases = cases, default = default}
                    val () = IInstr.replaceTransfer (imil, itfer, t)
                    val () = WS.addInstr (ws, itfer)
                  in ()
                  end

              val () = 
                  (case tfer
                    of M.TGoto (M.T {block, arguments}) => 
                       let
                         val contO = <@ ILD.lookup (seen, block)
                         val cont = <- contO
                         val t = 
                             (case cont
                               of CGoto (l, p) => 
                                  let
                                    val () = Try.require (l <> block)
                                    val outargs = instantiate (p, arguments)
                                    val tg = M.T {block = l,
                                                  arguments = outargs}
                                    val t = M.TGoto tg
                                    val () = Click.gotoShortcut d
                                  in t
                                  end
                                | CReturn p => 
                                  let
                                    val outargs = instantiate (p, arguments)
                                    val t = M.TReturn outargs
                                    val () = Click.returnShortcut d
                                  in t
                                  end)
                         val () = IInstr.replaceTransfer (imil, itfer, t)
                         val () = WS.addInstr (ws, itfer)
                       in ()
                       end
                     | M.TCase sw => doSwitch (M.TCase, sw)
                     | M.TInterProc {callee, ret, fx} => 
                       let
                         val {rets, block, cuts} = <@ MU.Return.Dec.rNormal ret
                         val contO = <@ ILD.lookup (seen, block)
                         val cont = <- contO
                         val ret = 
                             (case cont
                               of CGoto (l, p) => 
                                  let
                                    val () = Try.require (Vector.isEmpty p)
                                    val ret = M.RNormal {rets = rets, block = l, cuts = cuts}
                                    val () = Click.iPShortcut d
                                  in ret
                                  end
                                | CReturn p => 
                                  let
                                    val () = Try.require (Vector.length p = Vector.length rets)
                                    val () = Try.require (Vector.forall2 (p, rets, argIsVar))
                                    val () = Try.require (LS.isEmpty (MU.Cuts.targets cuts))
                                    val () =
                                        case callee
                                         of M.IpCall {call = M.CCode {ptr, ...}, ...} =>
                                            (* We shouldn't tail call unmanaged code,
                                             * probably could tighten this code
                                             *)
                                            Try.require (IM.Var.kind (imil, ptr) <> M.VkExtern)
                                          | _ => ()
                                    val ret = M.RTail {exits = MU.Cuts.exits cuts}
                                    val () = Click.tailCall d
                                  in ret
                                  end)
                         val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                         val () = IInstr.replaceTransfer (imil, itfer, t)
                         val () = WS.addInstr (ws, itfer)
                       in ()
                       end
                     | M.TReturn args => Try.fail ()
                     | M.TCut _ => Try.fail ()
                     | M.THalt _ => Try.fail ()
                     | M.TPSumCase sw => doSwitch (M.TPSumCase, sw))
            in ()
            end)

  val collapse = 
   fn ((d, imil, ws, seen), b) =>
      Try.exec
        (fn () => 
            let
              val bTransfer = IBlock.getTransfer (imil, b)
              val doSwitch = 
               fn {eq, con, dec, sw} => 
                  let
                    val {on, cases, default} = sw
                    val {block, arguments} = MU.Target.Dec.t (<- default)
                    val () = Try.V.isEmpty arguments
                    val iFunc = IInstr.getIFunc (imil, bTransfer)
                    (* Ensure that the fall thru block is empty, and has a single in edge. *)
                    val fallthruBlock = IFunc.getBlockByLabel (imil, iFunc, block)
                    (* Make sure we're not looping to ourselves *)
                    val () = Try.require (fallthruBlock <> b)
                    val params = IBlock.getParameters (imil, fallthruBlock)
                    val () = Try.V.isEmpty params
                    val () = Try.require (IBlock.isEmpty (imil, fallthruBlock))
                    val () = case IBlock.inEdges (imil, fallthruBlock)
                              of [_] => ()
                               | _ => Try.fail ()
                    val ftTransfer = IBlock.getTransfer (imil, fallthruBlock)
                    val {on = on2, cases = cases2, default = default2} = <@ dec <! IInstr.toTransfer @@ ftTransfer
                    val () = Try.require (MU.Operand.eq (on, on2))
                    val check = fn x => (fn (y, _) => not (eq (x, y)))
                    val notAnArmInFirst = 
                     fn (x, _) => Vector.forall (cases, check x)
                    val cases2 = Vector.keepAll (cases2, notAnArmInFirst)
                    val cases = Vector.concat [cases, cases2]
                    val t = con {on = on, cases = cases, default = default2}
                    val () = IInstr.replaceTransfer (imil, bTransfer, t)
                    (* Kill the fall thru block to prevent temporary quadratic blowup *)
                    val t2 = M.TGoto (M.T {block = block, arguments = Vector.new0()})
                    val () = IInstr.replaceTransfer (imil, ftTransfer, t2)
                    val () = WS.addInstr (ws, bTransfer)
                    val () = WS.addInstr (ws, ftTransfer)
                    val () = Click.collapseSwitch d
                  in ()
                  end
              val () = 
                  (case <@ IInstr.toTransfer bTransfer
                    of M.TCase sw     => doSwitch {eq = MU.Constant.eq, con = M.TCase, 
                                                   dec = MU.Transfer.Dec.tCase, sw = sw}
                     | M.TPSumCase sw => doSwitch {eq = op =, con = M.TPSumCase, 
                                                   dec = MU.Transfer.Dec.tPSumCase, sw = sw}
                     | _ => Try.fail ())
            in ()
            end)

  val rec transform = 
   fn (d, imil, ws, seen, b) =>
      let
        val () = 
            (case IInstr.toLabel o IBlock.getLabel @@ (imil, b)
              of SOME (l, parms) =>
                 (case ILD.lookup (seen, l)
                   of SOME sc => ()
                    | NONE => 
                      let
                        val () = ILD.insert (seen, l, NONE)
                        val succs = IBlock.succs (imil, b)
                        val () = List.foreach (succs, fn t => transform (d, imil, ws, seen, t))
(*                        val () = 
                          (print "Block ";
                           print (Identifier.labelString l);
                           print "got shortcuts ";
                           List.foreach (conts, fn c => (printCont c;print ", "));
                           print "\n";
                           print "And got shortcut ";
                           printCont cont;
                           print "\n")
 *)
                        val () = rewrite ((d, imil, ws, seen), b)
                        val () = collapse ((d, imil, ws, seen), b)
                        val contO = shortcut (d, imil, b)
                        val () = ILD.insert (seen, l, contO)
                      in ()
                      end)
               | NONE => ())
      in ()
      end
      
  val cleanIFunc = 
   fn (d, p, ws, iFunc) =>
      let
        val start = IFunc.getStart (p, iFunc)
        val () = transform (d, p, ws, ILD.empty (), start)
      in ()
      end
      
  val cleanIFuncs = 
   fn (d, p, ws) =>
      let
        val iFuncs = IFunc.getIFuncs p
        val () = List.foreach (iFuncs, fn (_, iFunc) => cleanIFunc(d, p, ws, iFunc))
      in ()
      end

  val function' = 
   fn (d, p, ws, iFunc) => cleanIFunc (d, p, ws, iFunc)

  val program' =
   fn (d, p, ws) => cleanIFuncs (d, p, ws)

  val function = 
   fn (d, p, iFunc) => function' (d, p, WS.new (), iFunc)

  val program =
   fn (p, d) => program' (d, p, WS.new())

  val stats = Click.stats
end;

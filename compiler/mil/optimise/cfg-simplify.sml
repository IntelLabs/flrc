(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_CFG_SIMPLIFY =
sig
  val program'  : PassData.t * IMil.t * IMil.WorkSet.ws -> unit
  val function' : PassData.t * IMil.t * IMil.WorkSet.ws * IMil.iFunc -> unit
  val function  : PassData.t * IMil.t * IMil.iFunc -> unit
  val stats : (string * string) list
  val debugs : Config.Debug.debug list
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
  structure L = Layout
  structure LU = LayoutUtils

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

  val (showD, show) =
      Config.Debug.mk (passname ^ ":show", "Show shortcuts")

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
    val {stats, click = switchReduce} = PD.clicker {stats = stats, passname = passname,
                                                    name = "SwitchReduce", desc =   "Switch reductions"}
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
         | CCase of {select : M.selector, on : arg, cases : (M.constant * target) Vector.t, default : target option}

  val layoutArg = 
   fn (imil, v) =>
      (case v
        of VParam i => L.seq [L.str "Arg_", Int.layout i]
         | VFree oper => L.seq [L.str "Oper_", 
                                MilLayout.layoutOperand (T.getConfig imil, T.getSi imil, oper)])

  val layoutPattern = 
   fn (imil, args) => Vector.layout (fn arg => layoutArg (imil, arg)) args

  val layoutTarget = 
   fn (imil, (l, p)) => L.seq [L.str (Identifier.labelString l), 
                               layoutPattern (imil, p)]
      
  val layoutSwitch = 
   fn (imil, {select, on, cases, default}) => 
      let
        val on = 
            case select 
             of M.SeSum _    => L.seq [L.str "tagof", LU.paren (layoutArg (imil, on))]
              | M.SeConstant => layoutArg (imil, on)
        val doCase = fn (a, t) => L.seq [MilLayout.layoutConstant (T.getConfig imil, T.getSi imil, a), 
                                         L.str " => ", layoutTarget (imil, t)]
        val cases = Vector.toListMap (cases, doCase)
        val default = Option.layout (fn t => layoutTarget (imil, t)) default
        val header = L.seq [L.str "case ", on, L.str " of "]
        val l = L.seq [L.str "Case ", L.align [header, LU.indent (L.mayAlign (cases @ [default]))]]
      in l
      end

  val layoutCont = 
   fn (imil, c) =>
      (case c
        of NONE => L.str "None"
         | SOME c => 
           (case c
             of CReturn p   => L.seq [L.str "Return ", layoutPattern (imil, p)]
              | CGoto t     => L.seq [L.str "Goto ", layoutTarget (imil, t)]
              | CCase sw    => layoutSwitch (imil, sw)
           )
      )

  val printCont = LU.printLayout o layoutCont

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

              val doTarget = 
               fn (M.T {block, arguments}) => 
                  let
                    val () = Try.require (block <> l)
                    val pattern = generalize (parms, arguments)
                    val t = (block, pattern)
                  in t
                  end
              val doSwitch = 
               fn {select, on, cases, default} => 
                  let
                    val on = generalize1 (parms, on)
                    val cases = Vector.map (cases, (fn (d, t) => (d, doTarget t)))
                    val default = Option.map (default, doTarget)
                  in {select = select, on = on, cases = cases, default = default}
                  end
              val res = 
                  case tfer
                   of M.TGoto t           => CGoto (doTarget t)
                    | M.TCase sw          => CCase (doSwitch sw)
                    | M.TInterProc _      => Try.fail ()
                    | M.TReturn arguments => 
                      let 
                        val pattern = generalize (parms, arguments)
                        val sc = CReturn pattern
                      in sc
                      end 
                    | M.TCut _            => Try.fail ()
                    | M.THalt _           => Try.fail ()
            in res
            end)


  (* Given a switch which is targeted by branch (block, arguments),
   * attempt to use arguments to shorcut the switch and replace the branch.
   * .e.g
   *  Goto L1(True)
   * L1(b)
   *   case b of True => L2() | False => L3()
   * rewrites to
   *  Goto L2()
   * 
   *)
  val rewriteGotoOfCCase =
   fn ((d, imil, ws, seen), M.T {block, arguments}, {select, on, cases, default}) => 
      Try.try
        (fn () =>
            let
              val on = instantiate1 (on, arguments)
              val c = 
                  case select
                   of M.SeSum fk => 
                      let
                        val v = <@ MU.Operand.Dec.sVariable on
                        val tag = #tag <! MU.Def.Out.sum <! IMil.Def.toMilDef o IMil.Def.get @@ (imil, v)
                      in tag
                      end
                    | M.SeConstant  => <@ MU.Operand.Dec.sConstant on
              val (l, p) = 
                  case Vector.peek (cases, fn (c', t) => MU.Constant.eq (c, c'))
                   of SOME (_, t) => t
                    | NONE        => <- default
              val () = Try.require (block <> l)
              val arguments = instantiate (p, arguments)
              val () = Click.switchReduce d
            in M.T {block = l, arguments = arguments}
            end)

  val rewriteGoto = 
   fn (s as (d, imil, ws, seen), t as M.T {block, arguments}) => 
      Try.try
        (fn () => 
            let
              val contO = <@ ILD.lookup (seen, block)
              val cont = <- contO
              val t = 
                  case cont
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
                      end
                    | CCase sw    => M.TGoto (<@ rewriteGotoOfCCase (s, t, sw))
            in t
            end)
            
  val rewriteTCase = 
   fn (s as (d, imil, ws, seen), {select, on, cases, default}) =>
      Try.try
        (fn () =>
            let
              val changed = ref false
              val doTarget = 
               fn (t as (M.T {block, arguments})) =>
                  Try.try 
                    (fn () => 
                        let
                          val contO = <@ ILD.lookup (seen, block)
                          val cont = <- contO
                          val t = 
                              case cont
                               of CGoto (l, p) => 
                                  let
                                    val () = Try.require (block <> l)
                                    val () = changed := true
                                    val args = instantiate (p, arguments)
                                    val t = M.T {block = l,
                                                 arguments = args}
                                    val () = Click.switchTarget d
                                  in t
                                  end
                                | CReturn _ => Try.fail ()
                                | CCase sw => 
                                  let
                                    val t = <@ rewriteGotoOfCCase (s, t, sw)
                                    val () = changed := true
                                  in t
                                  end
                        in t
                        end)
              val doCase = 
               fn (a, tg) => case doTarget tg
                              of SOME tg => (a, tg)
                               | NONE    => (a, tg)
              val cases = Vector.map (cases, doCase)
              val default = Option.map (default, fn tg => Utils.Option.get (doTarget tg, tg))
              val () = Try.require (!changed)
              val t = M.TCase {select = select, on = on, cases = cases, default = default}
            in t
            end)


  val rewriteTInterProc = 
   fn ((d, imil, ws, seen), {callee, ret, fx}) => 
      Try.try
        (fn () => 
            let
              val {rets, block, cuts} = <@ MU.Return.Dec.rNormal ret
              val contO = <@ ILD.lookup (seen, block)
              val cont = <- contO
              val ret = 
                  case cont
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
                      end
                    | _ => Try.fail ()
              val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
            in t
            end)

  val rewrite =
   fn (s as (d, imil, ws, seen), b) =>
      Try.exec
        (fn () => 
            let
              val itfer = IBlock.getTransfer (imil, b)
              val tfer = <@ IInstr.toTransfer itfer
              val t = 
                  case tfer
                   of M.TGoto tg     => <@ rewriteGoto (s, tg)
                    | M.TCase sw     => <@ rewriteTCase (s, sw)
                    | M.TInterProc r => <@ rewriteTInterProc (s, r)
                    | M.TReturn args => Try.fail ()
                    | M.TCut _       => Try.fail ()
                    | M.THalt _      => Try.fail ()
              val () = IInstr.replaceTransfer (imil, itfer, t)
              val () = WS.addInstr (ws, itfer)
            in ()
            end)

  (* Look for cascaded cases and collapse them into single case statements. e.g.
   *  case x of 0 => L1() | _ => L2()
   * L2() 
   *  case x of 1 => L3 () | ...
   * becomes
   * case x of 0 => L1 () | 1 => L3 () | .... 
   *)
  val collapse = 
   fn ((d, imil, ws, seen), b) =>
      Try.exec
        (fn () => 
            let
              val bTransfer = IBlock.getTransfer (imil, b)
              val doSwitch = 
               fn {select, on, cases, default} => 
                  let
                    (* A switch with a default with no outargs *)
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
                    val {select = select2, on = on2, cases = cases2, default = default2} = 
                        <@ MU.Transfer.Dec.tCase <! IInstr.toTransfer @@ ftTransfer
                    val () = Try.require (MU.Operand.eq (on, on2))
                    val () = Try.require (select = select2)
                    val check = fn x => (fn (y, _) => not (MU.Constant.eq (x, y)))
                    val notAnArmInFirst = 
                     fn (x, _) => Vector.forall (cases, check x)
                    val cases2 = Vector.keepAll (cases2, notAnArmInFirst)
                    val cases = Vector.concat [cases, cases2]
                    val t = M.TCase {select = select, on = on, cases = cases, default = default2}
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
                  case <@ IInstr.toTransfer bTransfer
                   of M.TCase sw     => doSwitch sw
                    | _ => Try.fail ()
            in ()
            end)

  val showShortcut = 
   fn (imil, l, contO) =>
      let
        val l = L.seq [L.str "Block ", L.str (Identifier.labelString l), 
                       L.str " has shortcut ", layoutCont (imil, contO)]
      in LU.printLayout l
      end

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
                        val () = rewrite ((d, imil, ws, seen), b)
                        val () = collapse ((d, imil, ws, seen), b)
                        val contO = shortcut (d, imil, b)
                        val () = ILD.insert (seen, l, contO)
                        val () = if show (T.getConfig imil) then showShortcut (imil, l, contO) else ()
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
  val debugs = [showD]
end;

(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, September 2007 *)

signature MIL_CFG_SIMPLIFY =
sig
  val program'  : PassData.t * IMil.t * IMil.WorkSet.ws -> unit
  val function' : PassData.t * IMil.t * IMil.WorkSet.ws * IMil.cfg -> unit
  val function  : PassData.t * IMil.t * IMil.cfg -> unit
end;

structure MilCFGSimplify :> MIL_CFG_SIMPLIFY =
struct

  structure PD = PassData
  structure LS = Identifier.LabelSet
  structure ILD = Identifier.ImpLabelDict
  structure M = Mil
  structure IM = IMil
  structure WS = IM.WorkSet
  structure Block = IM.Block
  structure Use = IM.Use
  structure IInstr = IM.Instr
  structure IFunc = IM.IFunc

  val passname = "MilCfgSimplify"

  structure Chat = ChatF(type env = PD.t
                         val extract = PD.getConfig
                         val name = passname
                         val indent = 0)
                   
  datatype Val = 
           VArg of int
         | VVar of M.variable
         | VConst of M.operand
  datatype PList = 
           PassThru
         | Permute of Val Vector.t

  datatype Cont = 
           CNone
         | CReturn of PList
         | CGoto of M.label * PList
  (* Could back propogate switches as well - needs thought *)

  fun layoutVal v = 
      (case v
        of VArg i => Layout.seq [Layout.str "Arg_", Int.layout i]
         | VVar v => Layout.seq [Layout.str "Arg_", Identifier.layoutVariable' v]
         | VConst (M.SVariable v) => Layout.seq [Layout.str "Oper_", Identifier.layoutVariable' v]
         | VConst _ => Layout.seq [Layout.str "Oper_", Layout.str "const"])

  fun printVals vs = 
      LayoutUtils.printLayout (Vector.layout layoutVal vs)

  fun printPList p = 
      (case p
        of PassThru => print "PassThru"
         | Permute vs => (print "Perm";
                          printVals vs))

  fun printCont c = 
      (case c
        of CNone => print "None"
         | CReturn p => 
           (print "Return ";
            printPList p)
         | CGoto (l, p) => 
           (print "Goto ";
            print (Identifier.labelString l);
            printPList p))

  fun shortcut (d, imil, b) = 
      Try.try 
        (fn () => 
            let
              val () = Try.require (Block.isEmpty (imil, b))
              val (l, parms) = 
                  Try.<- (MOU.iinstrToLabel (imil, Block.getLabel (imil, b)))
              val itfer = Block.getTransfer (imil, b)
              val tfer = Try.<- (MOU.iinstrToTransfer (imil, itfer))

              (* Every parameter must either be unused, or used only in
               * the transfer.
               *)
              fun parmIsPassThru v = 
                  let
                    val uses = Use.getUses (imil, v)
                    val () = 
                        case Vector.length uses
                         of 0 => ()
                          | 1 => 
                            let
                              val iuse = 
                                  Try.<- (MOU.useToIInstr 
                                            (imil, Vector.sub (uses, 0)))
                            in Try.require (iuse = itfer)
                            end
                          | _ => Try.fail()
                  in ()
                  end
                  
              fun varToVal v = 
                  (case Vector.index (parms, fn v' => v = v')
                    of SOME i => VArg i
                     | _ => VVar v)

              fun argToVal a = 
                  (case a
                    of M.SConstant c => VConst a
                     | M.SVariable v => 
                       if M.variableIsGlobal' (IMil.getSTM imil, v) then
                         VConst (M.SVariable v)
                       else varToVal v
                     | _ => Try.fail ())
                  
              fun argsToVals args = Vector.map (args, argToVal)

              fun plist args = 
                  let
                    val vals = argsToVals args
                    val eqLen = Vector.length vals = Vector.length parms
                    val eqVArgI = 
                     fn (i, v) => 
                        (case v
                          of VArg i' => i = i'
                           | _       => false)
                    val eq = eqLen andalso
                             Vector.foralli (vals, eqVArgI)
                  in if eq then 
                       PassThru
                     else
                       Permute vals
                  end

              val res = 
                  (case tfer
                    of M.TGoto (M.T {block, arguments}) => 
                       let
                         val () = Vector.foreach (parms, parmIsPassThru)
                         val sc = CGoto (block, plist arguments)
                       in sc
                       end
                     | M.TReturn args => 
                       CReturn (plist args)
                     | M.TCall (call, args, l, cuts, fx) => 
                       Try.fail ()
                     | M.TTailCall (call, args, fx) => 
                       Try.fail ()
                     | M.TEvalThunk _ => 
                       Try.fail ()
                     | M.TCase sw => 
                       Try.fail ()
                     | M.TPSumCase sw => 
                       Try.fail ()
                     | M.TCut (k, args, cuts) => 
                       Try.fail ()
                     | M.TBulkSpawn _ =>
                       Try.fail ())
                    (*                 val () = (printCont res; print "\n")*)
            in res
            end)


  fun eqVs (imil, vs1, vs2) = 
      let
        fun eqV (v1, v2) = 
            (case (v1, v2)
              of (VArg i, VArg i') => i = i'
               | (VVar v, VVar v') => v = v'
               | (VConst c1, VConst c2) => MOU.eqOperand (imil, c1, c2)
               | _ => false)
        val eq = Vector.equals (vs1, vs2, eqV)
      in eq
      end

  fun eqPList (imil, p1, p2) = 
      (case (p1, p2)
        of (PassThru, PassThru) => true
         | (Permute vs1, Permute vs2) => eqVs (imil, vs1, vs2)
         | _ => false)

  fun summarize (imil, conts) = 
      let
        fun sum (c1, c2) = 
            (case (c1, c2) 
              of (CReturn p1, CReturn p2) => 
                 (if eqPList (imil, p1, p2) then
                    CReturn p1
                  else
                    CNone)
               | (CGoto (l1, p1), CGoto (l2, p2)) => 
                 if (l1 = l2) andalso eqPList (imil, p1, p2) then
                   CGoto (l1, p1)
                 else
                   CNone
               | _ => CNone)
        val c = 
            case conts
             of [] => CNone
              | c::cs => 
                List.fold (cs, c, sum)
      in c
      end

  fun rewrite (d, imil, wl, seen, b, cont) = 
      Try.try 
        (fn () => 
            let
              val itfer = Block.getTransfer (imil, b)
              val tfer = Try.<- (MOU.iinstrToTransfer (imil, itfer))
              fun valToArg (v, args) = 
                  (case v
                    of VArg i => Try.V.sub (args, i)
                     | VVar v => M.SVariable v
                     | VConst oper => oper)

              fun valsToArgs (vs, args) =
                  Vector.map (vs, fn v => valToArg (v, args))

              fun pListToArgs (p, args) = 
                  (case p
                    of PassThru   => args
                     | Permute vs => valsToArgs (vs, args))

              fun isPassThru p = 
                  (case p
                    of PassThru   => true
                     | Permute vs => false)

                  
              fun doSwitch (construct, (arg, arms, default)) = 
                  let
                    val changed = ref false
                    fun doTarget (t as (M.T {block, arguments})) = 
                        (case ILD.lookup (seen, block)
                          of SOME cont => 
                             (case cont
                               of CGoto (l, p) => 
                                  let
                                    val () = changed := true
                                    val args = pListToArgs (p, arguments)
                                    val t = M.T {block = l,
                                                 arguments = args}
                                  in t
                                  end
                                | _=> t)
                           | NONE => t)
                    val arms = Vector.map (arms, 
                                        fn (a, tg) => (a, doTarget tg))
                    val default = Option.map (default, doTarget)
                    val () = 
                        if !changed then
                          let
                            val t = construct (arg, arms, default)
                            val it = IMil.MTransfer t
                            val () = Instr.replaceMil (imil, itfer, it)
                            val () = WS.addInstr (wl, itfer)
                          in ()
                          end
                        else ()
                  in ()
                  end

              val () = 
                  (case tfer
                    of M.TGoto (M.T {block, arguments}) => 
                       (case cont
                         of CGoto (l, p) => 
                            let
                              val () = Try.require (block <> l)
                              val args = pListToArgs (p, arguments)
                              val tg = M.T {block = l,
                                            arguments = args}
                              val t = M.TGoto tg
                              val it = IMil.MTransfer t
                              val () = Instr.replaceMil (imil, itfer, it)
                              val () = WS.addInstr (wl, itfer)
                            in ()
                            end
                          | CReturn p => 
                            let
                              val args = pListToArgs (p, arguments)
                              val t = M.TReturn args
                              val it = IMil.MTransfer t
                              val () = Instr.replaceMil (imil, itfer, it)
                              val () = WS.addInstr (wl, itfer)
                            in ()
                            end
                          | CNone => Try.fail ())

                     | M.TReturn args => Try.fail ()
                     | M.TCall (call, args, l', cuts, fx) => 
                       (case cont
                         of CGoto (l, p) => 
                            let
                              val () = Try.require (isPassThru p)
                              val () = Try.require (l <> l')
                              val t = M.TCall (call, args, l, cuts, fx)
                              val it = IMil.MTransfer t
                              val () = Instr.replaceMil (imil, itfer, it)
                              val () = WS.addInstr (wl, itfer)
                            in ()
                            end
                          | CReturn p => 
                            let
                              val () = Try.require (isPassThru p)
                              val t = M.TTailCall (call, args, fx)
                              val it = IMil.MTransfer t
                              val () = Instr.replaceMil (imil, itfer, it)
                              val () = WS.addInstr (wl, itfer)
                            in ()
                            end
                          | CNone => Try.fail ())
                     | M.TTailCall (call, args, fx) => Try.fail ()
                     | M.TEvalThunk _ => Try.fail ()
                     | M.TCase sw => doSwitch (M.TCase, sw)
                     | M.TPSumCase sw => doSwitch (M.TPSumCase, sw)
                     | M.TCut (k, args, cuts) => Try.fail ()
                     | M.TBulkSpawn _ =>  
                       Try.fail())
            in ()
            end)

  fun transform (d, imil, wl, seen, b) = 
      let
        val cont = 
            (case MOU.iinstrToLabel (imil, Block.getLabel (imil, b))
              of SOME (l, parms) =>
                 (case ILD.lookup (seen, l)
                   of SOME sc => sc
                    | NONE => 
                      let
                        val () = ILD.insert (seen, l, CNone)
                        val succs = Block.succs (imil, b)
                        val conts = 
                            List.map (succs, 
                                   fn t => transform (d, imil, wl, seen, t))
                        val cont = summarize (imil, conts)
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
                        val _ = rewrite (d, imil, wl, seen, b, cont)
                        val cont = Try.otherwise (shortcut (d, imil, b), CNone)
                        val () = ILD.insert (seen, l, cont)
                      in cont
                      end)
               | NONE => CNone)
      in cont
      end
      
  val cleanCfg = 
   fn (d, p, wl, cfg) =>
      let
        val start = Cfg.getStart (p, cfg)
        val scs = transform (d, p, wl, ILD.empty (), start)
      in ()
      end
      
  val cleanCfgs = 
   fn (d, p, wl) =>
      let
        val cfgs = IMil.Cfg.getCfgs p
        val () = List.foreach (cfgs, fn (_, cfg) => cleanCfg(d, p, wl, cfg))
      in ()
      end

  val function' = 
   fn (d, p, wl, cfg) => cleanCfg (d, p, wl, cfg)

  val program' =
   fn (d, p, wl) => cleanCfgs (d, p, wl)

  val function = 
   fn (d, p, cfg) => function' (d, p, WS.new (), cfg)

  val program =
   fn (p, d) => program' (d, p, WS.new())

end;

(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Various optimisations on functions with all known callers *)

signature MIL_FUN_KNOWN =
sig
  val debugs : Config.Debug.debug list
  val stats : (string * string) list
  val flatten : PassData.t 
                * IMil.t 
                * IMil.WorkSet.ws 
                * Mil.variable
                * IMil.cfg
                -> unit Try.t
  val parameters : PassData.t 
                   * IMil.t 
                   * IMil.WorkSet.ws
                   * Mil.variable
                   * IMil.cfg
                   -> unit Try.t
  val return : PassData.t 
               * IMil.t 
               * IMil.WorkSet.ws
               * Mil.variable
               * IMil.cfg
               -> unit Try.t
end;

structure MilFunKnown :> MIL_FUN_KNOWN =
struct

  val passname = "MilFunKnown"

  val stats =
      [("FlattenTupleArg", "Tuple arguments flattened"        ),
       ("DeadParamElim",   "Dead parameters eliminated"       ),
       ("ParamConstProp",  "Constant parameters propagated"   ),
       ("DeadFvElim",      "Dead free variable eliminated"    ),
       ("FvConstProp",     "Constant free variable propagated"),
       ("DeadReturnElim",  "Dead return values eliminated"    ),
       ("ReturnConstProp", "Constant return values propagated")]

  structure I = Identifier
  structure M = Mil
  structure PD = PassData
  structure MOU = MilOptUtils
  structure WS = IMil.WorkSet

  fun callConvExists2 (cc, f) =
      case cc
       of M.CcCode            => false
        | M.CcExtern          => false
        | M.CcClosure (x, xs) => Vector.exists (xs, f)
        | M.CcThunk (x, xs)   => Vector.exists (xs, f)
        | M.CcBulkSpawn       => false

  fun callConvMap (cc, f) =
      case cc
       of M.CcCode            => M.CcCode
        | M.CcExtern          => M.CcExtern
        | M.CcClosure (x, xs) => M.CcClosure (f x, Vector.map (xs, f))
        | M.CcThunk (x, xs)   => M.CcThunk (f x, Vector.map (xs, f))
        | M.CcBulkSpawn       => M.CcBulkSpawn

  fun callConvMap2 (cc, ys, f) =
      case cc
       of M.CcCode            => M.CcCode
        | M.CcExtern          => M.CcExtern
        | M.CcClosure (x, xs) => M.CcClosure (x, Vector.map2 (xs, ys, f))
        | M.CcThunk (x, xs)   => M.CcThunk (x, Vector.map2 (xs, ys, f))
        | M.CcBulkSpawn       => M.CcBulkSpawn

  fun callConvKeepAll (cc1, cc2, f) =
      case (cc1, cc2)
       of (M.CcCode, M.CcCode) => M.CcCode
        | (M.CcExtern, M.CcExtern) => M.CcExtern
        | (M.CcClosure (x1, xs1), M.CcClosure (x2, xs2)) =>
          M.CcClosure (x1, Vector.keepAllMap2 (xs1, xs2, f))
        | (M.CcThunk (x1, xs1), M.CcThunk (x2, xs2)) =>
          M.CcThunk (x1, Vector.keepAllMap2 (xs1, xs2, f))
        | (M.CcBulkSpawn, M.CcBulkSpawn) => M.CcBulkSpawn
        | (_, _) => Fail.fail ("MilFunKnown", "callConvKeepAll", "mismatch")

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname,
                       "debug the Mil known function optimisation pass")

  fun debug (d, f) =
      if Config.debug andalso debugPass (PD.getConfig d) then
        let
          val s = passname ^ ": " ^ f () ^ "\n"
          val () = print s
        in ()
        end
      else ()

  fun updateType (d, imil, f, cc, atyps, rtyps) =
      let
        val t = M.TCode (cc, atyps, rtyps)
        val stm = IMil.getSTM imil
        val () = M.variableSetInfo (stm, f, t, true)
      in ()
      end

  datatype init =
           IRhsFun of IMil.instr * M.variable option *
                      (M.variable * M.variable option * M.operand Vector.t)
         | IRhsThunk of IMil.instr * M.variable option *
                        {thunk    : M.variable, 
                         effects  : M.effects,
                         code     : M.variable,
                         freeVars : M.operand Vector.t}
         | IGlobalFun of IMil.global * M.variable option

  fun rewriteInits (d, imil, f, e, rewrite) =
      let
        val uses = IMil.Use.getUses (imil, f)
        fun fail () =
            if e then ()
            else Fail.fail ("MilFunKnown", "rewriteInits",
                            "bad use of non-escaping function") 
        fun doOne use =
            case use
             of IMil.UseInstr i =>
                (case IMil.Instr.getMil (imil, i)
                  of IMil.MTransfer (M.TCall _) => ()
                   | IMil.MTransfer (M.TTailCall _) => ()
                   | IMil.MTransfer (M.TEvalThunk _) => ()
                   | IMil.MInstr (M.I {rhs, dest}) =>
                     (case rhs
                       of M.RhsPFunctionInit x =>
                          rewrite (IRhsFun (i, dest, x))
                        | M.RhsThunkInit x => rewrite (IRhsThunk (i, dest, x))
                        | _ => fail ())
                   | _ => fail ())
              | IMil.UseGlobal g =>
                (case IMil.Global.getMil (imil, g)
                  of IMil.GGlobal (_, M.GPFunction vo) =>
                     rewrite (IGlobalFun (g, vo))
                   | _ => fail ())
              | _ => fail ()
        val () = Vector.foreach (uses, doOne)
      in ()
      end

  datatype call =
           CCall of M.call * M.operand Vector.t * M.label * M.cuts * M.effects
         | CTailCall of M.call * M.operand Vector.t * M.effects
         | CEval of M.eval * M.label * M.cuts * M.effects

  fun rewriteCallers (d, imil, f, rewrite) =
      let
        val uses = IMil.Use.getUses (imil, f)
        fun fail () = Fail.fail ("MilFunKnown", "rewriteCallers",
                                 "use of non-escaping function not a call") 
        fun doOne use =
            case use
             of IMil.UseInstr i =>
                (case IMil.Instr.getMil (imil, i)
                  of IMil.MTransfer (M.TCall x) => rewrite (i, CCall x)
                   | IMil.MTransfer (M.TTailCall x) => rewrite (i, CTailCall x)
                   | IMil.MTransfer (M.TEvalThunk x) => rewrite (i, CEval x)
                   | IMil.MInstr (M.I {rhs, ...}) =>
                     (case rhs
                       of M.RhsPFunctionInit _ => ()
                        | M.RhsThunkInit _ => ()
                        | _ => fail ())
                   | _ => fail ())
              | IMil.UseGlobal g =>
                (case IMil.Global.getMil (imil, g)
                  of IMil.GGlobal (_, M.GPFunction _) => ()
                   | _ => fail ())
              | _ => fail ()
        val () = Vector.foreach (uses, doOne)
      in ()
      end

  fun flatten (d, imil, wl, f, cfg) =
      Try.try
      (fn () =>
          let
            val () = Try.require (not (IMil.Cfg.getEscapes (imil, cfg)))
            (* Check that there is one arg of tuple type *)
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " checking single arg")
            val args = IMil.Cfg.getArgs (imil, cfg)
            val arg = Try.V.singleton args
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " checking tuple type")
            val ts =
                case M.variableTyp' (IMil.getSTM imil, arg)
                 of M.TTuple (_, ts, NONE) => ts
                  | _ => Try.fail ()
            (* Check that all uses are projections *)
            val arg = Vector.sub (args, 0)
            val uses = IMil.Use.getUses (imil, arg)
            fun checkOne use =
                case use
                 of IMil.UseInstr i =>
                    (case IMil.Instr.getMil (imil, i)
                      of IMil.MInstr (M.I {rhs = M.RhsTupleSub (_, off, idxo),
                                           dest}) =>
                         let
                           val off =
                               case idxo
                                of NONE => off
                                 | SOME (M.SConstant (M.CIntegral idx)) =>
                                   let
                                     val t = IntArb.T (IntArb.S32,
                                                       IntArb.Unsigned)
                                     val () =
                                         Try.require (IntArb.isTyp (idx, t))
                                   in
                                     off + (IntInf.toInt (IntArb.toIntInf idx))
                                   end
                                 | _ => Try.fail ()
                           val _ = Try.V.sub (ts, off)
                         in (i, dest, off)
                         end
                       | _ => Try.fail ())
                  | _ => Try.fail ()
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " checking uses are projections")
            val prjs = Vector.map (uses, checkOne)
            (*** Do optimisation ***)
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   "[" ^ Int.toString (Vector.length ts) ^ "]")
            val () = PD.click (d, "FlattenTupleArg")
            fun doOne v = M.variableTyp' (IMil.getSTM imil, v)
            val cct = callConvMap (IMil.Cfg.getCallConv (imil, cfg), doOne)
            val rtyps = IMil.Cfg.getRtyps (imil, cfg)
            val () = updateType (d, imil, f, cct, ts, rtyps)
            (* Make new argument variables *)
            fun doOne t = IMil.newVar (imil, "mfa", t, false)
            val nargs = Vector.map (ts, doOne)
            val () = IMil.Cfg.setArgs (imil, cfg, nargs)
            (* Rewrite projections *)
            fun rewriteUse (i, dest, off) =
                let
                  val v = Vector.sub (nargs, off)
                  val mi = M.I {dest = dest, rhs = M.RhsSimple (M.SVariable v)}
                  val () = IMil.Instr.replaceInstruction (imil, i, mi)
                  val () = WS.addInstr (wl, i)
                in ()
                end
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " rewriting projections")
            val () = Vector.foreach (prjs, rewriteUse)
            (* Rewrite callers *)
            fun rewriteCaller (i, c) =
                let
                  fun fail () = Fail.fail ("MilFunKnown",
                                           "flatten.rewriteCaller",
                                           "not a call or not of variable")
                  val params =
                      case c
                       of CCall (_, os, _, _, _) => os
                        | CTailCall (_, os, _) => os
                        | CEval _ => fail ()
                  val param =
                      case Vector.sub (params, 0)
                       of M.SVariable v => v
                        | _ => fail ()
                  fun genOne t = IMil.newVar (imil, "mfa", t, false)
                  val pvs = Vector.map (ts, genOne)
                  fun prjOne (j, pv) =
                      let
                        val rhs = M.RhsTupleSub (param, j, NONE)
                        val mi = M.I {dest = SOME pv, rhs = rhs}
                        val i = IMil.Instr.insertBefore (imil, mi, i)
                        val () = WS.addInstr (wl, i)
                      in ()
                      end
                  val () = Vector.foreachi (pvs, prjOne)
                  val os = Vector.map (pvs, M.SVariable)
                  val t =
                      case c
                       of CCall (c, _, ret, cuts, fx) =>
                          M.TCall (c, os, ret, cuts, fx)
                        | CTailCall (c, _, fx) => M.TTailCall (c, os, fx)
                        | CEval _ => fail ()
                  val mi = IMil.MTransfer t
                  val () = IMil.Instr.replaceMil (imil, i, mi)
                  val () = WS.addInstr (wl, i)
                in ()
                end
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " rewriting callers")
            val () = rewriteCallers (d, imil, f, rewriteCaller)
            val () = WS.addItem (wl, IMil.ICode cfg)
            (* Done *)
            val () =
                debug (d, fn () => "flatten tuple: " ^ I.variableString' f ^
                                   " done")
          in ()
          end)

  datatype parameterInfo =
           PiUnknown | PiDead | PiConstant of Mil.simple | PiKeep

  fun layoutParameter pi =
      case pi
       of PiUnknown    => "U"
        | PiDead       => "D"
        | PiConstant _ => "C"
        | PiKeep       => "K"

  fun layoutParameters (cc, pis) =
      let
        val l1 =
            case cc
             of M.CcClosure (_, pis) =>
                ["["] @ (Vector.toListMap (pis, layoutParameter)) @ ["]"]
              | M.CcThunk (_, pis) =>
                ["["] @ (Vector.toListMap (pis, layoutParameter)) @ ["]"]
              | _ => []
        val l2 = Vector.toListMap (pis, layoutParameter)
        val s = String.concat (l1 @ l2)
      in s
      end

  fun useIsNotClosureProjection (d, imil, c, u) = 
      let
        val cOp = M.SVariable c
        fun isClosOp oper = MOU.eqOperand (imil, oper, cOp)
        fun cNotIn ops = Vector.forall (ops, not o isClosOp)

        fun doTransfer t = 
            case t
             of M.TCall (M.CDirectClosure _, args, _, _, _)  => cNotIn args
              | M.TTailCall (M.CDirectClosure _, args, _)    => cNotIn args
              | M.TEvalThunk (M.EDirectThunk _, _, _, _)     => true
              | _ => false

        fun doRhs rhs = 
            case rhs
             of M.RhsPFunctionInit (clos, _, fvs) => cNotIn fvs
              | M.RhsThunkInit {thunk, freeVars, ...} => cNotIn freeVars
              | _ => false

        fun doIInstr i =
            case IMil.Instr.getMil (imil, i)
             of IMil.MInstr (M.I {rhs, ...}) => doRhs rhs
              | IMil.MTransfer t => doTransfer t
              | IMil.MLabel _ => false
              | IMil.MDead => false

        val res = 
            (case u
              of IMil.UseGlobal g => false
               | IMil.UseInstr i => doIInstr i
               | IMil.Used => false)
      in res
      end 

  fun closureIsNotProjected (d, imil, c) = 
      let
        val uses = IMil.Use.getUses (imil, c)
        val res = 
            Vector.forall (uses, 
                        fn u => useIsNotClosureProjection (d, imil, c, u))
      in res
      end

  fun closureArgIsNotProjected (d, imil, cc) =
      case cc
       of M.CcCode            => true
        | M.CcExtern          => true
        | M.CcClosure (c, _)  => closureIsNotProjected (d, imil, c)
        | M.CcThunk (c, _)    => closureIsNotProjected (d, imil, c)
        | M.CcBulkSpawn       => true

  fun parameters (d, imil, wl, f, cfg) = Try.try (fn () => 
      let
        (*** Analyse the parameters ***)
        val cc = IMil.Cfg.getCallConv (imil, cfg)
        val args = IMil.Cfg.getArgs (imil, cfg)
        (* Initial analysis based on the parameters' uses *)
        fun init1 v =
            if Vector.length (IMil.Use.getUses (imil, v)) = 0 then PiDead
            else PiUnknown
        val e = IMil.Cfg.getEscapes (imil, cfg)
        val p = closureArgIsNotProjected (d, imil, cc)
        val keep = fn _ => PiKeep
        val init2 = if e then keep else init1
        val init3 = if p then init1 else keep
        val a = (callConvMap (cc, init3), Vector.map (args, init2))
        val uses = IMil.Use.getUses (imil, f)
        (* Analyse callers *)
        fun fail a =
            if e then a
            else Fail.fail ("MilFunKnown", "parameters",
                            "use of non-escaping function not a call")
        fun doParm (pi, opnd) =
            case (pi, opnd)
             of (PiUnknown, M.SConstant _) => PiConstant opnd
              | (PiUnknown, M.SVariable v) =>
                if M.variableIsGlobal' (IMil.getSTM imil, v) then
                  PiConstant opnd
                else
                  PiKeep
              | (PiUnknown, _) => PiKeep
              | (PiDead, _) => pi
              | (PiConstant s, _) =>
                if MilCmp.simple (s, opnd) = EQUAL then pi else PiKeep
              | (PiKeep, _) => pi
        fun doArgs ((cc, pis), os) = (cc, Vector.map2 (pis, os, doParm))
        fun doFvs ((cc, pis), os) = (callConvMap2 (cc, os, doParm), pis)
        fun keepFvs (cc, pis) = (callConvMap (cc, keep), pis)
        fun doOne (use, a as (cc, pis)) =
            case use
             of IMil.UseInstr i =>
                (case IMil.Instr.getMil (imil, i)
                  of IMil.MTransfer (t as M.TCall (_, os, _, _, _)) =>
                     doArgs (a, os)
                   | IMil.MTransfer (t as M.TTailCall (_, os, _)) =>
                     doArgs (a, os)
                   | IMil.MTransfer (t as M.TEvalThunk _) =>
                     doArgs (a, Vector.new0 ())
                   | IMil.MInstr (M.I {rhs, ...}) =>
                     (case rhs
                       of M.RhsPFunctionInit (c, _, fvs) => 
                          if closureIsNotProjected (d, imil, c) then
                            doFvs (a, fvs)
                          else
                            keepFvs a
                        | M.RhsThunkInit {freeVars, thunk, ...} =>
                          if closureIsNotProjected (d, imil, thunk) then 
                            doFvs (a, freeVars)
                          else
                            keepFvs a
                        | _ => fail a)
                   | _ => fail a)
              | IMil.UseGlobal g =>
                (case IMil.Global.getMil (imil, g)
                  of IMil.GGlobal (_, M.GPFunction _) => a
                   | _ => fail a)
              | _ => fail a
        val () =
            debug (d, fn () => "parameters: " ^ I.variableString' f ^ ": " ^
                               layoutParameters a)
        val a as (cca, argsa) = Vector.fold (uses, a, doOne)
        val () =
            debug (d, fn () => "parameters: " ^ I.variableString' f ^ ": " ^
                               layoutParameters a)
        val () = 
            let
              val help = 
                  fn p => 
                     case p 
                      of PiUnknown => false
                       | PiKeep => false
                       | PiDead => true
                       | PiConstant _ => true
            in Try.require (callConvExists2 (cca, help) orelse
                            Vector.exists (argsa, help))
            end

        (*** Transform ***)
        (* Transform the parameters *)
        fun doParm fv (v, pi) =
            case pi
             of PiUnknown => (* Treat it like keep *) SOME v
              | PiDead =>
                let
                  val stat = if fv then "DeadFvElim" else "DeadParamElim"
                  val () = PD.click (d, stat)
                in NONE
                end
              | PiConstant c =>
                let
                  val stat = if fv then "FvConstProp" else "ParamConstProp"
                  val () = PD.click (d, stat)
                  val () = WS.addUses (wl, IMil.Use.getUses (imil, v))
                  val () = IMil.Use.replaceUses (imil, v, c)
                in NONE
                end
              | PiKeep => SOME v
        val cc = callConvKeepAll (cc, cca, doParm true)
        val () = IMil.Cfg.setCallConv (imil, cfg, cc)
        val args = Vector.keepAllMap2 (args, argsa, doParm false)
        val () = IMil.Cfg.setArgs (imil, cfg, args)
        (* Update type *)
        fun doOne v = M.variableTyp' (IMil.getSTM imil, v)
        val cct = callConvMap (cc, doOne)
        val argts = Vector.map (args, doOne)
        val rtyps = IMil.Cfg.getRtyps (imil, cfg)
        val () = updateType (d, imil, f, cct, argts, rtyps)
        (* Transform the calls *)
        fun doParm (opnd, pi) =
            case pi
             of PiUnknown => (* Treat it like keep *) SOME opnd
              | PiDead => NONE
              | PiConstant c => NONE
              | PiKeep => SOME opnd
        fun rewriteCaller (i, c) =
            let
              fun doArgs os = Vector.keepAllMap2 (os, argsa, doParm)
              val t =
                  case c
                   of CCall (c, os, ret, cuts, fx) =>
                      M.TCall (c, doArgs os, ret, cuts, fx)
                    | CTailCall (c, os, fx) =>
                      M.TTailCall (c, doArgs os, fx)
                    | CEval (e, ret, cuts, fx) =>
                      M.TEvalThunk (e, ret, cuts, fx)
              val mi = IMil.MTransfer t
              val usedBy = IMil.Instr.getUsedBy (imil, i)
              val () = IMil.Instr.replaceMil (imil, i, mi)
              val () = WS.addInstr (wl, i)
              val () = WS.addItems (wl, usedBy)
            in ()
            end
        val () = if e then () else rewriteCallers (d, imil, f, rewriteCaller)
        (* Transform the closure and thunk inits *)
        fun fail () =
            Fail.fail ("MilFunKnown", "parameters.rewriteInit", "mismatch")
        fun failMk () =
            Fail.fail ("MilFunKnown", "parameters.rewriteInit",
                       "bad closure/thunk make")
        fun rewriteInit u =
            case u
             of IRhsFun (i, d, (c, co, fvs)) =>
                let
                  (* Adjust the make closure *)
                  val def = IMil.Def.getDef (imil, c)
                  val usedBy = IMil.Instr.getUsedBy (imil, i)
                  val ts =
                      case cct of M.CcClosure (_, ts) => ts | _ => failMk ()
                  val () =
                      case def
                       of IMil.DefInstr i =>
                          (case IMil.Instr.getMil (imil, i)
                            of IMil.MInstr
                                 (M.I {rhs = M.RhsPFunctionMk (pobj, ts'),
                                       ...}) =>
                               let
                                 val rhs = M.RhsPFunctionMk (pobj, ts)
                                 val mi = M.I {dest = SOME c, rhs = rhs}
                                 val () =
                                     IMil.Instr.replaceInstruction
                                       (imil, i, mi)
                                 val () = WS.addInstr (wl, i)
                               in ()
                               end
                             | _ => failMk ())
                        | _ => failMk ()
                  (* Adjust the init closure *)
                  val pis =
                      case cca
                       of M.CcClosure (_, pis) => pis
                        | _ => fail ()
                  val fvs = Vector.keepAllMap2 (fvs, pis, doParm)
                  val mi =
                      M.I {dest = d, rhs = M.RhsPFunctionInit (c, co, fvs)}
                  val () = IMil.Instr.replaceInstruction (imil, i, mi)
                  val () = WS.addInstr (wl, i)
                  val () = WS.addItems (wl, usedBy)
                in ()
                end
              | IRhsThunk (i, d, {thunk, effects, code, freeVars}) =>
                let
                  (* Adjust the make thunk *)
                  val usedBy = IMil.Instr.getUsedBy (imil, i)
                  val def = IMil.Def.getDef (imil, thunk)
                  val ts =
                      case cct of M.CcThunk (_, ts) => ts | _ => failMk ()
                  val () =
                      case def
                       of IMil.DefInstr i =>
                          (case IMil.Instr.getMil (imil, i)
                            of IMil.MInstr
                                 (M.I {rhs = M.RhsThunkMk ts', ...}) =>
                               let
                                 val rhs = M.RhsThunkMk ts
                                 val mi = M.I {dest = SOME thunk, rhs = rhs}
                                 val () =
                                     IMil.Instr.replaceInstruction
                                       (imil, i, mi)
                                 val () = WS.addInstr (wl, i)
                               in ()
                               end
                             | _ => failMk ())
                        | _ => failMk ()
                  (* Adjust the init thunk *)
                  val pis =
                      case cca
                       of M.CcThunk (_, pis) => pis
                        | _ => fail ()
                  val fvs = Vector.keepAllMap2 (freeVars, pis, doParm)
                  val mi =
                      M.I {dest = d,
                           rhs = M.RhsThunkInit {thunk = thunk,
                                                 effects = effects,
                                                 code = code,
                                                 freeVars = fvs}}
                  val () = IMil.Instr.replaceInstruction (imil, i, mi)
                  val () = WS.addInstr (wl, i)
                  val () = WS.addItems (wl, usedBy)
                in ()
                end
              | IGlobalFun (g, co) => ()
        val () = rewriteInits (d, imil, f, e, rewriteInit)
        val () = WS.addItem (wl, IMil.ICode cfg)
      in ()
      end)

  (* Bool indicates whether deleting the return value is allowed. *)
  datatype returnInfo = RiDead 
                      | RiNotDead of bool
                      | RiConstant of bool * M.simple 
                      | RiKeep

  fun layoutReturnInfo ri =
      case ri
       of RiDead       => "D"
        | RiNotDead  _ => "N"
        | RiConstant _ => "C"
        | RiKeep       => "K"

  fun return (d, imil, wl, f, cfg) =
      Try.try
      (fn () =>
          let
            val () = Try.require (not (IMil.Cfg.getEscapes (imil, cfg)))
            (*** Analyse the return ***)
            (* Only optimise functions with a single return *)
            val () =
                debug (d, fn () => "return: " ^ I.variableString' f ^
                                   " checking single return")
            val rtyps = IMil.Cfg.getRtyps (imil, cfg)
            val () = Try.require (Vector.length rtyps = 1)
            (* Analyse each caller first *)
            val () =
                debug (d, fn () => "return: " ^ I.variableString' f ^
                                   " checking callers")
            fun fail () = Fail.fail ("MilFunKnown", "return",
                                     "use of non-escaping function not a call")
            fun doRet (a, cfg, ret) =
                let
                  val retb = IMil.Cfg.getBlockByLabel (imil, cfg, ret)
                  val hasDest = 
                      List.length (IMil.Block.preds (imil, retb)) = 1
                in
                  case a
                   of RiDead => 
                      let
                        val param =
                            Vector.sub (IMil.Block.getParameters (imil, retb), 
                                        0)
                        val uses = IMil.Use.getUses (imil, param)
                        val a =
                            if (Vector.length uses = 0) andalso hasDest then 
                              RiDead 
                            else RiNotDead hasDest
                      in a
                      end
                    | RiNotDead a => RiNotDead (a andalso hasDest)
                    | RiConstant (a, c) => RiConstant (a andalso hasDest, c)
                    | RiKeep => RiKeep
                end

            fun notDead a =
                case a
                 of RiDead       => RiNotDead false
                  | RiNotDead a  => RiNotDead false
                  | RiConstant _ => RiKeep 
                  | RiKeep       => RiKeep

            fun doOne (use, a) =
                case use
                 of IMil.UseInstr i =>
                    (case IMil.Instr.getMil (imil, i)
                      of IMil.MTransfer (t as M.TCall (_, _, ret, _, _)) =>
                         doRet (a, IMil.Instr.getCfg (imil, i), ret)
                       | IMil.MTransfer (t as M.TTailCall _) => RiKeep
                       | IMil.MTransfer (t as M.TEvalThunk _) => 
                         notDead a
                       | IMil.MInstr (M.I {rhs, ...}) =>
                         (case rhs
                           of M.RhsPFunctionInit (_, _, fvs) => a
                            | M.RhsThunkInit {freeVars, ...} => 
                              notDead a
                            | _ => fail ())
                       | _ => fail ())
                  | IMil.UseGlobal g =>
                    (case IMil.Global.getMil (imil, g)
                      of IMil.GGlobal (_, M.GPFunction _) => a
                       | _ => fail ())
                  | _ => fail ()

            val a = case IMil.Cfg.getCallConv (imil, cfg)
                     of M.CcThunk _ => RiNotDead false 
                      | _ => RiDead

            val uses = IMil.Use.getUses (imil, f)
            val a = Vector.fold (uses, a, doOne)
            (* Analyse each return next *)
            val () =
                debug (d, fn () => "return: " ^ I.variableString' f ^
                                   " checking function exits")
            val exits = IMil.Cfg.getExits (imil, cfg)
            fun fail () = Fail.fail ("MilFunKnown", "return",
                                     "bad function exit")
            fun analyseExit (exit, a) =
                case
                  IMil.Instr.getMil (imil, IMil.Block.getTransfer (imil, exit))
                 of IMil.MTransfer t =>
                    (case t
                      of M.TReturn os =>
                         (case a
                           of RiDead => RiDead
                            | RiNotDead kill =>
                              (case Vector.sub (os, 0)
                                of c as M.SVariable v =>
                                   if M.variableIsGlobal' (IMil.getSTM imil, v)
                                   then RiConstant (kill, c)
                                   else RiKeep
                                 | c as M.SConstant _ => RiConstant (kill, c)
                                 | _ => RiKeep)
                            | RiConstant (kill, c) =>
                              if MilCmp.simple (c, Vector.sub (os, 0)) = EQUAL
                              then a
                              else RiKeep
                            | RiKeep => RiKeep)
                       | M.TCall _ =>
                         (* Might cut out, so ignore *) a
                       | M.TTailCall _ =>
                         (* Can't transform this without losing the tailcall,
                          * so punt.
                          *)
                         Try.fail ()
                       | M.TEvalThunk _ =>
                         (* Might cut out, so ignore *) a
                       | M.TCut _ =>
                         (* Might cut out, so ignore *) a
                       | _ => fail ())
                  | _ => fail ()
            val a = List.fold (exits, a, analyseExit)
            val () =
                debug (d, fn () => "return: " ^ I.variableString' f ^ ": " ^
                                   layoutReturnInfo a)
            val () = 
                case a
                 of RiDead => ()
                  | RiNotDead _ => Try.fail ()
                  | RiConstant _ => ()
                  | RiKeep => Try.fail ()

            (*** Transform ***)
            (* Transform the calls *)
            (* If no progress is made, then fail *)
            val progress = ref false
            fun rewriteRet (i, ret) =
                let
                  val ccfg = IMil.Instr.getCfg (imil, i)
                  val retb = IMil.Cfg.getBlockByLabel (imil, ccfg, ret)
                  val retb = 
                      (case a 
                        of RiConstant (false, _) =>
                           IMil.Block.makeSinglePred (imil, retb)
                         | _ => retb)
                  val (retl, params) = IMil.Block.getLabel' (imil, retb)
                  val rm =
                      case a
                       of RiDead => (progress := true; true)
                        | RiNotDead _ => (* Treat as keep *) false
                        | RiConstant (kill, c) =>
                          let
                            val param = Vector.sub (params, 0)
                            val uses = IMil.Use.getUses (imil, param)
                            val () = IMil.Use.replaceUses (imil, param, c)
                            val () = WS.addUses (wl, uses)
                            val () = 
                                if Vector.length uses > 0 orelse kill then
                                  progress := true
                                else
                                  ()
                          in kill
                          end
                        | RiKeep => false
                  val () =
                      if rm then
                        IMil.Block.replaceLabel (imil, retb,
                                                 (retl, Vector.new0 ()))
                      else
                        ()
                  val () = WS.addInstr (wl, IMil.Block.getLabel (imil, retb))
                in retl
                end
            fun rewriteCaller (i, c) =
                case c
                 of CCall (c, os, ret, cuts, fx) =>
                    let
                      val ret = rewriteRet (i, ret)
                      val t = M.TCall (c, os, ret, cuts, fx)
                      val mi = IMil.MTransfer t
                      val () = IMil.Instr.replaceMil (imil, i, mi)
                      val () = WS.addInstr (wl, i)
                    in ()
                    end
                  | CTailCall _ => ()
                  | CEval (e, ret, cuts, fx) =>
                    let
                      val ret = rewriteRet (i, ret)
                      val t = M.TEvalThunk (e, ret, cuts, fx)
                      val mi = IMil.MTransfer t
                      val () = IMil.Instr.replaceMil (imil, i, mi)
                      val () = WS.addInstr (wl, i)
                    in ()
                    end
            val () = rewriteCallers (d, imil, f, rewriteCaller)

            val () = Try.require (!progress)

            (* Transform the returns *)
            fun fail () = Fail.fail ("MilFunKnown", "return",
                                     "bad function exit")
            fun transformExit exit =
                let
                  val i = IMil.Block.getTransfer (imil, exit)
                  fun mkDead () =
                      let
                        val usedBy = IMil.Instr.getUsedBy (imil, i)
                        val t = M.TReturn (Vector.new0 ())
                        val mi = IMil.MTransfer t
                        val () = IMil.Instr.replaceMil (imil, i, mi)
                        val () = WS.addInstr (wl, i)
                        val () = WS.addItems (wl, usedBy)
                      in ()
                      end
                  val () =
                      case IMil.Instr.getMil (imil, i)
                       of IMil.MTransfer t =>
                          (case t
                            of M.TReturn os =>
                               (case a
                                 of RiDead => mkDead ()
                                  | RiNotDead _ => (* Treat as keep *) ()
                                  | RiConstant (kill, _) =>
                                    if kill then mkDead () else ()
                                  | RiKeep => ())
                             | M.TCall _ => ()
                             | M.TEvalThunk _ => ()
                             | M.TCut _ => ()
                             | _ => fail ())
                        | _ => fail ()
                in ()
                end
            val () = List.foreach (exits, transformExit)

            (* Transform the types *)
            val rtyps = IMil.Cfg.getRtyps (imil, cfg)
            val rtyps =
                case a
                 of RiDead =>
                    let
                      val () = PD.click (d, "DeadReturnElim")
                    in Vector.new0 ()
                    end
                  | RiNotDead _ => rtyps
                  | RiConstant (kill, _) =>
                    let
                      val () = PD.click (d, "ReturnConstProp")
                    in
                      if kill then Vector.new0 () else rtyps
                    end
                  | RiKeep => rtyps
            val (cct, argts) =
                case M.variableTyp' (IMil.getSTM imil, f)
                 of M.TCode (cct, argts, _) => (cct, argts)
                  | _ => Fail.fail ("MilFunKnown", "return", "bad code type")
            val () = updateType (d, imil, f, cct, argts, rtyps)
            val () = IMil.Cfg.setRtyps (imil, cfg, rtyps)

            val () = WS.addItem (wl, IMil.ICode cfg)
          in ()
          end)

  fun function (d, imil, f, cfg) =
      let
        fun msg () =
            (if IMil.Cfg.getEscapes (imil, cfg) then "escapes: "
             else "known: ") ^
            I.variableString' f
        val () = debug (d, msg)
        val wl = WS.new ()
        val _ = flatten (d, imil, wl, f, cfg)
        val _ = parameters (d, imil, wl, f, cfg)
        val _ = return (d, imil, wl, f, cfg)
      in ()
      end

  fun program (imil, d) =
      let
        val cs = IMil.Cfg.getCfgs imil
        val () = List.foreach (cs, fn (v, cfg) => function (d, imil, v, cfg))
        val () = PD.report (d, passname)
      in ()
      end

  val debugs = [debugPassD]

end;

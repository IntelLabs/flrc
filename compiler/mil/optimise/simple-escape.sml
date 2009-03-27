(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, December 2007 *)

signature MIL_ESCAPE_ANALYSIS = 
sig
  val stats : (string * string) list
  val optimize : PassData.t * IMil.t -> unit
end

functor MilSimpleEscapeF (structure Chat : CHAT 
                                             where type env = PassData.t
                          val simplify : PassData.t 
                                         * IMil.t
                                         * IMil.WorkSet.ws
                                         -> unit)
        :> MIL_ESCAPE_ANALYSIS = 
struct


  val stats = [("NonEscape",    "Functions marked non-escaping" ),
               ("NonRecursive", "Functions marked non-recursive")]

  structure M = Mil
  structure PD = PassData
  structure Cfg = IMil.Cfg
  structure Instr = IMil.Instr
  structure Use = IMil.Use
  structure MOU = MilOptUtils
  structure WS = IMil.WorkSet

  structure L = Layout

  fun useUsesOnlyAsClosure (d, imil, c, u) = 
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
              | M.RhsPFunctionGetFv (v, i) => true
              | M.RhsThunkInit {thunk, freeVars, ...} => cNotIn freeVars
              | M.RhsThunkGetFv (v, i) => true
              | _ => false

        fun doGlobal g = 
            case MOU.iglobalToGlobal (imil, g)
             of SOME (_, g) => 
                (case g
                  of M.GPFunction _ => true
                   | _ => false)
              | _ => false

        fun doIInstr i =
            case Instr.getMil (imil, i)
             of IMil.MInstr (M.I {rhs, ...}) => doRhs rhs
              | IMil.MTransfer t => doTransfer t
              | IMil.MLabel _ => false
              | IMil.MDead => false

        val res = 
            (case u
              of IMil.UseGlobal g => doGlobal g
               | IMil.UseInstr i => doIInstr i
               | IMil.Used => false)
      in res
      end 

  fun usedOnlyAsClosure (d, imil, c) = 
      let
        val uses = Use.getUses (imil, c)
        val res = 
            Vector.forall (uses, fn u => useUsesOnlyAsClosure (d, imil, c, u))
      in res
      end

  fun hasInternalEscapes (d, imil, fname, cfg) = 
      let
        val conv = Cfg.getCallConv (imil, cfg)
        fun getClosure conv = 
            case conv
             of M.CcClosure (c, _) => SOME c
              | M.CcThunk (c, _)   => SOME c
              | _ => NONE
        val res = 
            case getClosure conv
             of SOME c => not (usedOnlyAsClosure (d, imil, c))
              | NONE => false
      in res
      end

  fun useIsNotEscapingClosure (d, imil, u) = 
      let
        fun warn () = 
            let
              val () = Chat.warn0 (d, "Unexpected use of function pointer")
            in false
            end
        val nonEscaping = 
            case u
             of IMil.UseInstr i => 
                (case MOU.iinstrToRhs (imil, i)
                  of SOME rhs => 
                     (case rhs
                       of M.RhsPFunctionInit (c , _, _) => 
                          usedOnlyAsClosure (d, imil, c)
                        | M.RhsThunkInit {thunk, ...} => 
                          usedOnlyAsClosure (d, imil, thunk)
                        | _ => warn())
                   | NONE => true)
                  (* Any other use of a function pointer 
                   * should not induce unknown calls *)
              | IMil.UseGlobal g =>
                (case MOU.iglobalToGlobal (imil, g)
                  of SOME (clos, M.GPFunction _) => 
                     usedOnlyAsClosure (d, imil, clos)
                   | _ => warn())
              | IMil.Used => false
      in nonEscaping
      end

  fun hasExternalEscapes (d, imil, fname, cfg) = 
      let
        val uses = Use.getUses (imil, fname)
        val ok = Vector.forall (uses, 
                             fn u => useIsNotEscapingClosure (d, imil, u))
      in not ok
      end

  fun analyzeCfg (d : PD.t, imil, w, fname, cfg) = 
      let
        val changed = 
            if Cfg.getEscapes (imil, cfg) andalso 
               not (Cfg.isProgramEntry (imil, cfg)) then
              let
                val iEscapes = hasInternalEscapes (d, imil, fname, cfg)
                val eEscapes = hasExternalEscapes (d, imil, fname, cfg)
                val changed = 
                    if not iEscapes andalso not eEscapes then
                      let
                        val () = Cfg.markNonEscaping (imil, cfg)
                        val () = PD.click (d, "NonEscape")
                        val () = WS.addUses (w, IMil.Use.getUses (imil, fname))
                        val () = WS.addItem (w, IMil.ICode cfg)
                        val () = simplify (d, imil, w)
                      in true
                      end
                    else 
                      false
              in changed
              end
            else false
      in changed
      end

  fun optimize (d, imil) = 
      let
        fun loop () = 
            let
              val cfgs = IMil.Cfg.getCfgs imil
              val w = WS.new ()
              val help = 
               fn (f, c) => analyzeCfg (d, imil, w, f, c)
              val changed = List.exists (cfgs, help)
            in 
              if changed then
                loop()
              else ()
            end
      in loop()
      end

end
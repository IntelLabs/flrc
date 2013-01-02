(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation from ANormLazy.t to ANormStrict.t.
 *)
signature ANORM_LAZY_STRICTNESS =
sig
  val pass : (ANormLazy.t, ANormLazy.t) Pass.t
end

structure ANormLazyStrictness :> ANORM_LAZY_STRICTNESS =
struct
  structure I = Identifier
  structure AL = ANormLazy
  structure AC = AbsCoreF (struct structure Dom = Pointed end) 
  structure LC = ANormLazyToAbsCoreF (struct structure AbsCore = AC end)
  structure AE = AbsCoreEvalF (struct structure AbsCore = AC end)
  structure ACL = AbsCoreLayoutF (struct structure AbsCore = AC 
                                         type ty = ANormLazy.ty
                                         val layoutTy = ANormLazyLayout.layoutTy
                                  end)
  structure ADL = AbsCoreLayoutF (struct structure AbsCore = AC 
                                         type ty = AE.demand
                                         val layoutTy = AE.layoutDemand
                                  end)

  val passname = "ANormLazyStrictness"

  fun setVars (st, vs) = 
      let
        fun setter (v, strict) =
            let
              val strict' = case I.variableInfo (st, v) of AE.L => false | _ => true
            in 
              (v, strict orelse strict')
            end
      in
        List.map (vs, setter)
      end

  fun setVars' (st, vs) = 
      let
        fun setter (v, ty, strict) =
            let
              val strict' = case I.variableInfo (st, v) of AE.L => false | _ => true
            in 
              (v, ty, strict orelse strict')
            end
      in
        List.map (vs, setter)
      end


  val rec doExp =
   fn (st, e) =>
      (case e
        of AL.ConApp (c, vs) => AL.ConApp (c, setVars (st, vs))
         | AL.App (e, v) => AL.App (doExp (st, e), v)
         | AL.Lam ((v, vty, _), e) => 
          let
            val strict = case I.variableInfo (st, v) of AE.L => false | _ => true
          in
            AL.Lam ((v, vty, strict), doExp (st, e)) 
          end
         | AL.Let (vdefg, e) => AL.Let (doVDefg (st, vdefg), doExp (st, e))
         | AL.Case (e, (v, vty), ty, alts) =>
          let
            val e = doExp (st, e)
            val doAlt = 
               fn AL.Acon (c, vs, e) => AL.Acon (c, setVars' (st, vs), doExp (st, e))
                | AL.Alit (l, t, e) => AL.Alit (l, t, doExp (st, e))
                | AL.Adefault e => AL.Adefault (doExp (st, e))
          in
            AL.Case (e, (v, vty), ty, List.map (alts, doAlt))
          end
         | AL.Cast (e, t1, t2) => AL.Cast (doExp (st, e), t1, t2)
         | e => e
      )

  and rec doVDef = 
   fn (st, vd) => 
      (case vd
        of AL.Vdef (AL.VbSingle (v, vty, _), e) => 
          let 
            val strict = case I.variableInfo (st, v) of AE.L => false | _ => true
          in
            AL.Vdef (AL.VbSingle (v, vty, strict), doExp (st, e))
          end
         | AL.Vdef (vs, e) => AL.Vdef (vs, doExp (st, e)))

  and rec doVDefg =
   fn (st, vdg) => 
      (case vdg
        of AL.Rec vdefs => AL.Rec (List.map (vdefs, fn def => doVDef (st, def)))
         | AL.Nonrec vdef => AL.Nonrec (doVDef (st, vdef)))

  fun runPass (pd, bn, pass, p) = Pass.apply (Pass.doSubPass pass) (pd, bn, p)

  fun program (p as (AL.Module (main, vdefgs), im), pd, bn) = 
      let
        val q = runPass (pd, bn, LC.pass, p)
        val (_, st) = runPass (pd, bn, AE.pass, q)
        val vdefgs = List.map (vdefgs, fn vdefg => doVDefg (st, vdefg))
      in
        (AL.Module (main, vdefgs), im)
      end
 
  fun layout (module, _) = ANormLazyLayout.layoutModule module
  val ir = { printer = layout, stater = layout }
  val description = {name        = passname,
                     description = "Strictness Analysis on A-Normal Form",
                     inIr        = ir,
                     outIr       = ir,
                     mustBeAfter = [],
                     stats       = []}

  val associates = { controls = [], debugs = [], features = [], 
                     extraDriverInfo = SOME ((Function.curry Pass.addPassDriverInfo LC.pass) o 
                                             (Function.curry Pass.addPassDriverInfo AE.pass)) }

  val pass = Pass.mkOptFullPass (description, associates, program) 

end

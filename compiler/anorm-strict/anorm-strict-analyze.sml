(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature ANORM_STRICT_ANALYZE =
sig
  type state
  type env
  val exp     : state * env * ANormStrict.exp -> unit
  val alt     : state * env * ANormStrict.alt -> unit
  val vDef    : state * env * ANormStrict.vDef -> unit
  val vDefg   : state * env * ANormStrict.vDefg -> env
  val module  : state * env * ANormStrict.module -> unit
  val program : state * env * ANormStrict.t -> unit
end;

functor ANormStrictAnalyzeF (
  type state
  type env
  val config : env -> Config.t
  val variableBind       : (state * env * ANormStrict.var -> env) option
  val variableUse        : (state * env * ANormStrict.var -> unit) option
  val analyzeExp         : (state * env * ANormStrict.exp -> unit) option
  val analyzeAlt         : (state * env * ANormStrict.alt -> unit) option
  val analyzeVDef        : (state * env * ANormStrict.vDef -> unit) option
  val analyzeVDefg       : (state * env * ANormStrict.vDefg -> env) option
) :> ANORM_STRICT_ANALYZE where type state = state
                            and type env = env 
= 
struct
  structure AS = ANormStrict
  structure ANSU = ANormStrictUtils

  val clientBind = variableBind
  val clientVariable = variableUse
  val clientExp = analyzeExp
  val clientAlt = analyzeAlt
  val clientVDef = analyzeVDef
  val clientVDefg = analyzeVDefg

  type state = state
  type env = env

  val analyzeBinder =
   fn (s, e, (v, ty)) =>
      case clientBind
       of NONE => e
        | SOME vb => vb (s, e, v)

  val analyzeBinders =
   fn (s, e, vs) =>
      List.fold (vs, e, fn (v, e) => analyzeBinder (s, e, v))

  val analyzeVariable =
   fn (s, e, v) =>
      case clientVariable
       of NONE => ()
        | SOME vu => vu (s, e, v)

  val analyzeVariables =
   fn (s, e, vs) => List.foreach (vs, fn v => analyzeVariable (s, e, v))

  val rec analyzeExp : state * env * AS.exp -> unit =
   fn (state, env, e) => 
      let
        val () = 
            case clientExp
             of SOME f => f (state, env, e)
              | NONE   => ()
        val () =
            case e
             of AS.Return vs                    => analyzeVariables (state, env, vs)
              | AS.PrimApp (s, vs)              => analyzeVariables (state, env, vs)
              | AS.ExtApp (pname, cc, s, t, vs) => analyzeVariables (state, env, vs)
              | AS.ConApp (c, vs)               => analyzeVariables (state, env, vs)
              | AS.App (f, vs, _)               => 
                let
                  val () = analyzeVariable (state, env, f)
                  val () = analyzeVariables (state, env, vs)
                in ()
                end
              | AS.Let (defG, e) => 
                let
                  val env = analyzeVDefg (state, env, defG)
                  val () = analyzeExp (state, env, e)
                in ()
                end
              | AS.Case (v, alts) => 
                let
                  val () = analyzeVariable (state, env, v)
                  val () = analyzeAlts (state, env, alts)
                in ()
                end
              | AS.Lit (l, t) => ()
              | AS.Cast (AS.FromAddr v) => analyzeVariable (state, env, v)
              | AS.Cast (AS.ToAddr v) => analyzeVariable (state, env, v)
              | AS.Cast _ => ()
              | AS.Eval v     => analyzeVariable (state, env, v)
      in ()
      end

  and rec analyzeAlts : state * env * AS.alt List.t -> unit  =
   fn (state, env, alts) => List.foreach (alts, fn alt => analyzeAlt (state, env, alt))

  and rec analyzeAlt : state * env * AS.alt -> unit =
   fn (state, env, alt) =>
      let
        val () = 
            case alt
             of AS.Acon (con, binds, e) => 
                let
                  val env = analyzeBinders (state, env, binds)
                  val e = analyzeExp (state, env, e)
                in ()
                end
              | AS.Alit (l, t, e)       => analyzeExp (state, env, e)
              | AS.Adefault e           => analyzeExp (state, env, e)
      in ()
      end
      
    and rec analyzeVDef : state * env * AS.vDef -> unit = 
     fn (state, env, vd) => 
        let
          val () = 
              case clientVDef 
               of SOME f => f (state, env, vd)
                | NONE   => ()

          val () = 
              case vd
               of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} => 
                  let
                    val () = analyzeVariables (state, env, fvs)
                    val env = analyzeBinders (state, env, args)
                    val () = analyzeExp (state, env, body)
                  in ()
                  end
                | AS.Vthk {name, ty, escapes, recursive, fvs, body} => 
                  let
                    val () = analyzeVariables (state, env, fvs)
                    val () = analyzeExp (state, env, body)
                  in ()
                  end
        in ()
        end

    and rec analyzeVDefg : state * env * AS.vDefg -> env = 
     fn (state, env, vdg) => 
        let
          val env = 
              case vdg
               of AS.Rec vDefs => 
                  let
                    val binders = List.map (vDefs, ANSU.VDef.binder)
                    val env = analyzeBinders (state, env, binders)
                    val () = List.foreach (vDefs, fn vd => analyzeVDef (state, env, vd))
                  in env
                  end
                | AS.Nonrec vDef => 
                  let
                    val () = analyzeVDef (state, env, vDef)
                    val env = analyzeBinder (state, env, ANSU.VDef.binder vDef)
                  in env
                  end
                | AS.Vdef (vts, e) => 
                  let
                    val () = analyzeExp (state, env, e)
                    val env = List.fold (vts, env, fn (vt, env) => analyzeBinder (state, env, vt))
                  in env
                  end
        in env
        end
        
    val analyzeModule : state * env * AS.module -> unit =
     fn (state, env, m) => 
        let
          val AS.Module (v, vdgs) = m
          (* val env = analyzeBinders (state, env, tds) *)
          val env = List.fold (vdgs, env, fn (vdg, env) => analyzeVDefg (state, env, vdg))
          val () = analyzeVariable (state, env, v)
        in ()
        end

    val analyzeProgram : state * env * AS.t -> unit = 
     fn (state, env, (m, im)) => analyzeModule (state, env, m)


        
    val exp     = analyzeExp
    val alt     = analyzeAlt
    val vDef    = analyzeVDef
    val vDefg   = analyzeVDefg
    val module  = analyzeModule
    val program = analyzeProgram

end (* functor ANormStrictAnalyzeF *)

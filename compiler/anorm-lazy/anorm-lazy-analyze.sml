(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature ANORM_STRICT_ANALYZE =
sig
  type state
  type env
  val ty      : state * env * ANormLazy.ty -> unit
  val exp     : state * env * ANormLazy.exp -> unit
  val alt     : state * env * ANormLazy.alt -> unit
  (* vDef does not analyze the bound variable nor its type *)
  val vDef    : state * env * ANormLazy.vDef -> unit
  val vDefg   : state * env * ANormLazy.vDefg -> env
  val module  : state * env * ANormLazy.module -> unit
  (* For now just analyzes the type of each entry *)
  val st      : state * env * ANormLazy.symbolTable -> unit
  val program : state * env * ANormLazy.t -> unit
end;

functor ANormLazyAnalyzeF (
  type state
  type env
  val config : env -> Config.t
  val variableBind       : (state * env * ANormLazy.var -> env) option
  val variableUse        : (state * env * ANormLazy.var -> unit) option
  val analyzeTy          : (state * env * ANormLazy.ty -> unit) option
  val analyzeExp         : (state * env * ANormLazy.exp -> unit) option
  val analyzeAlt         : (state * env * ANormLazy.alt -> unit) option
  val analyzeVDef        : (state * env * ANormLazy.vDef -> unit) option
) :> ANORM_STRICT_ANALYZE where type state = state
                            and type env = env 
= 
struct
  structure I = Identifier
  structure AL = ANormLazy
  structure GPT = GHCPrimType

  val clientBind = variableBind
  val clientVariable = variableUse
  val clientTy  = analyzeTy
  val clientExp = analyzeExp
  val clientAlt = analyzeAlt
  val clientVDef = analyzeVDef

  type state = state
  type env = env

  val analyzeVariable =
   fn (s, e, v) =>
      case clientVariable
       of NONE => ()
        | SOME vu => vu (s, e, v)

  val analyzeVariables =
   fn (s, e, vs) => List.foreach (vs, fn v => analyzeVariable (s, e, v))

  val rec analyzeBinder =
   fn (s, e, (v, ty)) =>
      let
        val () = analyzeTy (s, e, ty)
      in 
        case clientBind
         of NONE => e
          | SOME vb => vb (s, e, v)
      end

  and rec analyzeBinders =
   fn (s, e, vs) =>
      List.fold (vs, e, fn (v, e) => analyzeBinder (s, e, v))

  and rec analyzeVBinder =
   fn (s, e, vb) =>
      let
        val vts = case vb
                    of AL.VbSingle (v, t, _) => [(v, t)]
                     | AL.VbMulti (vts, _) => vts
      in
        analyzeBinders (s, e, vts)
      end

  and rec analyzeVBinders =
   fn (s, e, vs) =>
      List.fold (vs, e, fn (v, e) => analyzeVBinder (s, e, v))


  and rec analyzePrimTy = 
      fn (s, e, pt) => 
         let
           val _ = GPT.mapPrimTy (pt, fn t => analyzeTy (s, e, t))
         in ()
         end

  and rec analyzePrimTys =
      fn (s, e, pts) => List.foreach (pts, fn pt => analyzePrimTy (s, e, pt))

  and rec analyzeTy =
      fn (s, e, t) => 
         let
           val () = 
               case clientTy 
                of NONE => ()
                 | SOME dt => dt (s, e, t)
         in case TypeRep.repToBase t
             of AL.Data               => ()
              | AL.Prim pt            => analyzePrimTy (s, e, pt)
              | AL.Arr (t0, t1, fx)   => 
                let
                  val () = analyzeTy (s, e, t0) 
                  val () = analyzeTy (s, e, t1)
                in ()
                end
              | AL.Sum ctss           => List.foreach (ctss, fn (c, ts) => analyzeTys (s, e, List.map (ts, #1)))
         end

  and rec analyzeTys =
      fn (s, e, ts) => List.foreach (ts, fn t => analyzeTy (s, e, t))

  and rec analyzeExp : state * env * AL.exp -> unit =
   fn (state, env, e) => 
      let
        val () = 
            case clientExp
             of SOME f => f (state, env, e)
              | NONE   => ()
        val () =
            case e
             of AL.Var v                        => analyzeVariable  (state, env, v)
              | AL.Multi vs                     => analyzeVariables (state, env, vs)
              | AL.PrimApp (s, vs)              => analyzeVariables (state, env, vs)
              | AL.ExtApp (pname, cc, s, t, vs) => 
                let
                  val () = analyzeTy (state, env, t)
                  val () = analyzeVariables (state, env, vs)
                in ()
                end
              | AL.ConApp (c, vs)               => analyzeVariables (state, env, List.map (vs, #1))
              | AL.App (f, v)                   => 
                let
                  val () = analyzeExp (state, env, f)
                  val () = analyzeVariable (state, env, v)
                in ()
                end
              | AL.Lam ((v, vty, _), e)         =>
                let
                  val env = analyzeBinder (state, env, (v, vty))
                  val () = analyzeExp (state, env, e)
                in ()
                end
              | AL.Let (defG, e) => 
                let
                  val env = analyzeVDefg (state, env, defG)
                  val () = analyzeExp (state, env, e)
                in ()
                end
              | AL.Case (e, (v, vty), ty, alts) => 
                let
                  val () = analyzeExp (state, env, e)
                  val () = analyzeVariable (state, env, v)
                  val () = analyzeTy (state, env, vty)
                  val () = analyzeTy (state, env, ty)
                  val () = analyzeAlts (state, env, alts)
                in ()
                end
              | AL.Lit (l, t) => analyzeTy (state, env, t)
              | AL.Cast (e, t1, t2) => 
                let
                  val () = analyzeExp (state, env, e)
                  val () = analyzeTy (state, env, t1)
                  val () = analyzeTy (state, env, t2)
                in ()
                end
      in ()
      end

  and rec analyzeAlts : state * env * AL.alt List.t -> unit  =
   fn (state, env, alts) => List.foreach (alts, fn alt => analyzeAlt (state, env, alt))

  and rec analyzeAlt : state * env * AL.alt -> unit =
   fn (state, env, alt) =>
      let
        val () = 
            case alt
             of AL.Acon (con, binds, e) => 
                let
                  val env = analyzeBinders (state, env, List.map (binds, fn (v, t, _) => (v, t)))
                  val e = analyzeExp (state, env, e)
                in ()
                end
              | AL.Alit (l, t, e)       => 
                let
                  val () = analyzeTy (state, env, t)
                  val () = analyzeExp (state, env, e)
                in ()
                end
              | AL.Adefault e           => analyzeExp (state, env, e)
      in ()
      end
      
    and rec analyzeVDef : state * env * AL.vDef -> unit = 
     fn (state, env, vd) => 
        let
          val () = 
              case clientVDef 
               of SOME f => f (state, env, vd)
                | NONE   => ()

          val () = 
              case vd
               of AL.Vdef (vb, e) =>
                  let
                    val env = analyzeVBinder (state, env, vb)
                    val () = analyzeExp (state, env, e)
                  in ()
                  end
        in ()
        end

    and rec analyzeVDefg : state * env * AL.vDefg -> env = 
     fn (state, env, vdg) => 
        let
          val env = 
              case vdg
               of AL.Rec vDefs => 
                  let
                    val binders = List.map (vDefs, fn AL.Vdef (vb, _) => vb)
                    val env = analyzeVBinders (state, env, binders)
                    val () = List.foreach (vDefs, fn vd => analyzeVDef (state, env, vd))
                  in env
                  end
                | AL.Nonrec (vDef as AL.Vdef (vb, _)) => 
                  let
                    val () = analyzeVDef (state, env, vDef)
                    val env = analyzeVBinder (state, env, vb)
                  in env
                  end
        in env
        end
        
    val analyzeModule : state * env * AL.module -> unit =
     fn (state, env, m) => 
        let
          val AL.Module (v, vdgs) = m
          val env = List.fold (vdgs, env, fn (vdg, env) => analyzeVDefg (state, env, vdg))
          val () = analyzeVariable (state, env, v)
        in ()
        end

    val analyzeST =
     fn (state, env, im) => 
        let
          val vs = I.listVariables im
          val () = List.foreach (vs, fn v => analyzeTy (state, env, I.variableInfo (im, v)))
        in ()
        end

    val analyzeProgram : state * env * AL.t -> unit = 
     fn (state, env, (m, st, tm)) => 
        let
          val () = analyzeST (state, env, st)
          val () = analyzeModule (state, env, m)
        in ()
        end

    val ty      = analyzeTy
    val exp     = analyzeExp
    val alt     = analyzeAlt
    val vDef    = analyzeVDef
    val vDefg   = analyzeVDefg
    val module  = analyzeModule
    val st      = analyzeST
    val program = analyzeProgram

end (* functor ANormLazyAnalyzeF *)

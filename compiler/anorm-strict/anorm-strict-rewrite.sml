(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

structure ANormStrictRewriterClient = 
struct
  datatype 'a change = Stop | Continue | StopWith of 'a | ContinueWith of 'a 
  type ('s, 'e, 'o) rewriter = 's * 'e * 'o -> ('e * 'o) change
  type ('s, 'e, 'o) binder = 's * 'e * 'o  -> ('e * 'o option)
end;

signature ANORM_STRICT_REWRITER = 
sig

  type state
  type env

  type 'a rewriter  = state * env * 'a -> 'a
  type 'a rewriterE = state * env * 'a -> env * 'a

  val var     : ANormStrict.var rewriter
  val exp     : ANormStrict.exp rewriter
  val alt     : ANormStrict.alt rewriter
  (* vDef does not call clientBind on the bound variable, and does not
   * produce a new environment *)
  val vDef    : ANormStrict.vDef rewriter
  val vDefg   : ANormStrict.vDefg rewriterE
  val module  : ANormStrict.module rewriter
  val program : ANormStrict.t rewriter

end;

functor ANormStrictRewriterF (
  type state
  type env
  val config   : env -> Config.t
  val bind     : (state, env, Identifier.variable) ANormStrictRewriterClient.binder
  val variable : (state, env, Identifier.variable) ANormStrictRewriterClient.rewriter
  val exp      : (state, env, ANormStrict.exp) ANormStrictRewriterClient.rewriter
  val alt      : (state, env, ANormStrict.alt) ANormStrictRewriterClient.rewriter 
  val vDef     : (state, env, ANormStrict.vDef) ANormStrictRewriterClient.rewriter 
  val vDefg    : (state, env, ANormStrict.vDefg) ANormStrictRewriterClient.rewriter
  val module   : (state, env, ANormStrict.module) ANormStrictRewriterClient.rewriter 
) :> ANORM_STRICT_REWRITER where type state = state and type env = env = 
struct

  structure RC = ANormStrictRewriterClient
  structure AS = ANormStrict 

  type state = state
  type env = env
  type 'a rewriter  = state * env * 'a -> 'a
  type 'a rewriterE = state * env * 'a -> env * 'a

  val getConfig      = config
  val clientVariable = variable
  val clientExp      = exp
  val clientAlt      = alt
  val clientVDef     = vDef
  val clientVDefg    = vDefg
  val clientModule   = module
  val clientBind     = bind

  val bindVar = 
   fn (state, env, v) =>
      let
	val (env, vo) = clientBind (state, env, v)
	val v = Pervasive.Option.getOpt (vo, v)
      in (env, v)
      end

  val bindVars =
   fn (state, env, vs) =>
      let
        val doOne = fn (v, env) => Utils.Function.flipOut bindVar (state, env, v)
        val (vs, env) = Utils.List.mapFoldl (vs, env, doOne)
      in (env, vs)
      end

  val bindVars2 =
   fn (state, env, vs) =>
      let
        val doOne = fn ((v, t), env) => 
                       let
                         val (env, v) = bindVar (state, env, v)
                       in ((v, t), env)
                       end
        val (vs, env) = Utils.List.mapFoldl (vs, env, doOne)
      in (env, vs)
      end

  val callClientCode =
   fn (itemhandler, doitem, state, env, item) =>
      case itemhandler (state, env, item)
       of RC.StopWith     (env, i) => i
	| RC.ContinueWith (env, i) => doitem (state, env, i)
	| RC.Continue              => doitem (state, env, item)     
	| RC.Stop                  => item

  val doVar =
   fn (state, env, v) =>
      let
        val doIt = fn (state, env, v) => v
      in
        callClientCode (clientVariable, doIt, state, env, v)
      end

  val doVars = 
   fn (state, env, vs) => List.map (vs, fn v => doVar (state, env, v))

  val doTy = 
   fn (state, env, t) => t

  val rec doExp =
   fn (state, env, e) => 
      let
        val doIt = 
         fn (state, env, e) => 
            let
              val e = 
                  case e
                   of AS.Return vs => 
                      let
                        val vs = doVars (state, env, vs)
                        val e = AS.Return vs
                      in e
                      end
                    | AS.PrimApp (s, vs) => 
                      let
                        val vs = doVars (state, env, vs)
                        val e = AS.PrimApp (s, vs)
                      in e
                      end
                    | AS.ExtApp (pname, cc, s, t, vs) => 
                      let
                        val vs = doVars (state, env, vs)
                        val e = AS.ExtApp (pname, cc, s, t, vs)
                      in e
                      end
                    | AS.ConApp (c, vs) => 
                      let
                        val vs = doVars (state, env, vs)
                        val e = AS.ConApp (c, vs)
                      in e
                      end
                    | AS.App (f, vs, effect) => 
                      let
                        val f = doVar (state, env, f)
                        val vs = doVars (state, env, vs)
                        val e = AS.App (f, vs, effect)
                      in e
                      end
                    | AS.Let (defG, e) => 
                      let
                        val (env, defG) = doVDefg (state, env, defG)
                        val e = doExp (state, env, e)
                        val e = AS.Let (defG, e)
                      in e
                      end
                    | AS.Case (v, alts) => 
                      let
                        val v = doVar (state, env, v)
                        val alts = doAlts (state, env, alts)
                        val e = AS.Case (v, alts)
                      in e
                      end
                    | AS.Lit (l, t) => 
                      let
                        val e = AS.Lit (l, t)
                      in e
                      end
                    | AS.Cast cast => 
                      let
                        fun doV v = doVar (state, env, v)
                        val cast = case cast 
                                    of AS.FromAddr v => AS.FromAddr (doV v)
                                     | AS.ToAddr v => AS.ToAddr (doV v)
                                     | AS.Bottom v => AS.Bottom (doV v)
                                     | _ => cast
                        val e = AS.Cast cast
                      in e
                      end
                    | AS.Eval v => 
                      let
                        val v = doVar (state, env, v)
                        val e = AS.Eval v
                      in e
                      end
            in e
            end
      in callClientCode (clientExp, doIt, state, env, e)
      end

    and rec doAlts =
     fn (state, env, alts) => List.map (alts, fn alt => doAlt (state, env, alt))

    and rec doAlt =
     fn (state, env, alt) =>
        let
          val doIt = 
           fn (state, env, alt) => 
              let
                val alt = 
                    case alt
                     of AS.Acon (con, binds, e) => 
                        let
                          val (env, binds) = bindVars2 (state, env, binds)
                          val e = doExp (state, env, e)
                        in AS.Acon (con, binds, e)
                        end
                      | AS.Alit (l, t, e) => 
                        let
                          val e = doExp (state, env, e)
                        in AS.Alit (l, t, e)
                        end
                      | AS.Adefault e => 
                        let
                          val e = doExp (state, env, e)
                        in AS.Adefault e
                        end
              in alt
              end
        in callClientCode (clientAlt, doIt, state, env, alt)
        end

    (* Variables already bound *)
    and rec doVDef = 
     fn (state, env, vd) => 
        let
          val doIt = 
           fn (state, env, vd) => 
              (case vd
                of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} => 
                  let
                    val ty = doTy (state, env, ty)
                    val fvs = doVars (state, env, fvs)
                    val (env, args) = bindVars2 (state, env, args)
                    val body = doExp (state, env, body)
                  in AS.Vfun {name = name, ty = ty, escapes = escapes, recursive = recursive, 
                              fvs = fvs, args = args, body = body}
                  end
                | AS.Vthk {name, ty, escapes, recursive, fvs, body} => 
                  let
                    val ty = doTy (state, env, ty)
                    val fvs = doVars (state, env, fvs)
                    val body = doExp (state, env, body)
                  in AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive,
                              fvs = fvs, body = body}
                  end)
        in callClientCode (clientVDef, doIt, state, env, vd)
        end

    and rec doVDefg = 
     fn (state, env, vdg) => 
        let
          val bindVDef = 
           fn (vd, env) => 
              (case vd
                of AS.Vfun {name, ty, fvs, escapes, recursive, args, body} => 
                   let
                     val (env, name) = bindVar (state, env, name)
                   in (AS.Vfun {name = name, ty = ty, escapes = escapes, recursive = recursive, 
                                fvs = fvs, args = args, body = body}
                      , env)
                   end
                 | AS.Vthk {name, ty, escapes, recursive, fvs, body}    => 
                   let
                     val (env, name) = bindVar (state, env, name)
                   in (AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive, 
                                fvs = fvs, body = body}
                      , env)
                   end)

          val bindVDefs = 
           fn (vds, env) => Utils.List.mapFoldl (vds, env, bindVDef)
                     
          val doIt = 
           fn (state, env, vdg) => 
              case vdg
               of AS.Rec vDefs => 
                  let
                    val (vDefs, env) = bindVDefs (vDefs, env)
                    val vDefs = List.map (vDefs, fn vDef => doVDef (state, env, vDef))
                  in (env, AS.Rec vDefs)
                  end
                | AS.Nonrec vDef => 
                  let
                    val vDef = doVDef (state, env, vDef)
                    val (vDef, env) = bindVDef (vDef, env)
                  in (env, AS.Nonrec vDef)
                  end
                |  AS.Vdef (vts, e) => 
                   let
                     val (vs, ts) = List.unzip vts
                     val ts = List.map (ts, fn t => doTy (state, env, t))
                     val e = doExp (state, env, e)
                     val folder = 
                      fn (v, env) => 
                         let val (env, v) = bindVar (state, env, v) in (v, env) end
                     val (vs, env) = Utils.List.mapFoldl (vs, env, folder)
                     val vts = List.zip (vs, ts)
                   in (env, AS.Vdef (vts, e))
                   end
        in 
          case clientVDefg (state, env, vdg)
           of RC.StopWith     (env, i) => (env, i)
	    | RC.ContinueWith (env, i) => doIt (state, env, i)
	    | RC.Continue              => doIt (state, env, vdg)
	    | RC.Stop                  => (env, vdg)
        end
        
    val doModule =
     fn (state, env, m) => 
        let
          val doIt = 
           fn (state, env, AS.Module (v, vdgs))=> 
              let
                val fold = 
                 fn (vdg, env) => 
                    let
                      val (env, vdg) = doVDefg (state, env, vdg)
                    in (vdg, env)
                    end
                val (vdgs, env) = Utils.List.mapFoldl (vdgs, env, fold)
                val v = doVar (state, env, v)
              in AS.Module (v, vdgs)
              end
        in callClientCode (clientModule, doIt, state, env, m)
        end

    val doT = 
     fn (state, env, (m, im)) => (doModule (state, env, m), im)



    (* EXPORT *)
    val var     : ANormStrict.var rewriter     = doVar
    val exp     : ANormStrict.exp rewriter     = doExp
    val alt     : ANormStrict.alt rewriter     = doAlt
    val vDef    : ANormStrict.vDef rewriter    = doVDef
    val vDefg   : ANormStrict.vDefg rewriterE  = doVDefg
    val module  : ANormStrict.module rewriter  = doModule
    val program : ANormStrict.t rewriter       = doT

end; (* Functor ANormStrictRewriterF*)


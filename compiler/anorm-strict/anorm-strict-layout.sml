(* COPYRIGHT_NOTICE_1 *)

signature ANORM_STRICT_LAYOUT =
sig
  val controls : Config.Control.control list
  val debugs   : Config.Debug.debug list

  type 'a layout = Config.t * ANormStrict.symbolInfo * 'a -> Layout.t

  val var    : ANormStrict.var layout
  val ty     : ANormStrict.ty layout
  val alt    : ANormStrict.alt layout
  val exp    : ANormStrict.exp layout
  val vDef   : ANormStrict.vDef layout
  val vDefg  : ANormStrict.vDefg layout
  val tDef   : ANormStrict.tDef layout
  val module : ANormStrict.module layout
  val layout : Config.t * ANormStrict.t -> Layout.t
end

structure ANormStrictLayout :> ANORM_STRICT_LAYOUT =
struct
  val modulename = "ANormStrictLayout"

  structure ANS = ANormStrict
  structure CP = CoreHsPrims
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure U = Utils
  structure L = Layout
  structure LU = LayoutUtils
  structure I = Identifier
  structure IS = Identifier.SymbolInfo
  structure PTL = GHCPrimTypeLayout

  type options = {
       showBinderTypes : bool,
       showSymbolTable : bool
  }
                 
  val describe =
   fn () =>
      L.align [L.str (modulename ^ " control string consists of:"),
               LU.indent (L.align [L.str "b => show types on variable binders",
                                   L.str "S => show symbol table",
                                   L.str "+ => show all of the above"]),
               L.str "default is nothing"]
      
  val parse =
   fn str =>
      let
        val binderTyps = ref false
        val symbols    = ref false
        fun doOne c =
            case c
             of #"b" => let val () = binderTyps := true in true end
              | #"S" => let val () = symbols := true in true end
              | #"+" =>
                let
                  val () = binderTyps := true
                  val () = symbols := true
                in true
                end
              | _    => false
      in
        if List.forall (String.explode str, doOne) then
          SOME ({showBinderTypes = !binderTyps, showSymbolTable = !symbols})
        else
          NONE
      end
      
  val dft = fn _ =>({showBinderTypes = false, showSymbolTable = false})
                   
  val (control, controlGet) = Config.Control.mk (modulename, describe, parse, dft)

  type 'a layout = Config.t * ANormStrict.symbolInfo * 'a -> Layout.t
                              
  datatype env = E of {
           config  : Config.t,
           si      : ANS.symbolInfo,
           options : options
  }
                      
  val getConfig  = fn (E {config,  ...}) => config
  val getSI      = fn (E {si,      ...}) => si
  val getOptions = fn (E {options, ...}) => options

  val showBinderTypes = fn e => #showBinderTypes (getOptions e)
  val showSymbolTable = fn e => #showSymbolTable (getOptions e)
                                
  val indent = LU.indent
  val angleList = LU.angleSeq
  val spaceList = fn l => LU.sequence ("", "", " ") l

  val semiMap =
   fn (env, l, f) => 
      (case l
        of []  => []
         |  [x] => [f (env, x)]
         |  l   => List.map (l, fn v => L.seq [f (env, v), L.str ";"]))

  val name = 
   fn (env, n) => IS.layoutName (n, getSI env)

  val variable = 
   fn (env, v) => 
      let
        val si = getSI env
        val l = IS.layoutVariable (v, si)
      in
        if IS.variableExists (si, v) then
          let
            val c = case IS.variableInfo (getSI env, v)
                     of (_, ANS.VkGlobal) => L.str "g"
                      | (_, ANS.VkLocal)  => L.str "l"
          in
            L.seq [c, l]
          end
        else
          L.seq [L.str "BAD_VAR_", l]
      end

  val rec ty = 
   fn (env, vTy) =>
      (case vTy
        of ANS.Boxed        => L.str "%boxed"
         | ANS.Tname v      => variable (env, v)
         | ANS.Thunk t      => L.seq [L.str "Thunk(", ty (env, t), L.str ")"] 
         | ANS.Prim t       => L.seq [L.str "%primtype ", PTL.layoutPrimTy (fn t => ty (env, t)) t]
         | ANS.Arr (t1, t2, effect) => L.mayAlign [angleList (tys (env, t1)) , 
                                                   L.seq [Effect.layout effect, L.str "-> ", angleList (tys (env, t2))]]
         | ANS.Sum cons     => 
           let 
             val layCon = fn ((con, _), ts) => L.seq [name (env, con), L.str " ", 
                                                      angleList (tys (env, ts))]
           in
             LU.braceSeq (List.map (cons, layCon))
           end)
  and rec tys = 
   fn (env, tys) => List.map (tys, fn t => ty (env, t))

  val tDef = fn (env, (v, t)) => L.seq [L.str "%data ", variable (env, v), L.str  " = ", ty (env, t)]

  val vBind = fn (env, (v, t)) =>
                 if showBinderTypes env then
                   L.seq [L.str "<", variable (env, v), L.str " :: ", ty (env, t), L.str ">"]
                 else
                   variable (env, v)

  val vBinds = fn (env, vbs) => List.map (vbs, fn vb => vBind (env, vb))

  val vBindsWithFVS = fn (env, (fvs, vbs)) =>
                         L.seq [ L.sequence ("<", ";", ",") (List.map (fvs, fn v => variable (env, v)))
                               , L.sequence (" ", ">", ",") (vBinds (env, vbs))]

  val literal = CL.layoutCoreLit

  val variableSeq = fn (env, vs) => angleList (List.map (vs, fn v => variable (env, v)))

  val showBinder = 
   fn (env, t) => 
      if showBinderTypes env then 
        L.seq [L.str " :: ", ty (env, t)]
      else
        L.empty

  fun variableSeqWithTy (env, vts) 
    = angleList (List.map (vts, fn (v, t) => L.seq [variable (env, v), showBinder (env, t)]))

  val rec alt = 
   fn (env, a) => 
      (case a
        of ANS.Acon ((con, _), vbs, e) =>
           L.mayAlign [ L.seq [name (env, con), L.str " ", spaceList (vBinds (env, vbs)), L.str " ->"]
                      , indent (exp (env, e))]
         | ANS.Alit (l, t, e)    =>
            L.mayAlign [ L.seq [literal l, L.str " :: ", ty (env, t) , L.str " ->"]
                         , indent (exp (env, e))]
         | ANS.Adefault e          => L.mayAlign [ L.str "%_ -> ", indent (exp (env, e))])

  and rec alts = 
   fn (env, alternatives) => List.map (alternatives, fn a => alt (env, a))

  and rec aExp =
   fn (env, ae) =>
      (case ae
        of ANS.PrimApp (f, xs) => 
           L.seq [CL.layoutQName (CP.pv (GHCPrimOp.toString f)), L.str " ", variableSeq (env, xs)]
         | ANS.ConApp ((c, _), xs) => L.seq [name (env, c), L.str " " , variableSeq (env, xs)]
         | ANS.Return xs => variableSeq (env, xs)
         | ANS.ExtApp (p, cc, n, t, xs) =>
           L.paren (L.seq [ L.paren (L.seq [ L.str "%external "
                                           , CL.layoutCC cc
                                           , L.str (" \"" ^ CL.escape p ^ "\" \"" ^ CL.escape n ^ "\"")
                                           , L.str "::", L.paren (ty (env, t)) ])
                          , L.str " ", variableSeq (env, xs)])
         | ANS.App (f, xs, _) => L.seq [variable (env, f), L.str " ", variableSeq (env, xs)]
         | ANS.Lit (l, t) => L.paren (L.seq [literal l, L.str " :: ", ty (env, t)])
         | ANS.Cast (ANS.ToAddr v) => L.paren (L.seq [ L.str "%castToAddr ", variable (env, v)])
         | ANS.Cast (ANS.FromAddr v) => L.paren (L.seq [ L.str "%castFromAddr ", variable (env, v)])
         | ANS.Cast (ANS.NullRef) => L.str "%castNullRef"
         | ANS.Cast (ANS.Bottom v) => L.paren (L.seq [L.str "%castBottom ", variable (env, v)])
         | ANS.Eval v      => L.seq [L.str "%eval ", variable (env, v)]
         | e           => L.paren (exp (env, e)))

  and rec exp = 
   fn (env, e) => 
      (case e
        of ANS.Let (vd, e) => 
           let
             val rec doNested = 
              fn e => 
                 (case e
                   of ANS.Let (vd, e) => 
                      L.align [ L.seq [L.str "%    ", vDefg (env, vd)], doNested e]
                    | _ => L.seq [L.str "%in ", exp (env, e)])
           in L.align [ L.seq [L.str "%let ", vDefg (env, vd)], doNested e]
           end
         | ANS.Case (v, alternatives) =>
           L.align [ L.seq [L.str "%case ", variable (env, v), L.str " of "]
                   , indent (L.sequence ("{", "}", ";") (alts (env, alternatives)))]
         | e => aExp (env, e))

  and rec vDef =
   fn (env, vd) => 
      let
        val lesc = fn escapes => if escapes then L.str "^" else L.empty
        val lrec = fn recursive => if recursive then L.str "*" else L.empty
      in
        case vd
         of ANS.Vthk {name, ty, escapes, recursive, fvs, body} =>
            L.mayAlign [ L.seq [ variable (env, name), lesc escapes, lrec recursive, 
                                 showBinder (env, ty), L.str " = %thunk ", vBindsWithFVS (env, (fvs, [])) ], 
                         indent (exp (env, body)) ]
          | ANS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
            L.mayAlign [ L.seq [ variable (env, name), lesc escapes, lrec recursive, showBinder (env, ty), L.str " ="]
                       , indent (L.mayAlign [ L.seq [ L.str "\\ ", vBindsWithFVS (env, (fvs, args)), L.str " -> "]
                                            , exp (env, body)])]
      end
             
  and rec vDefg =
   fn (env, vdg) => 
      (case vdg 
        of ANS.Rec vdefs   => L.mayAlign [ L.str "%rec {"
                                         , indent (L.align (semiMap (env, vdefs, vDef)))
                                         , L.str "}"]
         | ANS.Nonrec vdef => vDef (env, vdef)
         | ANS.Vdef (vts, e) =>
           L.mayAlign [ L.seq [ variableSeqWithTy (env, vts), L.str " =" ]
                      , indent (exp (env, e))])
    
  val module =
      fn (env, m) => 
         (case m
           of ANS.Module (v, vdefgs)=>
              L.align [ L.str "%module"
                      , indent (L.align (semiMap (env, vdefgs, vDefg)))
                      , L.seq [L.str "%entry ", variable (env, v)]])

  val symbolTable = 
   fn (env, im) => 
      if showSymbolTable env then
        let
          val vs = I.listVariables im
          val layout = 
           fn x => L.seq [ variable (env, x)
                         , L.str " :: "
                         , ty (env, #1 (I.variableInfo (im, x)))]
                                                               
        in
          L.align [ L.str "%variables"
                  , indent (L.align (List.map (vs, layout)))
                  ]
        end
      else
        L.empty
        
  val envMk = 
   fn (config, si) => E {config = config,
                         si = si,
                         options = controlGet config}
  val layout = 
   fn (config, (m, im)) =>
      let
        val env = envMk (config, IS.SiTable im)
      in
        L.align [ symbolTable (env, im)
                , module(env, m)
                , L.str "\n"]
      end

  val lift = 
   fn f => fn (config, si, a) => f (envMk (config, si), a)

  val var    : ANormStrict.var layout    = lift variable
  val ty     : ANormStrict.ty layout     = lift ty
  val alt    : ANormStrict.alt layout    = lift alt
  val exp    : ANormStrict.exp layout    = lift exp
  val vDef   : ANormStrict.vDef layout   = lift vDef
  val vDefg  : ANormStrict.vDefg layout  = lift vDefg
  val tDef   : ANormStrict.tDef layout   = lift tDef
  val module : ANormStrict.module layout = lift module

  val controls = [control]
  val debugs = []
end



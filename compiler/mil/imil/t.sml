(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_T = 
sig
  include IMIL_PUBLIC_TYPES

  val build    : Config.t * Mil.t -> t
  val unBuild  : t -> Mil.t
  val check    : t -> unit
                      
  val getSi : t -> Mil.symbolInfo
  val getEntry : t -> Mil.variable
  val getIncludes : t -> Mil.includeFile Vector.t
  val getExterns : t -> Identifier.VariableSet.t
  val getConfig : t -> Config.t

  val callGraph : t -> MilCallGraph.Graph.t
  val splitCriticalEdges : t -> unit
end

structure IMilT :
sig
  include IMIL_T

  val getStm : t -> Mil.symbolTableManager
end
 = 
struct
  open IMilPublicTypes

  structure M = Mil
  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure VD = IMT.VD
  structure VS = IMT.VS
  structure IMC = IMilCommon
  structure FV = IMC.FV
  structure IM = Identifier.Manager
  structure Func = IMilFunc
  structure Global = IMilGlobal
  structure Use = IMilUse
  structure MCG = MilCallGraph

  type t = IMT.t

  val fail = 
   fn (f, s) => Fail.fail ("imil-t.sml", f, s)


  val getEntry = IMT.tGetEntry
  val getIncludes = IMT.tGetIncludes
  val getExterns = IMT.tGetExterns
  val getStm = IMT.tGetStm
  val getSi = IMT.tGetSi
  val getConfig = IMT.tGetConfig

  fun buildExtern (p, v) = IMilDef.add (p, v, IMT.DefExtern)

  val buildGlobal =
   fn (p, v, m) => 
      let
        val () = 
            case m
             of M.GCode c => 
                let
                  val c = Func.new (p, v, c)
                in ()
                end
              | _ => 
                let
                  val g = Global.build (p, (v, m))
                in ()
                end
      in ()
      end

  val build =
   fn (config, m) => 
      let
        val M.P {includes, externs, symbolTable = milST, entry = milEntry, globals = milGlobals} = m
        val globals = IVD.empty ()
        val defs    = IVD.empty ()
        val uses    = IVD.empty ()
        val funs    = IVD.empty ()
        val milSTM  = IM.fromExistingAll milST
        val p = IMT.P {includes = includes,
                       externs  = externs,
                       nextId   = ref 0,
                       config   = config,
                       stm      = milSTM,
                       entry    = milEntry,
                       iGlobals = globals,
                       defs     = defs,
                       uses     = uses,
                       iFuncs   = funs}
        val () = VS.foreach (externs, fn v => buildExtern (p, v))
        val () =
            Vector.foreach (includes, fn (M.IF {externs, ...}) => VS.foreach (externs, fn v => buildExtern (p, v)))
        val () = VD.foreach (milGlobals, fn (v, g) => buildGlobal (p, v, g))
        val () = Use.markUsed (p, milEntry)
      in p
      end

  val tCheck =
   fn (p, m) => 
      let
        val () = Func.checkAll p
        val s = FV.program (p, m)
        val () = 
            if VS.isEmpty s then ()
            else
              (print "Program has free variables:\n";
               LayoutUtils.printLayout (VS.layout (s, fn v => IMilLayout.var (p, v)));
               print "\n";
               MilLayout.print (IMT.tGetConfig p, m);
               fail ("check", "bad program"))
      in ()
      end
      
  val debugCheck =
   fn (p, m) => 
      IMC.debugDo (p, fn () => tCheck (p, m))
      
  val unBuildGlobals =
   fn globals => List.keepAllMap (globals, fn (v, g) => Global.unBuild g)

  val unBuildFun =
   fn (v, f) => 
      let
        val vg = Func.unBuild f
      in vg
      end

  val unBuildFuns =
   fn funs => List.map (funs, unBuildFun)

  val unBuildSTM =
   fn (defs, stm) => 
      let
        val new = IM.fromExistingNoInfo' stm
        val add =
         fn (v, _) => IM.variableSetInfo (new, v, IM.variableInfo (stm, v))
        val () = IVD.foreach (defs, add)
        val st = IM.finish new
      in st
      end

  val unBuild =
   fn p => 
      let
        val IMT.P {includes, externs, nextId, config, stm, entry, iGlobals, defs, uses, iFuncs} = p
        val g1 = unBuildGlobals (IVD.toList iGlobals)
        val g2 = unBuildFuns (IVD.toList iFuncs)
        val globals = g1 @ g2
        val globals = VD.fromList globals 
        val st = unBuildSTM (defs, stm)
        val m = M.P {includes = includes, externs = externs, globals = globals, symbolTable = st, entry = entry}
        val () = debugCheck (p, m)
      in m
      end
  
  val check =
   fn p => 
      let
        val m = unBuild p
        val () = tCheck (p, m)
        val () = MilCheck.program (getConfig p, m)
      in ()
      end

  val layout = IMilLayout.t

  val print = LayoutUtils.printLayout o IMilLayout.t
  val printIFunc = 
      fn (imil, iFunc) => 
         MilLayout.printGlobal
           (IMT.tGetConfig imil, 
            IMT.tGetSi imil, 
            unBuildFun (Func.getFName (imil, iFunc),
                        iFunc))

   
  val splitCriticalEdges = 
   fn imil => List.foreach (IMilEnumerate.T.funcs imil, fn f => IMilFunc.splitCriticalEdges (imil, f))

  val callGraph =
   fn imil => 
      let
        val config = getConfig imil
        val si = getSi imil
        val p  = unBuild imil
        val cg = MCG.program (config, si, p)
        val graph = MCG.Graph.make cg
      in graph
      end
            
end
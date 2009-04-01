(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, July 2007 *)

signature MIL_SIMPLIFY =
sig
  val simplify : PassData.t * IMil.t * IMil.WorkSet.ws -> unit
  val program : PassData.t * IMil.t -> unit
  val pass : (BothMil.t, BothMil.t) Pass.t
end;

signature REDUCE = 
sig
  type t
  val reduce : (PassData.t * IMil.t * IMil.WorkSet.ws) * t -> IMil.item List.t option
end

structure MilSimplify :> MIL_SIMPLIFY = 
struct

  val passname = "MilSimplify"

  structure M = Mil
  structure MU = MilUtils
  structure I = IMil
  structure IInstr = I.IInstr
  structure IGlobal = I.IGlobal
  structure IFunc = I.IFunc
  structure Var = I.Var
  structure Use = I.Use
  structure Def = I.Def
  structure Item = I.Item
  structure Enumerate = I.Enumerate
  structure WS = I.WorkSet
  structure MCG = MilCallGraph
  structure IPLG = ImpPolyLabeledGraph
  structure IVD = Identifier.ImpVariableDict
  structure PD = PassData
  structure SD = StringDict

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 0
                          end)

  val <- = Try.<-
  val <@ = Try.<@
  val <! = Try.<!
  val << = Try.<<
  val oo = Try.oo
  val @@ = Utils.Function.@@
  infix 3 << @@ oo <!

 (* Reports a fail message and exit the program.
  * param f: The function name.
  * param s: the messagse. *)
  val fail = 
   fn (f, m) => Fail.fail ("simplify.sml", f, m)

 (* Fail and reports a message if assert is false.
  * param f: The function name.
  * param s: the messagse. *) 
  val assert = 
   fn (f, m, assert) => if assert then () else fail (f, m)

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname ^ ":debug", "Debug the simplifier according to debug level")

  val mkDebug : string * string * int -> (Config.Debug.debug * (PassData.t -> bool)) = 
   fn (tag, description, level) =>
      let
        val (debugD, debug) = 
            Config.Debug.mk (passname ^ ":" ^ tag, description)
        val debug = 
         fn d => 
            let
              val config = PD.getConfig d
            in debug config orelse 
               (debugPass config andalso Config.debugLevel (config, passname) >= level)
            end
      in (debugD, debug)
      end

  val (checkPhaseD, checkPhase) =
      mkDebug ("check-phase", "Check IR between each phase", 0)

  val (showPhaseD, showPhase) =
      mkDebug ("show-phase", "Show IR between each phase", 1)

  val (showEachD, showEach) = 
      mkDebug ("show", "Show each reduction attempt", 1)

  val (checkIrD, checkIr) =
      mkDebug ("check-ir", "Check IR after each successful reduction", 2)

  val (showIrD, showIr) =
      mkDebug ("show-ir", "Show IR after each successful reduction", 2)

  val debugs = [debugPassD, showEachD, showIrD, checkIrD, showPhaseD, checkPhaseD]

  val mkLogFeature : string * string * int -> (Config.Feature.feature * (PassData.t -> bool)) = 
   fn (tag, description, level) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk (passname ^ ":" ^ tag, description)
        val feature = 
         fn d => 
            let
              val config = PD.getConfig d
            in feature config orelse 
               (Config.logLevel (config, passname) >= level)
            end
      in (featureD, feature)
      end

  val (statPhaseF, statPhase) = 
      mkLogFeature ("stat-phase", "Show stats between each phase", 2)

  val mkFeature : string * string -> (Config.Feature.feature * (PassData.t -> bool)) = 
   fn (tag, description) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk (passname ^ ":" ^ tag, description)
        val feature = 
         fn d => feature (PD.getConfig d)
      in (featureD, feature)
      end

  val (noIterateF, noIterate) = 
      mkFeature ("no-iterate", "Don't iterate simplification and cfg simplification")

  val (skipUnreachableF, skipUnreachable) = 
      mkFeature ("skip-unreachable", "Skip unreachable object elimination")

  val (skipSimplifyF, skipSimplify) = 
      mkFeature ("skip-simplify", "Skip simplification")

  val (skipCfgF, skipCfg) = 
      mkFeature ("skip-cfg-simplify", "Skip cfg simplification")

  val (skipEscapeF, skipEscape) = 
      mkFeature ("skip-escape", "Skip escape analysis")

  val (skipRecursiveF, skipRecursive) = 
      mkFeature ("skip-recursive", "Skip recursive analysis")

  val features = [statPhaseF, noIterateF, skipUnreachableF, skipSimplifyF, skipCfgF, skipEscapeF, skipRecursiveF]

  structure Click = 
  struct
    val localNms =
        [("BlockKill",       "Blocks killed"                  ),
         ("BlockMerge",      "Blocks merged"                  ),
         ("CallDirect",      "Calls made direct"              ),
         ("CaseCollapse",    "Cases collapsed"                ),
         ("CaseReduce",      "Cases reduced"                  ),
         ("CopyProp",        "Copies/Constants propagated"    ),
         ("CutReduce",       "Cuts reduced"                   ),
         ("DoubleArith",     "Double arith ops reduced"       ),
         ("DoubleCmp",       "Double cmp ops reduced"         ),
         ("EvalDirect",      "Evals made direct"              ),
         ("FloatArith",      "Float arith ops reduced"        ),
         ("FloatCmp",        "Float cmp ops reduced"          ),
         ("FunctionGetFv",   "Function fv projections reduced"),
         ("IdxGet",          "Index Get operations reduced"   ),
         ("InlineOnce",      "Functions inlined once"         ),
         ("TInlineOnce",     "Thunks inlined once"            ),
         ("LoopFlatten",     "Loop tuple args flattened"      ),
         ("NumConv",         "Numeric conversions reduced"    ),
         ("PhiReduce",       "Phi transfers reduced"          ),
         ("Prim",            "Primitives reduced"             ),


         ("SwitchToSetCond", "Switches converted to SetCond"  ),
         ("SwitchETAReduce", "Case switches converted to Goto"),
         ("ThunkGetFv",      "Thunk fv projections reduced"   ),
         ("ThunkVal",        "ThunkValues reduced"            ),
         ("ThunkToThunkVal", "Thunks made ThunkValues"        ),
         ("TupleSub",        "Tuple subscripts reduced"       )

        ]

    val localNms = 
        [
         ("DCE",              "Dead instrs/globals eliminated" ),
         ("Globalized",       "Objects globalized"             ),
         ("PFunctionGetFv",   "Free var projections reduced"   ),
         ("PFunctionInitCode","Closure code ptrs killed"       ), 
         ("PSetGet",          "SetGet ops reduced"             ),
         ("PSetNewEta",       "SetNew ops eta reduced"         ),
         ("PSumProj",         "Sum projections reduced"        ),
         ("PSetQuery",        "SetQuery ops reduced"           ),
         ("PSetCond",         "SetCond ops reduced"            ),
         ("ThunkGetValue",    "ThunkGetValue ops reduced"      ),
         ("ThunkSpawnFX",     "Spawn fx pruned"                ),
         ("ThunkValueEta",    "ThunkValues eta reduced"        ),
         ("Unreachable",      "Unreachable objects killed"     )
        ]
    val globalNm = 
     fn s => passname ^ ":" ^ s

    val nmMap = 
        let
          val check = 
           fn ((nm, info), d) => 
              if SD.contains (d, nm) then
                fail ("LocalStats", "Duplicate stat")
              else
                SD.insert (d, nm, globalNm nm)
          val _ = List.fold (localNms, SD.empty, check)
        in ()
        end

    val click = 
     fn (pd, s) => PD.click (pd, globalNm s)

    val clicker = 
     fn s => 
        let
          val nm = globalNm s
        in fn pd => PD.click (pd, nm)
        end

    val stats = List.map (localNms, fn (nm, info) => (globalNm nm, info))

    val dce = clicker "DCE"
    val unreachable = clicker "Unreachable"
    val globalized = clicker "Globalized"
    val pSumProj = clicker  "PSumProj"
    val pSetQuery = clicker  "PSetQuery"
    val pSetCond = clicker  "PSetCond"
    val pSetGet = clicker  "PSetGet"
    val pSetNewEta = clicker "PSetNewEta"
    val thunkSpawnFx = clicker "ThunkSpawnFX"
    val pFunctionInitCode = clicker "PFunctionInitCode"
    val pFunctionGetFv = clicker "PFunctionGetFv"
    val thunkValueEta = clicker "ThunkValueEta"
    val thunkGetValue = clicker "ThunkGetValue"

    val wrap : (PD.t -> unit) * ((PD.t * I.t * WS.ws) * 'a -> 'b option) 
               -> ((PD.t * I.t * WS.ws) * 'a -> 'b option) =
     fn (click, reduce) => 
     fn args => 
        let
          val r = reduce args
          val () = if isSome r then click (#1 (#1 args)) else ()
        in r
        end
                           
  end   (*  structure Click *)


  val try = 
   fn (clicker, reduce) => Click.wrap (clicker, Try.lift reduce)

  
  structure FuncR : REDUCE = 
  struct
    type t = I.iFunc

    val reduce = 
     fn _ => NONE

  end (* structure FuncR *)

  structure GlobalR : REDUCE =
  struct
    type t = I.iGlobal 

    val reduce = 
     fn _ => NONE

  end (* structure GlobalR *)

  structure TransferR : REDUCE =
  struct
    type t = I.iInstr * M.transfer

    val reduce = 
     fn _ => NONE
  end (* structure TransferR *)

  structure LabelR : REDUCE =
  struct
    type t = I.iInstr * (M.label * M.variable Vector.t)

    val reduce = 
     fn _ => NONE
  end (* structure LabelR *)

  structure InstructionR : REDUCE =
  struct
    type t = I.iInstr * M.instruction

    val globalize = 
        try
        (Click.globalized,
         (fn ((d, imil, ws), (i, M.I {dest, rhs})) => 
             let
               val add = 
                fn (v, g) =>
                   let
                     val gv = Var.related (imil, v, "", Var.typ (imil, v), true)
                     val () = IInstr.delete (imil, i)
                     val g = IGlobal.new (imil, gv, g)
                     val () = WS.addGlobal (ws, g)
                     val () = Use.replaceUses (imil, v, M.SVariable gv)
                   in ()
                   end
                   
               val const = 
                fn c => 
                   (case c
                     of M.SConstant c => true
                      | M.SVariable v => Var.isGlobal (imil, v))
                   
               val consts = 
                fn ops => Vector.forall (ops, const)
                                
               val () = 
                   (case rhs
                     of M.RhsSimple op1 => 
                        if const op1 then 
                          add (<- dest, M.GSimple op1)
                        else 
                          Try.fail ()
                      | M.RhsTuple {vtDesc, inits} =>
                        if MU.VTableDescriptor.immutable vtDesc andalso
                           (not (MU.VTableDescriptor.hasArray vtDesc)) andalso
                           Vector.forall (inits, const) 
                        then
                          add (<- dest, M.GTuple {vtDesc = vtDesc, inits = inits})
                        else
                          Try.fail ()
                      | M.RhsThunkValue {typ, thunk, ofVal} =>
                        if const ofVal then
                          add (<@ Utils.Option.atMostOneOf (thunk, dest), 
                               M.GThunkValue {typ = typ, ofVal = ofVal})
                        else
                          Try.fail ()
                      | M.RhsPFunctionInit {cls, code, fvs} =>
                        if Option.forall (code, fn v => Var.isGlobal (imil, v)) andalso 
                           Vector.isEmpty fvs 
                        then
                          add (<@ Utils.Option.atMostOneOf (cls, dest), M.GPFunction code)
                        else
                          Try.fail ()
                      | M.RhsPSetNew op1 => 
                        if const op1 then
                          add (<- dest, M.GPSet op1)
                        else
                          Try.fail ()
                      | M.RhsPSum {tag, typ, ofVal} => 
                        if const ofVal then
                          add (<- dest, M.GPSum {tag = tag, typ = typ, ofVal = ofVal})
                        else
                          Try.fail ()
                      | _ => Try.fail ())
             in []
             end))

   val getUniqueInit =
       Try.lift
       (fn (imil, v) =>
           let
             val {inits, others} = Use.splitUses (imil, v)
             val init = <@ Use.toInstruction o Try.V.singleton @@ inits
           in init
           end)

   val getClosureOrThunkParameters = 
    Try.lift
      (fn (imil, c) =>
          let
            val (v, vs) = 
                case IMil.Def.get (imil, c)
                 of IMil.DefParameter iFunc =>
                    (case IFunc.getCallConv (imil, iFunc)
                      of M.CcClosure {cls, fvs} => (cls, fvs)
                       | M.CcThunk {thunk, fvs} => (thunk, fvs)
                       | _ => Try.fail ())
                  | _ => Try.fail ()
            val () = Try.require (v = c)
            val opers = Vector.map (vs, M.SVariable)
          in opers
          end)

   val pruneEffects =
       Try.lift
         (fn (imil, codeptr, fx1) =>
             let
               val iFunc = IFunc.getIFuncByName (imil, codeptr)
               val fx2 = IFunc.getEffects (imil, iFunc)
               val () = Try.require (not (Effect.subset (fx1, fx2)))
               val fx = Effect.intersection (fx1, fx2)
             in fx
             end)

    val template = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, _)) =>
              let
              in []
              end
        in try (Click.pSetCond, f)
        end

    val simple = fn (state, (i, dest, r)) => NONE
    val prim = fn (state, (i, dest, r)) => NONE
    val tuple = fn (state, (i, dest, r)) => NONE
    val tupleSub = fn (state, (i, dest, r)) => NONE
    val tupleSet = fn (state, (i, dest, r)) => NONE
    val tupleInited = fn (state, (i, dest, r)) => NONE
    val idxGet = fn (state, (i, dest, r)) => NONE
    val cont = fn (state, (i, dest, r)) => NONE
    val objectGetKind = fn (state, (i, dest, r)) => NONE
    val thunkMk = fn (state, (i, dest, r)) => NONE
    val thunkInit = fn (state, (i, dest, r)) => NONE
    val thunkGetFv = fn (state, (i, dest, r)) => NONE
    val thunkValue = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {thunk, ofVal, ...})) =>
              let
                val dv = <@ Utils.Option.atMostOneOf (dest, thunk)
                val vv = <@ MU.Simple.Dec.sVariable ofVal
                val {callee, ret, ...} = <@ MU.Transfer.Dec.tInterProc <! Def.toTransfer o Def.get @@ (imil, vv)
                val vv' = Try.V.singleton o #rets <! MU.Return.Dec.rNormal @@ ret
                val () = assert ("thunkValue", "Strange def", vv = vv')
                val thunk' = MU.Eval.thunk o #eval <! MU.InterProc.Dec.ipEval @@ callee
                val () = Use.replaceUses (imil, dv, M.SVariable thunk')
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.thunkValueEta, f)
        end

    val thunkGetValue = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {typ, thunk})) =>
              let
                val dv = <- dest
                val ofVal = 
                    (case <@ Def.toMilDef o Def.get @@ (imil, thunk)
                      of MU.Def.DefRhs (M.RhsThunkValue {ofVal, ...})  => ofVal
                       | MU.Def.DefGlobal (M.GThunkValue {ofVal, ...}) => ofVal
                       | MU.Def.DefRhs (M.RhsThunkMk _) =>
                         #ofVal <! MU.Rhs.Dec.rhsThunkValue o MU.Instruction.rhs <! getUniqueInit @@ (imil, thunk)
                       | _ => Try.fail ())
                val () = Use.replaceUses (imil, dv, ofVal)
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.thunkGetValue, f)
        end

    val thunkSpawn = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {thunk, fx, typ})) =>
              let
                val code = <@ #code <! MU.Rhs.Dec.rhsThunkInit <! Def.toRhs o Def.get @@ (imil, thunk)
                val fx = <@ pruneEffects (imil, code, fx)
                val r = {thunk = thunk, fx = fx, typ = typ}
                val rhs = M.RhsThunkSpawn r
                val mi = M.I {dest = dest, rhs = rhs}
                val () = IInstr.replaceInstruction (imil, i, mi)
              in [I.ItemInstr i]
              end
        in try (Click.thunkSpawnFx, f)
        end

    val pFunctionMk = fn (state, (i, dest, r)) => NONE

    val pFunctionInit = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {cls, code, fvs})) =>
              let
                 val fcode = <- code
                 val iFunc = IFunc.getIFuncByName (imil, fcode)
                 val () = Try.require (not (IFunc.getEscapes (imil, iFunc)))
                 val rhs = M.RhsPFunctionInit {cls = cls, code = NONE, fvs = fvs}
                 val mi = M.I {dest = dest, rhs = rhs}
                 val () = IInstr.replaceInstruction (imil, i, mi)
              in [I.ItemInstr i]
              end
        in try (Click.pFunctionInitCode, f)
        end

    val pFunctionGetFv = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {cls, idx, ...})) =>
              let
                val v = <- dest
                val fv =
                    case getUniqueInit (imil, cls)
                     of SOME init => 
                        let
                          val {fvs, ...} = <@ MU.Rhs.Dec.rhsPFunctionInit o MU.Instruction.rhs @@ init
                          val (_, fv) = Try.V.sub (fvs, idx)
                        in fv
                        end
                      | NONE => 
                        let
                          val fvs = <@ getClosureOrThunkParameters (imil, cls)
                          val fv = Try.V.sub (fvs, idx)
                        in fv
                        end
                val () = Use.replaceUses (imil, v, fv)
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.pFunctionGetFv, f)
        end

    val pSetNew = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, p)) =>
              let
                val dv = <- dest
                val v = <@ MU.Simple.Dec.sVariable p
                val setv =   
                    (case <@ Def.toMilDef o Def.get @@ (imil, v)
                      of MU.Def.DefRhs (M.RhsPSetGet setv) => setv
                       | _ => Try.fail ())
                val p = M.SVariable setv
                val () = Use.replaceUses (imil, dv, p)
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.pSetNewEta, f)
        end

    val pSetGet = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, v)) =>
              let
                val dv = <- dest
                val c = 
                    (case <@ Def.toMilDef o Def.get @@ (imil, v)
                      of MU.Def.DefRhs (M.RhsPSetNew c)             => c
                       | MU.Def.DefRhs (M.RhsPSetCond {ofVal, ...}) => ofVal
                       | MU.Def.DefGlobal (M.GPSet c)               => c
                       | _ => Try.fail ())
                val () = Use.replaceUses (imil, dv, c)
                val () = IInstr.delete (imil, i)
              in []
              end 
         in try (Click.pSetGet, f)
         end

    val pSetCond = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {bool, ofVal})) =>
              let
                val c = <@ MU.Simple.Dec.sConstant ofVal
                val rhs = 
                    (case MU.Bool.toBool (PD.getConfig d, c)
                      of SOME true => M.RhsPSetNew ofVal
                       | SOME false => M.RhsSimple (M.SConstant (M.COptionSetEmpty))
                       | NONE => Try.fail (Chat.warn0 (d, "Unexpected boolean constant")))
                val mi = M.I {dest = dest,
                              rhs = rhs}
                val () = IInstr.replaceInstruction (imil, i, mi)
              in [I.ItemInstr i]
              end
        in try (Click.pSetCond, f)
        end

    val pSetQuery = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, r)) => 
              let
                val T = M.SConstant (MU.Bool.T (PD.getConfig d))
                val F = M.SConstant (MU.Bool.F (PD.getConfig d))

                val v = <- dest
                val b = 
                    case r
                     of M.SConstant (M.COptionSetEmpty) => F
                      | M.SVariable v => 
                        (case <@ Def.toMilDef o Def.get @@ (imil, v)
                          of MU.Def.DefRhs (M.RhsPSetNew _)            => T
                           | MU.Def.DefRhs (M.RhsPSetCond {bool, ...}) => bool
                           | MU.Def.DefGlobal (M.GPSet op2)            => T
                           | _ => Try.fail ())
                      | _ => Try.fail ()
                val () = Use.replaceUses (imil, v, b)
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.pSetQuery, f)
        end

    val pSum = fn (state, (i, dest, r)) => NONE

    val pSumProj = 
        let
          val f = 
           fn ((d, imil, ws), (i, dest, {typ, sum, tag})) => 
              let
                val v = <- dest
                val {ofVal, ...} = <@ MU.Def.Out.pSum <! Def.toMilDef o Def.get @@ (imil, v)
                val () = Use.replaceUses (imil, v, ofVal)
                val () = IInstr.delete (imil, i)
              in []
              end
        in try (Click.pSumProj, f)
        end

    val simplify = 
     fn (state, (i, M.I {dest, rhs})) =>
        let
          val r = 
              case rhs
               of M.RhsSimple r         => simple (state, (i, dest, r))
                | M.RhsPrim r           => prim (state, (i, dest, r))
                | M.RhsTuple r          => tuple (state, (i, dest, r))
                | M.RhsTupleSub r       => tupleSub (state, (i, dest, r))
                | M.RhsTupleSet r       => tupleSet (state, (i, dest, r))
                | M.RhsTupleInited r    => tupleInited (state, (i, dest, r))
                | M.RhsIdxGet r         => idxGet (state, (i, dest, r))
                | M.RhsCont r           => cont (state, (i, dest, r))
                | M.RhsObjectGetKind r  => objectGetKind (state, (i, dest, r))
                | M.RhsThunkMk r        => thunkMk (state, (i, dest, r))
                | M.RhsThunkInit r      => thunkInit (state, (i, dest, r))
	        | M.RhsThunkGetFv r     => thunkGetFv (state, (i, dest, r))
                | M.RhsThunkValue r     => thunkValue (state, (i, dest, r))
                | M.RhsThunkGetValue r  => thunkGetValue (state, (i, dest, r))
                | M.RhsThunkSpawn r     => thunkSpawn (state, (i, dest, r))
                | M.RhsPFunctionMk r    => pFunctionMk (state, (i, dest, r))
                | M.RhsPFunctionInit r  => pFunctionInit (state, (i, dest, r))
                | M.RhsPFunctionGetFv r => pFunctionGetFv (state, (i, dest, r))
                | M.RhsPSetNew r        => pSetNew (state, (i, dest, r))
                | M.RhsPSetGet r        => pSetGet (state, (i, dest, r))
                | M.RhsPSetCond r       => pSetCond (state, (i, dest, r))
                | M.RhsPSetQuery r      => pSetQuery (state, (i, dest, r))
                | M.RhsPSum r           => pSum (state, (i, dest, r))
                | M.RhsPSumProj r       => pSumProj (state, (i, dest, r))
        in r
        end

    val reduce = Try.or (globalize, simplify)
  end (* structure InstructionR *)

  structure InstrR : REDUCE =
  struct
    type t = I.iInstr

    val reduce = 
     fn (s as (d, imil, ws), i) => 
        let
          val t = 
             case IInstr.getMil (imil, i)
              of IMil.MDead       => Try.failure ()
               | IMil.MTransfer t => TransferR.reduce (s, (i, t))
               | IMil.MLabel l    => LabelR.reduce (s, (i, l))
               | IMil.MInstr mi   => InstructionR.reduce (s, (i, mi))
       in t
       end

  end (* structure InstrR *)

  structure ItemR = 
  struct

    val reduce = 
     fn (d, imil, ws, i, uses) =>
       let
         val doOne = 
          fn (getUsedBy, reduce) =>
          fn obj => 
             let
               val usedByI = getUsedBy (imil, obj)
               val res = 
                   case reduce ((d, imil, ws), obj)
                    of SOME is => 
                       let
                         val () = WS.addItems (ws, usedByI)
                         val () = WS.addUses (ws, uses)
                         val () = List.foreach (is, fn i => WS.addItem (ws, i))
                       in true
                       end
                     | _ => false
             in res
             end

         val res = 
             case i
              of IMil.ItemGlobal g    => doOne (IGlobal.getUsedBy, GlobalR.reduce) g
               | IMil.ItemInstr i  => doOne (IInstr.getUsedBy, InstrR.reduce) i
               | IMil.ItemFunc f   => doOne (IFunc.getUsedBy, FuncR.reduce) f
       in res
       end


  end (* structure ItemR *)


  val rec killItem = 
   fn (d, imil, ws, i, inits) =>
       let
         val usedByI = Item.getUsedBy (imil, i)
         val () = Vector.foreach (inits, 
                                  (fn u => killUse (d, imil, ws, u)))
         val () = Click.dce d
         val () = Item.delete (imil, i)
         val () = WS.addItems (ws, usedByI)
       in ()
       end
   and rec killUse = 
    fn (d, imil, ws, u) =>
       let
         val inits = Vector.new0 ()
         val () = (case u
                    of I.UseInstr i => 
                       killItem (d, imil, ws, I.ItemInstr i, inits) 
                     | I.UseGlobal g => 
                       killItem (d, imil, ws, I.ItemGlobal g, inits)
                     | I.Used => ())
       in ()
       end

  val deadCode = 
   fn (d, imil, ws, i, uses) => 
      let
        val {inits, others} = Item.splitUses' (imil, i, uses)
        val dead = Vector.isEmpty others
        val ok = Effect.subset(Item.fx (imil, i), Effect.ReadOnly)
        val kill = dead andalso ok
        val () = if kill then killItem (d, imil, ws, i, inits) else ()
      in kill
      end

  fun optimizeItem (d, imil, ws, i) = 
       let
         val () = if showEach d then (print "R: ";Item.print (imil, i)) else ()

         val uses = Item.getUses (imil, i)

         val reduced = deadCode (d, imil, ws, i, uses) orelse ItemR.reduce (d, imil, ws, i, uses)

         val () = if reduced andalso showEach d then (print "-> ";Item.print (imil, i)) else ()
       in reduced
       end


  val postReduction = 
   fn (d, imil) => 
      let
        val () = if checkIr d then IMil.T.check imil else ()
        val () = if showIr d then MilLayout.printGlobalsOnly (PD.getConfig d, IMil.T.unBuild imil) else ()
      in ()
      end

  val simplify = 
   fn (d, imil, ws) => 
      let
        val rec loop = 
         fn () =>
            case WS.chooseWork ws
             of SOME i => 
                let
                  val () = 
                      if optimizeItem (d, imil, ws, i) then postReduction (d, imil) else ()
                in loop ()
                end
              | NONE => ()
      in loop ()
      end



  (* Eliminate global objects and functions that are not reachable from the entry point.
   * Make a graph with a node for each global object or function, and with edges to each 
   * global object or function from every other global object or function which contains a use
   * of it.  All the nodes that are unreachable in this graph (starting at the entry function)
   * are dead and can be eliminated.
   *)
  val unreachableCode = 
   fn (d, imil) =>
      let
        datatype global = Func of I.iFunc | Object of I.iGlobal
        val graph = IPLG.new ()
        val nodes = IVD.empty ()
        val varToNode = fn v => Option.valOf (IVD.lookup(nodes, v))
        val nodeToGlobal = IPLG.Node.getLabel
        val useToNode = 
            fn u => 
               (case u
                 of I.Used => NONE
                  | I.UseInstr i => 
                    let
                      val iFunc = IInstr.getIFunc (imil, i)
                      val fname = IFunc.getFName (imil, iFunc)
                      val node = varToNode fname
                    in SOME node
                    end
                  | I.UseGlobal g => 
                    let
                      val v = IGlobal.getVar (imil, g)
                      val node = varToNode v
                    in SOME node
                    end)

        val fNodes = 
            let
              val iFuncs = Enumerate.T.funcs imil
              val addIFuncNode = 
               fn iFunc => 
                  let
                    val v = IFunc.getFName (imil, iFunc)
                    val n = IPLG.newNode (graph, (Func iFunc))
                    val () = IVD.insert (nodes, v, n)
                    val uses = IFunc.getUses (imil, iFunc)
                  in (n, uses)
                  end
              val fNodes = List.map (iFuncs, addIFuncNode)
            in fNodes
            end
        val gNodes = 
            let
              val objects = Enumerate.T.globals imil
              val addGlobalNode = 
               fn g => 
                  case IGlobal.getMil (imil, g)
                   of I.GDead => NONE
                    | I.GGlobal (v, _) => 
                      let
                        val n = IPLG.newNode (graph, (Object g))
                        val () = IVD.insert (nodes, v, n)
                        val uses = IGlobal.getUses (imil, g)
                      in SOME (n, uses)
                      end
              val gNodes = List.keepAllMap (objects, addGlobalNode)
            in gNodes
            end
        val () = 
            let
              val nodes = fNodes@gNodes
              val addEdges = 
               fn (n1, uses) => 
                  let
                    val addEdge = 
                     fn u => 
                        case useToNode u
                         of SOME n2 => ignore (IPLG.addEdge(graph, n2, n1, ()))
                          | NONE => ()
                    val () = Vector.foreach (uses, addEdge)
                  in ()
                  end
              val () = List.foreach (nodes, addEdges)
            in ()
            end
        val () = 
            let
              val entry = I.T.getEntry imil
              val dead = IPLG.unreachable (graph, varToNode entry)
              val killNode = 
               fn n => 
                  let
                    val () = Click.unreachable d
                    val () =
                        (case nodeToGlobal n
                          of Func f => IFunc.delete (imil, f)
                           | Object g => IGlobal.delete (imil, g))
                  in ()
                  end
              val () = List.foreach (dead, killNode)
            in ()
            end
      in ()
      end

  val postPhase = 
   fn (d, imil) => 
      let
        val () = if statPhase d then Stats.report (PD.getStats d) else ()
        val () = if checkPhase d then IMil.T.check imil else ()
        val () = if showPhase d then MilLayout.printGlobalsOnly (PD.getConfig d, IMil.T.unBuild imil) else ()
      in ()
      end

  val doPhase = 
   fn (skip, f, name) =>
   fn (d, imil) => 
      if skip d then
        Chat.log1 (d, "Skipping "^name)
      else
        let
          val () = Chat.log1 (d, "Doing "^name)
          val () = f (d, imil)
          val () = Chat.log1 (d, "Done with "^name)
          val () = postPhase (d, imil)
        in ()
        end

  val skip = 
   fn name => 
   fn (d, imil) => 
        Chat.log1 (d, "Skipping "^name)


  val trimCfgs = fn (d, imil, ws) => ()

  val doUnreachable = doPhase (skipUnreachable, unreachableCode, "unreachable object elimination")
  val doSimplify = 
   fn ws => doPhase (skipSimplify, fn (d, imil) => simplify (d, imil, ws), "simplification")
(*val doCfgSimplify = 
   fn ws => doPhase (skipCfg, fn (d, imil) => trimCfgs (d, imil, ws), "cfg simplification")
  val doEscape = doPhase (skipEscape, SimpleEscape.optimize, "closure escape analysis")
  val doRecursive = doPhase (skipRecursive, analyizeRecursive, "recursive function analysis") *)

  val doCfgSimplify = fn ws => skip "cfg simplification"
  val doEscape = skip "closure escape analysis"
  val doRecursive = skip "recursive function analysis"
      
  val doIterate = 
   fn (d, imil) => 
      let
        val ws = WS.new ()
        val () = WS.addAll (ws, imil)
        val step = 
         fn () =>
            let
              val () = doSimplify ws (d, imil)
              val () = doCfgSimplify ws (d, imil)
            in ()
            end

        val () = step ()
        val () = 
            if noIterate d then 
              step () 
            else 
              while WS.hasWork ws do step ()
      in ()
      end

  val optimize = 
   fn (d, imil) =>
      let
        val () = doUnreachable (d, imil)
        val () = doIterate (d, imil)
        val () = doEscape (d, imil)
        val () = doRecursive (d, imil)
      in ()
      end

  val program = 
   fn (d, imil) =>
      let
        val () = optimize (d, imil)
        val () = PD.report (d, passname)
      in ()
      end

  val stats = Click.stats (*@ MilFunKnown.stats @ SimpleEscape.stats*)
  val description =
      {name        = passname,
       description = "Mil simplifier",
       inIr        = BothMil.irHelpers,
       outIr       = BothMil.irHelpers,
       mustBeAfter = [],
       stats       = Click.stats}

  val associates = {controls  = [],
                    debugs    = debugs (*@ MilFunKnown.debugs*),
                    features  = features,
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates,
                      BothMil.mkIMilPass (program o Utils.flip2))

end

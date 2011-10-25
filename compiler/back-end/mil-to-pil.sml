(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Convert Mil into C/Pillar *)

signature MIL_TO_PIL =
sig
  val instrumentAllocationSites : Config.t -> bool
  val backendYields : Config.t -> bool
  val lightweightThunks : Config.t -> bool
  val assertSmallInts : Config.t -> bool  
  val features : Config.Feature.feature list
  val program : PassData.t * string * Mil.t -> Layout.t
end;

structure MilToPil :> MIL_TO_PIL =
struct

  val passname = "Outputter"

  val fail = fn (f, msg) => Fail.fail (passname, f, msg)
  fun unimplemented (f, msg) = Fail.unimplemented (passname, f, msg)

  structure L = Layout
  structure I = Identifier
  structure IM = I.Manager
  structure VS = I.VariableSet
  structure VD = I.VariableDict
  structure ND = I.NameDict
  structure LD = I.LabelDict
  structure LLS = I.LabelLabelSet
  structure LS = I.LabelSet
  structure M = Mil
  structure MU = MilUtils
  structure MSTM = MU.SymbolTableManager
  structure MTT = MilType.Typer
  structure MFV = MilFreeVars
  structure POM = PObjectModelLow

  (*** The pass environment ***)

  datatype env = E of {config: Config.t, gdefs : M.globals, func: M.code option, backEdges : LLS.t option}

  fun newEnv (config, gdefs) = E {config = config, func = NONE, gdefs = gdefs, backEdges = NONE}

  fun getConfig (E {config, ...}) = config

  fun typToFieldKind (e, t) =
      case t
       of M.TNone => M.FkRef
        | _ => MU.FieldKind.fromTyp (getConfig e, t)

  fun outputKind env = Config.output (getConfig env)
  fun parStyle env = Config.parStyle (getConfig env)

  fun vtTagOnly          env = #tagOnly         (Config.gc (getConfig env))
  fun vtReg              env = #registerVtables (Config.gc (getConfig env))
  fun gcGRoots           env = #reportRoots     (Config.gc (getConfig env))
  fun gcGRootsInGlobals  env = #rootsInGlobals  (Config.gc (getConfig env))
  fun gcGlobals          env = #reportGlobals   (Config.gc (getConfig env))

  fun rewriteThunks (env, conv) =
      case conv of M.CcThunk {thunk, ...} => SOME thunk | _ => NONE

  fun interceptCuts env = parStyle env = Config.PAll

  fun getGlobalDef (E {gdefs, ...}, v) = MU.Globals.get (gdefs, v)

  fun getFunc (E {func, ...}) = Option.valOf func

  fun getBackEdges (E {backEdges, ...}) = Option.valOf backEdges

  fun enterFunction (E {config, func, gdefs, backEdges}, f, be) =
      E {config = config, func = SOME f, gdefs = gdefs, backEdges = SOME be}

  (*** Build structures ***)

  structure RT = Runtime

  structure Chat = ChatF(type env = env
                         val extract = getConfig
                         val name = passname
                         val indent = 0)

  fun notCoreMil (env, f, msg) =
      fail (f, "not core Mil: " ^ msg)

  (*** Vtable Info ***)

  (* This really belongs below in the VT module, but we need it memoised in
   * the state, and SML's module system can't handle this.
   *)

  (* vtInfo contains all information needed about a vtable.
   * Currently this is:
   *   The P object kind.
   *   The size of the fixed part of the object.
   *   For each word offset in the fixed part of the object, whether it is
   *     a reference or not.
   *   An option variable-sized portion description consisting of:
   *     The size of the elements of the variable portion.
   *     The offset in the fixed portion of the field containing the
   *       number of elements in the variable portion.
   *     Whether the variable portion is a reference or not (we currently
   *       support only one reference field).
   *   A boolean for whether the object is definitely refless and never escapes
   *     before it is initialised.
   *)

  datatype vtMutability =
           VtmAlwaysMutable | VtmCreatedMutable | VtmAlwaysImmutable

  datatype vtInfo = Vti of {
           name      : string,
           tag       : M.pObjKind,
           fixedSize : int,
           fixedRefs : bool Vector.t,
           array     : {size : int, offset : int, isRef : bool} option,
           mut       : vtMutability
  }

  (* An order on vtInfo *)
  local
    open Compare
    fun arrayCompare (a1, a2) =
        option (rec3 (#size, Int.compare,
                      #offset, Int.compare,
                      #isRef, Bool.compare))
               (a1, a2)
    fun vtMutabilityCompare (vtm1, vtm2) =
        case (vtm1, vtm2)
         of (VtmAlwaysMutable,   VtmAlwaysMutable  ) => EQUAL
          | (VtmAlwaysMutable,   _                 ) => LESS
          | (_,                  VtmAlwaysMutable  ) => GREATER
          | (VtmCreatedMutable,  VtmCreatedMutable ) => EQUAL
          | (VtmCreatedMutable,  _                 ) => LESS
          | (_,                  VtmCreatedMutable ) => GREATER
          | (VtmAlwaysImmutable, VtmAlwaysImmutable) => EQUAL
  in
  fun vtInfoCompare (Vti x1, Vti x2) =
      rec6 (#name, String.compare,
            #tag, MU.PObjKind.compare,
            #fixedSize, Int.compare,
            #fixedRefs, vector Bool.compare,
            #array, arrayCompare,
            #mut, vtMutabilityCompare)
           (x1, x2)
  end

  local
    structure Ord =
    struct
      type t = vtInfo
      val compare = vtInfoCompare
    end
  in
  structure VtiD = DictF(Ord)
  end

  (*** The pass state ***)

  (* We generate a global for each name that appears in the program, the
   * names dictionary records for each name the variable its global is in,
   * if one has been generated so far.
   *
   * For vtables we generate one for each different each possibly combination
   * of information we might store in it or register with the GC.  This is
   * memoised in the state by recording for this information the variable
   * bound to the vtable for the information if it has been generated already.
   *
   * For various things: names, vtables, etc.; we generate globals, these are
   * held in xtrGlbs.
   *
   * For types we may generate typedefs, these are held in typDecs.
   *
   * For registering stuff in the init function, we keep two list of statements
   * in regs0 and regs1.  Everything in regs0 will preceed the reporting of
   * the global objects, and hence cannot put (ppiler generated) globals on 
   * the stack. Everthing in regs1 will follow the reporting of the globals
   * and hence can refer to them.
   *
   * For reporting global objects that are allocated in the static data
   * segment we need a list of pil expressions for the addresses of these
   * objects.
   *
   * For reporting global roots: For roots in global objects, we just report
   * the global object.  For roots outside global objects, we need to know
   * the locations of refs.  Since we don't have any of these right now, we
   * record and do nothing.  If refs outside of global objects are generated,
   * then this scheme needs to be modified.  
   * The vtables for the global roots must be registered before the global
   * objects are reported.
   *
   * We collect a set of local variables for a procedure,
   * and declare them at the head of C function.
   *
   * For continuations, we generate a new variable for use in continuation
   * implementation, the conts dict stores the association and also which
   * labels are also continuations.
   *)
  datatype state = S of {
    stats   : Stats.t,
    stm     : M.symbolTableManager,
    names   : I.variable ND.t ref,
    vtables : I.variable VtiD.t ref,
    xtrGlbs : Pil.D.t list ref,
    typDecs : Pil.D.t list ref,
    regs0   : Pil.S.t list ref,
    regs1   : Pil.S.t list ref,
    globals : Pil.E.t list ref,
    gRoots  : unit,
    locals  : VS.t ref,
    conts   : I.variable LD.t ref
  }

  fun newStats () =
      let
        val s = Stats.new ()
        val () = Stats.newStat (s, "vtables", "vtables generated")
        val () = Stats.newStat (s, "names", "names generated")
        val () = Stats.newStat (s, "conts", "continuations")
        val () = Stats.newStat (s, "aliases", "global aliases")
        val () = Stats.newStat (s, "singleGlobals", "single globals")
        val () = Stats.newStat (s, "multipleGlobals",
                               "multiply recursive globals")
        val () = Stats.newStat (s, "globalRoots", "global roots")
        val () = Stats.newStat (s, "registrations", "registrations")
        val () = Stats.newStat (s, "xtrGlbs", "extra globals")
        val () = Stats.newStat (s, "typDecs", "type definitions")
      in s
      end

  fun newState st =
      S {stats = newStats (),
         stm = IM.fromExistingAll st,
         names = ref ND.empty,
         vtables = ref VtiD.empty,
         xtrGlbs = ref [],
         typDecs = ref [],
         regs0 = ref [],
         regs1 = ref [],
         globals = ref [],
         gRoots = (),
         locals = ref VS.empty,
         conts = ref LD.empty}

  fun getStats (S {stats, ...}) = stats

  fun incVtables s = Stats.incStat (getStats s, "vtables")
  fun incNames s = Stats.incStat (getStats s, "names")
  fun incConts s = Stats.incStat (getStats s, "conts")
  fun incAliases s = Stats.incStat (getStats s, "aliases")
  fun incSingleGlobals s = Stats.incStat (getStats s, "singleGlobals")
  fun incMultipleGlobals s = Stats.incStat (getStats s, "multipleGlobals")
  fun incXtrGlbs s = Stats.incStat (getStats s, "xtrGlbs")
  fun incTypDecs s = Stats.incStat (getStats s, "typDecs")

  fun getStm (S {stm, ...}) = stm
  fun getSymbolInfo s = I.SymbolInfo.SiManager (getStm s)

  fun getVarTyp  (s, v) = MSTM.variableTyp  (getStm s, v)
  fun getVarKind (s, v) = MSTM.variableKind (getStm s, v)

  fun getName (S {names, ...}, n) = ND.lookup (!names, n)
  fun addName (S {names, ...}, n, v) = names := ND.insert (!names, n, v)

  fun getVtable (S {vtables, ...}, vti) = VtiD.lookup (!vtables, vti)
  fun addVtable (S {vtables, ...}, vti, v) =
      vtables := VtiD.insert (!vtables, vti, v)

  fun getXtrGlbs (S {xtrGlbs, ...}) = List.rev (!xtrGlbs)
  fun addXtrGlb (s as S {xtrGlbs, ...}, g) =
      let
        val () = incXtrGlbs s
        val () = xtrGlbs := g :: !xtrGlbs
      in ()
      end

  fun getTypDecs (S {typDecs, ...}) = List.rev (!typDecs)
  fun addTypDec (s as S {typDecs, ...}, td) =
      let
        val () = incTypDecs s
        val () = typDecs := td :: !typDecs
      in ()
      end

  fun getRegs0 (S {regs0, ...}) = List.rev (!regs0)
  fun addReg0 (S {regs0, ...}, reg) = regs0 := reg::(!regs0)

  fun getRegs1 (S {regs1, ...}) = List.rev (!regs1)
  fun addReg1 (S {regs1, ...}, reg) = regs1 := reg::(!regs1)

  fun getGlobals (S {globals, ...}) = List.rev (!globals)
  fun addGlobal (S {globals, ...}, g) = globals := g::(!globals)

  fun getGRoots (S {gRoots, ...}) = gRoots

  fun getLocals (S {locals,...}) = VS.toList (!locals)
  fun clearLocals (S {locals,...}) = locals := VS.empty
  fun addLocal (S {locals,...}, x) = locals := VS.insert(!locals, x)
  fun addLocals (S {locals,...}, xs) = 
       let
         val s = !locals
         val s = Vector.fold(xs, s, VS.insert o Utils.flip2)
       in locals := s
       end

  fun getCont (S {conts, ...}, cl) = LD.lookup (!conts, cl)
  fun getConts (S {conts, ...}) = !conts
  fun addCont (S {conts, ...}, cl, cv) = conts := LD.insert (!conts, cl, cv)

  val (lightweightThunksF, lightweightThunks) =
      Config.Feature.mk ("Plsr:lightweight-thunks",
                         "Use lightweight sequential thunks")
                        

  (*** Object Model ***)

  (* This structure defines the object model, ie, the layout of objects in
   * the heap.  It can determine the offset of fields in objects, and the
   * size of the fixed portion and the variable portion's element size.
   *)

  structure OM
  :> sig
       (* Return the word size *)
       val wordSize : state * env -> int
       (* Return the offset of the vtable in objects *)
       val vtableOffset : state * env -> int
       (* Given a tuple descriptor, return the offset of
        * the given field (indexed from zero).
        *)
       val fieldOffset : state * env * M.tupleDescriptor * int -> int
       (* Return the offset of the array portion of the tuple descriptor *)
       val arrayOffset : state * env * M.tupleDescriptor -> int
       (* Return the size of the fixed portion of objects of the given type *)
       val fixedSize : state * env * M.tupleDescriptor -> int
       (* Return the size of the elements of the variable portion of the given
        * type
        *)
       val extraSize : state * env * M.tupleDescriptor -> int
       (* Return the size for a thunk *)
       val thunkSize : state * env * M.fieldKind * M.fieldKind Vector.t -> int
       (* Return the offset of the result in a thunk *)
       val thunkResultOffset :
           state * env * M.fieldKind * M.fieldKind Vector.t -> int
       (* Return the offset of a free variable in a thunk *)
       val thunkFvOffset :
           state * env * M.fieldKind * M.fieldKind Vector.t * int-> int
       (* Generate Pil definitions of object model constants needed by the
        * runtime.
        *)
       val genDefs : state * env -> Pil.D.t
   end =
  struct

    fun align (off : int, a : int) : int =
        Int.div (off + a - 1, a) * a

    fun wordSize (state, env) =
        MU.ValueSize.numBytes (MU.ValueSize.ptrSize (getConfig env))

    fun vtableOffset (state, env) = 0

    fun fieldBase (state, env) =
        vtableOffset (state, env) + wordSize (state, env)

    fun get (fds, fdo, i) =
        if i < Vector.length fds then
          Vector.sub (fds, i)
        else
          (case fdo
            of NONE => Fail.fail ("MilToPil.OM", "get",
                                  "not enough fields")
             | SOME fd => fd)

    fun fieldOffsetG (state, env, nb, fds, fdo, i, j, pre, off) =
        if i = j andalso pre then
          off
        else
          let
            val fd = get (fds, fdo, i)
            val fsz = nb (getConfig env, fd)
            val off = align (off, fsz)
          in
            if i = j then
              off
            else
              fieldOffsetG (state, env, nb, fds, fdo, i + 1, j, pre, off + fsz)
          end

    fun fieldOffset (state, env, M.TD {fixed, array, ...}, i) =
        fieldOffsetG (state, env, MU.FieldDescriptor.numBytes,
                      fixed, array, 0, i, false,
                      fieldBase (state, env))

    fun arrayOffset (state, env, M.TD {fixed, array, ...}) =
        fieldOffsetG (state, env, MU.FieldDescriptor.numBytes,
                      fixed, array, 0, Vector.length fixed, false,
                      fieldBase (state, env))

    fun fixedSize (state, env, M.TD {fixed, array, ...}) =
        fieldOffsetG (state, env, MU.FieldDescriptor.numBytes,
                      fixed, array, 0, Vector.length fixed,
                      true, fieldBase (state, env))

    fun extraSize (state, env, M.TD {array, ...}) =
        case array
         of NONE => Fail.fail ("MilToPil.OM", "extraSize", "no array portion")
          | SOME fd => MU.FieldDescriptor.numBytes (getConfig env, fd)

    (* Highly runtime specific assumptions:
     *   A thunk is a fixed structure, followed by the results field, followed
     *   by free variables.
     *)

    fun thunkBase (state, env, fk) =
        (if lightweightThunks (getConfig env) then 2
         else case parStyle env
               of Config.PNone => 3
                | Config.PAll  => 5
                | Config.PAuto => 5
                | Config.PPar  => 5) * wordSize (state, env)

    fun thunkFixedSize (state, env, fk) =
        let
          val tb = thunkBase (state, env, fk)
          val rs = MU.FieldKind.numBytes (getConfig env, fk)
          val tb = align (tb, rs)
        in tb + rs
        end

    fun thunkSize (state, env, typ, fks) =
        fieldOffsetG (state, env, MU.FieldKind.numBytes,
                      fks, NONE, 0, Vector.length fks, true,
                      thunkFixedSize (state, env, typ))

    fun thunkResultOffset (state, env, typ, fks) = thunkBase (state, env, typ)

    fun thunkFvOffset (state, env, typ, fks, i) =
        fieldOffsetG (state, env, MU.FieldKind.numBytes,
                      fks, NONE, 0, i, false,
                      thunkFixedSize (state, env, typ))

    fun genDefs (state, env) =
        let
          val c = getConfig env
          fun mk (id, n) = Pil.D.constantMacro (id, Pil.E.int n)
          val fieldsBase = mk (RT.Object.fieldsBase, fieldBase (state, env))
          val setTD = POM.OptionSet.td c
          val setOffset =
              mk (RT.Object.setOffset,
                  fieldOffset (state, env, setTD, POM.OptionSet.ofValIndex))
          val setSize = mk (RT.Object.setSize, fixedSize (state, env, setTD))
          val typeSize =
              mk (RT.Object.typeSize, fixedSize (state, env, POM.Type.td))
          val ratTD = POM.Rat.td c
          val ratOffset =
              mk (RT.Object.ratOffset,
                  fieldOffset (state, env, ratTD, POM.Rat.ofValIndex c))
          val ratSize = mk (RT.Object.ratSize, fixedSize (state, env, ratTD))
          val floatOffset =
              fieldOffset (state, env, POM.Float.td, POM.Float.ofValIndex)
          val floatOffset = mk (RT.Object.floatOffset, floatOffset)
          val floatSize =
              mk (RT.Object.floatSize, fixedSize (state, env, POM.Float.td))
          val doubleOffset =
              fieldOffset (state, env, POM.Double.td, POM.Double.ofValIndex)
          val doubleOffset = mk (RT.Object.doubleOffset, doubleOffset)
          val doubleSize =
              mk (RT.Object.doubleSize, fixedSize (state, env, POM.Double.td))
          val arrayOTD = POM.OrdinalArray.tdVar (c, M.FkRef)
          val arrayOLenOffset =
              fieldOffset (state, env, arrayOTD, POM.OrdinalArray.lenIndex)
          val arrayOLenOffset = mk (RT.Object.arrayOLenOffset, arrayOLenOffset)
          val arrayOEltOffset = arrayOffset (state, env, arrayOTD)
          val arrayOEltOffset = mk (RT.Object.arrayOEltOffset, arrayOEltOffset)
          val arrayOBaseSize = fixedSize (state, env, arrayOTD)
          val arrayOBaseSize = mk (RT.Object.arrayOBaseSize, arrayOBaseSize)
          val arrayITD = POM.IndexedArray.tdVar (c, M.FkRef)
          val arrayILenOffset =
              fieldOffset (state, env, arrayITD, POM.IndexedArray.lenIndex)
          val arrayILenOffset = mk (RT.Object.arrayILenOffset, arrayILenOffset)
          val arrayIIdxOffset =
              fieldOffset (state, env, arrayITD, POM.IndexedArray.idxIndex)
          val arrayIIdxOffset = mk (RT.Object.arrayIIdxOffset, arrayIIdxOffset)
          val arrayIEltOffset = arrayOffset (state, env, arrayITD)
          val arrayIEltOffset = mk (RT.Object.arrayIEltOffset, arrayIEltOffset)
          val arrayIBaseSize = fixedSize (state, env, arrayITD)
          val arrayIBaseSize = mk (RT.Object.arrayIBaseSize, arrayIBaseSize)
          val funTD = POM.Function.td (c, Vector.new0 ())
          val funCodeOffset =
              fieldOffset (state, env, funTD, POM.Function.codeIndex)
          val funCodeOffset = mk (RT.Object.functionCodeOffset, funCodeOffset)
          val funSize = fixedSize (state, env, funTD)
          val funSize = mk (RT.Object.functionSize, funSize)
          val sumTD = POM.Sum.td (c, M.FkRef)
          val sumTagOffset = fieldOffset(state, env, sumTD, POM.Sum.tagIndex)
          val sumTagOffset = mk (RT.Object.sumTagOffset, sumTagOffset)
          val sumValOffset = fieldOffset(state, env, sumTD, POM.Sum.ofValIndex)
          val sumValOffset = mk (RT.Object.sumValOffset, sumValOffset)
          val sumSize = fixedSize (state, env, sumTD)
          val sumSize = mk (RT.Object.sumSize, sumSize)
          val tfsRef = thunkFixedSize (state, env, M.FkRef)
          val thunkSizeRef = mk (RT.Thunk.fixedSize M.FkRef, tfsRef)
          val tfs32 = thunkFixedSize (state, env, M.FkBits M.Fs32)
          val thunkSize32 = mk (RT.Thunk.fixedSize (M.FkBits M.Fs32), tfs32)
          val tfs64 = thunkFixedSize (state, env, M.FkBits M.Fs64)
          val thunkSize64 = mk (RT.Thunk.fixedSize (M.FkBits M.Fs64), tfs64)
          val tfsFloat = thunkFixedSize (state, env, M.FkFloat)
          val thunkSizeFloat = mk (RT.Thunk.fixedSize M.FkFloat, tfsFloat)
          val tfsDouble = thunkFixedSize (state, env, M.FkDouble)
          val thunkSizeDouble = mk (RT.Thunk.fixedSize M.FkDouble, tfsDouble)
          val smallRationalMin = mk (RT.Rat.smallMin, 
                                     IntInf.toInt RT.Rat.optMin)
          val smallRationalMax = mk (RT.Rat.smallMax, 
                                     IntInf.toInt RT.Rat.optMax)
          val smallIntegerMin = mk (RT.Integer.smallMin, 
                                    IntInf.toInt RT.Integer.optMin)
          val smallIntegerMax = mk (RT.Integer.smallMax, 
                                    IntInf.toInt RT.Integer.optMax)
        in
          Pil.D.sequence [fieldsBase, setOffset, setSize, typeSize,
                          ratOffset, ratSize, floatOffset, floatSize,
                          doubleOffset, doubleSize,
                          arrayOLenOffset, arrayOEltOffset,
                          arrayOBaseSize, arrayILenOffset, arrayIEltOffset,
                          arrayIIdxOffset, arrayIBaseSize,
                          funCodeOffset, funSize,
                          sumTagOffset, sumValOffset, sumSize,
                          thunkSizeRef, thunkSize32, thunkSize64, thunkSizeFloat, thunkSizeDouble,
                          smallRationalMax, smallRationalMin, smallIntegerMax, smallIntegerMin]
        end

  end (* structure OM *)

  (*** Variables ***)

  fun freshVariableDT (state, env, hint, g) =
      let
        val t = MU.Uintp.t (getConfig env)
      in
        MSTM.variableFresh (getStm state, hint, t, g)
      end

  fun printVar (state, env, v)  = 
      LayoutUtils.printLayout (IM.layoutVariable (v, getStm state))

  local
    (* P source identifiers can use characters not allowed in Pil,
     * this code translates them into something useable.
     *)
    val map = CharDict.fromList[
	      (#"_",  "__"),  (* Avoids conflicts with the other expansions *)
	      (#"&",  "_AND"),
	      (#"`",  "_ANTIQUOTE"),
	      (#"*",  "_AST"),
	      (#"@",  "_AT"),
	      (#"!",  "_BANG"),
	      (#"|",  "_BAR"),
	      (#"\\", "_BSLASH"),
	      (#"$",  "_DOLLAR"),
	      (#":",  "_COLON"),
	      (#".",  "_DOT"),
	      (#"=",  "_EQ"),
	      (#"^",  "_EXP"),
	      (#"/",  "_FSLASH"),
	      (#">",  "_GT"),
	      (#"[",  "_LBRACKET"),
	      (#"]",  "_RBRACKET"),
	      (#"{",  "_LBRACE"),
	      (#"}",  "_RBRACE"),
	      (#"(",  "_LPAREN"),
	      (#")",  "_RPAREN"),
	      (#"<",  "_LT"),
	      (#"-",  "_MINUS"),
	      (#"%",  "_PERCENT"),
	      (#"+",  "_PLUS"),
	      (#"#",  "_POUND"),
	      (#"'",  "_PRIME"),
	      (#"?",  "_QM"),
	      (#"~",  "_TWIDDLE")
	      ]
    fun expand c = 
	case CharDict.lookup (map, c)
	 of SOME s => s
	  | NONE => String.fromChar c
  in
  fun stringOfVar (state, env, v) = 
      let
	val s = IM.variableString (getStm state, v)
	val s = String.translate (s, expand)
      in s
      end
  end

  fun deriveVarFromLabel (state, env, l) =
      let
        val hint = I.labelString l
        val t = M.TContinuation (Vector.new0 ())
      in
        MSTM.variableFresh (getStm state, hint, t, M.VkLocal)
      end

  (* Given a variable, derive a separate internal variable in a predictable
   * known fashion.  
   *)
  fun deriveInternalVar (state, env, str, var) = 
      stringOfVar (state, env, var) ^ "_" ^ str

  fun derivedVar' (state, env, v, hint, t) =
      MSTM.variableRelated (getStm state, v, hint, t, M.VkLocal)

  fun derivedVar args = 
       let
         val v = derivedVar' args
         val () = addLocal (#1 args, v)
       in v
       end

  fun genVar (state, env, v) =
      case MSTM.variableKind (getStm state, v)
       of M.VkExtern => Pil.identifier (IM.variableName (getStm state, v))
        | _ => Pil.identifier (stringOfVar (state, env, v))
  fun genVarE (state, env, v) = Pil.E.variable (genVar (state, env, v))
  fun genVars (state, env, vs) =
      Vector.toListMap (vs, fn v => genVar (state, env, v))

  (*** Metadata ***)

  (* This structure creates and memoises metadata for various objects
   * we want to create.
   *)

  structure MD
  :> sig
       (* Return a pointer to the metadata for the given metadata descriptor,
        * generating it if necessary.
        *)
       val genMetaData : state * env * string option * M.metaDataDescriptor * bool -> Pil.E.t
       (* Return a pointer to the metadata for the given thunk,
        * generating it if necessary.
        *)
       val genMetaDataThunk :
           state * env * string option * M.fieldKind * M.fieldKind Vector.t * bool * bool
           -> Pil.E.t
     end =
  struct

    fun vtiToName (state, env, pok, fixedSize, fixedRefs, array) =
        (String.fromChar (MU.PObjKind.toChar pok)) ^
        (String.concat (Vector.toListMap (fixedRefs,
                                       fn b => if b then "r" else "w"))) ^
        (String.make (fixedSize mod OM.wordSize (state, env), #"b")) ^
        (case array
          of NONE => ""
           | SOME {size, offset, isRef} =>
             "[" ^
             Int.toString offset ^ "," ^
             Int.toString size ^ "," ^
             (if isRef then "r" else "w") ^
             "]")

    (* From the components of a tuple type, compute its vtable information *)
    fun deriveVtInfo (state, env, no, mdd, nebi) =
        let
          val M.MDD {pok, fixed, array} = mdd
          val td = MU.MetaDataDescriptor.toTupleDescriptor mdd
          val ws = OM.wordSize (state, env)
          val fs = OM.fixedSize (state, env, td)
          val frefs = Array.new (fs div ws, false)
          fun doOne (i, fd) =
              if MU.FieldDescriptor.isRef fd then
                let
                  val off = OM.fieldOffset (state, env, td, i)
                  val () =
                      if off mod ws <> 0 then
                        Fail.unimplemented ("MilToPil.VT", "deriveVtInfo",
                                            "unaligned reference field")
                      else
                        ()
                  val idx = off div ws
                  val () = Array.update (frefs, idx, true)
                in
                  ()
                end
              else
                ()
          val () = Vector.foreachi (fixed, doOne)
          val frefs = Array.toVector frefs
          val a =
              case array
               of NONE => NONE
                | SOME (lenIdx, fd) =>
                  let
                    val es = OM.extraSize (state, env, td)
                    val lenOff = OM.fieldOffset (state, env, td, lenIdx)
                    val eref = MU.FieldDescriptor.isRef fd
                  in
                    SOME {size = es, offset = lenOff, isRef = eref}
                  end
          val n =
              case no
               of NONE => vtiToName (state, env, pok, fs, frefs, a)
                | SOME n => n
          val mut = not (MU.MetaDataDescriptor.immutable mdd)
          val vtm =
              if mut then
                VtmAlwaysMutable
              else if nebi then 
                VtmAlwaysImmutable
              else
                VtmCreatedMutable
        in
          Vti {name = n, tag = pok, fixedSize = fs, fixedRefs = frefs,
               array = a, mut = vtm}
        end

    fun genVtMutability vtm =
        case vtm
         of VtmAlwaysMutable   => RT.MD.alwaysMutable
          | VtmCreatedMutable  => RT.MD.createdMutable
          | VtmAlwaysImmutable => RT.MD.alwaysImmutable

    (* Given vtable information generate the global for the unboxed vtable
     * and return the variable bound to it.
     *)
    fun genMetaDataUnboxed (state, env, vti) =
        let
          val () = incVtables state
          val Vti {name, tag, fixedSize, fixedRefs, array, mut} = vti
          (* Generate the actual vtable *)
          val vt = freshVariableDT (state, env, "vtable", M.VkGlobal)
          val vt' = genVarE (state, env, vt)
          val tag = Pil.E.namedConstant (RT.MD.pObjKindTag tag)
          val args = [vt', tag, Pil.E.string name]
          val vtg = Pil.D.macroCall (RT.MD.static, args)
          val () = addXtrGlb (state, vtg)
          (* Generate an array of the fixed reference information *)
          fun doOne b = Pil.E.int (if b then 1 else 0)
          val refs = Pil.E.strctInit (Vector.toListMap (fixedRefs, doOne))
          val refsv = freshVariableDT (state, env, "vtrefs", M.VkGlobal)
          val refsv = genVar (state, env, refsv)
          val isRefT = Pil.T.named RT.MD.isRefTyp
          val refsvd = Pil.varDec (Pil.T.array isRefT, refsv)
          val refsg = Pil.D.staticVariableExpr (refsvd, refs)
          val () = if vtReg env then addXtrGlb (state, refsg) else ()
          val refsv = Pil.E.variable refsv
          (* Generate code to register vtable with GC *)
          val fs = Pil.E.int fixedSize
          val (vs, vlo, vr) =
              case array
               of NONE => (Pil.E.int 0, Pil.E.int 0, Pil.E.int 0)
                | SOME {size, offset, isRef} =>
                  (Pil.E.int size, Pil.E.int offset,
                   Pil.E.int (if isRef then 1 else 0))
          val mut = Pil.E.namedConstant (genVtMutability mut)
          val args = [Pil.E.addrOf vt', fs, refsv, vs, vlo, vr, mut]
          val vtr = Pil.E.call (Pil.E.namedConstant RT.MD.register, args)
          val () = if vtReg env then addReg0 (state, Pil.S.expr vtr) else ()
        in vt
        end

    (* Given a vtable information, return a pointer to a vtable for it,
     * generating the vtable if necessary.
     *)
    fun vTableFromInfo (state, env, vti) =
        let
          val vt =
              case getVtable (state, vti)
               of NONE =>
                  let
                    val vt = genMetaDataUnboxed (state, env, vti)
                    val () = addVtable (state, vti, vt)
                  in vt
                  end
                | SOME v => v
        in
          Pil.E.addrOf (genVarE (state, env, vt))
        end

    fun genMetaData (state, env, no, mdd as M.MDD {pok, ...}, nebi) =
        if vtTagOnly env then
          Pil.E.namedConstant (RT.MD.pObjKindMetaData pok)
        else
          let
            val vti = deriveVtInfo (state, env, no, mdd, nebi)
            val vt = vTableFromInfo (state, env, vti)
          in vt
          end

    fun genMetaDataThunk (state, env, no, typ, fks, backpatch, value) =
        if vtTagOnly env then
          Pil.E.namedConstant (RT.Thunk.vTable typ)
        else
          let
            val fs = OM.thunkSize (state, env, typ, fks)
            (* Assumptions: The only refs are the free vars and the result.
             * XXX: This is highly runtime specific
             *)
            val ws = OM.wordSize (state, env)
            val frefs = Array.new (fs div ws, false)
            fun doOne (i, fk) =
                if MU.FieldKind.isRef fk then
                  let
                    val off = OM.thunkFvOffset (state, env, typ, fks, i)
                    val () =
                        if off mod ws <> 0 then
                          Fail.unimplemented ("MilToPil.MD", "genMetaDataThunk",
                                              "unaligned free variable")
                        else
                          ()
                    val idx = off div ws
                    val () = Array.update (frefs, idx, true)
                  in
                    ()
                  end
                else
                  ()
            val () = Vector.foreachi (fks, doOne)
            val off = OM.thunkResultOffset (state, env, typ, fks)
            val () =
                if off mod ws <> 0 then
                  Fail.unimplemented ("MilToPil.MD", "genMetaDataThunk",
                                      "unaligned result")
                else
                  ()
            val idx = off div ws
            val isRef = MU.FieldKind.isRef typ
            val () = Array.update (frefs, idx, isRef)
            val frefs = Array.toVector frefs
            val n =
                case no
                 of NONE => vtiToName (state, env, M.PokCell, fs, frefs, NONE)
                  | SOME n => n
            val mut = 
                if value then
                  if backpatch then
                    VtmCreatedMutable
                  else
                    VtmAlwaysImmutable
                else
                  VtmAlwaysMutable
            val vti = Vti {name = n, tag = M.PokCell, fixedSize = fs,
                           fixedRefs = frefs, array = NONE,
                           mut = mut}
            val vt = vTableFromInfo (state, env, vti)
          in vt
          end

  end (* structure MD *)

  (*** Names ***)

  fun stringOfName (state, env, n) = IM.nameString (getStm state, n)

  fun genNameUnboxed (state, env, n) =
      let
        val v =
            (* We generate one name structure per name,
             * see if we have generated one for this name yet.
             *)
            case getName (state, n)
             of SOME v => v
              | NONE =>
                (* No structure generated, so generate it and record this in
                 * the state.
                 *)
                let
                  val () = incNames state
                  val stm = getStm state
                  val v = freshVariableDT (state, env, "name", M.VkGlobal)
                  val ev = genVarE (state, env, v)
                  val str = IM.nameString (stm, n)
                  val el = Pil.E.int (String.length str)
                  val cs = String.explode (str ^ "\000")
                  val es = List.map (cs, Pil.E.char)
                  val h = String.hash str
                  val eh = Pil.E.word h
                  val num = I.nameNumber n
                  val en = Pil.E.int num
                  val g = Pil.D.macroCall (RT.Name.static, ev::en::eh::el::es)
                  val () = addXtrGlb (state, g)
                  val () = addName (state, n, v)
                  val () =
                      addGlobal (state, Pil.E.addrOf (genVarE (state, env, v)))
                in v
                end
      in genVar (state, env, v)
      end

  fun genName (state, env, n) =
      Pil.E.cast (Pil.T.named RT.T.pAny,
                Pil.E.addrOf (Pil.E.variable (genNameUnboxed (state, env, n))))

  (*** Labels ***)

  fun genLabel (state, env, l) = Pil.identifier (I.labelString l)

  (* Given the label of a continuation, get the variable associated with
   * it, generating this variable if necessary.  This variable is used by
   * the runtime to implement continuations.
   *)
  fun genContVar (state, env, l) =
      case getCont (state, l)
       of NONE =>
          let
            val () = incConts state
            val cv = deriveVarFromLabel (state, env, l)
            val () = addCont (state, l, cv)
          in cv
          end
        | SOME cv => cv

  (*** Types ***)

  (* Return the C type for various Mil types *)

  fun genReturnType (state, env, conv, rts) = 
      case rewriteThunks (env, conv)
       of SOME _ => 
          (case Vector.length rts
            of 1 => Pil.T.named (RT.Thunk.returnTyp (typToFieldKind (env, Vector.sub (rts, 0))))
             | _ => fail ("genReturnType", "Thunk code requires exactly 1 return type"))
        | NONE   => 
          (case Vector.length rts
            of 0 => Pil.T.void
             | 1 => genTyp (state, env, Vector.sub (rts,0))
             | _ => fail ("genReturnType", "Single returns only"))
  and genCodeType (state, env, (conv, ats, rts)) =
      let
        val rt = genReturnType (state, env, conv, rts)
        val ats = genTyps (state, env, ats)
        val ats = 
            case conv
             of M.CcCode => ats
              | M.CcClosure _ => notCoreMil (env, "genCodeType", "CcClosure")
              | M.CcThunk {thunk, ...} => (genTyp (state, env, thunk))::ats
      in
        Pil.T.code (rt, ats)
      end
  and genTyp (state, env, t) = 
      case t
       of M.TAny => Fail.fail ("MilToPil", "genTyp", "TAny")
        | M.TAnyS _ => Fail.fail ("MilToPil", "genTyp", "TAnyS")
        | M.TNonRefPtr =>  Pil.T.ptr Pil.T.void (* Fail.fail ("MilToPil", "genTyp", "TNonRefPtr") *)
        | M.TRef => Pil.T.named RT.T.object
        | M.TBits vs =>
          (case vs
            of M.Vs8    => Pil.T.uint8
             | M.Vs16   => Pil.T.uint16
             | M.Vs32   => Pil.T.uint32
             | M.Vs64   => Pil.T.uint64
             | M.Vs128  => Fail.fail ("MilToPil", "genTyp", "TBits128")
             | M.Vs256  => Fail.fail ("MilToPil", "genTyp", "TBits256")
             | M.Vs512  => Fail.fail ("MilToPil", "genTyp", "TBits512")
             | M.Vs1024 => Fail.fail ("MilToPil", "genTyp", "TBits1024"))
        | M.TNone => Pil.T.named RT.T.object
        | M.TNumeric t => RT.Prims.numericTyp (getConfig env, t)
        | M.TBoolean   => Pil.T.named RT.T.boolean
        | M.TName => Pil.T.named RT.T.pAny
        | M.TViVector {vectorSize, elementTyp} => 
          let
            val fk = typToFieldKind (env, elementTyp)
            val t = RT.Prims.vectorTyp (getConfig env, vectorSize, fk)
          in t
          end
        | M.TViMask et => Fail.fail ("MilToPil", "genTyp", "TViMask")
        | M.TCode {cc, args, ress} =>
          Pil.T.ptr (genCodeType (state, env, (cc, args, ress)))
        | M.TTuple _ => Pil.T.named RT.T.pAny
        | M.TCString => Pil.T.ptr Pil.T.char
        | M.TIdx => Pil.T.named RT.T.idx
        | M.TContinuation _ => Pil.T.continuation
        | M.TThunk t =>
          let
            val fk = typToFieldKind (env, t)
            val t = Pil.T.named (RT.Thunk.boxedTyp fk)
          in t
          end
        | M.TPAny => Pil.T.named RT.T.pAny
        | M.TClosure _ => notCoreMil (env, "genTyp", "TClosure")
        | M.TPSum _ => notCoreMil (env, "genTyp", "TPSum")
        | M.TPType _ => notCoreMil (env, "genTyp", "TPType")
        | M.TPRef _ => notCoreMil (env, "genTyp", "TPRef")
  and genTyps (state, env, ts) =
      Vector.toListMap(ts, fn t => genTyp (state, env, t))


  (* Return the C type for the unboxed version of a Mil type *)

  (* XXX: This is highly object layout specific *)
  fun tupleUnboxedTyp (pok, ts, xt) =
      let
        fun doOne (i, t) = (RT.Tuple.fixedField i, t)
        val fts = (RT.Tuple.vtable, Pil.T.named RT.T.vtable)::
                  (List.mapi (ts, doOne))
        val fts =
            case xt
             of NONE => fts
              | SOME xt => fts @ [(RT.Tuple.xtras, Pil.T.array xt)]
      in
        Pil.T.strct (NONE, fts)
      end

  (*** Variable Binders ***)

  fun genVarDec (state, env, x) = 
      Pil.varDec (genTyp (state, env, getVarTyp (state, x)),
                  genVar (state, env, x))
  and genVarsDec (state, env, xs) = 
      List.map (xs, fn x => genVarDec (state, env, x))

  (*** Constants ***)

  fun genConstant (state, env, constant) = 
      case constant    
       of M.CRat i =>
          Pil.E.call (Pil.E.namedConstant RT.Rat.optFromSInt32,
                      [Pil.E.int32 (IntInf.toInt i)])
        | M.CInteger i =>
          Pil.E.call (Pil.E.namedConstant RT.Integer.optFromSInt32,
                      [Pil.E.int32 (IntInf.toInt i)])
        | M.CName name => genName (state, env, name)
        (* This used to silently do the wrong thing.  I've
         * changed it to raise an error.  At some point
         * we should figure out how to make it do the right
         * thing.  -leaf *)
        | M.CIntegral i => 
          (case IntArb.toInt i
            of SOME i => Pil.E.int i
             | NONE   => 
               let
                 val iif = IntArb.toIntInf i
               in
                 if iif < 0 then 
                   Fail.fail ("MilToPil", "genConstant", "Can't produce integer constant" ^ (IntArb.stringOf i))
                 else
                   Pil.E.intInf iif
               end)
        | M.CBoolean b => Pil.E.boolean b
        | M.CFloat r => Pil.E.float r
        | M.CDouble r => Pil.E.double r
        (* FIXME: WL: add runtime routine *)
        | M.CViMask _ => Fail.fail ("MilToPil", "genConstant", "CViMask")
(*          (if VI.numMaskBytes (targetVectorSize env, typ) > 4 then 
             Fail.fail ("MilTOPil", "genConstant", "Unspported mask size > 32")
           else  
             let
               fun shiftBool (b, w) =
                   let
                     val w = Word32.<< (w, Word32.fromInt 1)
                   in
                     if b then Word32.orb (w, Word32.fromInt 1) else w
                   end
               val m = Vector.foldr (elts, Word32.fromInt 0, shiftBool)
             in 
               Pil.E.word32 m
             end) *)
        | M.CPok pok => Pil.E.namedConstant (RT.MD.pObjKindTag pok)
        | M.CRef i   => Pil.E.cast (genTyp (state, env, M.TRef), Pil.E.intInf i)
        | M.COptionSetEmpty =>
          notCoreMil (env, "genConstant", "COptionSetEmpty")
        | M.CTypePH =>
          notCoreMil (env, "genConstant", "CTypePH")

  (*** Simples ***)

  (* Global variables that occur in other globals cannot simply 
   * be referenced, since the resulting reference is not itself
   * constant.  Consequently, we must split globals into two parts:
   * an internal variable containing the actual global, and the actual
   * variable which is assigned the address of the internal global.
   * All static uses of the global variable must be likewise replaced.
   * However, all code globals are bound to C functions, which in a
   * sense are the unboxed versions of the variable.
   *)
  fun unboxedVar (state, env, v) = 
      case getGlobalDef (env, v)
       of M.GCode _ => genVar (state, env, v)
        | _ => Pil.identifier (deriveInternalVar (state, env, "unboxed", v))
  fun globalTVar (state, env, v) =
      Pil.identifier (deriveInternalVar (state, env, "T", v))

  (* Generate a C expression for a local simple *)
  fun genSimple (state, env, s) =
      case s
       of M.SVariable v => genVarE (state, env, v)
        | M.SConstant c => genConstant (state, env, c)

  (*** Primitives ***)

  fun genPrim (state, env, p, t, ds, typs, args) = RT.Prims.call (getConfig env, ds, p, t, typs, args)

  (*** Operands ***)

  val genOperand = genSimple

  fun genOperands (state, env, os) =
      Vector.toListMap (os, fn opnd => genOperand (state, env, opnd))
       
  (*** Right-hand side ***)

  fun writeBarrier (state, env, base, trg, src, fk, opt) =
      if MU.FieldKind.isRef fk then
        Pil.E.call (Pil.E.namedConstant
                      (if opt then RT.GC.writeBarrierRefOpt
                       else RT.GC.writeBarrierRef),
                    [base, trg, src])
      else
        Pil.E.assign (trg, src)

  fun genTuple (state, env, no, dest, mdd, inits) = 
      let
        val M.MDD {pok, fixed, array} = mdd
        val td = MU.MetaDataDescriptor.toTupleDescriptor mdd
        val (fdo, lenIdx, nebi) =
            case array
             of NONE => (NONE, 0, true)
              | SOME (li, fd) => (SOME fd, li, false)
        val nebi = nebi andalso Vector.length inits = Vector.length fixed
        val fixedSize = OM.fixedSize (state, env, td)
        val dest = genVarE (state, env, dest)
        val vtable = MD.genMetaData (state, env, no, mdd, nebi)
        val newTuple =
            case array
             of SOME (i, _) =>
                Pil.E.call (Pil.E.variable RT.Tuple.newVariable,
                            [dest,
                             vtable,
                             Pil.E.int fixedSize,
                             Pil.E.int (OM.extraSize (state, env, td)),
                             genOperand (state, env, Vector.sub (inits, i))])
              | NONE =>
                Pil.E.call (Pil.E.variable RT.Tuple.newFixed,
                            [dest, vtable, Pil.E.int fixedSize])
        val newTuple = Pil.S.expr newTuple
        fun af (off, fk, t, e) =
            let
              val pt = Pil.T.ptr (genTyp (state, env, t))
              val field = Pil.E.call (Pil.E.variable RT.Object.field,
                                      [dest, Pil.E.int off, Pil.E.hackTyp pt])
            in
              Pil.S.expr (writeBarrier (state, env, dest, field, e, fk, true))
            end
        fun doFixedField (i, oper) =
            let
              val c = getConfig env
              val si = getSymbolInfo state
              val fd =
                  if i < Vector.length fixed then
                    Vector.sub (fixed, i)
                  else
                    #2 (Option.valOf array)
              val fk = MU.FieldDescriptor.kind fd
              val t = MTT.operand (c, si, oper)
              val off = OM.fieldOffset (state, env, td, i)
              val oper = genOperand (state, env, oper)
            in
              af (off, fk, t, oper)
            end
        val fixedInit = Vector.toList (Vector.mapi (inits, doFixedField))
        val code = Pil.S.noyield (Pil.S.sequence (newTuple :: fixedInit))
      in code
      end

  datatype subSetKind =
      SskScalarFixed
    | SskScalarVariable of Pil.E.t
    | SskVectorFixed of Mil.Prims.vectorDescriptor
    | SskVectorVariableStrided of Mil.Prims.vectorDescriptor * Pil.E.t * Pil.E.t
    | SskVectorVariableIndexed of Mil.Prims.vectorDescriptor * Pil.E.t
    | SskVectorVariableVectorStrided of Mil.Prims.vectorDescriptor * Pil.E.t * Pil.E.t
    | SskVectorVariableVectorIndexed of Mil.Prims.vectorDescriptor * Pil.E.t

  fun getFieldDescriptor (M.TD {fixed, array, ...}, i) =
      if i < Vector.length fixed
      then Vector.sub (fixed, i)
      else Option.valOf array

  fun getArrayDescriptor (M.TD {array, ...}) = Option.valOf array

  fun doTupleField (state, env, M.TF {tupDesc, tup, field}) =
      case field
       of M.FiFixed i =>
          let
            val off = OM.fieldOffset (state, env, tupDesc, i)
            val ssk = SskScalarFixed
            val fd = getFieldDescriptor (tupDesc, i)
          in (off, ssk, fd)
          end
        | M.FiVariable opnd =>
          let
            val off = OM.arrayOffset (state, env, tupDesc)
            val ssk = SskScalarVariable (genOperand (state, env, opnd))
            val fd = getArrayDescriptor tupDesc
          in (off, ssk, fd)
          end
        | M.FiVectorFixed {descriptor, mask, index} => 
          let
            val () =
                case mask 
                 of SOME _ => Fail.fail ("MilToPil", "doTupleField", "FiVectorFixed: masking unimplimented")
                  | NONE   => ()
            val off = OM.fieldOffset (state, env, tupDesc, index)
            val ssk = SskVectorFixed descriptor
            val fd = getFieldDescriptor (tupDesc, index)
          in (off, ssk, fd)
          end
        | M.FiVectorVariable {descriptor, base, mask, index, kind} =>
          let
            val () =
                case mask 
                 of SOME _ => Fail.fail ("MilToPil", "doTupleField", "FiVectorVariable: masking unimplimented")
                  | NONE   => ()
            val off = OM.arrayOffset (state, env, tupDesc)
            val ext = genOperand (state, env, index)
            val ssk = 
                (case (base, kind)
                  of (M.TbScalar, M.VikStrided i) => SskVectorVariableStrided (descriptor, Pil.E.int i, ext)
                   | (M.TbScalar, M.VikVector)    => SskVectorVariableIndexed (descriptor, ext)
                   | (M.TbVector, M.VikStrided i) => SskVectorVariableVectorStrided (descriptor, Pil.E.int i, ext)
                   | (M.TbVector, M.VikVector)    => SskVectorVariableVectorIndexed (descriptor, ext))
            val fd = getArrayDescriptor tupDesc
          in (off, ssk, fd)
          end

  fun genTupleSub (state, env, dest, tf) =
      let
        val M.TF {tup, ...} = tf
        val v = genVarE (state, env, tup)
        val ft = Pil.T.ptr (genTyp (state, env, getVarTyp (state, dest)))
        val fte = Pil.E.hackTyp ft
        val (off, ssk, M.FD {kind, ...}) = doTupleField (state, env, tf)
        val off = Pil.E.int off
        val d = genVarE (state, env, dest)
        val assign = fn (loader, args) => Pil.E.assign (d, Pil.E.call (Pil.E.namedConstant loader, args))
        val call = fn (loader, args) => Pil.E.call (Pil.E.namedConstant loader, d::args)
        val config = getConfig env
        val a =
            case ssk
             of SskScalarFixed                            => assign (RT.Object.field, [v, off, fte])
              | SskScalarVariable e                       => assign (RT.Object.extra, [v, off, fte, e])
              | SskVectorFixed et                         => call (RT.Prims.vectorLoadF   (config, et, kind), 
                                                                   [v, off])
              | SskVectorVariableStrided (et, i, e)       => call (RT.Prims.vectorLoadVS  (config, et, kind), 
                                                                   [v, off, e, i])
              | SskVectorVariableIndexed (et, e)          => call (RT.Prims.vectorLoadVI  (config, et, kind), 
                                                                   [v, off, e])
              | SskVectorVariableVectorStrided (et, i, e) => call (RT.Prims.vectorLoadVVS (config, et, kind), 
                                                                   [v, off, e, i])
              | SskVectorVariableVectorIndexed (et, e)    => call (RT.Prims.vectorLoadVVI (config, et, kind), 
                                                                   [v, off, e])
      in Pil.S.expr a
      end

  fun genTupleSet (state, env, tf, ofVal) =
      let
        val M.TF {tup, ...} = tf
        val v = genVarE (state, env, tup)
        val ft = MTT.operand (getConfig env, getSymbolInfo state, ofVal)
        val ft = Pil.E.hackTyp (Pil.T.ptr (genTyp (state, env, ft)))
        val (off, ssk, M.FD {kind, ...}) = doTupleField (state, env, tf)
        val off = Pil.E.int off
        val nv = genOperand (state, env, ofVal)
        fun doWB trg = writeBarrier (state, env, v, trg, nv, kind, false)
        val scalar = fn (field, args) => doWB (Pil.E.call (Pil.E.namedConstant field, args))
        val vector = fn (field, args) => Pil.E.call (Pil.E.namedConstant field, args)
        val config = getConfig env
        val set =
            case ssk
             of SskScalarFixed                            => scalar (RT.Object.field, [v, off, ft])
              | SskScalarVariable e                       => scalar (RT.Object.extra, [v, off, ft, e])
              | SskVectorFixed et                         => vector (RT.Prims.vectorStoreF (config, et, kind), 
                                                                     [v, off, nv])
              | SskVectorVariableStrided (et, i, e)       => vector (RT.Prims.vectorStoreVS (config, et, kind), 
                                                                     [v, off, e, i, nv])
              | SskVectorVariableIndexed (et, e)          => vector (RT.Prims.vectorStoreVI (config, et, kind), 
                                                                     [v, off, e, nv])
              | SskVectorVariableVectorStrided (et, i, e) => vector (RT.Prims.vectorStoreVVS (config, et, kind), 
                                                                     [v, off, e, i, nv])
              | SskVectorVariableVectorIndexed (et, e)    => vector (RT.Prims.vectorStoreVVI (config, et, kind), 
                                                                     [v, off, e, nv])
      in set
      end

  val (instrumentAllocationSitesF, instrumentAllocationSites) =
      Config.Feature.mk ("Plsr:instrument-allocation-sites",
                         "gather allocation statistics per alloc site")

  (* Make optional name for allocation site *)
  fun mkAllocSiteName (state, env, dest) =
      if instrumentAllocationSites (getConfig env) then 
        case dest
         of NONE => NONE
          | SOME v => SOME (I.variableString' v)
      else
        NONE

  fun mkThunk0 (state, env, dest, typ, fvs, backpatch, value) = 
      let
        val no = mkAllocSiteName (state, env, SOME dest)
        val vt = MD.genMetaDataThunk (state, env, no, typ, fvs, backpatch, value)
        val sz = Pil.E.int (OM.thunkSize (state, env, typ, fvs))
      in (vt, sz)
      end

  fun mkThunk (state, env, dest, typ, fvs) =
      let
        val (vt, sz) = mkThunk0 (state, env, dest, typ, fvs, true, false)
        val new = Pil.E.namedConstant (RT.Thunk.new typ)
        val v = genVarE (state, env, dest)
        val mk = Pil.E.call (new, [v, vt, sz])
      in Pil.S.expr mk
      end

  fun mkThunkValue (state, env, dest, typ, fvs, value) =
      let
        val (vt, sz) = mkThunk0 (state, env, dest, typ, fvs, false, true)
        val new = Pil.E.namedConstant (RT.Thunk.newValue typ)
        val v = genVarE (state, env, dest)
        val mk = Pil.E.call (new, [v, vt, sz, value])
      in Pil.S.expr mk
      end

  fun genThunkInit (state, env, dest, typ, thunk, code, fvs) =
      let
        val c = getConfig env
        val si = getSymbolInfo state
        fun fail s = Fail.fail ("MilToPil", "genRhs", "ThunkInit: " ^ s)
        val (mk, thunk) =
            case (thunk, dest)
             of (NONE, NONE) => fail "expecting dest"
              | (NONE, SOME v) =>
                let
                  val fvfks = Vector.map (fvs, #1)
                  val mk = mkThunk (state, env, v, typ, fvfks)
                in (mk, v)
                end
              | (SOME v, SOME _) => fail "returns no value"
              | (SOME v, NONE) => (Pil.S.empty, v)
        val thunk = genVarE (state, env, thunk)
        val code  =
            case code
             of NONE => Pil.E.null
              | SOME v => genVarE (state, env, v)
        val fks = Vector.map (fvs, #1)
        fun doInit (i, (fk, opnd)) =
            let
              val t = MTT.operand (c, si, opnd)
              val t = Pil.T.ptr (genTyp (state, env, t))
              val off = Pil.E.int (OM.thunkFvOffset (state, env, typ, fks, i))
              val f = Pil.E.call (Pil.E.namedConstant RT.Object.field,
                                  [thunk, off, Pil.E.hackTyp t])
              val opnd = genOperand (state, env, opnd)
              val b = writeBarrier (state, env, thunk, f, opnd, fk, false)
            in Pil.S.expr b
            end
        val initFvs = Vector.mapi (fvs, doInit)
        val initFvs = Vector.toList initFvs
        val init =
            Pil.S.expr (Pil.E.call (Pil.E.variable (RT.Thunk.init typ),
                                    [thunk, code]))
      in
        Pil.S.sequence [mk, Pil.S.sequence initFvs, init]
      end

  fun genThunkFvProjection (state, env, typ, fvs, thunk, idx, t) =
      let
        val off = Pil.E.int (OM.thunkFvOffset (state, env, typ, fvs, idx))
        val get = Pil.E.call (Pil.E.namedConstant RT.Object.field,
                              [thunk, off, Pil.E.hackTyp (Pil.T.ptr t)])
      in get
      end

  fun genThunkGetFv (state, env, dest, typ, fvs, thunk, idx) =
      let
        val thunk = genVarE (state, env, thunk)
        val t = genTyp (state, env, getVarTyp (state, dest))
        val get = genThunkFvProjection (state, env, typ, fvs, thunk, idx, t)
        val set = Pil.E.assign (genVarE (state, env, dest), get)
      in Pil.S.expr set
      end

  fun genThunkValue (state, env, dest, typ, thunk, opnd) =
      let
        fun fail s = Fail.fail ("MilToPil", "genRhs", "ThunkValue: " ^ s)
        val value = genOperand (state, env, opnd)
        val s =
            case (thunk, dest)
             of (NONE, NONE) => fail "expecting dest"
              | (NONE, SOME v) =>
                let
                  val fvfks = Vector.new0 ()
                  val mk = mkThunkValue (state, env, v, typ, fvfks, value)
                in mk
                end
              | (SOME v, SOME _) => fail "returns no value"
              | (SOME v, NONE)   => 
                let
                  val t = genVarE (state, env, v)
                  val set = Pil.E.call (Pil.E.variable (RT.Thunk.setValue typ), [t, value])
                  val s = Pil.S.expr set
                in s
                end
      in s
      end

  fun genThunkGetValueE (state, env, typ, t, thunk) =
      let
        val off =
            Pil.E.int (OM.thunkResultOffset (state, env, typ, Vector.new0 ()))
        val get = Pil.E.call (Pil.E.namedConstant RT.Object.field,
                              [thunk, off, Pil.E.hackTyp (Pil.T.ptr t)])
      in get
      end

  fun genThunkGetValue (state, env, dest, typ, thunk) =
      let
        val t = genTyp (state, env, getVarTyp (state, dest))
        val thunk = genVarE (state, env, thunk)
        val get = genThunkGetValueE (state, env, typ, t, thunk)
        val set = Pil.E.assign (genVarE (state, env, dest), get)
      in Pil.S.expr set
      end

  fun genRhs (state, env, dests, rhs) =
      let

        val zeroOneDest = 
         fn dests => 
            (case Utils.Option.fromVector dests
              of SOME opt => opt
               | NONE => fail ("genRhs", "Don't know how to generate multiple destinations"))

        (* Assign effectful *)
        fun assign rhs =
            case zeroOneDest dests
             of SOME v =>
                Pil.S.expr (Pil.E.assign (genVarE (state, env, v), rhs))
              | NONE   => Pil.S.expr rhs

        (* Assign pure *)
        fun assignP rhs =
            case zeroOneDest dests 
             of SOME v =>
                Pil.S.expr (Pil.E.assign (genVarE (state, env, v), rhs))
              | NONE   => Pil.S.empty

        (* Assign pure and cast to target type *)
        fun assignPCast rhs =
            case zeroOneDest dests
             of SOME v =>
                let
                  val t = getVarTyp (state, v)
                  val t = genTyp (state, env, t)
                  val rhs = Pil.E.cast (t, rhs)
                  val v = genVarE (state, env, v)
                  val s = Pil.S.expr (Pil.E.assign (v, rhs))
                in s
                end
              | NONE => Pil.S.empty

        (* Pass in dest, drop if no dest. *)
        fun bind f = 
            case zeroOneDest dests 
             of SOME v => f v
              | NONE   => Pil.S.empty

      in
        case rhs
         of M.RhsSimple s => assignP (genSimple (state, env, s))
          | M.RhsPrim {prim, createThunks, typs, args} =>
            let
              val ds = Vector.map (dests, fn v => genVarE (state, env, v))
              val args = genOperands(state, env, args)
              val typs = Vector.map (typs, fn t => typToFieldKind (env, t))
            in genPrim (state, env, prim, createThunks, ds, typs, args)
            end
          | M.RhsTuple {mdDesc, inits} =>
            let
              fun doIt v =
                  let
                    val no = mkAllocSiteName (state, env, zeroOneDest dests)
                    val t = genTuple (state, env, no, v, mdDesc, inits)
                  in t
                  end
            in bind doIt
            end
          | M.RhsTupleSub tf =>
            bind (fn v => genTupleSub (state, env, v, tf))
          | M.RhsTupleSet {tupField, ofVal} => 
            assign (genTupleSet (state, env, tupField, ofVal))
          | M.RhsTupleInited {mdDesc, tup} =>
            let
              val () = Fail.assert ("MilToPil",
                                    "genInstr",
                                    "TupleInited returns no value",
                                    (fn () => Vector.isEmpty dests))
              val fvtb = MD.genMetaData (state, env, NONE, mdDesc, true)
              val tpl = genVarE (state, env, tup)
              val finalise =
                  Pil.E.call (Pil.E.namedConstant RT.GC.vtableChange,
                              [tpl, fvtb])
            in Pil.S.expr finalise
            end
          | M.RhsIdxGet {idx, ofVal} =>
            assignP (Pil.E.call (Pil.E.variable RT.Idx.get, 
                                 [genVarE (state, env, idx),
                                  genOperand (state, env, ofVal)]))
          | M.RhsCont l =>
            let
              val M.F {body = M.CB {blocks, ...}, ...} = getFunc env
              fun doIt v =
                  if LD.contains (blocks, l) then
                    let
                      val v = genVarE (state, env, v)
                      val cv = genContVar (state, env, l)
                      val cl = genLabel (state, env, l)
                      val cv = genVar (state, env, cv)
                    in Pil.S.contMake (v, cl, cv)
                    end
                  else
                    Pil.S.expr (Pil.E.assign (genVarE (state, env, v),
                                              Pil.E.null))
            in bind doIt
            end
          | M.RhsObjectGetKind v =>
            let
              val v = genVarE (state, env, v)
              val e = Pil.E.call (Pil.E.namedConstant RT.Object.getKind, [v])
            in assignP e
            end
          | M.RhsThunkMk {typ, fvs} =>
            bind (fn v => mkThunk (state, env, v, typ, fvs))
          | M.RhsThunkInit {typ, thunk, fx, code, fvs} =>
            genThunkInit (state, env, zeroOneDest dests, typ, thunk, code, fvs)
          | M.RhsThunkGetFv {typ, fvs, thunk, idx} =>
            bind (fn dest =>
                     genThunkGetFv (state, env, dest, typ, fvs, thunk, idx))
          | M.RhsThunkValue {typ, thunk, ofVal} =>
            genThunkValue (state, env, zeroOneDest dests, typ, thunk, ofVal)
          | M.RhsThunkGetValue {typ, thunk} =>
            bind (fn dest => genThunkGetValue (state, env, dest, typ, thunk))
          | M.RhsThunkSpawn {typ, thunk, fx} =>
            let
              val v = genVarE (state, env, thunk)
            in
              assign (Pil.E.call (Pil.E.variable (RT.Thunk.spawn typ), [v]))
            end
          | M.RhsClosureMk _ => notCoreMil (env, "genRhs", "ClosureMk")
          | M.RhsClosureInit _ => notCoreMil (env, "genRhs", "ClosureInit")
          | M.RhsClosureGetFv _ =>
            notCoreMil (env, "genRhs", "ClosureGetGv")
          | M.RhsPSetNew _ => notCoreMil (env, "genRhs", "PSetNew")
          | M.RhsPSetGet _ => notCoreMil (env, "genRhs", "PSetGEt")
          | M.RhsPSetCond _ => notCoreMil (env, "genRhs", "PSetCond")
          | M.RhsPSetQuery _ => notCoreMil (env, "genRhs", "PSetQuery")
          | M.RhsPSum _ => notCoreMil (env, "genRhs", "PSum")
          | M.RhsPSumProj _ => notCoreMil (env, "genRhs", "PSumProj")

      end

  (*** Instructions ***)

  fun genInstr (state, env, (M.I {dests, n, rhs})) = 
      let
        val () = addLocals (state, dests)
        val code = genRhs (state, env, dests, rhs)
      in
        code
      end

  fun genInstrs (state, env, is) = 
      Pil.S.sequence (Vector.toListMap (is, fn i => genInstr(state, env, i)))
       
  (*** Transfers ***)

  fun doSsaMoves (state, env, cb, block, arguments) = 
      let
        val M.B {parameters, ...} = MU.CodeBody.block (cb, block)
        val () = Fail.assert ("MilToPil", "doSsaMoves", "mismatch in phi args",
                              (fn () => (Vector.length parameters = 
                                         Vector.length arguments)))
        val parametersl = Vector.toList parameters
        val pi = List.mapi (parametersl, Utils.flip2)

        (* Find any variables that parameter i reads from *)
        fun depsOf (v, i) =
            case Vector.sub (arguments, i)
             of M.SVariable v' => VS.singleton v'
              | _              => VS.empty
        (* Topo sort such that things that parameter i reads from don't follow it,
         * then emit SSA moves in reverse order such that parameter i gets written
         * before anything that it reads from does.  For parameters involved in 
         * a strongly connected component, add temporaries.
         *)
        val scc = I.variableTopoSort (pi, depsOf)
        fun doOne vis =
            case vis
             of [(p, i)] =>
                let
                  val p' = genVarE (state, env, p)
                  val arg = Vector.sub (arguments, i)
                  val arg = genOperand (state, env, arg)
                in
                  Pil.S.expr (Pil.E.assign (p', arg))
                end
              | _ =>
                let
                  fun doOne (p, i) =
                      let
                        val nv = IM.variableClone (getStm state, p)
                        val p = genVarE (state, env, p)
                        val arg = Vector.sub (arguments, i)
                        val arg = genOperand (state, env, arg)
                        val vd = (genVarDec (state, env, nv), SOME arg)
                        val nv = genVarE (state, env, nv)
                        val a = Pil.S.expr (Pil.E.assign (p, nv))
                      in (vd, a)
                      end
                  val (vds, agsns) = List.unzip (List.map (vis, doOne))
                  val blk = Pil.S.block (vds, agsns)
                in blk
                end
        val moves = List.revMap (scc, doOne)
      in moves
      end

  val (backendYieldsF, backendYields) =
      Config.Feature.mk ("PPiler:use-backend-yields",
                         "Rely on backend compiler for yields")

  val isBackEdge = 
   fn (state, env, e) => LLS.member (getBackEdges env, e)

  fun genGoto (state, env, cb, src, M.T {block, arguments}) =
      let
        val pre = 
            if not (backendYields (getConfig env)) andalso isBackEdge (state, env, (src, block)) then
              Pil.S.yield
            else
              Pil.S.empty
        val moves = doSsaMoves (state, env, cb, block, arguments)
        val succ = genLabel (state, env, block)
      in
        Pil.S.sequence [pre, Pil.S.sequence moves, Pil.S.goto succ]
      end

  fun genCase (state, env, cb, src, {on,cases,default} : Mil.constant Mil.switch) =
      let
        val on = genOperand (state, env, on)
        fun isZero c =
            case c
             of M.CIntegral i => IntInf.isZero (IntArb.toIntInf i)
              | M.CBoolean b => not b
              | _ => false
        fun doIf ((c, t1), t2) =
            let
              val c' = genConstant (state, env, c)
              val gt1 = genGoto (state, env, cb, src, t1)
              val gt2 = genGoto (state, env, cb, src, t2)
              val s =
                  if isZero c then
                    Pil.S.ifThenElse (on, gt2, gt1)
                  else
                    Pil.S.ifThenElse (Pil.E.equal (on, c'), gt1, gt2)
            in s
            end
        fun doNameArm (c, t) =
            case c
             of M.CName n => 
                (Pil.E.int (I.nameNumber n), genGoto (state, env, cb, src, t))
              | _ =>
                Fail.fail ("MilToPil", "genTransfer", "Mixed constants")
        fun doGenArm (c, t) =
            (genConstant (state, env, c), genGoto (state, env, cb, src, t))
        fun doRefArm (c, t) =
            (Pil.E.cast (Pil.T.sintp, genConstant (state, env, c)), genGoto (state, env, cb, src, t))
      in
        case (Vector.length cases, default)
         of (0, NONE) => Fail.fail ("MilToPil", "genCase", "no cases")
          | (0, SOME t) => genGoto (state, env, cb, src, t)
          | (1, NONE) => genGoto (state, env, cb, src, #2 (Vector.sub (cases, 0)))
          | (1, SOME t) => doIf (Vector.sub (cases, 0), t)
          | (2, NONE) =>
            let
              val c1 = Vector.sub (cases, 0)
              val c2 = Vector.sub (cases, 1)
              val (c1, c2) = if isZero (#1 c1) then (c1, c2) else  (* only swap if disjoint *)
                             if isZero (#1 c2) then (c2, c1) else (c1, c2)
            in doIf (c1, #2 c2)
            end
          | _ =>
            let
              val (on, doArm) =
                  case #1 (Vector.sub (cases, 0))
                   of M.CName _ => (Pil.E.call (Pil.E.namedConstant RT.Name.getTag, [on]), doNameArm)
                    | M.CRef _  => (Pil.E.cast (Pil.T.sintp, on), doRefArm)
                    | _         => (Pil.E.cast (Pil.T.sintp, on), doGenArm)
              val arms = Vector.toListMap (cases, doArm)
              val default =
                  case default
                   of NONE => NONE
                    | SOME t => SOME (genGoto (state, env, cb, src, t))
              val res = Pil.S.switch (on, arms, default)
            in res
            end
      end

  fun genCutsTo (state, env, M.C {targets = cuts, ...}) =
      List.map (LS.toList cuts,
             fn t => genVar (state, env, genContVar (state, env, t)))

  fun genCall (state, env, cc, c, args, ret) =
      let
        val args = genOperands (state, env, args)
        val (f, args) =
            case c
             of M.CCode {ptr, ...} => (genVarE (state, env, ptr), args)
              | M.CClosure _       => notCoreMil (env, "genCall", "CClosure")
              | M.CDirectClosure _ => notCoreMil (env, "genCall",
                                                  "CDirectClosure")
        val s =
            case ret
             of M.RNormal {rets, block, cuts, ...} =>
                let
                  val cuts = genCutsTo (state, env, cuts)
                  val c = Pil.E.callAlsoCutsTo (getConfig env, f, args, cuts)
                  val c =
                      case Vector.length rets
                       of 0 => Pil.S.expr c
                        | 1 =>
                          let
                            val rv = Vector.sub (rets, 0)
                            val () = addLocal (state, rv)
                            val rv = genVarE (state, env, rv)
                          in
                            Pil.S.expr (Pil.E.assign (rv, c))
                          end
                        | _ => 
                          Fail.fail ("MilToPil",
                                     "genCall",
                                     "non-single return call")
                  val block = genLabel (state, env, block)
                  val s = Pil.S.sequence [c, Pil.S.goto block]
                in s
                end
              | M.RTail {exits} =>
                (case rewriteThunks (env, cc)
                  of SOME t =>
                     let
                       val call = Pil.E.call (f, args)
                       val rt = MU.Code.thunkTyp (getFunc env)
                       val typ = typToFieldKind (env, rt)
                       val stm = getStm state
                       val vret = MSTM.variableFresh (stm, "ret", rt, M.VkLocal)
                       val () = addLocal (state, vret)
                       val vret = genVarE (state, env, vret)
                       val calls = Pil.S.expr (Pil.E.assign (vret, call))
                       val ret = Pil.E.namedConstant (RT.Thunk.return typ)
                       val rets = Pil.S.call (ret, [genVarE (state, env, t), vret])
                     in Pil.S.sequence [calls, rets]
                     end
                   | NONE =>
                     let
                       val M.F {rtyps, ...} = getFunc env
                       val void = Vector.length rtyps = 0
                     in
                       Pil.S.tailCall (getConfig env, void, f, args)
                     end)
      in s
      end

  fun genEval (state, env, cc, fk, e, ret) =
      let
        val (thunk, slowf, slowargs) =
            case e
             of M.EThunk {thunk, ...} => (thunk, RT.Thunk.call fk, [thunk])
              | M.EDirectThunk {thunk, code, ...} =>
                (thunk, RT.Thunk.callDirect fk, [code, thunk])
        val (cuts, t, cont, g) =
            case ret
             of M.RNormal {rets, block, cuts} =>
                (case Vector.length rets
                  of 1 =>
                     let
                       val rv = Vector.sub (rets, 0)
                       val () = addLocal (state, rv)
                       val rt = getVarTyp (state, rv)
                       val rv = genVarE (state, env, rv)
                       fun cont e = Pil.S.expr (Pil.E.assign (rv, e))
                       val g = Pil.S.goto (genLabel (state, env, block))
                     in (cuts, rt, cont, g)
                     end
                   | _ => Fail.fail ("MilToPil", "genEval", "rets must be 1"))
              | M.RTail {exits} =>
                let
                  val cuts = M.C {exits = exits, targets = LS.empty}
                  val rtyp = MU.Code.thunkTyp (getFunc env)
                  val cont = 
                      (case rewriteThunks (env, cc)
                        of SOME t => 
                           (fn e =>
                               let
                                 val stm = getStm state
                                 val vret = MSTM.variableFresh (stm, "ret", rtyp, M.VkLocal)
                                 val () = addLocal (state, vret)
                                 val vret = genVarE (state, env, vret)
                                 val evals = Pil.S.expr (Pil.E.assign (vret, e))
                                 val rets = 
                                     Pil.S.call (Pil.E.namedConstant (RT.Thunk.return fk),
                                                 [genVarE (state, env, t), vret])
                               in Pil.S.sequence [evals, rets]
                               end)
                         | NONE => fn e => Pil.S.returnExpr e)
                  val g = Pil.S.empty
                in (cuts, rtyp, cont, g)
                end
        val t = genTyp (state, env, t)
        val thunk = genVarE (state, env, thunk)
        val slowf = Pil.E.namedConstant slowf
        val slowargs = List.map (slowargs, fn v => genVarE (state, env, v))
        val cuts = genCutsTo (state, env, cuts)
        val slowpath = Pil.E.callAlsoCutsTo (getConfig env, slowf, slowargs, cuts)
        val slowpath = Pil.E.cast (t, slowpath)
        val slowpath = cont slowpath
        val fastpath = genThunkGetValueE (state, env, fk, t, thunk)
        val fastpath = cont fastpath
        val control =
            Pil.E.call (Pil.E.namedConstant (RT.Thunk.isEvaled fk), [thunk])
        val res = Pil.S.ifThenElse (control, fastpath, slowpath)
        val res = Pil.S.sequence [res, g]
      in res
      end

  fun genInterProc (state, env, cc, ip, ret) =
      case ip
       of M.IpCall {call, args} => genCall (state, env, cc, call, args, ret)
        | M.IpEval {typ, eval} => genEval (state, env, cc, typ, eval, ret)

  fun genTransfer (state, env, src, t) = 
      let
        val M.F {cc, body = cb, rtyps, ...} = getFunc env
      in
        case t
         of M.TGoto tg => genGoto (state, env, cb, src, tg)
          | M.TCase s => genCase (state, env, cb, src, s)
          | M.TInterProc {callee, ret, ...} => genInterProc (state, env, cc, callee, ret)
          | M.TReturn os =>
            (case Vector.length os
              of 0 => Pil.S.return
               | 1 =>
                 let
                   val opnd = genOperand (state, env, Vector.sub (os, 0))
                 in
                   case rewriteThunks (env, cc)
                    of SOME t =>
                       let
                         val rt = MU.Code.thunkTyp (getFunc env)
                         val typ = typToFieldKind (env, rt)
                         val ret = Pil.E.namedConstant (RT.Thunk.return typ)
                         val args = [genVarE (state, env, t), opnd]
                         val s = Pil.S.call (ret, args)
                       in s
                       end
                     | NONE => Pil.S.returnExpr opnd
                 end
               | _ => Fail.fail ("MilToPil", "genTransfer", "multiple returns not supported"))
          | M.TCut {cont, args, cuts} =>
            let
              (* XXX NG: there is a bug in Pillar for arguments and no C implementation - for now punt and hope! *)
              val args = [] (*genOperands (state, env, args)*)
              val cuts = genCutsTo (state, env, cuts)
              val c = Pil.E.cast (Pil.T.continuation, genVarE (state, env, cont))
              val cut = Pil.S.contCutTo (getConfig env, c, args, cuts)
            in cut
            end
          | M.THalt opnd => 
            let
              val M.F {rtyps, ...} = getFunc env
              val void = Vector.length rtyps = 0
              val halt = if void then RT.haltV else RT.halt
            in Pil.S.call (Pil.E.namedConstant halt, [genOperand (state, env, opnd)])
            end
          | M.TPSumCase _ => notCoreMil (env, "genTransfer", "TPSumCase")
       end

  (*** Blocks ***)

  fun genPrintLabel (state, env, l) =
      Pil.S.expr (Pil.E.call (Pil.E.variable (Pil.identifier "printf"),
                              [Pil.E.string "L%d\n",
                               Pil.E.int (I.labelNumber l)]))

  val (instrumentBlocksF, instrumentBlocks) =
      Config.Feature.mk ("Pil:instrument-blocks",
                         "every block prints its label")

  fun genBlock (state, env, bid, block) = 
      let
        val label = Pil.S.label (genLabel (state, env, bid))
        val label =
            if instrumentBlocks (getConfig env) then 
              Pil.S.sequence [label, genPrintLabel (state, env, bid)]
            else
              label
        val M.B {parameters, instructions, transfer} = block
        val () = addLocals(state, parameters)
        val is = genInstrs(state, env, instructions)
        val xfer = genTransfer(state, env, bid, transfer)
      in Pil.S.sequence [label, is, xfer]
      end

  (*** Code Bodies ***)

  fun genCB (state, env, cb as (M.CB {entry, blocks, ...})) =
      let
        val () = clearLocals state
        fun doBlock (bid, b) = (bid, genBlock (state, env, bid, b))
        val entry = Pil.S.goto (genLabel (state, env, entry))
        val config = getConfig env
        val blocks = List.map (MU.CodeBody.listRPO (config, cb), doBlock)
        fun doOne ((bid, s), (decs, ss)) =
            case getCont (state, bid)
             of NONE => (decs, s::ss)
              | SOME cv =>
                let
                  val cl = genLabel (state, env, bid)
                  val cv = genVar (state, env, cv)
                  val cdec = Pil.contDec cv
                  val centry = Pil.S.contEntry (cl, cv)
                in
                  (cdec::decs, centry::s::ss)
                end
        val (cdecs, blocks) = List.foldr (blocks, ([], []), doOne)
        val decs = genVarsDec (state, env, getLocals state) @ cdecs
        val decs = List.map (decs, fn vd => (vd, NONE))
        val () = clearLocals(state)
      in
        (decs, entry::blocks)
      end

  (*** Generating the forward declarations ***)

  fun genForward (state, env, v, g) =
      let
        (* static vut vu;
         * #define v ((vt)&vu)
         *)
        val declareAndDefine = 
         fn (v, vuT) =>
            let
              val vT = genTyp (state, env, getVarTyp (state, v))
              val vu = unboxedVar (state, env, v)
              val v = genVar (state, env, v)
              val dec1 = Pil.D.staticVariable (Pil.varDec (vuT, vu))
              val dec2 = 
                  Pil.D.constantMacro (v, Pil.E.cast (vT, Pil.E.addrOf (Pil.E.variable vu)))
            in Pil.D.sequence [dec1, dec2]
            end
        val error =
         fn s => fail ("genForward", s^" shouldn't be in a connect component")
        val d = 
            case g
             of M.GCode (M.F {cc, args, rtyps, ...}) =>
                let
                  val c = getConfig env
                  val si = getSymbolInfo state
                  val cc = MTT.callConv (c, si, cc)
                  val args = Vector.map (args, fn a => MTT.variable (c, si, a))
                  val ubt = genCodeType (state, env, (cc, args, rtyps))
                  val x = genVar (state, env, v)
                  val d = Pil.D.staticVariable (Pil.varDec (ubt, x))
                in d
                end
              | M.GErrorVal t => error "GErrorVal"
              | M.GIdx t => error "GIdx"
              | M.GTuple {mdDesc, inits} =>
                let
                  val M.MDD {pok, ...} = mdDesc
                  val tv = globalTVar (state, env, v)
                  val ts = MTT.operands (getConfig env, getSymbolInfo state, inits)
                  val fs = genTyps (state, env, ts)
                  val utt = tupleUnboxedTyp (pok, fs, NONE)
                  val ut = Pil.D.typDef (utt, tv)
                  val dec = declareAndDefine (v, Pil.T.named tv)
                in Pil.D.sequence [ut, dec]
                end
              | M.GRat t => error "GRat"
              | M.GInteger t => error "GInteger"
              | M.GThunkValue {typ, ofVal} => 
                let
                  val ut = Pil.T.named (RT.Thunk.unboxedTyp typ)
                  val dec = declareAndDefine (v, ut)
                in dec
                end
              | M.GSimple s => (* This is supposed to be eliminated?  XXX -leaf *)
                Pil.D.constantMacro (genVar (state, env, v), genSimple (state, env, s))
              | _ => fail ("genForward", "Unlowered global")
      in d
      end

  (* If a global is in a strongly connected component by itself, we
   * don't generate forwards.  This function should generate that part
   * of the forwards that are really necessary.
   *)
  fun genGlobalSingle (state, env, v, g) =
      case g
       of M.GTuple {mdDesc, inits} =>
          let
            val M.MDD {pok, ...} = mdDesc
            val ts = MTT.operands (getConfig env, getSymbolInfo state, inits)
            val fs = genTyps (state, env, ts)
            val t = tupleUnboxedTyp (pok, fs, NONE)
            val tv = globalTVar (state, env, v)
            val ut = Pil.D.typDef (t, tv)
          in [ut]
          end
        | M.GSimple s => (* This is supposed to be eliminated?  XXX -leaf *)
          [Pil.D.constantMacro (genVar (state, env, v), genSimple (state, env, s))]
        | _ => []

  (*** Globals ***)       

  fun doThunkCallConv (state, env, f, thunk, fvs, decs) =
      let
        val rt = MU.Code.thunkTyp f
        val rfk = typToFieldKind (env, rt)
        val fvts = Vector.map (fvs, fn v => getVarTyp (state, v))
        val fvfks = Vector.map (fvts, fn t => typToFieldKind (env, t))
        val te = genVarE (state, env, thunk)
        fun getField (i, x) =
            let
              val d = genVarDec (state, env, x)
              val t = genTyp (state, env, getVarTyp (state, x))
              val f = genThunkFvProjection (state, env, rfk, fvfks, te, i, t)
            in (d, SOME f)
            end
        fun zeroField (i, _) =
            let
              val fk = Vector.sub (fvfks, i)
            in
              if MU.FieldKind.isRef fk then
                let
                  val t = genTyp (state, env, Vector.sub (fvts, i))
                  val f = genThunkFvProjection (state, env, rfk, fvfks, te, i, t)
                  val z = Pil.S.expr (writeBarrier (state, env, te, f, Pil.E.int 0, fk, false))
                in [z]
                end
              else
                []
            end
        val dec = genVarDec (state, env, thunk)
        val fvsl = Vector.toList fvs
        val unpacks = List.mapi (fvsl, getField)
        val zeros = List.concat (List.mapi (fvsl, zeroField))
        val bh = Pil.S.expr (Pil.E.call (Pil.E.variable (RT.Thunk.blackHole rfk), [te]))
      in (dec::decs, unpacks, zeros@[bh])
      end

  fun doCallConv (state, env, f, cc, args) = 
      let
        val decs = Vector.toListMap (args, fn x => genVarDec (state, env, x))
        val res = 
            case cc
             of M.CcCode => (decs, [], [])
              | M.CcClosure _ =>
                notCoreMil (env, "doCallConv", "CcClosure")
              | M.CcThunk {thunk, fvs} =>
                doThunkCallConv (state, env, f, thunk, fvs, decs)
      in res
      end
  
  fun genPrintFunction (state, env, f) =
      Pil.S.expr
        (Pil.E.call (Pil.E.variable (Pil.identifier "printf"),
                     [Pil.E.string "F%s\n",
                      Pil.E.string (IM.variableString (getStm state, f))]))

  val (instrumentFunctionsF, instrumentFunctions) =
      Config.Feature.mk ("Pil:instrument-functions",
                         "every function prints its name")

  val findBackEdges =
   fn (state, env, cb as M.CB {entry, blocks}) =>
      let
        (* By invariant, l has not been visited *)
        val rec visit = 
            fn (src, (seen, be)) => 
               let
                 val seen = LS.insert (seen, src)
                 val b = MU.CodeBody.block (cb, src)
                 val {blocks, exits} = MU.Block.successors b
                 val folder = fn (tgt, s) => visitEdge ((src, tgt), s)
               in LS.fold (blocks, (seen, be), folder)
               end
        and rec visitEdge = 
         fn (e as (src, tgt), s as (seen, be)) =>
            if LS.member (seen, tgt) then
              (seen, LLS.insert (be, e))
            else
               visit (tgt, s)
        val (seen, backEdges) = visit (entry, (LS.empty, LLS.empty))
      in backEdges
      end

  fun genFunction (state, env, f, func) =
      let
        val M.F {fx, cc, args, rtyps, body, ...} = func
        val backEdges = findBackEdges (state, env, body)
        val env = enterFunction (env, func, backEdges)
        val (decs, ls1, ss) = doCallConv (state, env, func, cc, args)
        val tcc = MU.CallConv.map (cc, fn v => getVarTyp (state, v))
        val rt = genReturnType (state, env, tcc, rtyps)
        val (ls, b) = genCB (state, env, body)
        val (ls2, b) =
            case rewriteThunks (env, cc)
             of SOME thunk =>
                if interceptCuts env then
                  let
                    val cont = Pil.identifier "cutHandle"
                    val arg = Pil.identifier "cutCont"
                    val ls = [(Pil.varDec (Pil.T.continuation, arg), NONE)]
                    val conts = Pil.S.continuation (cont, [arg])
                    val rt = MU.Code.thunkTyp func
                    val typ = typToFieldKind (env, rt)
                    val setcut =
                        Pil.S.call (Pil.E.namedConstant
                                      (RT.Thunk.cut typ),
                                    [genVarE (state, env, thunk),
                                     Pil.E.variable arg])
                    val b = [Pil.S.vse (cont,
                                        Pil.S.sequence (b @ [conts, setcut]))]
                  in (ls, b)
                  end
                else
                  ([], b) 
              | NONE => ([], b)
        val ls = ls1 @ ls @ ls2
        val i =
            if instrumentFunctions (getConfig env)
            then [genPrintFunction (state, env, f)]
            else []
        val b = i @ ss @ b
      in 
        Pil.D.staticFunction (rt, genVar (state, env, f), decs, ls, b)
      end

   val (assertSmallIntsF, assertSmallInts) = 
       Config.Feature.mk ("Plsr:tagged-ints-assert-small",
                          "use 32 bit ints for rats (checked)")
       
  fun genStaticIntInf (state, env, i) = 
      let
        val () = if assertSmallInts (getConfig env) then
                   Fail.fail ("MilToPil", "genStaticIntInf", 
                              "Failed small int assertion")
                 else
                   ()
        (* Just used to create unique variables for the
         * structure components.  These variables are needed
         * to work around a pillar bug.  -leaf *)
        val v_dummy = 
            let
              val stm = getStm state
              val v = MSTM.variableFresh (stm, "integer", M.TPAny, M.VkLocal)
            in v
            end
        fun genLocal s =
            let
              val v = deriveInternalVar (state, env, s, v_dummy)
            in Pil.E.variable (Pil.identifier v)
            end
        val sign = 
            Pil.E.variable (if i < 0 then 
                              RT.Integer.signNeg
                            else if i > 0 then 
                              RT.Integer.signPos
                            else
                              RT.Integer.signZero)
        val digits = Utils.intInfAbsDigits32 i (*msd first*)
        val folder = 
            let
              val defUnboxed = RT.Integer.staticConsUnboxedDef
              val refUnboxed = Pil.E.variable RT.Integer.staticConsRef
            in fn (i, d, tl) =>
                  let
                    val v = genLocal ("digit_ubx"^(Int.toString i))
                    val d = Pil.E.word32 d
                    val def = Pil.D.macroCall (defUnboxed, [v, d, tl])
                    val tl = Pil.E.call (refUnboxed, [v])
                    val () = addGlobal (state, tl)
                  in (def, tl)
                  end
            end
        val empty = Pil.E.variable (RT.Integer.staticEmpty)
        val (code, digits) = Utils.List.mapFoldli (digits, empty, folder)
        val vi = genLocal ("integer_ubx")
        val iDef = Pil.D.macroCall (RT.Integer.staticDef, [vi, sign, digits])
        val code = code@[iDef]
        val e = Pil.E.call (Pil.E.variable RT.Integer.staticRef, [vi])
        val () = addGlobal (state, e)
      in (code, e)
      end

  fun genGlobal (state, env, var, global, preDefined) = 
      let
        val define = fn (v, g) => Pil.D.constantMacro (genVar (state, env, v), g)
        val addGlobalReg = 
         fn vu => 
            let
              val vr = Pil.E.addrOf vu
              val () = addGlobal (state, vr)
            in ()
            end

        val mkGlobalDef = 
         fn (v, vu, s) => 
            if preDefined then
              Pil.D.comment (s ^ " forward defined above")
            else
              let
                val vr = Pil.E.addrOf vu
                val t = genTyp (state, env, getVarTyp (state, v))
                val g = Pil.E.cast (t, vr)
                val d = define (v, g)
              in d
              end

        val res = 
            case global
             of M.GCode code  => genFunction (state, env, var, code)
              | M.GErrorVal t => 
                let
                  val t = getVarTyp (state, var)
                  val g = Pil.E.call (Pil.E.variable RT.gErrorVal, [Pil.E.hackTyp (genTyp (state, env, t))])
                  val d = define (var, g)
                in d
                end
              | M.GIdx dict =>
                (* Here we declare the idx global, but its entries are
                 * initialised in genInit below.
                 *)
                let
                  val elts = ND.toList dict
                  val newv  = Pil.E.variable (unboxedVar (state, env, var))
                  val idx = 
                      case List.length elts
                       of 0 => Pil.D.macroCall (RT.Idx.staticEmpty, [newv])
                        | n => 
                          let
                            val len = RT.Idx.chooseLen n
                            val len' = Pil.E.int len
                            fun mkOne () = Pil.E.variable RT.Idx.staticElt
                            val inits = List.duplicate (len, mkOne)
                            val idx =
                                Pil.D.macroCall (RT.Idx.static,
                                                 newv :: len' :: inits)
                          in idx
                          end
                  val () = addGlobalReg newv
                  val d = mkGlobalDef (var, newv, "Idx")
                  val d = Pil.D.sequence [idx, d]
                in d
                end
              | M.GTuple {mdDesc, inits} =>
                let
                  val c = getConfig env
                  val tv = Pil.T.named (globalTVar (state, env, var))
                  val no =
                      if instrumentAllocationSites c
                      then SOME (I.variableString' var)
                      else NONE
                  val vtable = MD.genMetaData (state, env, no, mdDesc, true)
                  fun doOne s = genSimple (state, env, s)
                  val fields = Vector.toListMap (inits, doOne)
                  val elts = vtable::fields
                  val newvId = unboxedVar (state, env, var)
                  val newv  = Pil.E.variable newvId
                  val init = Pil.E.strctInit elts
                  val tuple = Pil.D.staticVariableExpr (Pil.varDec (tv, newvId), init)
                  val () = addGlobalReg newv
                  val tupleptr = mkGlobalDef (var, newv, "Tuple")
                  val d = Pil.D.sequence [tuple, tupleptr]
                in d
                end
              | M.GRat r =>
                let
                  val (num, den) = Rat.toInts r
                  val c = Pil.D.comment ("Rat: " ^ Rat.toString r)         
                  val (code_n, num) = genStaticIntInf (state, env, ~num)
                  val (code_d, den) = genStaticIntInf (state, env, den)
                  val newv  = Pil.E.variable (unboxedVar (state, env, var))
                  val rDef = Pil.D.macroCall (RT.Rat.staticDef, [newv, num, den])
                  val () = addGlobalReg newv
                  val d = mkGlobalDef (var, newv, "GRat")
                  val d = Pil.D.sequence (c::code_n@code_d@[rDef, d])
                in d
                end
              | M.GInteger i =>
                let
                  val c = Pil.D.comment ("Integer: " ^ IntInf.toString i)
                  val (code, i) = genStaticIntInf (state, env, i)
                  val d = define (var, i)
                  val d = Pil.D.sequence [Pil.D.sequence (c::code), d]
                in d
                end
              | M.GCString s =>
                Pil.D.staticVariableExpr
                  (Pil.varDec (Pil.T.array Pil.T.char, genVar (state, env, var)), Pil.E.string s)
              | M.GThunkValue {typ, ofVal} =>
                 let
                   val s = genSimple (state, env, ofVal)
                   val newv  = Pil.E.variable (unboxedVar (state, env, var))
                   val thnk = Pil.D.macroCall (RT.Thunk.staticValue typ, [newv, s])
                   val () = addGlobalReg newv
                   val d = mkGlobalDef (var, newv, "Thunk")
                   val d = Pil.D.sequence [thnk, d]
                 in d
                 end
              | M.GSimple s => Pil.D.sequence []  (* This is supposed to be eliminated?  XXX -leaf *)
              | M.GClosure code => notCoreMil (env, "genGlobal", "GClosure")
              | M.GPSum _ => notCoreMil (env, "genGlobal", "GPSum")
              | M.GPSet _ => notCoreMil (env, "genGlobal", "GPSet")
      in 
        res
      end

  fun genGlobals (state, env, gs) =
      let
        (* Compute the strongly connected components of the globals *)
        val () = Chat.log2 (env, "Computing SCCs")
        val c = getConfig env
        fun depsOf (v, g) = MFV.global (c, v, g)
        val scc = I.variableTopoSort (VD.toList gs, depsOf)
        val () = Chat.log2 (env, "Emitting globals")
        (* Process each strongly connected component *)
        fun doOneScc scc =
            case scc
             of [(v, g)] =>
                (* Only one global in this component, so do not generate
                 * forwards, but do emit the necessary stuff from the forwards.
                 *)
                let
                  val () = incSingleGlobals state
                  val ds = genGlobalSingle (state, env, v, g)
                in
                  Pil.D.sequence [Pil.D.comment "Global",
                                  Pil.D.sequence ds,
                                  genGlobal (state, env, v, g, false)]
                end
              | _ =>
                let
                  val () = incMultipleGlobals state
                  fun doForward (v, g) = genForward (state, env, v, g)
                  val fs = List.map (scc, doForward)
                  fun doGlobal (v, g) = genGlobal (state, env, v, g, true)
                  val gs = List.map (scc, doGlobal)
                  val ds = Pil.D.sequence [Pil.D.comment "Forwards",
                                           Pil.D.sequence fs,
                                           Pil.D.comment "Globals",
                                           Pil.D.sequence gs]
                in ds
                end
      in
        Pil.D.sequence (List.map (scc, doOneScc))
      end

  (*** Report global roots ***)

  fun genReportGlobals (state, env) =
      let
        val gs = getGlobals state
        val t = Pil.T.named RT.T.object
        val gs = List.map (gs, fn g => Pil.E.cast (t, g))
        val gsCount = List.length gs
        val init = Pil.E.strctInit gs
        val glbs = Pil.identifier "plsrGlobals"
        val t = Pil.T.array t
        val glbsdec = Pil.D.staticVariableExpr (Pil.varDec (t, glbs), init)
        val reg = Pil.E.call (Pil.E.namedConstant RT.GC.registerGlobals,
                              [Pil.E.variable glbs,
                               Pil.E.int gsCount])
      in ([glbsdec], [Pil.S.expr reg])
      end

  fun genReportRoots (state, env) =
      if gcGRoots env then
        let            
          val a1 = Pil.identifier "er"
          val a2 = Pil.identifier "env"
          val vd1 = Pil.varDec (Pil.T.named RT.GC.rseCallBack, a1)
          val vd2 = Pil.varDec (Pil.T.ptr Pil.T.void, a2)
          val () = getGRoots state
          val outOfGlobals = []
          val body = outOfGlobals
        in
          Pil.D.sequence
            [Pil.D.comment "Report Global Roots",
             Pil.D.managed (getConfig env, false),
             Pil.D.staticFunction
               (Pil.T.void, RT.GC.reportRoots, [vd1, vd2], [], body),
             Pil.D.managed (getConfig env, true),
             Pil.D.blank]
        end
      else
        Pil.D.sequence []

  (*** Initialisation ***)

  fun genInit (state, env, entry, globals) = 
      let
        (* The runtime needs to know the \core\char\ord name *)
        val ord = IM.nameMake (getStm state, Prims.ordString)
        val ord = genName (state, env, ord)
        val or = Pil.E.call (Pil.E.variable RT.Name.registerCoreCharOrd, [ord])
        val () = addReg1 (state, Pil.S.expr or)
        val registrations0 = getRegs0 state
        val registrations1 = getRegs1 state
        val () = Stats.addToStat (getStats state, "registrations",
                                  List.length registrations0 + List.length registrations1)
        (* For each global index, initialise its entries. *)
        fun initIdx (v, g, idxs) =
            case g
             of M.GIdx d => 
                let
                  val dest = genVarE (state, env, v)
                  val elts = ND.toList d
                  val stm = getStm state
                  fun cf ((n1, _), (n2, _)) =
                      IM.nameString (stm, n1) <= IM.nameString (stm, n2)
                  val elts = QuickSort.sortList (elts, cf)
                  fun genIndices (n, i) = 
                      let
                        val n = genName (state, env, n)
                        val res = Pil.E.call (Pil.E.namedConstant RT.Idx.set,
                                              [dest, n, Pil.E.int i])
                      in Pil.S.expr res
                      end
                  val inits = List.map (elts, genIndices)
                in
                  (Pil.S.sequence inits)::idxs
                end
              | _ => idxs
        val idxs = VD.fold (globals, [], initIdx)
        (* Report globals *)
        val (xtras, globals) = 
            if gcGlobals env then genReportGlobals (state, env) else ([], [])
        (* Run the program *)
        val run = Pil.S.expr (Pil.E.call (genVarE (state, env, entry), []))
        val body = registrations0 @ globals @ registrations1 @ idxs @ [run]
      in
        Pil.D.sequence (xtras @
                        [Pil.D.function (Pil.T.void, RT.pmain, [], [], body)])
      end

  (*** The main thing ***)

  fun program (pd, filename, p) =
      let
        val config = PassData.getConfig pd
        val M.P {includes, externs, globals, symbolTable, entry} = p
        val state = newState symbolTable
        val env = newEnv (config, globals)
        val () = Chat.log2 (env, "Starting CodeGen")

        fun doGroup (getKind, doIt, group) = 
            let
              fun doOne (g, m) =
                  let
                    val (s, m) =
                        case (m, getKind g)
                         of (false, M.IkC     ) => (Pil.D.sequence [],          false)
                          | (false, M.IkTarget) => (Pil.D.managed (getConfig env, true),  true )
                          | (true,  M.IkC     ) => (Pil.D.managed (getConfig env, false), false)
                          | (true,  M.IkTarget) => (Pil.D.sequence [],          true )
                    val d = Pil.D.sequence [s, doIt g]
                  in (d, m)
                  end
              val (incs, m) = Vector.mapAndFold (group, true, doOne)
              val incs = Pil.D.sequence (Vector.toList incs @ (if m then [] else [Pil.D.managed (getConfig env, true)]))
            in incs
            end

        val incs = doGroup (MU.IncludeFile.kind, Pil.D.includeLocalFile o MU.IncludeFile.name, includes)

        val exts = 
            let
              fun doIt g = 
                  let
                    fun doOne extern =
                        let
                          (* NG XXX: Should we special case this on functions for syntax? *)
                          val t = getVarTyp (state, extern)
                          val t = genTyp (state, env, t)
                          val d = Pil.D.externVariable (Pil.varDec (t, genVar (state, env, extern)))
                        in d
                        end
                    val externs = Pil.D.sequence (List.map (VS.toList (MU.ExternGroup.externs g), doOne))
                  in externs
                  end
            in doGroup (MU.ExternGroup.kind, doIt, externs)
            end

        val omDefs = OM.genDefs (state, env)
        val globs = genGlobals (state, env, globals)
        val () = Chat.log2 (env, "Emitting global root reporting and init code")
        val rgr = genReportRoots (state, env)
        val init = genInit (state, env, entry, globals)
        val () = Chat.log2 (env, "Finishing extras")
        val xtrs = Pil.D.sequence (getXtrGlbs state)
        val typs = Pil.D.sequence (getTypDecs state)
        val () = Chat.log2 (env, "Finished CodeGen")
        val d =
            Pil.D.sequence
              [Pil.D.comment "Ppiler generated code",
               Pil.D.comment ("Source: " ^ filename ^ ".p"),
               Pil.D.blank,
               omDefs,
               Pil.D.blank,
               Pil.D.includeLocalFile "pil",
               Pil.D.includeLocalFile "plsr",
               Pil.D.blank,
               incs,
               exts,
               Pil.D.blank,
               Pil.D.comment "Types",
               typs,
               Pil.D.blank,
               Pil.D.comment "Names and Vtables",
               Pil.D.blank,
               xtrs,
               Pil.D.blank,
               globs,
               Pil.D.blank,
               rgr,
               Pil.D.comment "Initializer",
               init
              ]
        val () = if Config.reportEnabled (config, passname) then Stats.report (getStats state) else ()
      in
        Pil.D.layout d
      end

  val features =
      [instrumentAllocationSitesF, 
       instrumentBlocksF, 
       instrumentFunctionsF,
       assertSmallIntsF,
       backendYieldsF,
       lightweightThunksF]

end;

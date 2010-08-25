(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_PARSE =
sig
end;

structure MilParse :> MIL_PARSE =
struct

  val modname = "MilParse"
  fun fail (f, m) = Fail.fail (modname, f, m)
  fun unimplemented (f, m) = Fail.unimplemented (modname, f, m)

  structure SD = StringDict

  type instreamBase = Pervasive.TextIO.StreamIO.instream

  type instream = instreamBase * int * int

  fun instreamMk (ins : instreamBase) : instream = (ins, 1, 0)

  local
    fun next (ins, l, c) =
        case Pervasive.TextIO.StreamIO.input1 ins
         of NONE => NONE
          | SOME (ch, ins) => SOME (if ch = Char.newline then (ins, l+1, 0) else (ins, l, c+1), ch)
  in
  structure P = ParserF(type elt = char
                        type stream = instream
                        type pos = int * int
                        type error = string
                        fun pos (_, l, c) = (l, c)
                        val eof = "Unexpected end of file"
                        val next = next)
  end

  val || = P.||
  val && = P.&&

  infix || &&

  val white = P.satisfy (fn c => c = Char.space orelse c = Char.newline, "Expected whitespace")

  val whitespace = P.ignore (P.zeroOrMore white)

  fun syntax (p : 'a P.t) : 'a P.t = P.map (p && whitespace, #1)

  fun keychar' (kc : char) : char P.t = P.satisfy (fn c => c = kc, "Expected " ^ String.fromChar kc)

  fun keycharL (kc : char) : unit P.t = P.ignore (keychar' kc)

  fun keycharS (kc : char) : unit P.t = syntax (keycharL kc)

  fun keywordL (kw : string) : unit P.t =
      P.ignore (P.all (List.map (String.explode kw, keychar'))) || P.fail ("Expected " ^ kw)

  fun keywordS (kw : string) : unit P.t = syntax (keywordL kw)

  fun optFlag (kc : char) : bool P.t = P.succeeds (keycharS kc)

  fun commaSeq (p : 'a P.t) : 'a Vector.t P.t = P.seqSepV (p, keycharS #",")

  fun semiComma (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      P.map (p1 && keycharS #";" && commaSeq p2, fn ((x, _), y) => (x, y))

  fun paren        (p : 'a P.t) : 'a P.t = P.map (keycharS #"(" && p && keycharS #")", fn ((_, x), _) => x)
  fun brace        (p : 'a P.t) : 'a P.t = P.map (keycharS #"{" && p && keycharS #"}", fn ((_, x), _) => x)
  fun bracket      (p : 'a P.t) : 'a P.t = P.map (keycharS #"[" && p && keycharS #"]", fn ((_, x), _) => x)
  fun angleBracket (p : 'a P.t) : 'a P.t = P.map (keycharS #"<" && p && keycharS #">", fn ((_, x), _) => x)

  val letters : string P.t =
      syntax (P.map (P.oneOrMore (P.satisfy (Char.isAlpha, "Expected letter")), String.implode))

  val identifier : string P.t =
      syntax (P.map (P.satisfy (Char.isAlpha, "Expected identifier")
                     && P.zeroOrMore (P.satisfy (Char.isAlphaNum, "Expected letter or digit")),
                     fn (c, cs) => String.implode (c::cs)))

  val decimal : int P.t =
      syntax (P.map (P.oneOrMore (P.satisfy (Char.isDigit, "Expected digit")),
                     fn cs => Option.valOf (Int.fromString (String.implode cs))))
      || P.fail "Expected nat"

  val int : int P.t =
      syntax (P.map (P.optional (keycharL #"~") && P.oneOrMore (P.satisfy (Char.isDigit, "Expected digit")),
                     fn (co, cs) =>
                        Option.valOf (Int.fromString (String.implode (case co of NONE => cs | SOME () => #"~"::cs)))))
      || P.fail "Expected int"

  val intInfL = 
      P.map (P.optional (keycharL #"~") && P.oneOrMore (P.satisfy (Char.isDigit, "Expected digit")),
             fn (n, cs) =>
                Option.valOf (IntInf.fromString (String.implode (case n of NONE => cs | SOME () => #"~"::cs))))

  val intInf = syntax intInfL || P.fail "Expected integer"

  val rat =
      syntax (P.map (intInfL && P.optional (keycharL #"/" && intInfL),
                     fn (i, io) => Rat.rat (i, case io of NONE => IntInf.one | SOME (_, i) => i)))
      || P.fail "Expected rational"

  structure I = Identifier
  structure IM = I.Manager
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure ND = I.NameDict
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure VI = VectorInstructions
  structure M = Mil
  structure MU = MilUtils

  datatype state = S of {stm : M.symbolTableManager, vars : M.variable SD.t ref, labels : M.label SD.t ref}

  fun stateMk (stm : M.symbolTableManager) : state = S {stm = stm, vars = ref SD.empty, labels = ref SD.empty}

  fun getStm (S {stm, ...} : state) : M.symbolTableManager = stm

  fun getVariable (S {stm, vars, ...} : state, pre : string, hint : string) : M.variable =
      let
        val full = pre ^ hint
      in
        case SD.lookup (!vars, full)
         of NONE => let val v = IM.variableFreshNoInfo (stm, hint) val () = vars := SD.insert (!vars, full, v) in v end
          | SOME v => v
      end

  fun getLabel (S {stm, labels, ...} : state, l : string) : M.label =
      case SD.lookup (!labels, l)
       of NONE => let val l' = IM.labelFresh stm val () = labels := SD.insert (!labels, l, l') in l' end
        | SOME l => l

  fun forkLabels (S {stm, vars, labels} : state) : state = S {stm = stm, vars = vars, labels = ref SD.empty}

  datatype env = E of {config : Config.t}

  fun envMk (config : Config.t) : env = E {config = config}

  val vectorElemType : VI.elemType P.t =
      P.required (P.map (identifier, VI.elemTypeOfString), "Expected vector element type")

  val vectorElemTypeShort : VI.elemType P.t =
      P.required (P.map (identifier, VI.elemTypeOfStringShort), "Expected vector element type")

  fun variable (state : state, env : env) : M.variable P.t =
      let
        val pre = keycharL #"v" && P.zeroOrMore (P.satisfy (Char.isDigit, "Expected digit")) && keycharL #"_"
        val pre = P.map (pre, fn ((_, cs), _) => "v" ^ String.implode cs ^ "_")
        fun hintChar c = Char.isAlphaNum c orelse c = #"_"
        val hint = P.map (P.zeroOrMore (P.satisfy (hintChar, "Expected hint character")), String.implode)
        val p = syntax (P.map (pre && hint, fn (p, h) => getVariable (state, p, h))) || P.fail "Expected variable"
      in p
      end

  fun name (state : state, env : env) : M.name P.t =
      let
        val pre = keycharL #"n" && P.zeroOrMore (P.satisfy (Char.isDigit, "Expected digit")) && keycharL #"_"
        val nameChar = Char.isAlphaNum
        val str = P.oneOrMore (P.satisfy (nameChar, "Expected name character"))
        val p = P.map (pre && str, fn (_, cs) => MU.SymbolTableManager.nameMake (getStm state, String.implode cs))
        val p = syntax p || P.fail "Expected name"
      in p
      end

  fun label (state : state, env : env) : M.label P.t =
      syntax (P.map (keycharL #"L" && P.oneOrMore (P.satisfy (Char.isDigit, "Expected digit")),
                     fn (_, ds) => getLabel (state, String.implode ds)))
      || P.fail "Expected label"

  fun effects (state : state, env : env) : M.effects P.t =
      let
        val p = P.zeroOrMore (P.required (P.map (P.get, Effect.charToSet), "Expected effect"))
        val p = P.map (p, Effect.unionL)
        val p = syntax (P.map (keycharL #"{" && p && keycharL #"}", fn ((_, fx), _) => fx))
      in p
      end

  fun callConv (state : state, env : env, f : state * env -> 'a P.t) : 'a M.callConv P.t =
      let
        val doAux = paren (semiComma (f (state, env), f (state, env)))
        fun doIt s =
            case s
             of "CcCode" => P.succeed M.CcCode
              | "CcClosure" => P.bind doAux (fn (x, y) => P.succeed (M.CcClosure {cls = x, fvs = y}))
              | "CcThunk" => P.bind doAux (fn (x, y) => P.succeed (M.CcThunk {thunk = x, fvs = y}))
              | _ => P.fail "Expected calling convention"
        val p = P.bind letters doIt
      in p
      end

  fun typKind (state : state, env : env) : M.typKind P.t =
      syntax (P.satisfyMap (MU.TypKind.fromChar, "Expected type kind"))

  fun pObjKind (state : state, env : env) : M.pObjKind P.t =
      syntax (P.satisfyMap (MU.PObjKind.fromChar, "Expected P object kind"))

  fun valueSize (state : state, env : env) : M.valueSize P.t =
      P.required (P.map (identifier, MU.ValueSize.fromString), "Expected value size")

  fun fieldVariance (state : state, env : env) : M.fieldVariance P.t =
      syntax (P.satisfyMap (MU.FieldVariance.fromChar, "Expected field variance"))

  fun typ (state : state, env : env) : M.typ P.t =
      let
        val nameTyp = P.map (name (state, env) && keycharS #":" && typ (state, env), fn ((n, _), t) => (n, t))
        fun doId s =
            case s
             of "Any" => P.succeed M.TAny
              | "Any8" => P.succeed (M.TAnyS M.Vs8)
              | "Any16" => P.succeed (M.TAnyS M.Vs16)
              | "Any32" => P.succeed (M.TAnyS M.Vs32)
              | "Any64" => P.succeed (M.TAnyS M.Vs64)
              | "Any128" => P.succeed (M.TAnyS M.Vs128)
              | "Any256" => P.succeed (M.TAnyS M.Vs256)
              | "Any512" => P.succeed (M.TAnyS M.Vs512)
              | "Bits8" => P.succeed (M.TBits M.Vs8)
              | "Bits16" => P.succeed (M.TBits M.Vs16)
              | "Bits32" => P.succeed (M.TBits M.Vs32)
              | "Bits64" => P.succeed (M.TBits M.Vs64)
              | "Bits128" => P.succeed (M.TBits M.Vs128)
              | "Bits256" => P.succeed (M.TBits M.Vs256)
              | "Bits512" => P.succeed (M.TBits M.Vs512)
              | "Cont" => P.map (paren (commaSeq (typ (state, env))), fn ts => M.TContinuation ts)
              | "CStr" => P.succeed M.TCString
              | "Double" => P.succeed M.TDouble
              | "Float" => P.succeed M.TFloat
              | "Idx" => P.succeed M.TIdx
              | "Int" => P.succeed M.TInteger
              | "Mask" => P.map (paren vectorElemType, M.TViMask)
              | "Name" => P.succeed M.TName
              | "None" => P.succeed M.TNone
              | "PAny" => P.succeed M.TPAny
              | "PSet" => P.map (paren (typ (state, env)), fn t => M.TPType {kind = M.TkE, over = t})
              | "PSum" => P.map (brace (commaSeq nameTyp), fn nts => M.TPSum (ND.fromVector nts))
              | "PRef" => P.map (paren (typ (state, env)), M.TPRef)
              | "Ptr" => P.succeed M.TPtr
              | "PType" => P.map (paren (typ (state, env)), fn t => M.TPType {kind = M.TkI, over = t})
              | "Rat" => P.succeed M.TRat
              | "Ref" => P.succeed M.TRef
              | "SInt8" => P.succeed (M.TIntegral (IntArb.T (IntArb.S8, IntArb.Signed)))
              | "SInt16" => P.succeed (M.TIntegral (IntArb.T (IntArb.S16, IntArb.Signed)))
              | "SInt32" => P.succeed (M.TIntegral (IntArb.T (IntArb.S32, IntArb.Signed)))
              | "SInt64" => P.succeed (M.TIntegral (IntArb.T (IntArb.S64, IntArb.Signed)))
              | "Thunk" => P.map (paren (typ (state, env)), M.TThunk)
              | "UInt8" => P.succeed (M.TIntegral (IntArb.T (IntArb.S8, IntArb.Unsigned)))
              | "UInt16" => P.succeed (M.TIntegral (IntArb.T (IntArb.S16, IntArb.Unsigned)))
              | "UInt32" => P.succeed (M.TIntegral (IntArb.T (IntArb.S32, IntArb.Unsigned)))
              | "UInt64" => P.succeed (M.TIntegral (IntArb.T (IntArb.S64, IntArb.Unsigned)))
              | "Vec" => P.map (paren vectorElemType, M.TViVector)
              | _ => P.fail "Expected type keyword"
        val idBased = P.bind identifier doId
        val code =
            P.map (paren (semiComma (callConv (state, env, typ), typ (state, env)))
                   && keywordS "->"
                   && paren (commaSeq (typ (state, env))),
                   fn (((cc, args), _), ress) => M.TCode {cc = cc, args = args, ress = ress})
        val typVar = typ (state, env) && fieldVariance (state, env)
        val tuple =
            P.map (angleBracket (semiComma (pObjKind (state, env), typVar) && bracket typVar),
                   fn ((pok, tvs), tv) => M.TTuple {pok = pok, fixed = tvs, array = tv})
        val closure =
            P.map (paren (commaSeq (typ (state, env)))
                   && keywordS "=>"
                   && paren (commaSeq (typ (state, env))),
                   fn ((args, _), ress) => M.TClosure {args = args, ress = ress})
        val p = idBased || code || tuple || closure || P.fail "Expected type"
      in p
      end

  fun binder (state : state, env : env, k : M.variableKind) : M.variable P.t =
      let
        fun doIt ((v, _), t) =
            let
              val () = MU.SymbolTableManager.variableSetInfo (getStm state, v, t, k)
            in v
            end
        val p = P.map (variable (state, env) && keycharS #":" && typ (state, env), doIt)
      in p
      end

  fun fieldSize (state : state, env : env) : M.fieldSize P.t =
      syntax (P.required (P.map (identifier, MU.FieldSize.fromString), "Expected field size"))

  fun fieldKind (state : state, env : env) : M.fieldKind P.t =
      P.bind identifier
             (fn s =>
                 case s
                  of "b8"  => P.succeed (M.FkBits M.Fs8)
                   | "b16" => P.succeed (M.FkBits M.Fs16)
                   | "b32" => P.succeed (M.FkBits M.Fs32)
                   | "b64" => P.succeed (M.FkBits M.Fs64)
                   | "d"   => P.succeed M.FkDouble
                   | "f"   => P.succeed M.FkFloat
                   | "r"   => P.succeed M.FkRef
                   | _     => P.fail "Expected field kind")
      || P.fail "Expected field kind"

  fun fieldDescriptor (state : state, env : env) : M.fieldDescriptor P.t =
      P.map (fieldKind (state, env) && fieldVariance (state, env), fn (k, v) => M.FD {kind = k, var = v})

  fun tupleDescriptor (state : state, env : env) : M.tupleDescriptor P.t =
      P.map (angleBracket (commaSeq (fieldDescriptor (state, env))
                           && P.optional (bracket (fieldDescriptor (state, env)))),
             fn (fds, fdo) => M.TD {fixed = fds, array = fdo})

  fun metaDataDescriptor (state : state, env : env) : M.metaDataDescriptor P.t =
      let
        val pok = pObjKind (state, env)
        val fd = fieldDescriptor (state, env)
        val array = fd && keycharS #"@" && decimal
        val array = P.optional (P.map (bracket array, fn ((fd, _), n) => (n, fd)))
        val p = angleBracket (semiComma (pok, fd) && array)
        val p = P.map (p, fn ((pok, fixed), array) => M.MDD {pok = pok, fixed = fixed, array = array})
      in p
      end

  fun constant (state : state, env : env) : M.constant P.t =
      let
        fun intArb (sign, size) =
            P.map (paren intInf, fn i => M.CIntegral (IntArb.fromIntInf (IntArb.T (size, sign), i)))
        val float = unimplemented ("constant", "float")
        val double = unimplemented ("constant", "double")
        fun parseBool c = case c of #"0" => SOME false | #"1" => SOME true | _ => NONE
        val bool = syntax (P.satisfyMap (parseBool, "Expected boolean"))
        fun doIt s =
            case s
             of "Array" => P.succeed (M.CPok M.PokArray)
              | "Cell" => P.succeed (M.CPok M.PokCell)
              | "Dict" => P.succeed (M.CPok M.PokDict)
              | "Empty" => P.succeed M.COptionSetEmpty
              | "F" => P.map (paren float, M.CFloat)
              | "Float" => P.succeed (M.CPok M.PokFloat)
              | "Fun" => P.succeed (M.CPok M.PokFunction)
              | "D" => P.map (paren double, M.CDouble)
              | "Double" => P.succeed (M.CPok M.PokDouble)
              | "I" => P.map (paren intInf, M.CInteger)
              | "M" =>
                P.map (paren (semiComma (vectorElemTypeShort, bool)), fn (et, bs) => M.CViMask {typ = et, elts = bs})
              | "Name" => P.succeed (M.CPok M.PokName)
              | "None" => P.succeed (M.CPok M.PokNone)
              | "Ptr" => P.succeed (M.CPok M.PokPtr)
              | "R" => P.map (paren intInf, M.CRat)
              | "Rat" => P.succeed (M.CPok M.PokRat)
              | "S8" => intArb (IntArb.Signed, IntArb.S8)
              | "S16" => intArb (IntArb.Signed, IntArb.S16)
              | "S32" => intArb (IntArb.Signed, IntArb.S32)
              | "S64" => intArb (IntArb.Signed, IntArb.S64)
              | "Set" => P.succeed (M.CPok M.PokOptionSet)
              | "Tag" => P.succeed (M.CPok M.PokTagged)
              | "Type" => P.succeed (M.CPok M.PokType)
              | "TypePH" => P.succeed M.CTypePH
              | "U8" => intArb (IntArb.Unsigned, IntArb.S8)
              | "U16" => intArb (IntArb.Unsigned, IntArb.S16)
              | "U32" => intArb (IntArb.Unsigned, IntArb.S32)
              | "U64" => intArb (IntArb.Unsigned, IntArb.S64)
              | "V" =>
                P.map (paren (semiComma (vectorElemTypeShort, constant (state, env))),
                       fn (et, cs) => M.CViVector {typ = et, elts = cs})
              | _ => P.fail "Expected constant keyword"
        val p = P.map (name (state, env), M.CName) || P.bind identifier doIt || P.fail "Expected constant"
      in p
      end

  fun simple (state : state, env : env) : M.simple P.t =
      P.map (variable (state, env), M.SVariable) ||
      P.map (constant (state, env), M.SConstant) ||
      P.fail "Expected simple"

  val operand : state * env -> M.operand P.t = simple

  fun fieldIdentifier (state : state, env : env) : M.fieldIdentifier P.t =
      let
        val kw = syntax (P.map (P.get && P.get && keycharL #":", #1))
        fun withType (p, f) = P.map (p && keycharS #":" && vectorElemTypeShort, fn ((x, _), t) => f (x, t))
        fun doIt (c1, c2) =
            case (c1, c2)
             of (#"s", #"f") => P.map (decimal, M.FiFixed)
              | (#"s", #"v") => P.map (operand (state, env), M.FiVariable)
              | (#"v", #"f") => withType (decimal, fn (i, t) => M.FiViFixed {typ = t, idx = i})
              | (#"v", #"v") => withType (operand (state, env), fn (opnd, t) => M.FiViVariable {typ = t, idx = opnd})
              | (#"v", #"i") => withType (operand (state, env), fn (opnd, t) => M.FiViIndexed {typ = t, idx = opnd})
              | _ => P.fail "Expected field identifier"
        val p = P.bind kw doIt
      in p
      end

  fun tupleField (state : state, env : env) : M.tupleField P.t =
      let
        val tup = variable (state, env)
        val tupDesc = P.map (keycharS #":" && tupleDescriptor (state, env), #2)
        val field = bracket (fieldIdentifier (state, env))
        val p = P.map (tup && tupDesc && field, fn ((t, td), f) => M.TF {tupDesc = td, tup = t, field = f})
      in p
      end

  fun codeOption (state : state, env : env) : M.variable option P.t =
      P.map (keycharS #"-", fn _ => NONE) ||
      P.map (variable (state, env), SOME) ||
      P.fail "Expected variable or -"

  fun fvsInits (state : state, env : env) : (M.fieldKind * M.operand) Vector.t P.t =
      commaSeq (P.map (operand (state, env) && keycharS #":" && fieldKind (state, env),
                       fn ((opnd, _), fk) => (fk, opnd)))

  fun tuple (state : state, env : env) : (M.metaDataDescriptor * M.operand Vector.t) P.t =
      angleBracket (semiComma (metaDataDescriptor (state, env), operand (state, env)))
             
  (* The patterns for right-hand sides are:
   *   name
   *   constant keyword ...
   *   variable
   *   variable : tuple descriptor [field identifier]
   *   variable : tuple descriptor [field identifier] <- operand
   *   variable <- ClosureInit ...
   *   variable <- ThunkInit ...
   *   variable <- ThunkMkVal ...
   *   <...> 
   *   ? ...
   *   prim(...)
   *   rhs keyword ...
   *)
  fun rhs (state : state, env : env) : M.rhs P.t =
      let
        fun doSubSet (v, ((_, td), fi)) =
            let
              val tf = M.TF {tupDesc = td, tup = v, field = fi}
              val set = P.map (keywordS "<-" && operand (state, env),
                            fn (_, opnd) => M.RhsTupleSet {tupField = tf, ofVal = opnd})
              val sub = P.succeed (M.RhsTupleSub tf)
              val p = set || sub
            in p
            end
        fun closureInit cls =
            P.map (paren (codeOption (state, env) && keycharS #";" && fvsInits (state, env)),
                   fn ((co, _), fvs) => M.RhsClosureInit {cls = cls, code = co, fvs = fvs})
        fun thunkInit t =
            P.map (paren (fieldKind (state, env) && keycharS #";" &&
                          codeOption (state, env) && keycharS #";" &&
                          effects (state, env) && keycharS #";" &&
                          fvsInits (state, env)),
                   fn ((((((fk, _), co), _), fx), _), fvs) =>
                      M.RhsThunkInit {typ = fk, thunk = t, fx = fx, code = co, fvs = fvs})
        fun thunkMkVal t =
            P.map (paren (fieldKind (state, env) && keycharS #";" && operand (state, env)),
                   fn ((fk, _), opnd) => M.RhsThunkValue {typ = fk, thunk = t, ofVal = opnd})
        fun doInit (v, kw) =
            case kw
             of "ClosureInit" => closureInit (SOME v)
              | "ThunkInit" => thunkInit (SOME v)
              | "ThunkMkVal" => thunkMkVal (SOME v)
              | _ => P.fail "Expected ClosureInit, ThunkInit, or ThunkMkVal"
        fun doVar v =
            P.bind (keycharS #":" && tupleDescriptor (state, env) && bracket (fieldIdentifier (state, env)))
                   (fn x => doSubSet (v, x)) ||
            P.bind (keywordS "<-" ) (fn _ => P.bind letters (fn kw => doInit (v, kw))) ||
            P.succeed (M.RhsSimple (M.SVariable v))
        val varStuff = P.bind (variable (state, env)) doVar
        val tup = P.map (tuple (state, env), fn (mdd, os) => M.RhsTuple {mdDesc = mdd, inits = os})
        val const = P.map (constant (state, env), fn c => M.RhsSimple (M.SConstant c))
        fun doPrim s =
            let
              val len = String.length s
              val ct = String.sub (s, len - 1) = #"T"
              val s = String.substring (s, 0, len - 1)
            in
              case Prims.fromString s
               of NONE => P.fail "Expected prim keyword"
                | SOME p => P.map (paren (commaSeq (operand (state, env))),
                                   fn os => M.RhsPrim {prim = p, createThunks = ct, args = os})
            end
        fun doKw s =
            case s
             of "ClosureGetFv" =>
                P.map (paren (variable (state, env) && keycharS #":" && paren (commaSeq (fieldKind (state, env))) &&
                              keycharS #"," &&
                              decimal),
                       fn ((((v, _), fks), _), i) => M.RhsClosureGetFv {fvs = fks, cls = v, idx = i})
              | "ClosureInit" => closureInit NONE
              | "ClosureMk" => P.map (paren (commaSeq (fieldKind (state, env))), fn fks => M.RhsClosureMk {fvs = fks})
              | "Cont" => P.map (paren (label (state, env)), M.RhsCont)
              | "GetKind" => P.map (paren (variable (state, env)), M.RhsObjectGetKind)
              | "IdxGet" => P.map (paren (variable (state, env) && keycharS #"," && operand (state, env)),
                                   fn ((i, _), v) => M.RhsIdxGet {idx = i, ofVal = v})
              | "Inited" => P.map (paren (metaDataDescriptor (state, env) && keycharS #"," && variable (state, env)),
                                   fn ((mdd, _), tup) => M.RhsTupleInited {mdDesc = mdd, tup = tup})
              | "Set" => P.map (paren (operand (state, env)), M.RhsPSetNew)
              | "SetCond" => P.map (paren (operand (state, env) && keycharS #"?" &&
                                           brace (operand (state, env)) && keycharS #":" &&
                                           brace (P.succeed ())),
                                    fn ((((o1, _), o2), _), _) => M.RhsPSetCond {bool = o1, ofVal = o2})
              | "SetGet" => P.map (paren (variable (state, env)), M.RhsPSetGet)
              | "Spawn" =>
                P.map (paren (variable (state, env) && keycharS #":" && fieldKind (state, env)) &&
                       effects (state, env),
                       fn (((v, _), fk), fx) => M.RhsThunkSpawn {typ = fk, thunk = v, fx = fx})
              | "SumProj" => P.map (paren (variable (state, env) && keycharS #"." && name (state, env) &&
                                           keycharS #":" && fieldKind (state, env)),
                                    fn ((((v, _), n), _), fk) => M.RhsPSumProj {typ = fk, sum = v, tag = n})
              | "Tagged" => P.map (paren (name (state, env) && keycharS #"," &&
                                          operand (state, env) && keycharS #":" && fieldKind (state, env)),
                                   fn ((((n, _), opnd), _), fk) => M.RhsPSum {tag = n, typ = fk, ofVal = opnd})
              | "ThunkGetFv" =>
                P.map (paren (variable (state, env) && keycharS #":" &&
                              angleBracket (semiComma (fieldKind (state, env), fieldKind (state, env))) &&
                              keycharS #"," &&
                              decimal),
                       fn ((((v, _), (fk, fks)), _), i) => M.RhsThunkGetFv {typ = fk, fvs = fks, thunk = v, idx = i})
              | "ThunkGetVal" =>
                P.map (paren (variable (state, env) && keycharS #":" && fieldKind (state, env)),
                       fn ((v, _), fk) => M.RhsThunkGetValue {typ = fk, thunk = v})
              | "ThunkInit" => thunkInit NONE
              | "ThunkMk" => P.map (paren (semiComma (fieldKind (state, env), fieldKind (state, env))),
                                    fn (fk, fks) => M.RhsThunkMk {typ = fk, fvs = fks})
              | "ThunkMkVal" => thunkMkVal NONE
              | _ => doPrim s
        val kw = P.bind identifier doKw
        val setQuery = P.map (keycharS #"?" && operand (state, env), fn (_, opnd) => M.RhsPSetQuery opnd)
        val p = varStuff || tup || const || kw || setQuery || P.fail "Expected right-hand side"
      in p
      end

  fun instruction (state : state, env : env) : M.instruction P.t =
      let
        val dests =
            paren (commaSeq (binder (state, env, M.VkLocal))) ||
            P.map (binder (state, env, M.VkLocal), Vector.new1) ||
            P.succeed (Vector.new0 ())
        val p = dests && keycharS #"=" && rhs (state, env) && keycharS #";"
        val p = P.map (p, fn (((vs, _), rhs), _) => M.I {dests = vs, n = 0, rhs = rhs})
      in p
      end

  fun target (state : state, env : env) : M.target P.t =
      P.map (label (state, env) && paren (commaSeq (operand (state, env))),
             fn (l, os) => M.T {block = l, arguments = os})

  fun switch (state : state, env : env, f : state * env -> 'a P.t) : 'a M.switch P.t =
      let
        val case1 = P.map (f (state, env) && keywordS "=>" && target (state, env), fn ((k, _), t) => (k, t))
        val cases = P.zeroOrMoreV case1
        val default = P.map (keywordS "Default" && keywordS "=>" && target (state, env), #2)
        val body = cases && P.optional default
        val p = P.map (operand (state, env) && brace body,
                       fn (opnd, (cs, d)) => {on = opnd, cases = cs, default = d})
      in p
      end

  fun codes (state : state, env : env) : M.codes P.t =
      let
        val exhaustive =
            P.map (keywordS "<=", fn _ => true) || P.map (keycharS #"?", fn _ => false) || P.fail "Expected codes"
        val vs = P.map (brace (commaSeq (variable (state, env))), VS.fromVector)
        val p = P.map (exhaustive && vs, fn (e, vs) => {possible = vs, exhaustive = e})
      in p
      end

  fun callA (state : state, env : env, s : string) : M.call P.t =
      case s
       of "Call" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.CCode {ptr = v, code = cs})
        | "CallClos" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.CClosure {cls = v, code = cs})
        | "CalLDir" =>
          P.map (paren (variable (state, env) && keycharS #"," && variable (state, env)),
                 fn ((v1, _), v2) => M.CDirectClosure {cls = v1, code = v2})
        | _ => P.fail "Expected call"

  fun call (state : state, env : env) : M.call P.t = P.bind letters (fn s => callA (state, env, s))

  fun evalA (state : state, env : env, s : string) : M.eval P.t =
      case s
       of "Eval" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.EThunk {thunk = v, code = cs})
        | "EvalDir" =>
          P.map (paren (variable (state, env) && keycharS #"," && variable (state, env)),
                 fn ((v1, _), v2) => M.EDirectThunk {thunk = v1, code = v2})
        | _ => P.fail "Expected eval"

  fun eval (state : state, env : env) : M.eval P.t = P.bind letters (fn s => evalA (state, env, s))

  fun interProcA (state : state, env : env, s : string) : M.interProc P.t =
      let
        fun doCall () =
            P.map (callA (state, env, s) && paren (commaSeq (operand (state, env))),
                   fn (c, os) => M.IpCall {call = c, args = os})
        fun doEval () =
            P.map (evalA (state, env, s) && keycharS #":" && fieldKind (state, env),
                   fn ((e, _), fk) => M.IpEval {typ = fk, eval = e})
      in
        case s
         of "Call" => doCall ()
          | "CallClos" => doCall ()
          | "CallDir" => doCall ()
          | "Eval" => doEval ()
          | "EvalDir" => doEval ()
          | _ => P.fail "Expected inter proc"
      end

  fun interProc (state : state, env : env) : M.interProc P.t = P.bind letters (fn s => interProcA (state, env, s))

  fun cuts (state : state, env : env) : M.cuts P.t =
      let
        val item = P.map (label (state, env), SOME) || P.map (keywordS "Exit", fn _ => NONE)
        fun doOne (i, {exits, targets}) =
            case i
             of NONE => {exits = true, targets = targets}
              | SOME l => {exits = exits, targets = LS.insert (targets, l)}
        fun process is = M.C (Vector.fold (is, {exits = false, targets = LS.empty}, doOne))
        val p = P.map (keywordS "/->/" && brace (commaSeq item), process o #2)
        val p = p || P.succeed MU.Cuts.none
      in p
      end

  fun return (state : state, env : env) : M.return P.t =
      let
        val normal =
            P.map (keywordS "->" &&
                   paren (commaSeq (binder (state, env, M.VkLocal))) &&
                   label (state, env) &&
                   cuts (state, env),
                   fn (((_, vs), l), cs) => M.RNormal {rets = vs, block = l, cuts = cs})
        val tail =
            P.map (keywordS "-|" && P.succeeds (keywordS "/->/" && keywordS "Exit"), fn (_, e) => M.RTail {exits = e})
        val p = normal || tail || P.fail "Expected return"
      in p
      end

  fun transfer (state : state, env : env) : M.transfer P.t =
      let
        fun interProc s =
            P.map (interProcA (state, env, s) && return (state, env) && effects (state, env),
                   fn ((ip, r), fx) => M.TInterProc {callee = ip, ret = r, fx = fx})
        fun doIt s =
            case s
             of "Call" => interProc s
              | "CallClos" => interProc s
              | "CallDir" => interProc s
              | "Case" => P.map (switch (state, env, constant), M.TCase)
              | "Cut" => P.map (variable (state, env) && paren (commaSeq (operand (state, env))) && cuts (state, env),
                                fn ((v, os), cs) => M.TCut {cont = v, args = os, cuts = cs})
              | "Eval" => interProc s
              | "EvalDir" => interProc s
              | "Goto" => P.map (target (state, env), M.TGoto)
              | "Halt" => P.map (operand (state, env), M.THalt)
              | "PSumCase" => P.map (switch (state, env, name), M.TPSumCase)
              | "Return" => P.map (paren (commaSeq (operand (state, env))), M.TReturn)
              | _ => P.fail "Expected transfer keyword"
        val p = P.bind letters doIt || P.fail "Expected transfer"
      in p
      end

  fun block (state : state, env : env) : (M.label * M.block) P.t =
      let
        val header = label (state, env) && paren (commaSeq (binder (state, env, M.VkLocal)))
        val p = header && P.zeroOrMoreV (instruction (state, env)) && transfer (state, env)
        val p = P.map (p, fn (((l, ps), is), t) => (l, M.B {parameters = ps, instructions = is, transfer = t}))
      in p
      end

  fun codeBody (state : state, env : env) : M.codeBody P.t =
      let
        val state = forkLabels state
        val entry = P.map (keywordS "Entry" && label (state, env), #2)
        val blks = P.map (P.zeroOrMore (block (state, env)), LD.fromList)
        val p = P.map (entry && blks, fn (e, bs) => M.CB {entry = e, blocks = bs})
      in p
      end

  fun codeA (state : state, env : env) : M.code P.t =
      let
        val binder' = fn (s, e) => binder (s, e, M.VkLocal)
        val args = paren (semiComma (callConv (state, env, binder'), binder (state, env, M.VkLocal)))
        val header = P.map (optFlag #"^" && optFlag #"*" && args && effects (state, env),
                            fn (((r, e), (cc, vs)), fx) => (r, e, cc, vs, fx))
        val rets = P.map (keycharS #":" && paren (commaSeq (typ (state, env))), #2)
        val body = brace (codeBody (state, env))
        fun mkIt (((r, e, cc, vs, fx), ts), cb) =
            M.F {fx = fx, escapes = e, recursive = r, cc = cc, args = vs, rtyps = ts, body = cb}
        val p = P.map (header && rets && body, mkIt)
      in p
      end

  fun code (state : state, env : env) : M.code P.t = P.map (keywordS "Code" && codeA (state, env), #2)

  fun global (state : state, env : env) : M.global P.t =
      let
        fun doIt s =
            case s
             of "Code" => P.map (codeA (state, env), M.GCode)
              | "CString" => unimplemented ("global", "CString")
              | "ErrorVal" => P.map (keycharS #":" && typ (state, env), fn (_, t) => M.GErrorVal t)
              | "I" => P.map (paren intInf, M.GInteger)
              | "Idx" =>
                P.map (brace (commaSeq (P.map (name (state, env) && keywordS "->" && int, fn ((n, _), i) => (n, i)))),
                       fn nis => M.GIdx (ND.fromVector nis))
              | "R" => P.map (paren rat, M.GRat)
              | "Closure" => P.map (paren (codeOption (state, env) && keycharS #";" && fvsInits (state, env)),
                                    fn ((co, _), fvs) => M.GClosure {code = co, fvs = fvs})
              | "Set" => P.map (paren (simple (state, env)), M.GPSet) 
              | "Tagged" =>
                P.map (paren (name (state, env) && keycharS #"," &&
                              simple (state, env) && keycharS #":" && fieldKind (state, env)),
                       fn ((((n, _), s), _), fk) => M.GPSum {tag = n, typ = fk, ofVal = s})
              | "ThunkValue" => P.map (paren (simple (state, env) && keycharS #":" && fieldKind (state, env)),
                                       fn ((s, _), fk) => M.GThunkValue {typ = fk, ofVal = s})
              | _ => P.fail "Expected global keyword"
        val tup = P.map (tuple (state, env), fn (mdd, os) => M.GTuple {mdDesc = mdd, inits = os})
        val p = P.bind letters doIt || tup || P.map (simple (state, env), M.GSimple) || P.fail "Expected global"
      in p
      end

  fun varGlobal (state : state, env : env) : (M.variable * M.global) P.t =
      P.map (binder (state, env, M.VkGlobal) && keycharS #"=" && global (state, env), fn ((v, _), g) => (v, g))

  fun globals (state : state, env : env) : M.global VD.t P.t =
      P.map (P.zeroOrMore (varGlobal (state, env)), VD.fromList)

  fun includeKind (state : state, env : env) : M.includeKind P.t =
      P.required (P.map (letters, MU.IncludeKind.fromString), "Expected include kind")

  fun includeFile (state : state, env : env) : M.includeFile P.t =
      P.map (unimplemented ("includeFile", "name") &&
             keycharS #":" && includeKind (state, env) &&
             brace (commaSeq (variable (state, env))),
             fn (((n, _), k), vs) => M.IF {name = n, kind = k, externs = VS.fromVector vs})

  fun program (config : Config.t) : M.t P.t =
      let
        val stm = IM.new Prims.ordString
        val state = stateMk stm
        val env = envMk config
        val includes = P.succeed (Vector.new0 ())
        val externs = P.succeed VS.empty
        val globals = P.map (keywordS "Globals:" && globals (state, env), #2)
        val entry = P.map (keywordS "Entry:" && variable (state, env), #2)
        fun finish (((is, es), gs), e) =
            let
              val st = IM.finish stm
              val p = M.P {includes = is, externs = es, globals = gs, symbolTable = st, entry = e}
            in p
            end
        val p = P.map (includes && externs && globals && entry, finish)
      in p
      end

end;

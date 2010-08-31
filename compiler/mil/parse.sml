(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_PARSE =
sig
  val pass : (unit, Mil.t) Pass.t
end;

structure MilParse :> MIL_PARSE =
struct

  val modname = "MilParse"
  fun fail (f, m) = Fail.fail (modname, f, m)
  fun unimplemented (f, m) = Fail.unimplemented (modname, f, m)

  structure SD = StringDict
  structure P = FileParser

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

  (*** Parser Utilities ***)

  val || = P.||
  val && = P.&&

  infix || &&

  (* Convention:
   *   With an F suffix, a parser should fail if the first lexical item does not look like the beginning of thing
   *   being parsed.  If the first lexical item matches, then errors result if the subsequent parse cannot be
   *   completed.
   *   Without an F suffix, a parser should produce an error if it cannot parse something.
   *)

  val whiteF = P.satisfy (fn c => c = Char.space orelse c = Char.newline orelse c = #"\t")

  val whitespace = P.ignore (P.zeroOrMore whiteF)

  (* Something is lexical if it matches exactly characters of the thing being parsed and not whitespace.
   * Something is syntactically if it parses surrounding whitespace as well as the thing being parsed.
   * Convention: all syntactic parsers assume there is no whitespace at the beginning and parse whitespace at the end;
   *             the only exception is top-level parsers such as program that must also parse the initial whitespace.
   *)
  fun syntax (p : 'a P.t) : 'a P.t = P.map (p && whitespace, #1)

  fun keychar' (kc : char) : char P.t = P.satisfy (fn c => c = kc)

  fun keycharLF (kc : char) : unit P.t = P.ignore (keychar' kc)

  fun keycharSF (kc : char) : unit P.t = syntax (keycharLF kc)

  fun keycharS (kc : char) : unit P.t = keycharSF kc || P.error ("Expected " ^ String.fromChar kc)

  fun keywordLF (kw : string) : unit P.t = P.ignore (P.all (List.map (String.explode kw, keychar')))

  fun keywordSF (kw : string) : unit P.t = syntax (keywordLF kw)
 
  fun keywordS (kw : string) : unit P.t = keywordSF kw || P.error ("Expected " ^ kw)

  fun optFlag (kc : char) : bool P.t = P.succeeds (keycharSF kc)

  fun semiCommaAux (left : unit P.t, right : char, p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      let
        fun pr () =
            P.map (keycharSF right, fn () => []) ||
            P.map (keycharSF #"," && p2 && P.$ pr, fn ((_, i), is) => i::is) ||
            P.error ("Expected , or " ^ String.fromChar right)
        val p = P.map (keycharSF right, fn () => Vector.new0 ()) ||
                P.map (p2 && P.$ pr, fn (i, is) => Vector.fromList (i::is))
        val p = P.map (left && p1 && keycharS #";" && p, fn (((_, x), _), y) => (x, y))
      in p
      end

  fun parenSemiCommaF (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      semiCommaAux (keycharSF #"(", #")", p1, p2)

  fun parenSemiComma (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      semiCommaAux (keycharS #"(", #")", p1, p2)

  fun angleBracketSemiCommaF (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      semiCommaAux (keycharSF #"<", #">", p1, p2)

  fun angleBracketSemiComma (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b Vector.t) P.t =
      semiCommaAux (keycharS #"<", #">", p1, p2)

  fun parenF        (p : 'a P.t) : 'a P.t = P.map (keycharSF #"(" && p && keycharS #")", fn ((_, x), _) => x)
  fun paren         (p : 'a P.t) : 'a P.t = P.map (keycharS  #"(" && p && keycharS #")", fn ((_, x), _) => x)
  fun brace         (p : 'a P.t) : 'a P.t = P.map (keycharS  #"{" && p && keycharS #"}", fn ((_, x), _) => x)
  fun bracketF      (p : 'a P.t) : 'a P.t = P.map (keycharSF #"[" && p && keycharS #"]", fn ((_, x), _) => x)
  fun bracket       (p : 'a P.t) : 'a P.t = P.map (keycharS  #"[" && p && keycharS #"]", fn ((_, x), _) => x)
  fun angleBracketF (p : 'a P.t) : 'a P.t = P.map (keycharSF #"<" && p && keycharS #">", fn ((_, x), _) => x)
  fun angleBracket  (p : 'a P.t) : 'a P.t = P.map (keycharS  #"<" && p && keycharS #">", fn ((_, x), _) => x)

  fun parenSeqF (pi : 'a P.t) : 'a Vector.t P.t =
      P.sequenceV
        {left = keycharSF #"(", sep = keycharSF #",", right = keycharSF #")", err = "Expected ) or ,", item = pi}

  fun parenSeq (pi : 'a P.t) : 'a Vector.t P.t =
      P.sequenceV
        {left = keycharS #"(", sep = keycharSF #",", right = keycharSF #")", err = "Expected ) or ,", item = pi}

  fun braceSeq (pi : 'a P.t) : 'a Vector.t P.t =
      P.sequenceV
        {left = keycharS #"{", sep = keycharSF #",", right = keycharSF #"}", err = "Expected } or ,", item = pi}

  fun pair (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b) P.t =
      P.map (paren (p1 && keycharS #"," && p2), fn ((x, _), y) => (x, y))

  val identifierF : string P.t =
      syntax (P.map (P.satisfy Char.isAlpha && P.zeroOrMore (P.satisfy Char.isAlphaNum),
                     fn (c, cs) => String.implode (c::cs)))
      || P.succeed ""

  val decimal : int P.t =
      syntax (P.map (P.oneOrMore (P.satisfy Char.isDigit),
                     fn cs => Option.valOf (Int.fromString (String.implode cs))))
      || P.error "Expected nat"

  val int : int P.t =
      syntax (P.map (P.optional (keycharLF #"~") && P.oneOrMore (P.satisfy Char.isDigit),
                     fn (co, cs) =>
                        Option.valOf (Int.fromString (String.implode (case co of NONE => cs | SOME () => #"~"::cs)))))
      || P.error "Expected int"

  val intInfLF = 
      P.map (P.optional (keycharLF #"~") && P.oneOrMore (P.satisfy Char.isDigit),
             fn (n, cs) =>
                Option.valOf (IntInf.fromString (String.implode (case n of NONE => cs | SOME () => #"~"::cs))))

  val intInf = syntax intInfLF || P.error "Expected integer"

  val rat =
      syntax (P.map (intInfLF && P.optional (keycharLF #"/" && intInfLF),
                     fn (i, io) => Rat.rat (i, case io of NONE => IntInf.one | SOME (_, i) => i)))
      || P.error "Expected rational"

  val cstringF =
      let
        fun s c = c <> #"\"" andalso c <> #"\\" andalso (let val ord = Char.ord c in ord >= 32 andalso ord <= 126 end)
        val simple = P.map (P.satisfy s, fn c => [c])
        val singleEscape =
            P.map (keycharLF #"\\", fn () => [#"\\", #"\\"]) ||
            P.map (keycharLF #"\"", fn () => [#"\\", #"\""]) ||
            P.map (keycharLF #"'", fn () => [#"\\", #"'"]) ||
            P.map (keycharLF #"?", fn () => [#"\\", #"?"]) ||
            P.map (keycharLF #"a", fn () => [#"\\", #"a"]) ||
            P.map (keycharLF #"b", fn () => [#"\\", #"b"]) ||
            P.map (keycharLF #"f", fn () => [#"\\", #"f"]) ||
            P.map (keycharLF #"n", fn () => [#"\\", #"n"]) ||
            P.map (keycharLF #"r", fn () => [#"\\", #"r"]) ||
            P.map (keycharLF #"t", fn () => [#"\\", #"t"]) ||
            P.map (keycharLF #"v", fn () => [#"\\", #"v"])
        fun d3 c = c >= #"0" andalso c <= #"3"
        fun d7 c = c >= #"0" andalso c <= #"7"
        val octalEscape =
            P.map (P.satisfy d3 && P.satisfy d7 && P.satisfy d7, fn ((c1, c2), c3) => [#"\\", c1, c2, c3])
        val escape = P.map (keycharLF #"\\" && (singleEscape || octalEscape), #2)
        val stritem = simple || escape
        val p = keycharLF #"\"" && (P.zeroOrMore stritem && keycharLF #"\"" || P.error "Bad string literal")
        fun f (_, (css, _)) = Option.valOf (String.fromCString (String.implode (List.concat css)))
        val p = syntax (P.map (p, f))
      in p
      end

  val cstring = cstringF || P.error "Expected string"

  (*** State ***)

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

  (*** Environment ***)

  datatype env = E of {config : Config.t}

  fun envMk (config : Config.t) : env = E {config = config}

  (*** The Parsers ***)

  val vectorElemType : VI.elemType P.t =
      P.required (P.map (identifierF, VI.elemTypeOfString), "Expected long vector element type")

  val vectorElemTypeShort : VI.elemType P.t =
      P.required (P.map (identifierF, VI.elemTypeOfStringShort), "Expected short vector element type")

  fun variableF (state : state, env : env) : M.variable P.t =
      let
        val pre = keycharLF #"v" && P.zeroOrMore (P.satisfy Char.isDigit) && keycharLF #"_"
        val pre = P.map (pre, fn ((_, cs), _) => "v" ^ String.implode cs ^ "_")
        fun hintChar c = Char.isAlphaNum c orelse c = #"_"
        val hint = P.map (P.zeroOrMore (P.satisfy hintChar), String.implode)
        val p = syntax (P.map (pre && hint, fn (p, h) => getVariable (state, p, h)))
      in p
      end

  fun variable (state : state, env : env) : M.variable P.t = variableF (state, env) || P.error "Expected variable"

  fun nameF (state : state, env : env) : M.name P.t =
      let
        val pre = keycharLF #"n" && P.zeroOrMore (P.satisfy Char.isDigit) && keycharLF #"_"
        fun nameChar c = Char.isAlphaNum c orelse c = #"\\"
        val str = P.oneOrMore (P.satisfy nameChar)
        val p = P.map (pre && str, fn (_, cs) => MU.SymbolTableManager.nameMake (getStm state, String.implode cs))
        val p = syntax p
      in p
      end

  fun name (state : state, env : env) : M.name P.t = nameF (state, env) || P.error "Expected name"

  fun labelF (state : state, env : env) : M.label P.t =
      syntax (P.map (keycharLF #"L" && P.oneOrMore (P.satisfy Char.isDigit),
                     fn (_, ds) => getLabel (state, String.implode ds)))

  fun label (state : state, env : env) : M.label P.t = labelF (state, env) || P.error "Expected label"

  fun effects (state : state, env : env) : M.effects P.t =
      let
        val one = P.required (P.map (P.get, Effect.charToSet), "Expected effect or }")
        fun pr () =
            P.map (keycharLF #"}", fn () => Effect.Total) ||
            P.map (one && P.$ pr, Effect.union)
        val p = P.map (keycharLF #"{" && P.$ pr, fn (_, fx) => fx)
        val p = syntax p || P.error "Expected effects"
      in p
      end

  fun callConvF (state : state, env : env, f : state * env -> 'a P.t) : 'a M.callConv P.t =
      let
        val doAux = parenSemiComma (P.$$ f (state, env), P.$$ f (state, env))
        fun doIt s =
            case s
             of "Code" => P.succeed M.CcCode
              | "Closure" => P.bind doAux (fn (x, y) => P.succeed (M.CcClosure {cls = x, fvs = y}))
              | "Thunk" => P.bind doAux (fn (x, y) => P.succeed (M.CcThunk {thunk = x, fvs = y}))
              | _ => P.fail
        val p = P.bind identifierF doIt
      in p
      end

  fun callConv (state : state, env : env, f : state * env -> 'a P.t) : 'a M.callConv P.t =
      callConvF (state, env, f)  || P.error "Expected calling convention"

  fun typKind (state : state, env : env) : M.typKind P.t =
      syntax (P.satisfyMap (MU.TypKind.fromChar)) || P.error "Expected type kind"

  fun pObjKind (state : state, env : env) : M.pObjKind P.t =
      syntax (P.satisfyMap (MU.PObjKind.fromChar)) || P.error "Expected P object kind"

  fun valueSize (state : state, env : env) : M.valueSize P.t =
      P.required (P.map (identifierF, MU.ValueSize.fromString), "Expected value size")

  fun fieldVariance (state : state, env : env) : M.fieldVariance P.t =
      syntax (P.satisfyMap (MU.FieldVariance.fromChar)) || P.error "Expected field variance"

  fun typ (state : state, env : env) : M.typ P.t =
      let
        val nameTyp = P.map (name (state, env) && keycharS #":" && P.$$ typ (state, env), fn ((n, _), t) => (n, t))
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
              | "Cont" => P.map (parenSeq (typ (state, env)), fn ts => M.TContinuation ts)
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
              | "PSum" => P.map (braceSeq nameTyp, fn nts => M.TPSum (ND.fromVector nts))
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
              | _ => P.fail
        val idBased = P.bind identifierF doId
        val code =
            P.map (parenSemiCommaF (callConvF (state, env, typ), P.$$ typ (state, env))
                   && keywordS "->"
                   && parenSeq (P.$$ typ (state, env)),
                   fn (((cc, args), _), ress) => M.TCode {cc = cc, args = args, ress = ress})
        val typVar = P.$$ typ (state, env) && fieldVariance (state, env)
        val tuple =
            P.map (semiCommaAux (keycharSF #"<", #"[", pObjKind (state, env), typVar) &&
                   typVar && keycharS #"]" && keycharS #">",
                   fn ((((pok, tvs), tv), _), _) => M.TTuple {pok = pok, fixed = tvs, array = tv})
        val closure =
            P.map (parenSeqF (P.$$ typ (state, env))
                   && keywordS "=>"
                   && parenSeq (P.$$ typ (state, env)),
                   fn ((args, _), ress) => M.TClosure {args = args, ress = ress})
        val p = idBased || code || tuple || closure || P.error "Expected type"
      in p
      end

  fun binderF (state : state, env : env, k : M.variableKind) : M.variable P.t =
      let
        fun doIt ((v, _), t) =
            let
              val () = MU.SymbolTableManager.variableSetInfo (getStm state, v, t, k)
            in v
            end
        val p = P.map (variableF (state, env) && keycharS #":" && typ (state, env), doIt)
      in p
      end

  fun binder (state : state, env : env, k : M.variableKind) : M.variable P.t =
      binderF (state, env, k) || P.error "Expected variable binder"

  fun fieldSize (state : state, env : env) : M.fieldSize P.t =
      syntax (P.required (P.map (identifierF, MU.FieldSize.fromString), "Expected field size"))

  fun fieldKind (state : state, env : env) : M.fieldKind P.t =
      P.bind identifierF
             (fn s =>
                 case s
                  of "b8"  => P.succeed (M.FkBits M.Fs8)
                   | "b16" => P.succeed (M.FkBits M.Fs16)
                   | "b32" => P.succeed (M.FkBits M.Fs32)
                   | "b64" => P.succeed (M.FkBits M.Fs64)
                   | "d"   => P.succeed M.FkDouble
                   | "f"   => P.succeed M.FkFloat
                   | "r"   => P.succeed M.FkRef
                   | _     => P.fail)
      || P.error "Expected field kind"

  fun fieldDescriptor (state : state, env : env) : M.fieldDescriptor P.t =
      P.map (fieldKind (state, env) && fieldVariance (state, env), fn (k, v) => M.FD {kind = k, var = v})

  fun tupleDescriptor (state : state, env : env) : M.tupleDescriptor P.t =
      let
        val array = P.map (keycharSF #"[" && fieldDescriptor (state, env) && keycharS #"]" && keycharS #">",
                           fn (((_, fd), _), _) => SOME fd)
        fun pr () =
            P.map (keycharSF #">", fn () => ([], NONE)) ||
            P.map (array, fn a => ([], a)) ||
            P.map (keycharSF #"," && fieldDescriptor (state, env) && P.$ pr, fn ((_, fd), (fds, a)) => (fd::fds, a)) ||
            P.error "Expected , or [ or >"
        val p = P.map (keycharSF #">", fn () => M.TD {fixed = Vector.new0 (), array = NONE}) ||
                P.map (array, fn a => M.TD {fixed = Vector.new0 (), array = a}) ||
                P.map (fieldDescriptor (state, env) && P.$ pr,
                       fn (fd, (fds, a)) => M.TD {fixed = Vector.fromList (fd::fds), array = a})
        val p = P.map (keycharS #"<" && p, #2)
      in p
      end

  fun metaDataDescriptor (state : state, env : env) : M.metaDataDescriptor P.t =
      let
        val fd = fieldDescriptor (state, env)
        val array = P.map (keycharSF #"[" && fd && keycharS #"@" && decimal && keycharS #"]" && keycharS #">",
                           fn (((((_, fd), _), n), _), _) => SOME (n, fd))
        fun pr () =
            P.map (keycharSF #">", fn () => ([], NONE)) ||
            P.map (array, fn a => ([], a)) ||
            P.map (keycharSF #"," && fd && P.$ pr, fn ((_, fd), (fds, a)) => (fd::fds, a)) ||
            P.error "Expected , or [ or >"
        val p = P.map (keycharSF #">", fn () => (Vector.new0 (), NONE)) ||
                P.map (array, fn a => (Vector.new0 (), a)) ||
                P.map (fd && P.$ pr, fn (fd, (fds, a)) => (Vector.fromList (fd::fds), a))
        val p = P.map (keycharS #"<" && pObjKind (state, env) && keycharS #";" && p,
                       fn (((_, pok), _), (fixed, array)) => M.MDD {pok = pok, fixed = fixed, array = array})
      in p
      end

  fun constantF (state : state, env : env) : M.constant P.t =
      let
        fun intArb (sign, size) =
            P.map (paren intInf, fn i => M.CIntegral (IntArb.fromIntInf (IntArb.T (size, sign), i)))
        val float = P.$$ unimplemented ("constant", "float")
        val double = P.$$ unimplemented ("constant", "double")
        fun parseBool c = case c of #"0" => SOME false | #"1" => SOME true | _ => NONE
        val bools = syntax (P.zeroOrMoreV (P.satisfyMap parseBool))
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
                P.map (angleBracket (vectorElemTypeShort && keycharS #";" && bools),
                       fn ((et, _), bs) => M.CViMask {typ = et, elts = bs})
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
                P.map (angleBracketSemiComma (vectorElemTypeShort, constant (state, env)),
                       fn (et, cs) => M.CViVector {typ = et, elts = cs})
              | _ => P.fail
        val p = P.map (nameF (state, env), M.CName) || P.bind identifierF doIt
      in p
      end

  and constant (state : state, env : env) : M.constant P.t = constantF (state, env) || P.error "Expected constant"

  fun simpleF (state : state, env : env) : M.simple P.t =
      P.map (variableF (state, env), M.SVariable) ||
      P.map (constantF (state, env), M.SConstant)

  fun simple (state : state, env : env) : M.simple P.t = simpleF (state, env) || P.error "Expected simple"

  val operand : state * env -> M.operand P.t = simple

  fun fieldIdentifier (state : state, env : env) : M.fieldIdentifier P.t =
      let
        val kw = syntax (P.map (P.get && P.get && keycharLF #":", #1))
        fun withType (p, f) = P.map (p && keycharS #":" && vectorElemTypeShort, fn ((x, _), t) => f (x, t))
        fun doIt (c1, c2) =
            case (c1, c2)
             of (#"s", #"f") => P.map (decimal, M.FiFixed)
              | (#"s", #"v") => P.map (operand (state, env), M.FiVariable)
              | (#"v", #"f") => withType (decimal, fn (i, t) => M.FiViFixed {typ = t, idx = i})
              | (#"v", #"v") => withType (operand (state, env), fn (opnd, t) => M.FiViVariable {typ = t, idx = opnd})
              | (#"v", #"i") => withType (operand (state, env), fn (opnd, t) => M.FiViIndexed {typ = t, idx = opnd})
              | _ => P.fail
        val p = P.bind kw doIt || P.error "Expected field identifier"
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
      P.map (keycharSF #"-", fn _ => NONE) ||
      P.map (variableF (state, env), SOME) ||
      P.error "Expected variable or -"

  (* Parsers a comma separated list of operand and field kind pairs and a closing parenthesis, but no opening *)
  fun fvsInits (state : state, env : env) : (M.fieldKind * M.operand) Vector.t P.t =
      let
        val i =
            P.map (operand (state, env) && keycharS #":" && fieldKind (state, env), fn ((opnd, _), fk) => (fk, opnd))
        fun pr () =
            P.map (keycharSF #")", fn () => []) ||
            P.map (keycharSF #"," && i && P.$ pr, fn ((_, i), is) => i::is) ||
            P.error "Expected , or )"
        val p =
            P.map (keycharSF #")", fn () => Vector.new0 ()) ||
            P.map (i && P.$ pr, fn (fk, fks) => Vector.fromList (fk::fks))
      in p
      end

  fun tupleF (state : state, env : env) : (M.metaDataDescriptor * M.operand Vector.t) P.t =
      angleBracketSemiCommaF (metaDataDescriptor (state, env), operand (state, env))

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
              val set = P.map (keywordSF "<-" && operand (state, env),
                               fn (_, opnd) => M.RhsTupleSet {tupField = tf, ofVal = opnd})
              val sub = P.succeed (M.RhsTupleSub tf)
              val p = set || sub
            in p
            end
        fun closureInit cls =
            P.map (keycharS #"(" && codeOption (state, env) && keycharS #";" && fvsInits (state, env),
                   fn (((_, co), _), fvs) => M.RhsClosureInit {cls = cls, code = co, fvs = fvs})
        fun thunkInit t =
            P.map (keycharS #"(" &&
                   fieldKind (state, env) && keycharS #";" &&
                   codeOption (state, env) && keycharS #";" &&
                   effects (state, env) && keycharS #";" &&
                   fvsInits (state, env),
                   fn (((((((_, fk), _), co), _), fx), _), fvs) =>
                      M.RhsThunkInit {typ = fk, thunk = t, fx = fx, code = co, fvs = fvs})
        fun thunkMkVal t =
            P.map (paren (fieldKind (state, env) && keycharS #";" && operand (state, env)),
                   fn ((fk, _), opnd) => M.RhsThunkValue {typ = fk, thunk = t, ofVal = opnd})
        fun doInit (v, kw) =
            case kw
             of "ClosureInit" => closureInit (SOME v)
              | "ThunkInit" => thunkInit (SOME v)
              | "ThunkMkVal" => thunkMkVal (SOME v)
              | _ => P.error "Expected ClosureInit, ThunkInit, or ThunkMkVal"
        fun doVar v =
            P.bind (keycharSF #":" && tupleDescriptor (state, env) && bracket (fieldIdentifier (state, env)))
                   (fn x => doSubSet (v, x)) ||
            P.bind (keywordSF "<-" ) (fn _ => P.bind identifierF (fn kw => doInit (v, kw))) ||
            P.succeed (M.RhsSimple (M.SVariable v))
        val varStuff = P.bind (variableF (state, env)) doVar
        val tup = P.map (tupleF (state, env), fn (mdd, os) => M.RhsTuple {mdDesc = mdd, inits = os})
        val const = P.map (constantF (state, env), fn c => M.RhsSimple (M.SConstant c))
        fun doPrim s =
            if s = "" then
              P.fail
            else
              let
                val len = String.length s
                val ct = String.sub (s, len - 1) = #"T"
                val s = String.substring (s, 0, len - 1)
              in
                case Prims.fromString s
                 of NONE => P.fail
                  | SOME p => P.map (parenSeq (operand (state, env)),
                                     fn os => M.RhsPrim {prim = p, createThunks = ct, args = os})
              end
        fun doKw s =
            case s
             of "ClosureGetFv" =>
                P.map (pair (variable (state, env) && keycharSF #":" && parenSeq (fieldKind (state, env)), decimal),
                       fn (((v, _), fks), i) => M.RhsClosureGetFv {fvs = fks, cls = v, idx = i})
              | "ClosureInit" => closureInit NONE
              | "ClosureMk" => P.map (parenSeq (fieldKind (state, env)), fn fks => M.RhsClosureMk {fvs = fks})
              | "Cont" => P.map (paren (label (state, env)), M.RhsCont)
              | "GetKind" => P.map (paren (variable (state, env)), M.RhsObjectGetKind)
              | "IdxGet" => P.map (pair (variable (state, env), operand (state, env)),
                                   fn (i, v) => M.RhsIdxGet {idx = i, ofVal = v})
              | "Inited" => P.map (pair (metaDataDescriptor (state, env), variable (state, env)),
                                   fn (mdd, tup) => M.RhsTupleInited {mdDesc = mdd, tup = tup})
              | "Set" => P.map (paren (operand (state, env)), M.RhsPSetNew)
              | "SetCond" => P.map (paren (operand (state, env) && keycharSF #"?" &&
                                           brace (operand (state, env)) && keycharSF #":" &&
                                           brace (P.succeed ())),
                                    fn ((((o1, _), o2), _), _) => M.RhsPSetCond {bool = o1, ofVal = o2})
              | "SetGet" => P.map (paren (variable (state, env)), M.RhsPSetGet)
              | "Spawn" =>
                P.map (paren (variable (state, env) && keycharSF #":" && fieldKind (state, env)) &&
                       effects (state, env),
                       fn (((v, _), fk), fx) => M.RhsThunkSpawn {typ = fk, thunk = v, fx = fx})
              | "SumProj" => P.map (paren (variable (state, env) && keycharSF #"." && name (state, env) &&
                                           keycharSF #":" && fieldKind (state, env)),
                                    fn ((((v, _), n), _), fk) => M.RhsPSumProj {typ = fk, sum = v, tag = n})
              | "Tagged" => P.map (pair (name (state, env),
                                         operand (state, env) && keycharSF #":" && fieldKind (state, env)),
                                   fn (n, ((opnd, _), fk)) => M.RhsPSum {tag = n, typ = fk, ofVal = opnd})
              | "ThunkGetFv" =>
                P.map (pair (variable (state, env) && keycharSF #":" &&
                             angleBracketSemiComma (fieldKind (state, env), fieldKind (state, env)),
                             decimal),
                       fn (((v, _), (fk, fks)), i) => M.RhsThunkGetFv {typ = fk, fvs = fks, thunk = v, idx = i})
              | "ThunkGetVal" =>
                P.map (paren (variable (state, env) && keycharSF #":" && fieldKind (state, env)),
                       fn ((v, _), fk) => M.RhsThunkGetValue {typ = fk, thunk = v})
              | "ThunkInit" => thunkInit NONE
              | "ThunkMk" => P.map (parenSemiComma (fieldKind (state, env), fieldKind (state, env)),
                                    fn (fk, fks) => M.RhsThunkMk {typ = fk, fvs = fks})
              | "ThunkMkVal" => thunkMkVal NONE
              | _ => doPrim s
        val kw = P.bind identifierF doKw
        val setQuery = P.map (keycharSF #"?" && operand (state, env), fn (_, opnd) => M.RhsPSetQuery opnd)
        val p = varStuff || tup || const || kw || setQuery || P.error "Expected right-hand side"
      in p
      end

  fun instructionF (state : state, env : env) : M.instruction P.t =
      let
        val dests =
            P.map (parenSeqF (binder (state, env, M.VkLocal)) && keycharS #"=", #1) ||
            P.map (binderF (state, env, M.VkLocal) && keycharS #"=", Vector.new1 o #1) ||
            P.map (keycharSF #"!", fn () => Vector.new0 ())
        val p = dests && rhs (state, env) && keycharS #";"
        val p = P.map (p, fn ((vs, rhs), _) => M.I {dests = vs, n = 0, rhs = rhs})
      in p
      end

  fun instruction (state : state, env : env) : M.instruction P.t =
      instructionF (state, env) || P.error "Expected instruction"

  fun target (state : state, env : env) : M.target P.t =
      P.map (label (state, env) && parenSeq (operand (state, env)),
             fn (l, os) => M.T {block = l, arguments = os})

  (* f should fail at least if sees Default or } *)
  fun switch (state : state, env : env, f : state * env -> 'a P.t) : 'a M.switch P.t =
      let
        val case1 = P.map (f (state, env) && keywordS "=>" && target (state, env), fn ((k, _), t) => (k, t))
        val cases = P.zeroOrMoreV case1
        val default = P.map (keywordSF "Default" && keywordS "=>" && target (state, env), #2)
        val body = cases && P.optional default
        val p = P.map (operand (state, env) && brace body,
                       fn (opnd, (cs, d)) => {on = opnd, cases = cs, default = d})
      in p
      end

  fun codes (state : state, env : env) : M.codes P.t =
      let
        val exhaustive =
            P.map (keywordSF "<=", fn _ => true) || P.map (keycharSF #"?", fn _ => false) || P.error "Expected codes"
        val vs = P.map (braceSeq (variable (state, env)), VS.fromVector)
        val p = P.map (exhaustive && vs, fn (e, vs) => {possible = vs, exhaustive = e})
      in p
      end

  fun callA (state : state, env : env, s : string) : M.call P.t =
      case s
       of "Call" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.CCode {ptr = v, code = cs})
        | "CallClos" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.CClosure {cls = v, code = cs})
        | "CallDir" =>
          P.map (pair (variable (state, env), variable (state, env)),
                 fn (v1, v2) => M.CDirectClosure {cls = v1, code = v2})
        | _ => P.error "Expected call"

  fun call (state : state, env : env) : M.call P.t = P.bind identifierF (fn s => callA (state, env, s))

  fun evalA (state : state, env : env, s : string) : M.eval P.t =
      case s
       of "Eval" =>
          P.map (paren (variable (state, env)) && codes (state, env), fn (v, cs) => M.EThunk {thunk = v, code = cs})
        | "EvalDir" =>
          P.map (pair (variable (state, env), variable (state, env)),
                 fn (v1, v2) => M.EDirectThunk {thunk = v1, code = v2})
        | _ => P.error "Expected eval"

  fun eval (state : state, env : env) : M.eval P.t = P.bind identifierF (fn s => evalA (state, env, s))

  fun interProcA (state : state, env : env, s : string) : M.interProc P.t =
      let
        fun doCall () =
            P.map (callA (state, env, s) && parenSeq (operand (state, env)),
                   fn (c, os) => M.IpCall {call = c, args = os})
        fun doEval () =
            P.map (evalA (state, env, s) && keycharSF #":" && fieldKind (state, env),
                   fn ((e, _), fk) => M.IpEval {typ = fk, eval = e})
      in
        case s
         of "Call" => doCall ()
          | "CallClos" => doCall ()
          | "CallDir" => doCall ()
          | "Eval" => doEval ()
          | "EvalDir" => doEval ()
          | _ => P.error "Expected inter proc"
      end

  fun interProc (state : state, env : env) : M.interProc P.t = P.bind identifierF (fn s => interProcA (state, env, s))

  fun cuts (state : state, env : env) : M.cuts P.t =
      let
        val item = P.map (labelF (state, env), SOME) || P.map (keywordSF "Exit", fn _ => NONE) ||
                   P.error "Expected cut item"
        fun doOne (i, {exits, targets}) =
            case i
             of NONE => {exits = true, targets = targets}
              | SOME l => {exits = exits, targets = LS.insert (targets, l)}
        fun process is = M.C (Vector.fold (is, {exits = false, targets = LS.empty}, doOne))
        val p = P.map (keywordSF "/->/" && braceSeq item, process o #2)
        val p = p || P.succeed MU.Cuts.none
      in p
      end

  fun return (state : state, env : env) : M.return P.t =
      let
        val normal =
            P.map (keywordSF "->" &&
                   parenSeq (binder (state, env, M.VkLocal)) &&
                   label (state, env) &&
                   cuts (state, env),
                   fn (((_, vs), l), cs) => M.RNormal {rets = vs, block = l, cuts = cs})
        val tail =
            P.map (keywordSF "-|" && P.succeeds (keywordSF "/->/" && keywordS "Exit"),
                   fn (_, e) => M.RTail {exits = e})
        val p = normal || tail || P.error "Expected return"
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
              | "Case" => P.map (switch (state, env, constantF), M.TCase)
              | "Cut" => P.map (variable (state, env) && parenSeq (operand (state, env)) && cuts (state, env),
                                fn ((v, os), cs) => M.TCut {cont = v, args = os, cuts = cs})
              | "Eval" => interProc s
              | "EvalDir" => interProc s
              | "Goto" => P.map (target (state, env), M.TGoto)
              | "Halt" => P.map (paren (operand (state, env)), M.THalt)
              | "PSumCase" => P.map (switch (state, env, nameF), M.TPSumCase)
              | "Return" => P.map (parenSeq (operand (state, env)), M.TReturn)
              | _ => P.fail
        val p = P.bind identifierF doIt || P.error "Expected transfer"
      in p
      end

  fun blockF (state : state, env : env) : (M.label * M.block) P.t =
      let
        val header = labelF (state, env) && parenSeq (binder (state, env, M.VkLocal))
        val p = header && P.zeroOrMoreV (instructionF (state, env)) && transfer (state, env)
        val p = P.map (p, fn (((l, ps), is), t) => (l, M.B {parameters = ps, instructions = is, transfer = t}))
      in p
      end

  fun block (state : state, env : env) : (M.label * M.block) P.t = blockF (state, env) || P.error "Expected block"

  fun codeBody (state : state, env : env) : M.codeBody P.t =
      let
        val state = forkLabels state
        val entry = P.map (keywordS "Entry" && label (state, env), #2)
        val blks = P.map (P.zeroOrMore (blockF (state, env)), LD.fromList)
        val p = P.map (entry && blks, fn (e, bs) => M.CB {entry = e, blocks = bs})
      in p
      end

  fun codeA (state : state, env : env) : M.code P.t =
      let
        val binder' = fn (s, e) => binder (s, e, M.VkLocal)
        val args = parenSemiComma (callConv (state, env, binder'), binder (state, env, M.VkLocal))
        val header = P.map (optFlag #"^" && optFlag #"*" && args && effects (state, env),
                            fn (((r, e), (cc, vs)), fx) => (r, e, cc, vs, fx))
        val rets = P.map (keycharS #":" && parenSeq (typ (state, env)), #2)
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
              | "CString" => P.map (paren cstring, M.GCString)
              | "ErrorVal" => P.map (keycharS #":" && typ (state, env), fn (_, t) => M.GErrorVal t)
              | "I" => P.map (paren intInf, M.GInteger)
              | "Idx" =>
                P.map (braceSeq (P.map (name (state, env) && keywordS "->" && int, fn ((n, _), i) => (n, i))),
                       fn nis => M.GIdx (ND.fromVector nis))
              | "R" => P.map (paren rat, M.GRat)
              | "Closure" => P.map (keycharS #"(" && codeOption (state, env) && keycharS #";" &&
                                    fvsInits (state, env),
                                    fn (((_, co), _), fvs) => M.GClosure {code = co, fvs = fvs})
              | "PSet" => P.map (paren (simple (state, env)), M.GPSet)
              | "Tagged" =>
                P.map (pair (name (state, env),
                             simple (state, env) && keycharS #":" && fieldKind (state, env)),
                       fn (n, ((s, _), fk)) => M.GPSum {tag = n, typ = fk, ofVal = s})
              | "ThunkValue" => P.map (paren (simple (state, env) && keycharS #":" && fieldKind (state, env)),
                                       fn ((s, _), fk) => M.GThunkValue {typ = fk, ofVal = s})
              | _ => P.fail
        val tup = P.map (tupleF (state, env), fn (mdd, os) => M.GTuple {mdDesc = mdd, inits = os})
        val p = P.bind identifierF doIt || tup || P.map (simpleF (state, env), M.GSimple) || P.error "Expected global"
      in p
      end

  fun varGlobalF (state : state, env : env) : (M.variable * M.global) P.t =
      P.map (binderF (state, env, M.VkGlobal) && keycharS #"=" && global (state, env), fn ((v, _), g) => (v, g))

  fun globals (state : state, env : env) : M.global VD.t P.t =
      P.map (P.zeroOrMore (varGlobalF (state, env)), VD.fromList)

  fun includeKind (state : state, env : env) : M.includeKind P.t =
      P.required (P.map (identifierF, MU.IncludeKind.fromString), "Expected include kind")

  fun includeFileF (state : state, env : env) : M.includeFile P.t =
      P.map (cstringF && keycharS #":" && includeKind (state, env) && braceSeq (binder (state, env, M.VkExtern)),
             fn (((n, _), k), vs) => M.IF {name = n, kind = k, externs = VS.fromVector vs})

  fun includeFile (state : state, env : env) : M.includeFile P.t =
      includeFileF (state, env) || P.error "Expected include file"

  fun program (config : Config.t) : M.t P.t =
      let
        val stm = IM.new Prims.ordString
        val state = stateMk stm
        val env = envMk config
        val includes = P.map (keywordS "Includes:" && P.zeroOrMoreV (includeFileF (state, env)), #2)
        val externs = P.map (keywordS "Externs:" && braceSeq (binder (state, env, M.VkExtern)), VS.fromVector o #2)
        val globals = P.map (keywordS "Globals:" && globals (state, env), #2)
        val entry = P.map (keywordS "Entry:" && variable (state, env), #2)
        fun finish (((((), is), es), gs), e) =
            let
              val st = IM.finish stm
              val p = M.P {includes = is, externs = es, globals = gs, symbolTable = st, entry = e}
            in p
            end
        val p = P.map (whitespace && includes && externs && globals && entry, finish)
      in p
      end

  (*** Pass Stuff ***)

  fun parseFile (config : Config.t, f : string) : M.t =
      let
        val strm = Pervasive.TextIO.openIn f
        val instrm = Pervasive.TextIO.getInstream strm
        val instrm = InStreamWithPos.mk instrm
        val parser = P.map (program config && P.atEnd "Expected end of file", #1)
        val p =
            case P.parse (parser, instrm)
             of P.Success (_, p) => p
              | P.Failure => fail ("parseFile", "Parse failed")
              | P.Error ({line, col}, e) =>
                fail ("parseFile", "Parse error: line " ^ Int.toString line ^ " col " ^ Int.toString col ^ ": " ^ e)
      in p
      end
                     
  fun readFile (() : unit, pd : PassData.t, basename : Path.t) : M.t =
      let
        val config = PassData.getConfig pd
        val basename = Path.toCygwinString basename
        val infile = basename ^ ".mil"
        fun cleanup () = ()
        val p = Exn.finally (fn () => parseFile (config, infile), cleanup)
      in p
      end

  val description = {name        = modname,
                     description = "Mil parser",
                     inIr        = Pass.unitHelpers,
                     outIr       = MilUtils2.irHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkFilePass (description, associates, readFile)

end;

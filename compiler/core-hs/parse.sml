(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature CORE_HS_PARSE =
sig
  val pass : (unit, CoreHs.t) Pass.t
  val parseFile : string * Config.t -> CoreHs.t
  val noMainWrapper : Config.t -> bool
end

structure CoreHsParse :> CORE_HS_PARSE =
struct
  val passname = "CoreHsParse"
  fun failMsg (f, m) = Fail.fail (passname, f, m)
  val desc = {disableable = false,
              describe = fn () => Layout.str "Parse GHC Core files (.hcr)" }
  structure CharParser = CharParserF (structure Parser = FileParser)
  structure Chat = ChatF (struct 
                            type env = Config.t
                            val extract = fn a => a
                            val name = passname
                            val indent = 2
                          end)
  open CharParser
  infix 5 ||
  infixr 0 >>
  infixr 0 >>=

  structure CoreHsDef : LANGUAGE_DEF =
  struct
    structure Parser    = CharParser
    val commentStart    = "{-"
    val commentEnd      = "-}"
    val commentLine     = "--"
    val nestedComments  = true
    val identStart      = lower
    val identLetter     = lower || upper || digit || oneChar #"'"
    val opLetter        = oneOf (explode ";=@:\\%_.*#?%")
    val opStart         = opLetter
    val reservedNames   = List.map (["module", "data", "newtype", "rec",
                                     "let", "in", "case", "of", "cast",
                                     "note", "external", "forall"],
                                    fn s => "%" ^ s)
    val reservedOpNames = [";", "=", "@", "::", "\\", "%_",
                          ".", "*", "#", "?"]
    val caseSensitive   = true
  end

  structure P = TokenParserF (structure LanguageDef = CoreHsDef)
  structure TextIO = Pervasive.TextIO
  structure L = CoreHsDef
  structure C = CoreHs
  structure CHU = CoreHsUtils
  structure CHP = CoreHsPrims
  structure IM = Identifier.Manager
  structure U  = Utils
  structure UF = Utils.Function
  structure UO = Utils.Option
  structure SS = StringSet
  structure MS = SetF (struct type t = C.anMName val compare = CHU.compareAnMName end)
  structure MD = DictF (struct type t = C.anMName' val compare = CHU.compareAnMName' end)
  structure QS = SetF (struct type t = C.identifier C.qualified val compare = CHU.compareQName end)
  structure QD = DictF (struct type t = C.identifier C.qualified val compare = CHU.compareQName end)
  structure SD = StringDict
  structure TMU = HsToMilUtils
  structure QTS = TopoSortF (struct structure Dict = QD structure Set = QS end)

  structure Debug =
  struct
    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug CoreHs parser")
    fun debug   (config, s) = if Config.debug andalso debugPass config then print (s ^ "\n") else ()
  end

  val sCache : string SD.t ref = ref SD.empty
  val pCache : C.pName SD.t ref = ref SD.empty
  val mCache : C.anMName MD.t ref = ref MD.empty

  fun clearCache () = 
      let
        val _ = sCache := SD.empty
        val _ = pCache := SD.empty
        val _ = mCache := MD.empty
      in ()
      end

  fun cache (m, lookup, insert, f) k = 
      case lookup (!m, k) 
        of SOME v => v
         | NONE   => let val v = f k
                         val _ = m := insert (!m, k, v) 
                     in v 
                     end

  val cacheS = cache (sCache, SD.lookup, SD.insert, (fn x => x))
  val cacheP = cache (pCache, SD.lookup, SD.insert, C.P)
  val cacheM = cache (mCache, MD.lookup, MD.insert, C.M)

  val stringLiteral = P.stringLiteral >>= (return o cacheS)
  val identifier = P.identifier >>= (return o cacheS)

  fun reservedH w = oneChar #"%" >> $$ P.reserved w

  val coreLabel =
      reservedH "label" >>
      P.stringLiteral   >>= (fn s =>
      return (C.External ("", C.Label, s, CHP.tAddrzh)))

  datatype CoercionTy = TransC | InstC | SymC | UnsafeC | LeftC | RightC | NthC

  val symCo    = oneString "sym"    >> return SymC
  val transCo  = oneString "trans"  >> return TransC
  val unsafeCo = oneString "unsafe" >> return UnsafeC
  val leftCo   = oneString "left"   >> return LeftC
  val rightCo  = oneString "right"  >> return RightC
  val nthCo    = oneString "nth"    >> return NthC
  val instCo   = oneString "inst"   >> return InstC

  datatype ATyOp
    = ATy     of C.ty
    | Trans   of (C.ty * C.ty -> C.ty)
    | Sym     of (C.ty        -> C.ty)
    | Unsafe  of (C.ty * C.ty -> C.ty)
    | LeftCo  of (C.ty        -> C.ty)
    | RightCo of (C.ty        -> C.ty)
    | NthCo   of (int * C.ty  -> C.ty)
    | InstCo  of (C.ty * C.ty -> C.ty)

  val upperName =
      upper                    >>= (fn firstChar =>
      zeroOrMore L.identLetter >>= (fn rest =>
      P.whiteSpace             >>
      return (cacheS (implode (firstChar :: rest)))))

  val coreHierModuleNames = upperName  >>= (return o CHU.splitModuleName)

  val corePackageName = (P.identifier || upperName) >>= (return o cacheP)

  val coreModuleName =
      corePackageName     >>= (fn pkgName =>
      oneChar #":"        >>
      coreHierModuleNames >>= (fn (modHierarchy, baseName) =>
      return (cacheM (pkgName, modHierarchy, baseName))))

  val coreQualifiedName =
      corePackageName                                >>= (fn (C.P packageIdOrVarName) =>
      optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
      case maybeRest
        of NONE =>  return (NONE, packageIdOrVarName)
         | SOME (modHierarchy, baseName) =>
           (oneChar #"." >>
            identifier >>= (fn theId =>
            return (SOME (cacheM (cacheP packageIdOrVarName, modHierarchy, baseName)), 
                    theId)))))

  fun isUpperName s = if String.isEmpty s then false else Char.isUpper (String.sub (s, 0))

  fun isDCon s = isUpperName s orelse s = "z7eU" orelse s = "z7eUzh"

  (* special type constructor ~ is not an upperName! *)
  val z7eU = P.lexeme (
      oneString "z7eU" >>= (fn x =>
      optional (oneString "zh") >>= (fn y => 
      case y of NONE => return x | SOME z => return (x ^ z))))

  val zt = P.symbol "zt"

  val coreQualifiedCon =
      corePackageName       >>= (fn (C.P pkgId) =>
      ((oneChar #":"        >>
        coreHierModuleNames >>= (fn (modHierarchy, baseName) =>
        oneChar #"."        >>
        (upperName || z7eU) >>= (fn conName =>
        return (SOME (cacheM (cacheP pkgId, modHierarchy, baseName)), conName)))) ||
       (if isUpperName pkgId
          then return (NONE, pkgId)
          else error ("Expected a constructor name, got: " ^ pkgId))))

  val coreTvarOrQualifiedCon =
      corePackageName >>= (fn (C.P packageIdOrVarName) =>
      optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
      case maybeRest
        of NONE =>  return (C.Tvar packageIdOrVarName)
         | SOME (modHierarchy, baseName) =>
             (oneChar #"."        >>
              (upperName || z7eU || zt) >>= (fn theId =>
              return (C.Tcon (SOME (cacheM (cacheP packageIdOrVarName, modHierarchy, baseName)), theId))))))

  val coreDconOrVar =
  corePackageName >>= (fn (C.P firstPart) =>
  optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
  let
    val name = (NONE, firstPart)
  in
    case maybeRest
      of NONE => return ((if isDCon firstPart then C.Dcon else C.Var) name)
       | SOME (modHierarchy, baseName) =>
          (oneChar #"." >>
           (upperName || identifier) >>= (fn theId =>
           let
             val fullname = (SOME (cacheM (cacheP firstPart, modHierarchy, baseName)), theId)
           in
             return ((if isDCon theId then C.Dcon else C.Var) fullname)
           end))
  end))

  val coreTcon =
      (coreTvarOrQualifiedCon >>= (return o ATy)) ||
      (oneChar #"%" >>
       any [symCo, transCo, unsafeCo, instCo, leftCo, rightCo, nthCo] >>= (fn maybeCoercion =>
       return (case maybeCoercion
                of TransC  => Trans   C.TransCoercion 
                 | SymC    => Sym     C.SymCoercion 
                 | UnsafeC => Unsafe  C.UnsafeCoercion
                 | LeftC   => LeftCo  C.LeftCoercion 
                 | RightC  => RightCo C.RightCoercion
                 | NthC    => NthCo   C.NthCoercion
                 | InstC   => InstCo  C.InstCoercion)))

  val liftedKind   = P.symbol "*" >> return C.Klifted
  val unliftedKind = P.symbol "#" >> return C.Kunlifted
  val openKind     = P.symbol "?" >> return C.Kopen

  fun coreTbindGen' () =
      identifier                            >>= (fn tyVar =>
      optional (P.symbol "::" >> $ coreKind) >>= (fn kdecl =>
      return (tyVar, UO.get (kdecl, C.Klifted))))

  and coreTbindGen sep = optional sep >> $ coreTbindGen'
  and coreTbind () = coreTbindGen (return ()) || P.parens ($ coreTbind)
  and coreAtTbind () = P.symbol "@" >> $ coreTbind
  and coreTBinding () = $ coreAtTbind >>= return o C.Tb

  and coreForallTy () =
      reservedH "forall"      >>
      oneOrMore ($ coreTbind) >>= (fn tBinds =>
      P.symbol "."            >>
      $ coreType              >>= (fn bodyTy =>
      return (List.foldr (tBinds, bodyTy, C.Tforall))))

  (* NOTE: quick hack to type check new core syntax *)
  and coreAty () = coreTcon || (P.parens (optional (oneString "ghczmprim:GHCziPrim.sym" >>
                                P.whiteSpace) >>= (fn _ => $ coreType)) >>= return o ATy)

  and coreAtySaturated () = $ coreAty >>= (fn t =>
      case t
        of ATy ty => return ty
         | _      => error "Unexpected coercion ty")

  and coreBty () =
      $ coreAty                       >>= (fn hd =>
      P.whiteSpace                    >>
      let
        fun fail err m n = error (err ^ " expects " ^ Int.toString m ^ " arguments, but got " ^ Int.toString n)
        fun app0 t tys = return (List.fold (tys, t, UF.flipIn C.Tapp))
        fun app1 k _   (x :: []) = return (k x)
          | app1 _ err args      = fail err 1 (length args)
        fun app2 k _   (x :: y :: []) = return (k (x, y))
          | app2 _ err args           = fail err 2 (length args)
        val moreTys = zeroOrMore ($ coreAtySaturated) 
        val nthTy = P.integer >>= (fn i => 
                    ($ coreAtySaturated) >>= (fn ty => 
                    return (IntInf.toInt i, ty)))
      in
        case hd
          of ATy t     => moreTys >>= app0 t
           | Trans k   => moreTys >>= app2 k "trans"
           | Sym k     => moreTys >>= app1 k "sym"
           | Unsafe k  => moreTys >>= app2 k "unsafe"
           | LeftCo k  => moreTys >>= app1 k "left"
           | RightCo k => moreTys >>= app1 k "right"
           | InstCo k  => moreTys >>= app2 k "inst"
           | NthCo k   => nthTy   >>= return o k
      end)

  and coreType () =
      $ coreForallTy ||
      ($ coreBty                                >>= (fn hd =>
       zeroOrMore (P.symbol "->" >>= (fn s =>
       coreType ()))                            >>= (fn rest =>
       return (case rest
                 of [] => hd
                  | _  => List.fold (hd::rest, (C.Tcon CHU.tcArrow), UF.flipIn C.Tapp)))))

  and equalityKind () =
      $ coreBty      >>= (fn ty1 =>
      P.symbol ":=:" >>
      $ coreBty      >>= (fn ty2 =>
      return (ty1, ty2)))

  and coreAtomicKind () =
      liftedKind || unliftedKind || openKind ||
      P.parens ($ coreKind || ($ equalityKind >>= return o C.Keq))

  and coreKind () =
      $ coreAtomicKind                                       >>= (fn hd =>
      (oneOrMore (P.symbol "->" >> $ coreKind) || return []) >>= (fn rest =>
      return (List.fold (rest, hd, UF.flipIn C.Karrow))))

  val coreTbinds = zeroOrMore ($ coreTbind)

  val coreAtySaturated1 = 
      optional (P.symbol "!") >>= (fn strict =>
      $ coreAtySaturated      >>= (fn ty => 
      return (ty, UO.bool strict)))

  (*
  fun coreTbindsOrTyGen separator =
      let
        val b1 = P.symbol "("       >>
                 optional separator >>= (fn sep =>
                 case sep
                   of NONE   => ($ coreType >>= (fn t => P.symbol ")" >> return ([], [t])))
                    | SOME _ => (coreTbindGen separator         >>= (fn tb =>
                                 P.symbol ")"                   >>
                                 $$ coreTbindsOrTyGen separator >>= (fn (tbs,tys) =>
                                 return (tb::tbs,tys)))))
        val b2 = separator                      >>
                 $ coreTbindGen'                >>= (fn b =>
                 $$ coreTbindsOrTyGen separator >>= (fn (tbs,tys) =>
                 return (b::tbs,tys)))
      in optional (b1 || b2) >>= (fn res => return (UO.get (res, ([], []))))
      end
  *)
  fun coreTbindsOrTyGen separator =
      let
        val b = separator >> $ coreTbindGen'
      in
        zeroOrMore (b || P.parens b) 
      end

  fun aCoreVbind idP =
      idP           >>= (fn nm =>
      P.symbol "::" >>
      $ coreType    >>= (fn t =>
      return (nm, t)))
  val lambdaBind = aCoreVbind identifier
  val topVbind = aCoreVbind coreQualifiedName
  val coreVbind = P.parens (lambdaBind >>= return o C.Vb)
  val coreTbinding = $ coreAtTbind >>= (return o C.Tb)
  val coreBind = coreTbinding || coreVbind
  val coreLambdaBinds = oneOrMore coreBind

  val intOrRatLit =
      P.integer || P.parens P.integer          >>= (fn lhs =>
      optional (P.symbol "%" >> P.integer) >>= (fn rhs =>
      case rhs
        of SOME rhs => return (C.Lrational (Rat.rat (lhs, rhs)))
         | NONE     => return (C.Lint lhs)))

  val charLit = P.charLiteral >>= return o C.Lchar
  val stringLit = stringLiteral >>= return o C.Lstring
  val aLit = intOrRatLit || charLit || stringLit

  val coreLiteral =
      aLit          >>= (fn l =>
      P.symbol "::" >>
      $ coreType    >>= (fn t =>
      return (C.Literal (l, t))))

  val coreLit = coreLiteral >>= (return o C.Lit)

  val callconv = 
      (P.symbol "prim" >> return C.Prim) || 
      (P.symbol "capi" >> return C.CCall) || 
      (P.symbol "ccall" >> return C.CCall) || 
      (P.symbol "stdcall" >> return C.StdCall)

  (* TODO: handle the difference between ccall and stdcall *)
  val coreExternal =
      (reservedH "external" >>
       callconv             >>= (fn c =>
       stringLiteral      >>= (fn s =>
       $ coreAtySaturated   >>= (fn t =>
       return (C.External ("", c, s, t)))))) ||
      (reservedH "dynexternal" >>
       callconv                >>= (fn c =>
       $ coreAtySaturated      >>= (fn t =>
       return (C.External ("", C.Dynamic, "", t)))))


  fun caseVarBinds () =
      optional ($ coreAtTbind) >>= (fn firstTbind =>
      case firstTbind
        of SOME tb => ($ caseVarBinds                 >>= (fn (tbs, vbs) =>
                       return (tb :: tbs, vbs)))
         | NONE    => (zeroOrMore (P.parens lambdaBind) >>= (fn vbs =>
                       return ([], vbs))))

  fun coreVdef () =
    (topVbind || (lambdaBind >>= (fn (v, ty) =>
                  return (CHU.unqual v, ty))))  >>= (fn (vdefLhs, vdefTy) =>
    P.whiteSpace   >>
    P.symbol "="   >>
    P.whiteSpace   >>
    $ coreFullExp  >>= (fn vdefRhs =>
    return (C.Vdef (vdefLhs, vdefTy, vdefRhs))))

  and coreRecVdef () =
      reservedH "rec" >>
      P.braces (sepBy1 ($ coreVdef) (P.symbol ";")) >>= (return o C.Rec)

  and coreNonrecVdef () = $ coreVdef >>= (return o C.Nonrec)

  and coreVdefg () = $ coreRecVdef || $ coreNonrecVdef

  and coreAtomicExp () =
      any [coreDconOrVar, P.parens (coreLit || $ coreFullExp)] >>= (fn res =>
      P.whiteSpace >> return res)

  and coreFullExp () =
      any [$ coreLam, $ coreLet, $ coreCase, $ coreCast, $ coreNote,
           coreExternal, coreLabel, $ coreAppExp] || $ coreAtomicExp

  and coreAppExp () =
      $ coreAtomicExp >>= (fn oper =>
      zeroOrMore (P.whiteSpace >>
        (($ coreAtomicExp >>= (return o U.Inl)) ||
         (P.symbol "@" >> $ coreAtySaturated >>= (return o U.Inr)))) >>= (fn args =>
      return (List.fold(args, oper,
                        fn (arg, opr) => case arg
                                           of U.Inl arg => C.App  (opr, arg)
                                            | U.Inr arg => C.Appt (opr, arg)))))

  and coreLam () =
      P.symbol "\\"     >>
      coreLambdaBinds   >>= (fn binds =>
      P.symbol "->"     >>
      $ coreFullExp     >>= (fn body =>
      return (List.foldr (binds, body, C.Lam))))

  and coreLet () =
      reservedH "let" >>
      $ coreVdefg     >>= (fn vdefg =>
      P.whiteSpace    >>
      reservedH "in"  >>
      $ coreFullExp   >>= (fn body =>
      return (C.Let (vdefg, body))))

  and coreCase () =
      reservedH "case"    >>
      $ coreAtySaturated  >>= (fn ty =>
      $ coreAtomicExp     >>= (fn scrut =>
      reservedH "of"      >>
      P.parens lambdaBind   >>= (fn vBind =>
      $ coreAlts          >>= (fn alts =>
      return (C.Case (scrut, vBind, ty, alts))))))

  and coreCast () =
      reservedH "cast"         >>
      P.whiteSpace             >>
      P.parens ($ coreFullExp) >>= (fn body =>
      $ coreAtySaturated       >>= (fn ty =>
      return (C.Cast (body, ty))))

  and coreNote () =
      reservedH "note" >>
      stringLiteral  >>= (fn s =>
      $ coreFullExp    >>= (fn e =>
      return (C.Note (s,e))))

  and conAlt () =
      coreQualifiedCon >>= (fn conName =>
      P.whiteSpace     >>
      $ caseVarBinds   >>= (fn (tBinds, vBinds) =>
      P.symbol "->"    >>
      $ coreFullExp    >>= (fn rhs =>
      return (C.Acon (conName, tBinds, vBinds, rhs)))))

  and litAlt () =
      P.parens coreLiteral >>= (fn l =>
      P.symbol "->"        >>
      $ coreFullExp        >>= (fn rhs =>
      return (C.Alit (l, rhs))))

  and defaultAlt () =
      reservedH "_" >>
      P.symbol "->" >>
      $ coreFullExp >>= return o C.Adefault

  and coreAlt () = $ conAlt || $ litAlt || $ defaultAlt

  and coreAlts () = P.braces (sepBy1 ($ coreAlt) (P.symbol ";"))

  fun coreVdefGroups () = optionalWith [] (
      $ coreVdefg       >>= (fn theFirstVdef =>
      P.symbol ";"      >>
      $ coreVdefGroups  >>= (fn others =>
      return (theFirstVdef::others))))

  fun withTerminator p term = p >>= (fn x => P.symbol term >> return x)
  fun withSemi p = withTerminator p ";"

  val coreCdef =
      coreQualifiedCon                              >>= (fn dataConName =>
      P.whiteSpace                                  >>
      coreTbindsOrTyGen (P.symbol "@" >> return ()) >>= (fn tbs         =>
      zeroOrMore coreAtySaturated1                  >>= (fn tys         =>
      return (C.Constr (dataConName, tbs, tys)))))

  val coreCdefs = sepBy coreCdef (P.symbol ";")

  val coreTRep = P.symbol "=" >> $ coreType

  val coreDataDecl =
      P.reserved "data"  >>
      coreQualifiedCon   >>= (fn tyCon  =>
      P.whiteSpace       >>
      coreTbinds         >>= (fn tBinds =>
      P.symbol "="       >>
      P.braces coreCdefs >>= (fn cDefs  =>
      return (C.Data (tyCon, tBinds, cDefs)))))

  val coreNewtypeDecl =
      P.reserved "newtype" >>
      coreQualifiedCon     >>= (fn tyCon  =>
      P.whiteSpace         >>
      coreQualifiedCon     >>= (fn coercionName =>
      coreTbinds           >>= (fn tBinds =>
      coreTRep             >>= (fn tyRep  =>
      return (C.Newtype (tyCon, coercionName, tBinds, tyRep))))))

  val coreTdef  = withSemi (oneChar #"%" >> (coreDataDecl || coreNewtypeDecl))
  val coreTdefs = zeroOrMore coreTdef

  val coreModule =
      P.whiteSpace              >>= (fn _ =>
      reservedH "module"        >>
      coreModuleName            >>= (fn mName =>
      P.whiteSpace              >>
      optionalWith [] coreTdefs >>= (fn tdefs =>
      $ coreVdefGroups          >>= (fn vdefGroups =>
      atEnd                     >>
      return (C.Module (mName, tdefs, vdefGroups))))))

  fun parseFile (f : string, config:Config.t) : C.t =
      let
        val _ = clearCache ()
        val strm = TextIO.openIn f
        val instrm = TextIO.getInstream strm
        val instrm = InStreamWithPos.mk instrm
        val result = parse (coreModule, instrm)
        val _ = clearCache ()
        val () = TextIO.closeIn strm
      in
        case result
          of Success (_, prog) => prog
           | Failure => failMsg (f, "parse GHC Core file failed!")
           | Error (pos, err) => failMsg (f, "GHC Core parse error: " ^ err ^ " at " ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#col pos))
      end

  fun foldL (l, m, f) = List.foldr (l, ([], m), fn (x, (xs, m)) => 
                          let val (x, m) = f (x, m) in (x :: xs, m) end)

  fun foldL' (l, (e, m), f) = List.foldr (l, ([], e, m), fn (x, (xs, e, m)) => 
                          let val (x, e, m) = f e (x, m) in (x :: xs, e, m) end)

  fun mNameToPath (C.M (C.P pname, prefix, name)) =
      let
        val d = CHU.zDecodeString pname
        val prefix = List.map (prefix, CHU.zDecodeString)
        val name = CHU.zDecodeString name
        (* look for main package in current directory *)
        val p = Path.fromString "."
        val p = List.fold (prefix, p, Utils.Function.flipIn Path.snoc)
        val p = Path.snoc (p, name)
      in 
        (d, p)
      end


  (*
   * A definition is either for a value or a type.
   *
   * The DCon is just a place holder for data constructors
   * since they are defined as part of a type definition.
   *)
  datatype definition = VDef of C.vDef | TDef of C.tDef | DCon

  (*
   * A defDict is a mapping between names and the definition they represent,
   * as well as a set of names that they depend on. Alternatively we can
   * think of it as a dependency graph between definitions.
   *)
  type defDict = (definition * QS.t) QD.t

  (*
   * Given a program , return a mapping from qualified names to their 
   * definitions (either value or type) as well as the set of names 
   * they depend on.
   *)
  val scanModule : C.module -> defDict = 
    fn (C.Module (mname as (C.M (C.P pName, _, _)), tdefs, vdefgs)) =>
      let

        fun scanQName ((NONE, x), m) = scanQName ((SOME mname, x), m)
          | scanQName (y, m) = (y, QS.insert (m, y))

        fun scanTy (x as (C.Tcon name), m) = 
            if name = CHU.tcArrow then (x, m)
              else let 
                     val (name, m) = scanQName (name, m) 
                   in 
                     (C.Tcon name, m) 
                   end
          | scanTy (C.Tapp (t1, t2), m) = 
            let 
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (C.Tapp (t1, t2), m)
            end
          | scanTy (C.Tforall (b, t), m) = 
            let 
              val (t, m) = scanTy (t, m) 
            in 
              (C.Tforall (b, t), m) 
            end
          | scanTy (C.TransCoercion (t1, t2), m) = 
            let 
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (C.TransCoercion (t1, t2), m)
            end
          | scanTy (C.SymCoercion t, m) = 
            let 
              val (t, m) = scanTy (t, m) 
            in 
              (C.SymCoercion t, m) 
            end
          | scanTy (C.UnsafeCoercion (t1, t2), m) = 
            let 
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (C.UnsafeCoercion (t1, t2), m)
            end
          | scanTy (C.InstCoercion (t1, t2), m)= 
            let 
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (C.InstCoercion (t1, t2), m)
            end
          | scanTy (C.LeftCoercion t, m) =
            let 
              val (t, m) = scanTy (t, m) 
            in 
              (C.LeftCoercion t, m) 
            end
          | scanTy (C.RightCoercion t, m) = 
            let 
              val (t, m) = scanTy (t, m) 
            in 
              (C.RightCoercion t, m) 
            end
          | scanTy (C.NthCoercion (i, t), m) = 
            let 
              val (t, m) = scanTy (t, m) 
            in 
              (C.NthCoercion (i, t), m) 
            end
          | scanTy (x, m) = (x, m)

        fun scanTBind env ((v, k), m) = 
            let
              val env = QS.insert (env, (NONE, v))
            in
              ((v, k), env, m) 
            end

        fun scanVBind env ((v, t), m) = 
            let
              val (t, m) = scanTy (t, m)
              val env = QS.insert (env, (NONE, v))
            in
              ((v, t), env, m) 
            end

        fun scanBind env (C.Vb vb, m) = 
            let
              val (vb, env, m) = scanVBind env (vb, m)
            in
              (C.Vb vb, env, m)
            end
          | scanBind env (C.Tb tb, m) = 
            let
              val (tb, env, m) = scanTBind env (tb, m)
            in
              (C.Tb tb, env, m)
            end

         fun scanTBinds env (tbs, m) = foldL' (tbs, (env, m), scanTBind)

         fun scanVBinds env (vbs, m) = foldL' (vbs, (env, m), scanVBind)

         fun scanTys (tys, m) = foldL (tys, m, scanTy) 

        fun scanCDef (C.Constr ((q, n), tbs, tys), m) = 
            let
              val q = case q of NONE => SOME mname | _ => q
              val (tys, sts) = List.unzip tys
              val (tys, m) = scanTys (tys, m)
            in
              (C.Constr ((q, n), tbs, List.zip (tys, sts)), m)
            end

        fun scanCDefs (cdefs, m) = foldL (cdefs, m, scanCDef)

        fun scanTDef (C.Data (tname, tbinds, cdefs), m) =
            let 
              val (tname, m) = scanQName (tname, m)
              val (cdefs, m) = scanCDefs (cdefs, m)
            in
              (C.Data (tname, tbinds, cdefs), m)
            end
          | scanTDef (C.Newtype (tname1, tname2, tbinds, ty), m) =
            let
              val (tname1, m) = scanQName (tname1, m)
              val (tname2, m) = scanQName (tname2, m)
              val (ty, m) = scanTy (ty, m)
            in
              (C.Newtype (tname1, tname2, tbinds, ty), m)
            end

        fun scanVDefg env (C.Rec vdefs, m) = 
            let
              val env = List.fold (vdefs, env, fn (C.Vdef (v, _, _), env) => QS.insert (env, v))
              val (vdefs, m) = foldL (vdefs, m, scanVDef env)
            in
              (C.Rec vdefs, env, m)
            end
          | scanVDefg env (C.Nonrec vdef, m) = 
            let
              val (vdef as (C.Vdef (v, _, _)), m) = scanVDef env (vdef, m)
              val env = QS.insert (env, v)
            in
              (C.Nonrec vdef, env, m)
            end

        and scanVDef env (C.Vdef (v, t, e), m) =
            let
              val (t, m) = scanTy (t, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.Vdef (v, t, e), m)
            end

        and scanExp env (C.Var name, m) = 
            if QS.member (env, name)
              then (C.Var name, m)
              else let 
                     val (name, m) = scanQName (name, m)
                   in
                     (C.Var name, m)
                   end
          | scanExp env (C.Dcon name, m) = 
            let 
              val (name, m) = scanQName (name, m)
            in 
              (C.Dcon name, m)
            end
          | scanExp env (C.Lit l, m) = 
            let
              val (l, m) = scanLit (l, m)
            in
              (C.Lit l, m)
            end
          | scanExp env (C.App (f, e), m) = 
            let
              val (f, m) = scanExp env (f, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.App (f, e), m)
            end
          | scanExp env (C.Appt (e, t), m) = 
            let
              val (e, m) = scanExp env (e, m)
              val (t, m) = scanTy (t, m)
            in
              (C.Appt (e, t), m)
            end
          | scanExp env (C.Lam (b, e), m) = 
            let
              val (b, env, m) = scanBind env (b, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.Lam (b, e), m)
            end
          | scanExp env (C.Let (vdefg, e), m) = 
            let
              val (vdefg, env, m) = scanVDefg env (vdefg, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.Let (vdefg, e), m)
            end
          | scanExp env (C.Case (e, vb, t, alts), m) = 
            let
              val (e, m) = scanExp env (e, m)
              val (vb, env, m) = scanVBind env (vb, m)
              val (t, m) = scanTy (t, m)
              val (alts, m) = scanAlts env (alts, m)
            in
              (C.Case (e, vb, t, alts), m)
            end
          | scanExp env (C.Cast (e, t), m) = 
            let
              val (e, m) = scanExp env (e, m)
              val (t, m) = scanTy (t, m)
            in
              (C.Cast (e, t), m)
            end
          | scanExp env (C.Note (s, e), m) = 
            let 
              val (e, m) = scanExp env (e, m)
            in
              (C.Note (s, e), m)
            end
          | scanExp env (C.External (_, c, s, t), m) = 
            let
              val (t, m) = scanTy (t, m)
            in
              (* TODO: pName here is imprecise because this external call might be a 
               * result of inlining, so this is only a best effort guess. *)
              (C.External (pName, c, s, t), m)
            end


        and scanLit (C.Literal (v, t), m) = 
            let
              val (t, m) = scanTy (t, m)
            in
              (C.Literal (v, t), m)
            end

        and scanAlt env (C.Acon (name, tbs, vbs, e), m) = 
            let
              val (name, m) = scanQName (name, m)
              val (tbs, env, m) = scanTBinds env (tbs, m)
              val (vbs, env, m) = scanVBinds env (vbs, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.Acon (name, tbs, vbs, e), m)
            end
          | scanAlt env (C.Alit (l, e), m) = 
            let
              val (l, m) = scanLit (l, m)
              val (e, m) = scanExp env (e, m)
            in
              (C.Alit (l, e), m)
            end
          | scanAlt env (C.Adefault e, m) = 
            let
              val (e, m) = scanExp env (e, m)
            in
              (C.Adefault e, m)
            end

        and scanAlts env (alts, m) = foldL (alts, m, scanAlt env)

        fun fromVDefg (C.Rec defs) = defs
          | fromVDefg (C.Nonrec def) = [def]

        fun fromVDefgs defgs = List.concat (List.map (defgs, fromVDefg))

        fun qualify (C.Vdef ((p, n), t, e)) = C.Vdef ((case p of NONE => SOME mname | _ => p, n), t, e)

        val vdefs = List.map (fromVDefgs vdefgs, fn d => scanVDef QS.empty (qualify d, QS.empty))
        val tdefs = List.map (tdefs, fn d => scanTDef (d, QS.empty))
        val defd  = List.fold (vdefs, QD.empty, fn ((x as C.Vdef (n, _, _), m), d) => QD.insert (d, n, (VDef x, m)))
        val defd  = List.fold (tdefs, defd, 
                        fn ((x as C.Data (n as (p, q), _, cdefs), m), d) => 
                            let val n' = (p, q ^ "_")
                            in
                            List.fold (cdefs, QD.insert (d, n', (TDef x, m)),
                                fn (C.Constr (c, _, _), d) => QD.insert (d, c,
                                  (DCon, QS.singleton n')))
                            end
                         | ((x as C.Newtype (n as (p, q), c, _, _), m), d) => 
                            let val n' = (p, q ^ "_")
                            in 
                            QD.insert (QD.insert (d, n', (TDef x, m)), c, (DCon,
                            QS.singleton n'))
                            end)
      in
        defd
      end

  (* linearize turns the dependency graph of value definitions into a linear list. 
   *
   * Note that the result is not the tightest grouping, we might ended up with
   * group two independent recursive groups into the same letrec.
   *)
  fun linearize (defdict : (C.vDef * QS.t) QD.t) : C.vDefg list =
      let
        val toDefg 
          = fn [] => failMsg ("linearize", "impossible: topo-sorted component can not be empty") 
             | [(n, (def, s))] => if QS.member (s, n) then C.Rec [def] else C.Nonrec def
             | defs  => C.Rec (List.map (defs, #1 o #2))
        val sorted = QTS.sort (QD.toList defdict, #2 o #2)
      in
        List.map (sorted, toDefg)
      end

  val (noMainWrapperF, noMainWrapper) =
      Config.Feature.mk (passname ^ ":noMainWrapper", "do not generate the usual GHC main wrapper")

  fun readModule ((), pd, basename) =
      let
        val config = PassData.getConfig pd
        fun debug s = Debug.debug (config, s)
        fun verbose s = Chat.log1 (config, s)
        val stats = ref SD.empty        
        fun lookupStat n = case SD.lookup (!stats, n) of SOME t => t | NONE => Time.zero
        val time = 
         fn name => 
         fn f => 
         fn a => 
            let
              val s = Time.now ()
              val r = f a
              val e = Time.now ()
              val acc = lookupStat name
              val () = stats := SD.insert (!stats, name, Time.+(acc, Time.-(e, s)))
            in r
            end
        fun chatStats () = 
            let
              fun chat n = verbose ("  " ^ n ^ "\t" ^ Time.toString (lookupStat n) ^ "s")
              val () = verbose "time spent in "
              val () = chat "parsing"
              val () = chat "scanning"
              val () = chat "linearizing"
            in
              ()
            end
        val odir = ref ""
        val _ = List.map (Config.ghcOpt config, fn s => 
                    if String.hasPrefix (s, { prefix = "-odir " }) 
                      then odir := String.substring2 (s, { start = 6, finish = String.length s})
                      else ())
        val opath = if (!odir) = "" then Path.fromString "." else Path.fromString (!odir)
        val () = debug ("odir = " ^ !odir ^ " opath = " ^ Config.pathToHostString (config, opath))
        val basename = Config.pathToHostString (config, basename)
        val infile = if (!odir) = "" then basename ^ ".hcr" 
                        else Config.pathToHostString (config, Path.snoc(opath, "Main.hcr"))
         
        fun readOne (mname : C.anMName, defd : defDict, scanned : MS.t) =
            if MS.member (scanned, mname) 
              then (defd, scanned)
              else
                let
                  val (pname, path) = mNameToPath mname
                  val hcrRoot = 
                      case pname
                        of "main" => opath
                         | _ => (case #dirs (#options (TMU.getGhcPkg (config, pname)))
                           of [p] => Path.fromString p
                            | ps => failMsg ("readModule",
                                             "invalid lib path returned by ghc-pkg " ^
                                             Layout.toString (List.layout String.layout ps)))
                  val () = debug ("hcrRoot = " ^ Config.pathToHostString (config, hcrRoot))
                  (* val f1 = Config.pathToHostString (config, path) ^ ".hcr" *)
                  val path = Path.append (hcrRoot, path)
                  val file = Config.pathToHostString (config, path) ^ ".hcr"
                  val () = verbose ("parse " ^ Layout.toString (CoreHsLayout.layoutAnMName mname) ^ " from " ^ file)
                  fun scan module = 
                      let
                        val defd' = time "scanning" scanModule module
                        val scanned = MS.insert (scanned, mname)
                        val defd  = QD.union (defd, defd', #2)
                      in
                        (defd, scanned)
                      end
                in
                  if File.doesExist file
                    then scan (time "parsing" parseFile (file, config))
                    else failMsg ("readModule", "file " ^ file ^ " is not found")
                end

        fun traceDef (name, def as (_, depends), state as (defd, traced, scanned)) =
            case QD.lookup (traced, name)
              of SOME _ => state
               | NONE   => 
                let
                  val () = debug ("traceDef: " ^ Layout.toString (CoreHsLayout.layoutQName name) ^ " => " ^ 
                             QS.fold (depends, "", fn (n, s) => Layout.toString (CoreHsLayout.layoutQName n) ^ " " ^ s))
                  val traced = QD.insert (traced, name, def)
                  fun trace (name as (SOME m, n), state as (defd, traced, scanned)) = 
                      if m = CHU.primMname then state
                        else
                          let
                            val (defd, scanned) = readOne (m, defd, scanned)
                            val name' = (SOME m, n ^ "_")
                          in
                            case QD.lookup (defd, name)
                              of SOME def => traceDef (name, def, (defd, traced, scanned))
                               | NONE => (case QD.lookup (defd, name')
                                 of SOME def => traceDef (name', def, (defd, traced, scanned))
                                  | NONE => 
                                    (* Some types have no type constructors so they are not in ExtCore *)
                                    if not (String.isEmpty n) andalso (Char.isUpper (String.sub (n, 0)))
                                      then state 
                                      else failMsg ("traceDef", Layout.toString (CoreHsLayout.layoutQName name) ^ " not found"))
                          end
                    | trace ((NONE,   _), state) = state

                in
                  QS.fold (depends, (defd, traced, scanned), trace)
                end
            
        fun readAll (mname, defd, scanned, mainVar) = 
            case QD.lookup (defd, mainVar)
             of NONE => Fail.fail (passname, "readModule", "main program not found")
              | SOME def => 
                let 
                  val (_, traced, _) = traceDef (mainVar, def, (defd, QD.empty, scanned)) 
                  val () = debug ("traced = " ^ Layout.toString (Layout.sequence ("{", "}", ",") 
                                  (List.map (QD.domain traced, CoreHsLayout.layoutQName))))
                  val (tdefs, vdefs) = QD.fold (traced, ([], []), fn (_, (TDef d, _), (ts, vs)) => (d :: ts, vs)
                                                                   | (n, (VDef d, s), (ts, vs)) => (ts, (n, (d, s)) :: vs)
                                                                   | (_, _, s) => s)
                  val names = QS.keepAll (List.fold (vdefs, QS.empty, fn ((_, (_, p)), q) => QS.union (p, q)),
                                       fn (m, _) => m = SOME CHU.primMname)
                  val () = debug ("GHC.Prims needed " ^ QS.fold (names, "", 
                             fn (n, s) => s ^ "\n    " ^ Layout.toString (CoreHsLayout.layoutQName n)))
                  val vdefgs = time "linearizing" linearize (QD.fromList vdefs)
                in
                  C.Module (mname, tdefs, vdefgs)
                end
                
        fun cleanup () = 
            if Config.keep (config, "hcr") then ()
            else File.remove infile

        fun process () = 
            let
              val module as (C.Module (mname, _, _)) = time "parsing" parseFile (infile, config)
              val defd = time "scanning" scanModule module 
              val scanned = MS.fromList [mname, CHU.primMname]
              val mainVar = if noMainWrapper config then CHU.mainVar else CHU.wrapperMainVar
              val m = readAll (mname, defd, scanned, mainVar)
              val () = chatStats ()
            in m
            end
      in
        Exn.finally (process, cleanup)
      end

  fun layout (module, config) = CoreHsLayout.layoutModule module

  val description = {name        = passname,
                     description = "Parser for GHC core",
                     inIr        = Pass.unitHelpers,
                     outIr       = { printer = layout,
                                     stater  = layout },
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [Debug.debugPassD], features = [noMainWrapperF], subPasses = []}

  val pass = Pass.mkFilePass (description, associates, readModule)

end

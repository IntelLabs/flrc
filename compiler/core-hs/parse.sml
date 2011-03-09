(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature CORE_HS_PARSE =
sig
  val pass : (unit, unit) Pass.t
end

structure CoreHsParse :> CORE_HS_PARSE =
struct
  val passname = "Parse"
  val desc = {disableable = false,
              describe = fn () => Layout.empty}
  structure CharParser = CharParserF (structure CharParser = FileParser)
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

  structure TokenParser = TokenParserF (structure LanguageDef = CoreHsDef)
  open TokenParser
  structure TextIO = Pervasive.TextIO
  structure L = CoreHsDef
  structure C = CoreHs
  structure CHU = CoreHsUtils
  structure CHP = CoreHsPrims
  structure IM = Identifier.Manager
  structure U  = Utils
  structure UF = Utils.Function
  structure UO = Utils.Option

  fun reservedH w = oneChar #"%" >> reservedH w

  val coreLabel =
      reservedH "label" >>
      stringLiteral     >>= (fn s =>
      return (C.External (s, CHP.tAddrzh)))

  datatype CoercionTy = TransC | InstC | SymC | UnsafeC | LeftC | RightC

  val symCo    = oneString "sym"    >> return SymC
  val transCo  = oneString "trans"  >> return TransC
  val unsafeCo = oneString "unsafe" >> return UnsafeC
  val leftCo   = oneString "left"   >> return LeftC
  val rightCo  = oneString "right"  >> return RightC
  val instCo   = oneString "inst"   >> return InstC

  datatype ATyOp
    = ATy     of C.ty
    | Trans   of (C.ty * C.ty -> C.ty)
    | Sym     of (C.ty        -> C.ty)
    | Unsafe  of (C.ty * C.ty -> C.ty)
    | LeftCo  of (C.ty        -> C.ty)
    | RightCo of (C.ty        -> C.ty)
    | InstCo  of (C.ty * C.ty -> C.ty)

val upperName =
      upper                    >>= (fn firstChar =>
      zeroOrMore L.identLetter >>= (fn rest =>
      whiteSpace               >>
      return (implode (firstChar :: rest))))

  val coreHierModuleNames = upperName  >>= (return o CHU.splitModuleName)

  val corePackageName = (identifier || upperName) >>= (return o C.P)

  val coreModuleName =
      corePackageName     >>= (fn pkgName =>
      oneChar #":"        >>
      coreHierModuleNames >>= (fn (modHierarchy, baseName) =>
      return (C.M (pkgName, modHierarchy, baseName))))

  val coreQualifiedName =
      corePackageName                                >>= (fn (C.P packageIdOrVarName) =>
      optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
      case maybeRest
        of NONE =>  return (NONE, packageIdOrVarName)
         | SOME (modHierarchy, baseName) =>
           (oneChar #"." >>
            identifier   >>= (fn theId =>
            return (SOME (C.M (C.P packageIdOrVarName, modHierarchy, baseName)), theId)))))

  fun isUpperName s = if String.isEmpty s then false else Char.isUpper (String.sub (s, 0))

  val coreQualifiedCon =
      corePackageName       >>= (fn (C.P pkgId) =>
      ((oneChar #":"        >>
        coreHierModuleNames >>= (fn (modHierarchy, baseName) =>
        oneChar #"."        >>
        upperName           >>= (fn conName =>
        return (SOME (C.M (C.P pkgId, modHierarchy, baseName)), conName)))) ||
       (if isUpperName pkgId
          then return (NONE,pkgId)
          else error ("Expected a constructor name, got: " ^ pkgId))))

  val coreTvarOrQualifiedCon =
      corePackageName >>= (fn (C.P packageIdOrVarName) =>
      optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
      case maybeRest
        of NONE =>  return (C.Tvar packageIdOrVarName)
         | SOME (modHierarchy, baseName) =>
             (oneChar #"." >>
              upperName    >>= (fn theId =>
              return (C.Tcon (SOME (C.M (C.P packageIdOrVarName, modHierarchy, baseName)), theId))))))

  val coreDconOrVar =
  corePackageName >>= (fn (C.P firstPart) =>
  optional (oneChar #":" >> coreHierModuleNames) >>= (fn maybeRest =>
  let
    fun isUpper s = String.length s > 0 andalso Char.isUpper (String.sub (s, 0))
    val name = (NONE, firstPart)
  in
    case maybeRest
      of NONE => return ((if isUpper firstPart then C.Dcon else C.Var) name)
       | SOME (modHierarchy, baseName) =>
          (oneChar #"." >>
           (upperName || identifier) >>= (fn theId =>
           let
             val fullname = (SOME (C.M (C.P firstPart, modHierarchy, baseName)), theId)
           in
             return ((if isUpper theId then C.Dcon else C.Var) fullname)
           end))
  end))

  val coreTcon =
      (coreTvarOrQualifiedCon >>= (return o ATy)) ||
      (oneChar #"%" >>
       any [symCo, transCo, unsafeCo, instCo, leftCo, rightCo] >>= (fn maybeCoercion =>
       return (case maybeCoercion
                of TransC  => Trans   (fn (x, y) => C.TransCoercion (x, y))
                 | SymC    => Sym     (fn x      => C.SymCoercion x)
                 | UnsafeC => Unsafe  (fn (x, y) => C.UnsafeCoercion (x, y))
                 | LeftC   => LeftCo  (fn x      => C.LeftCoercion x)
                 | RightC  => RightCo C.RightCoercion
                 | InstC   => InstCo  (fn (x, y) => C.InstCoercion (x, y)))))

  val liftedKind   = symbol "*" >> return C.Klifted
  val unliftedKind = symbol "#" >> return C.Kunlifted
  val openKind     = symbol "?" >> return C.Kopen

  fun coreTbindGen' () =
      identifier  >>= (fn tyVar =>
      optional (symbol "::" >> coreKind ()) >>= (fn kdecl =>
      return (tyVar, UO.get (kdecl, C.Klifted))))
  and coreTbindGen sep = optional sep >> coreTbindGen' ()
  and coreTbind () = coreTbindGen (return ())
  and coreAtTbind () = symbol "@" >> coreTbind ()
  and coreTBinding () = coreAtTbind () >>= return o C.Tb

  and coreForallTy () =
      reservedH "forall"       >>
      oneOrMore (coreTbind ()) >>= (fn tBinds =>
      symbol "."               >>
      coreType ()              >>= (fn bodyTy =>
      return (List.foldr (tBinds, bodyTy, C.Tforall))))

  and coreAty () = coreTcon || (parens (coreType ()) >>= return o ATy)

  and coreAtySaturated () = coreAty () >>= (fn t =>
      case t
        of ATy ty => return ty
         | _      => error "Unexpected coercion ty")

  and coreBty () =
      coreAty ()                       >>= (fn hd =>
      whiteSpace                       >>
      zeroOrMore (coreAtySaturated ()) >>= (fn maybeRest =>
      let
        fun fail err m n = raise Fail (err ^ " expects " ^ Int.toString m ^
                                       " arguments, but got " ^ Int.toString n)
        fun app1 k (x :: [])      _   = k x
          | app1 _ args           err = fail err 1 (length args)
        fun app2 k (x :: y :: []) _   = k (x, y)
          | app2 _ args           err = fail err 2 (length args)
        val t = case hd
                  of ATy t     => List.fold (maybeRest, t, UF.flipIn C.Tapp)
                   | Trans k   => app2 k maybeRest "trans"
                   | Sym k     => app1 k maybeRest "sym"
                   | Unsafe k  => app2 k maybeRest "unsafe"
                   | LeftCo k  => app1 k maybeRest "left"
                   | RightCo k => app1 k maybeRest "right"
                   | InstCo k  => app2 k maybeRest "inst"
      in return t
      end))

  and coreType () =
      coreForallTy () ||
      (coreBty ()                              >>= (fn hd =>
       zeroOrMore (symbol "->" >> coreType ()) >>= (fn rest =>
       return (case rest
                 of [] => hd
                  | _  => List.fold (hd::rest, (C.Tcon CHU.tcArrow), UF.flipIn C.Tapp)))))

  and equalityKind () =
      coreBty ()    >>= (fn ty1 =>
      symbol ":=:"  >>
      coreBty ()    >>= (fn ty2 =>
      return (ty1, ty2)))

  and coreAtomicKind () =
      liftedKind || unliftedKind || openKind ||
      parens (coreKind () || (equalityKind () >>= return o C.Keq))

  and coreKind () =
      coreAtomicKind ()                                     >>= (fn hd =>
      (oneOrMore (symbol "->" >> coreKind ()) || return []) >>= (fn rest =>
      return (List.fold (rest, hd, UF.flipIn C.Karrow))))

  val coreTbinds = zeroOrMore (coreTbind ())

  fun coreTbindsOrTyGen separator =
      let
        val b1 = symbol "("         >>
                 optional separator >>= (fn sep =>
                 case sep
                   of NONE   => (coreType () >>= (fn t => symbol ")" >> return ([], [t])))
                    | SOME _ => (coreTbindGen separator      >>= (fn tb =>
                                 symbol ")"                  >>
                                 coreTbindsOrTyGen separator >>= (fn (tbs,tys) =>
                                 return (tb::tbs,tys)))))
        val b2 = separator                   >>
                 coreTbindGen' ()            >>= (fn b =>
                 coreTbindsOrTyGen separator >>= (fn (tbs,tys) =>
                 return (b::tbs,tys)))
      in optional (b1 || b2) >>= (fn res => return (UO.get (res, ([], []))))
      end

  fun aCoreVbind idP =
      idP          >>= (fn nm =>
      symbol "::"  >>
      coreType ()  >>= (fn t =>
      return (nm, t)))
  val lambdaBind = aCoreVbind identifier
  val topVbind = aCoreVbind coreQualifiedName
  val coreVbind = parens (lambdaBind >>= return o C.Vb)
  val coreTbinding = coreAtTbind () >>= (return o C.Tb)
  val coreBind = coreTbinding || coreVbind
  val coreLambdaBinds = oneOrMore coreBind

  val intOrRatLit =
      integer || parens integer        >>= (fn lhs =>
      optional (symbol "%" >> integer) >>= (fn rhs =>
      case rhs
        of SOME rhs => return (C.Lrational (Rat.rat (lhs, rhs)))
         | NONE     => return (C.Lint lhs)))

  val charLit = charLiteral >>= return o C.Lchar
  val stringLit = stringLiteral >>= return o C.Lstring
  val aLit = intOrRatLit || charLit || stringLit

  val coreLiteral =
      aLit        >>= (fn l =>
      symbol "::" >>
      coreType () >>= (fn t =>
      return (C.Literal (l, t))))

  val coreLit = coreLiteral >>= (return o C.Lit)

  val coreExternal =
      (reservedH "external" >>
       symbol "ccall"       >>
       stringLiteral        >>= (fn s =>
       coreAtySaturated ()  >>= (fn t =>
       return (C.External (s, t))))) ||
      (reservedH "dynexternal" >>
       symbol "ccall"          >>
       coreAtySaturated ()     >>= (fn t =>
       return (C.External ("[dynamic]", t))))


  fun caseVarBinds () =
      optional (coreAtTbind ()) >>= (fn firstTbind =>
      case firstTbind
        of SOME tb => (caseVarBinds ()                >>= (fn (tbs, vbs) =>
                       return (tb :: tbs, vbs)))
         | NONE    => (zeroOrMore (parens lambdaBind) >>= (fn vbs =>
                       return ([], vbs))))

  fun coreVdef () =
    (topVbind || (lambdaBind >>= (fn (v, ty) =>
                  return (CHU.unqual v, ty))))  >>= (fn (vdefLhs, vdefTy) =>
    whiteSpace >>
    symbol "=" >>
    whiteSpace >>
    coreFullExp () >>= (fn vdefRhs =>
    return (C.Vdef (vdefLhs, vdefTy, vdefRhs))))

  and coreRecVdef () =
      reservedH "rec" >>
      braces (sepBy1 (coreVdef ()) (symbol ";")) >>= (return o C.Rec)

  and coreNonrecVdef () = coreVdef () >>= (return o C.Nonrec)

  and coreVdefg () = coreRecVdef () || coreNonrecVdef ()

  and coreVdefGroups () =
      coreVdefg ()      >>= (fn theFirstVdef =>
      symbol ";"        >>
      coreVdefGroups () >>= (fn others =>
      return (theFirstVdef::others)))

  and coreAtomicExp () =
      any [coreDconOrVar, parens (coreLit || coreFullExp ())] >>= (fn res =>
      whiteSpace >> return res)

  and coreFullExp () =
      any [coreLam (), coreLet (), coreCase (), coreCast (), coreNote (),
           coreExternal, coreLabel, coreAppExp ()] || coreAtomicExp ()

  and coreAppExp () =
      coreAtomicExp () >>= (fn oper =>
      zeroOrMore (whiteSpace >>
        ((coreAtomicExp () >>= (return o U.Inl)) ||
         (symbol "@" >> coreAtySaturated () >>= (return o U.Inr)))) >>= (fn args =>
      return (List.fold(args, oper,
                        fn (arg, opr) => case arg
                                           of U.Inl arg => C.App  (opr, arg)
                                            | U.Inr arg => C.Appt (opr, arg)))))

  and coreLam () =
      symbol "\\"     >>
      coreLambdaBinds >>= (fn binds =>
      symbol "->"     >>
      coreFullExp ()  >>= (fn body =>
      return (List.foldr (binds, body, C.Lam))))

  and coreLet () =
      reservedH "let" >>
      coreVdefg ()    >>= (fn vdefg =>
      whiteSpace      >>
      reservedH "in"  >>
      coreFullExp ()  >>= (fn body =>
      return (C.Let (vdefg, body))))

  and coreCase () =
      reservedH "case"    >>
      coreAtySaturated () >>= (fn ty =>
      coreAtomicExp ()    >>= (fn scrut =>
      reservedH "of"      >>
      parens lambdaBind   >>= (fn vBind =>
      coreAlts ()         >>= (fn alts =>
      return (C.Case (scrut, vBind, ty, alts))))))

  and coreCast () =
      reservedH "cast"        >>
      whiteSpace              >>
      parens (coreFullExp ()) >>= (fn body =>
      coreAtySaturated ()     >>= (fn ty =>
      return (C.Cast (body, ty))))

  and coreNote () =
      reservedH "note" >>
      stringLiteral    >>= (fn s =>
      coreFullExp ()   >>= (fn e =>
      return (C.Note (s,e))))

  and conAlt () =
      coreQualifiedCon >>= (fn conName =>
      whiteSpace       >>
      caseVarBinds ()  >>= (fn (tBinds, vBinds) =>
      symbol "->"      >>
      coreFullExp ()   >>= (fn rhs =>
      return (C.Acon (conName, tBinds, vBinds, rhs)))))

  and litAlt () =
      parens coreLiteral  >>= (fn l =>
      symbol "->"         >>
      coreFullExp ()      >>= (fn rhs =>
      return (C.Alit (l, rhs))))

  and defaultAlt () =
      reservedH "_"  >>
      symbol "->"    >>
      coreFullExp () >>= return o C.Adefault

  and coreAlt () = conAlt () || litAlt () || defaultAlt ()

  and coreAlts () = braces (sepBy1 (coreAlt ()) (symbol ";"))

  fun withTerminator p term = p >>= (fn x => symbol term >> return x)
  fun withSemi p = withTerminator p ";"

  val coreCdef =
      coreQualifiedCon                            >>= (fn dataConName =>
      whiteSpace                                  >>
      coreTbindsOrTyGen (symbol "@" >> return ()) >>= (fn (tbs,tys1) =>
      zeroOrMore (coreAtySaturated ())            >>= (fn tys2         =>
      return (C.Constr (dataConName, tbs, tys1 @ tys2)))))

  val coreCdefs = sepBy coreCdef (symbol ";")

  val coreTRep = symbol "=" >> coreType ()

  val coreDataDecl =
      reserved "data"  >>
      coreQualifiedCon >>= (fn tyCon  =>
      whiteSpace       >>
      coreTbinds       >>= (fn tBinds =>
      symbol "="       >>
      braces coreCdefs >>= (fn cDefs  =>
      return (C.Data (tyCon, tBinds, cDefs)))))

  val coreNewtypeDecl =
      reserved "newtype" >>
      coreQualifiedCon   >>= (fn tyCon  =>
      whiteSpace         >>
      coreQualifiedCon   >>= (fn coercionName =>
      coreTbinds         >>= (fn tBinds =>
      coreTRep           >>= (fn tyRep  =>
      return (C.Newtype (tyCon, coercionName, tBinds, tyRep))))))

  val coreTdef  = withSemi (oneChar #"%" >> (coreDataDecl || coreNewtypeDecl))
  val coreTdefs = zeroOrMore coreTdef

  val coreModule =
      whiteSpace                >>
      reservedH "module"        >>
      coreModuleName            >>= (fn mName =>
      whiteSpace                >>
      optionalWith [] coreTdefs >>= (fn tdefs =>
      coreVdefGroups ()         >>= (fn vdefGroups =>
      atEnd                     >>
      return (C.Module (mName, tdefs, vdefGroups)))))

  fun parseFile (f : string, config:Config.t) : C.t =
      let
        val strm = TextIO.openIn f
        val instrm = TextIO.getInstream strm
        val instrm = InStreamWithPos.mk instrm
        val () = print ("start parsing " ^ f ^ "\n")
        val result = parse (coreModule, instrm)
        val () = TextIO.closeIn strm
      in
        case result
          of Success (_, prog) => prog
           | Failure => raise Fail "parse GHC Core file failed!"
           | Error (pos, err) => raise Fail ("GHC Core parse error: " ^ err ^ " at " ^ Int.toString (#line pos) ^ ":" ^ Int.toString (#col pos))
      end

  fun readHs ((), pd, basename) =
      let
        val config = PassData.getConfig pd
        val basename = Config.pathToHostString (config, basename)
        val infile = basename ^ ".hcr"
        fun cleanup () =
            if Config.keepCp config then ()
            else File.remove infile
        val cp = Exn.finally (fn () => parseFile (infile, config),
                              cleanup)
      in ()
      end

  val description = {name        = passname,
                     description = "Core P parser",
                     inIr        = Pass.unitHelpers,
                     outIr       = { printer = fn (_, _) => Layout.str "",
                                     stater = fn (_, _) => Layout.str "" }, (* CoreHsUtils.irHelpers, *)
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkFilePass (description, associates, readHs)

end

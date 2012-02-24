(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* A Representation of Pillar & C *)

signature PIL = sig
  type identifier
  val identifier : string -> identifier

  (* Types *)
  structure T : sig
    type t
    val void : t
    val sint8 : t
    val sint16 : t
    val sint32 : t
    val sint64 : t
    val sint128 : t
    val uint8 : t
    val uint16 : t
    val uint32 : t
    val uint64 : t
    val uint128 : t
    val sintp : t
    val uintp : t
    val char : t
    val bool : t
    val float : t
    val double : t
    val continuation : t
    val named : identifier -> t
    val ptr : t -> t
    val array : t -> t
    val arrayConstant : t * int -> t
    val code : t * t list -> t
    val strct : identifier option * (identifier * t) list -> t
  end
  type varDec
  val varDec : T.t * identifier -> varDec
  val contDec : identifier -> varDec
  (* Expressions *)
  structure E : sig
    type t
    val null : t
    val namedConstant : identifier -> t
    val variable : identifier -> t
    val int : int -> t
    val int32 : Int32.t -> t
    val intInf : IntInf.t -> t (* Arbitrary precision *)
    val word   : Word.t -> t
    val word32 : Word32.word -> t
    val wordInf : IntInf.t -> t (* Lay out in hex *)
    val boolean : bool -> t
    val float : Real32.t -> t
    val double : Real64.t -> t
    val char : char -> t
    val string : string -> t
    val cast : T.t * t -> t
    val addrOf : t -> t
    datatype compareOp = CoEq | CoNe | CoLt | CoLe | CoGt | CoGe
    val compare : T.t * t * compareOp * t -> t
    val equal : t * t -> t
    val negate : t -> t
    datatype arithOp = AoAdd | AoSub | AoMul | AoDiv | AoMod
    val arith : t * arithOp * t -> t
    val call : t * t list -> t
    val callAlsoCutsTo : Config.t * t * t list * identifier list -> t
    val assign : t * t -> t
    val arrow : t * identifier -> t
    val sub : t * t -> t
    val strctInit : t list -> t
    (* NG: want to pass types to macros, this is for that *)
    val hackTyp : T.t -> t
  end
  type varDecInit = varDec * E.t option
  (* Statements *)
  structure S : sig
    type t
    val empty : t
    val expr : E.t -> t
    val label : identifier -> t
    val goto : identifier -> t
    val ifThen : E.t * t -> t
    val ifThenElse : E.t * t * t -> t
    val switch : E.t * (E.t * t) list * t option -> t
    val return : t
    val returnExpr : E.t -> t
    val tailCall : Config.t * bool (*void?*) * E.t * E.t list -> t
    val call : E.t * E.t list -> t
    val sequence : t list -> t
    val block : varDecInit list * t list -> t
    val contMake : E.t * identifier * identifier -> t
    val contEntry : identifier * identifier -> t
    val contCutTo : Config.t * E.t * E.t list * identifier list -> t
    val noyield : t -> t
    val yield : t
    (* Pillar only *)
    val continuation : identifier * identifier list -> t
    val vse : identifier * t -> t
  end
  (* Top-level declarations *)
  structure D : sig
    type t
    val alignedStaticVariable : int * varDec -> t
    val alignedStaticVariableExpr : int * varDec * E.t -> t
    val blank : t
    val comment : string -> t
    val includeLocalFile : string -> t
    val define : identifier * bool -> t
    val constantMacro : identifier * E.t -> t
    val macroCall : identifier * E.t list -> t
    val typDef : T.t * identifier -> t
    val externVariable : varDec -> t
    val staticVariable : varDec -> t
    val staticVariableExpr : varDec * E.t -> t
    val staticFunction : T.t * identifier * varDec list * varDecInit list * S.t list -> t
    val function : T.t * identifier * varDec list * varDecInit list * S.t list -> t
    val managed : Config.t * bool -> t (* Turn managed on or off *)
    val sequence : t list -> t
    val layout : t -> Layout.t
  end
end;

structure Pil :> PIL =
struct

  structure L = Layout
  structure LU = LayoutUtils

  val outputKind = Config.output 

  type identifier = L.t

  fun identifier v = L.str v

  (* SML uses ~ where C/Pillar use - *)
  fun fixNeg s =
      String.substituteAll (s, {substring = "~", replacement = "-"})


  (* Types *)
  structure T = struct

    type t = L.t * (L.t -> L.t) * (L.t * L.t -> L.t)

    fun dec ((b, _, f), i) = f (b, i)

    fun abs (b, f, _) = f b

    fun sdec (b, i) = L.seq [b, L.str " ", i]

    fun sabs b = b

    val void = (L.str "void", sabs, sdec)
    val sint8 = (L.str "sint32", sabs, sdec)
    val sint16 = (L.str "sint32", sabs, sdec)
    val sint32 = (L.str "sint32", sabs, sdec)
    val sint64 = (L.str "sint64", sabs, sdec)
    val sint128 = (L.str "sint32", sabs, sdec)
    val uint8 = (L.str "uint32", sabs, sdec)
    val uint16 = (L.str "uint32", sabs, sdec)
    val uint32 = (L.str "uint32", sabs, sdec)
    val uint64 = (L.str "uint64", sabs, sdec)
    val uint128 = (L.str "uint32", sabs, sdec)
    val sintp = (L.str "sintp", sabs, sdec)
    val uintp = (L.str "uintp", sabs, sdec)
    val char = (L.str "char", sabs, sdec)
    val bool = (L.str "bool", sabs, sdec)
    val float = (L.str "float", sabs, sdec)
    val double = (L.str "double", sabs, sdec)
    val continuation = (L.str "PilContinuation", sabs, sdec)

    fun named i = (i, sabs, sdec)

    fun ptr (b, _, f2) =
        let
          fun f3 b = f2 (b, L.str "(*)")
          fun f4 (b, i) = f2 (b, L.paren (L.seq [L.str "*", i]))
        in
          (b, f3, f4)
        end

    fun array (b, _, f2) =
        let
          fun f3 b = f2 (b, L.str "[]")
          fun f4 (b, i) = f2 (b, L.seq [i, L.str "[]"])
        in
          (b, f3, f4)
        end

    fun arrayConstant ((b, _, f2), n) =
        let
          fun f3 b = f2 (b, L.seq [L.str "[", Int.layout n, L.str "]"])
          fun f4 (b, i) = f2 (b, L.seq [i, L.seq [L.str "[", Int.layout n, L.str "]"]])
        in
          (b, f3, f4)
        end

    fun codeA ((b, _, f2), args) =
        let
          val args =
              if List.isEmpty args then
                L.str "(void)"
              else
                L.tuple args
          fun f3 b = Fail.fail ("Pil", "code",
                                "C doesn't allow abstract unboxed code type")
          fun f4 (b, i) = f2 (b, L.seq [i, args])
        in
          (b, f3, f4)
        end

    fun code (rt, ats) = codeA (rt, List.map (ats, abs))

    fun strct (n, fields) =
        let
          val l1 = case n of NONE => L.str " "
                           | SOME i => L.seq [L.str " ", i, L.str " "]
          fun doOne (f, t) = L.seq [dec (t, f), L.str ";"]
          val ls = List.map (fields, doOne)
          val b =
              L.mayAlign [L.seq [L.str "struct", l1, L.str "{"],
                          LU.indent (L.mayAlign ls),
                          L.str "}"]
        in
          (b, sabs, sdec)
        end

  end

  type varDec = Layout.t

  fun varDec (t, x) = T.dec (t, x)

  fun contDec cv =
      L.seq [L.str "pilContinuationLocal", L.paren cv]

  (* Expressions *)
  structure E = struct

    (* Precedences:
     *    0 Expression
     *    1 Assignment-expression
     *    2 Conditional-expression
     *    3 Logical-or-expression
     *    4 Logical-and-expression
     *    5 Inclusive-or-expression
     *    6 Exclusive-or-expression
     *    7 And-expression
     *    8 Equality-expression
     *    9 Relational-expression
     *   10 Shift-expression
     *   11 Additive-expression
     *   12 Multiplicative-expression
     *   13 Cast-expression
     *   14 Unary-expression
     *   15 Postfix-expression
     *   16 Primary-expression
     *)
    type t = L.t * int (* prec *)

    fun layout (l, _) = l
    fun inPrec ((l, p1), p2) = if p1 >= p2 then l else L.paren l

    val null = (L.str "0", 16)

    fun namedConstant i = (i, 16)

    fun variable i = (i, 16)

    fun word  w = (L.str ("0x" ^ (Word.toString w)), 16)
    fun word32 w = (L.str ("0x" ^ (Word32.toString w)), 16)
    fun wordInf w = (L.str ("0x" ^ (IntInf.format (w, StringCvt.HEX))), 16)

    val two2thirtyone = IntInf.<< (1, Word.fromInt 31)
    fun intInf i = 
        if i >= 0 andalso i < two2thirtyone then
          (IntInf.layout i, 16)
        else
          wordInf i

    fun int i = 
        if i >= 0 then
          (Int.layout i, 16)
        else
          word (Word.fromInt i)

    fun int32 i = 
        if i >= 0 then
          (Int32.layout i, 16)
        else
          word32 (Word32.fromInt i)

    fun boolean b = (L.str (if b then "1" else "0"), 16)

    fun float f = 
        let
          val s = 
              case Real32.class f
               of Real64.Class.INF => 
                  if f < 0.0 then 
                    "-INFINITY32"
                  else
                    "INFINITY32"
                | Real64.Class.NAN => 
                  if Real32.signBit f then
                    "-NAN32"
                  else
                    "NAN32"
                | _ => 
                  fixNeg (Real32.toString f)
        in (L.str s, 16)
        end
    fun double f = 
        let
          val s = 
              case Real64.class f
               of Real64.Class.INF => 
                  if f < 0.0 then 
                    "-INFINITY64"
                  else
                    "INFINITY64"
                | Real64.Class.NAN => 
                  if Real64.signBit f then
                    "-NAN64"
                  else
                    "NAN64"
                | _ => 
                  fixNeg (Real64.toString f)
        in (L.str s, 16)
        end

    fun char c = (L.seq [ L.str "'", L.str (Char.escapeC c), L.str "'"], 16)

    fun string s =
        (L.seq [L.str "\"", L.str (String.escapeC s), L.str "\""], 16)

    fun cast (t, e) =
        (L.seq [L.paren (T.abs t), inPrec (e, 13)], 13)

    fun addrOf e = (L.seq [L.str "&", inPrec (e, 13)], 14)

    datatype compareOp = CoEq | CoNe | CoLt | CoLe | CoGt | CoGe

    fun compare (t, e1, co, e2) =
        let
          val (lco, p) =
              case co
               of CoEq => ("==", 8)
                | CoNe => ("!=", 8)
                | CoLt => ("<",  9)
                | CoLe => ("<=", 9)
                | CoGt => (">",  9)
                | CoGe => (">=", 9)
        in
          (L.mayAlign [L.seq [inPrec (cast (t, e1), p), L.str " ", L.str lco],
                       LU.indent (inPrec (cast (t, e2), p+1))],
           p)
        end

    fun equal (e1, e2) =
        (L.mayAlign [L.seq [inPrec (e1, 8), L.str " =="],
                     LU.indent (inPrec (e2, 9))],
         8)

    fun negate e = (L.seq [L.str "-", inPrec (e, 13)], 14)

    datatype arithOp = AoAdd | AoSub | AoMul | AoDiv | AoMod

    fun arith (e1, ao, e2) =
        let
          val (lao, p) =
              case ao
               of AoAdd => ("+", 11)
                | AoSub => ("-", 11)
                | AoMul => ("*", 12)
                | AoDiv => ("/", 12)
                | AoMod => ("%", 12)
        in
          (L.mayAlign [L.seq [inPrec (e1, p), L.str " ", L.str lao],
                       LU.indent (inPrec (e2, p+1))],
           p)
        end

    fun callArgs es = L.tuple (List.map (es, fn e => inPrec (e, 1)))

    fun call (e, es) =
        (L.mayAlign [inPrec (e, 15), LU.indent (callArgs es)], 15)

    fun cutsTo (config, cuts) =
        if outputKind config = Config.OkPillar andalso
           not (List.isEmpty cuts) then
          let
            val cuts = L.sequence ("", "", ",") cuts
            val cuts = if Config.toolset config = Config.TsIpc then L.paren cuts else cuts
            val ls = [LU.indent (L.seq [L.str "also cuts to ", cuts])]
          in ls
          end
        else
          []

    fun callAlsoCutsTo (config, e, es, cuts) =
        (L.mayAlign (inPrec (e, 15) ::
                     (LU.indent (callArgs es)) ::
                     cutsTo (config, cuts)),
         15)

    fun assign (e1, e2) =
        (L.mayAlign [L.seq [inPrec (e1, 14), L.str " ="],
                     LU.indent (inPrec (e2, 1))],
         1)

    fun arrow (e, i) = (L.seq [inPrec (e, 15), L.str "->", i], 15)

    fun sub (e1, e2) =
        (L.mayAlign [inPrec (e1, 15), LU.indent (LU.bracket (layout e2))], 15)

    fun strctInit es =
        (L.sequence ("{","}", ",") (List.map (es, fn e => inPrec (e, 1))), 16)

    fun hackTyp t = (T.abs t, 1)

  end

  type varDecInit = varDec * E.t option

  fun addSemi l = L.seq [l, L.str ";"]

  fun varDecInit (vd, eo) =
      let
        val l =
            case eo
             of NONE => vd
              | SOME e =>
                L.mayAlign [L.seq [vd, L.str " ="],
                            LU.indent (E.inPrec (e, 2))]
      in
        addSemi l
      end

  (* Statements *)
  structure S = struct

    type t = L.t list (* list of atomic statements *)

    fun single ls =
        case ls
         of [] => ([L.str ";"], false)
          | [l] => ([LU.indent l], false)
          | _::_ => (List.map (ls, LU.indent), true)

    val empty = []

    fun expr e = [addSemi (E.layout e)]

    fun label i = [L.seq [i, L.str ":"]]

    fun goto i = [addSemi (L.seq [L.str "goto ", i])]

    fun addOpenBrace (l, b) = if b then L.seq [l, L.str " {"] else l

    (* NG: not quite right, need to deal with else disambiguation *)
    fun ifThen (e, s) =
        let
          val (ls, b) = single s
          val hd = L.seq [L.str "if ", L.paren (E.layout e)]
          val hd = addOpenBrace (hd, b)
          val post = if b then [L.str "}"] else []
        in
          [L.mayAlign ((hd :: ls) @ post)]
        end

    fun ifThenElse (e, s1, s2) =
        let
          val (ls1, b1) = single s1
          val (ls2, b2) = single s2
          val b = b1 orelse b2
          val hd = L.seq [L.str "if ", L.paren (E.layout e)]
          val hd = addOpenBrace (hd, b)
          val els = L.str (if b then "} else {" else "else")
          val post = if b then [L.str "}"] else []
        in
          [L.mayAlign ((hd :: ls1) @ (els :: ls2) @ post)]
        end

    fun switch (d, arms, dft) =
        let
          val b = L.mayAlign [L.str "switch",
                              LU.indent (L.paren (E.layout d)),
                              L.str "{"]
          fun doOne (c, s) =
              (L.seq [L.str "case ", E.layout c, L.str ":"]) ::
              (List.map (s, LU.indent))
          val cs = List.concat (List.map (arms, doOne))
          val dft =
              case dft
               of NONE => []
                | SOME dft =>
                  (L.str "default:") :: (List.map (dft, LU.indent))
          val f = [L.str "}"]
        in
          [L.mayAlign ((b :: cs) @ dft @ f)]
        end

    val return = [addSemi (L.str "return")]

    fun returnExpr e = [addSemi (L.seq [L.str "return ", E.layout e])]

    fun tailCall (config, v, e, es) =
        let
          val lcall = L.paren (L.mayAlign [E.inPrec (e, 15), E.callArgs es])
          val l = addSemi (L.mayAlign [L.str (if v then "TAILCALLV" else "TAILCALL"), LU.indent lcall])
        in [l]
        end

    fun call (e, es) = expr (E.call (e, es))

    fun sequence ss = List.concat ss

    fun block (vds, ss) =
        let
          val vds = List.map (vds, fn vd => LU.indent (varDecInit vd))
          val ss = List.map (List.concat ss, fn s => LU.indent s)
        in
          [L.mayAlign ((L.str "{")::vds @ ss @ [L.str "}"])]
        end

    fun contMake (e, cl, cv) =
        [L.seq [L.str "pilContinuationMake",
                L.tuple [E.inPrec (e, 1), cl, cv],
                L.str ";"]]

    fun contEntry (cl, cv) =
        [L.seq [L.str "pilContinuation", L.tuple [cl, cv], L.str ";"]]

    fun contCutTo (config, e, args, cuts) =
        [if List.isEmpty cuts then 
           if List.isEmpty args then
             L.seq [L.str "pilCutTo0", L.paren (E.inPrec (e, 1)), L.str ";"]
           else
             if outputKind config = Config.OkC then
               Fail.fail ("Pil", "contCutTo", "cut with arguments not supported on C")
             else
               L.seq [L.str "pilCutToA", L.tuple (List.map (e::args, fn e => E.inPrec (e, 1))), L.str ";"]
         else
           if List.isEmpty args then
             L.seq [L.str "pilCutToC", L.tuple ((E.inPrec (e, 1)) :: cuts), L.str ";"]
           else
             Fail.fail ("Pil", "contCutTo", "one or more args and one or more cuts not supported")
        ]

    fun noyield s =
        [L.mayAlign ([L.str "noyield {"] @
                     (List.map (s, LU.indent)) @
                     [L.str "}"])]

    val yield = [L.str "pilYield();"]

    fun continuation (i, is) =
        [L.seq [L.str "continuation ", i, L.tuple is, L.str ":"]]

    fun vse (l, s) =
        [L.mayAlign ([L.seq [L.str "VSE", L.paren l, L.str " {"]] @
                     (List.map (s, LU.indent)) @
                     [L.str "}"])]

  end

  (* Top-level declarations *)
  structure D = struct

    type t = L.t

    val blank = L.str " "

    fun comment s = L.seq [L.str "/* ", L.str s, L.str " */"]

    fun includeLocalFile f = L.seq [L.str "#include \"", L.str f, L.str ".h\""]

    fun define (i, b) =
        L.seq [L.str (if b then "#define " else "#undef "), i]

    (* NG: strictly speaking this doesn't work if e line breaks *)
    fun constantMacro (i, e) =
        L.seq [L.str "#define ", i, L.str " ", L.paren (E.layout e)]

    fun macroCall (i, args) =
        L.mayAlign [i, LU.indent (L.seq [E.callArgs args, L.str ";"])]

    fun typDef (t, tv) = L.seq [L.str "typedef ", T.dec (t, tv), L.str ";"]

    fun externVariable vd = L.seq [L.str "extern ", vd, L.str ";"]

    fun alignedStaticVariable (i, vd) = L.seq [L.str "pil_aligned(", Int.layout i, L.str ")",
                                               L.str " static ", vd, L.str ";"]

    fun alignedStaticVariableExpr (i, vd, e) =
        L.seq [L.str "pil_aligned(", Int.layout i, L.str ")",
               L.str " static ", vd, L.str " = ", E.inPrec (e, 2), L.str ";"]

    fun staticVariable vd = L.seq [L.str "static ", vd, L.str ";"]

    fun staticVariableExpr (vd, e) =
        L.seq [L.str "static ", vd, L.str " = ", E.inPrec (e, 2), L.str ";"]

    fun functionA (prefix, rt, f, args, locals, body) =
        let
          val hdr = T.dec (T.codeA (rt, args), f)
          val body = List.concat body
        in
          L.align [L.seq [prefix, hdr],
                   L.str "{",
                   LU.indent (L.align (List.map (locals, varDecInit))),
                   LU.indent (L.align body),
                   L.str "}"]
        end

    fun staticFunction (rt, f, args, locals, body) =
        functionA (L.str "static ", rt, f, args, locals, body)

    fun function (rt, f, args, locals, body) =
        functionA (L.empty, rt, f, args, locals, body)

    fun managed (config, true) =
        (case outputKind config
          of Config.OkPillar =>
             L.align [L.str "#undef to",
                      L.str "#pragma pillar_managed(on)"]
           | Config.OkC => L.empty)
      | managed (config, false) =
        (case outputKind config
          of Config.OkPillar =>
             L.align [L.str "#pragma pillar_managed(off)",
                      L.str "#define to __to__"]
           | Config.OkC => L.empty)

    fun sequence ds = L.align ds

    fun layout d = d

  end

end;

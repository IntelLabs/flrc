(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_PARSE =
sig

  structure Template :
  sig
    type t
    val layoutTemplate : Config.t * Mil.symbolInfo * t -> Layout.t
    datatype argument = AVar of Mil.variable | AName of Mil.name | AStream of MilStream.t | ACutSet of Mil.cuts
    val apply : Mil.symbolTableManager * Config.t * t * argument Vector.t -> MilStream.t
    val applyIs : Mil.symbolTableManager * Config.t * t * argument Vector.t -> Mil.instruction Vector.t
  end

  datatype templateFile = TF of {
    includes            : Mil.includeFile Vector.t,
    typs                : Mil.typ StringDict.t,
    tupleDescriptors    : Mil.tupleDescriptor StringDict.t,
    metaDataDescriptors : Mil.metaDataDescriptor StringDict.t,
    constants           : Mil.constant StringDict.t,
    fields              : Mil.fieldIdentifier StringDict.t,
    templates           : Template.t StringDict.t,
    globals             : Mil.globals,
    globalVars          : Mil.variable StringDict.t
  }

  val templateFile : Config.t * string * Mil.symbolTableManager -> templateFile

  val pass : (unit, Mil.t * string Identifier.VariableDict.t) Pass.t

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
  structure M = Mil
  structure MU = MilUtils
  structure PU = MU.Prims.Utils
  structure MF = MilFragment
  structure MS = MilStream
  structure MP = Mil.Prims
  structure MUP = MilUtils.Prims

  (*** Templates ***)

  structure Template =
  struct

    val modname = modname ^ ".Template"
    fun fail (f, m) = Fail.fail (modname, f, m)
    fun unimplemented (f, m) = Fail.unimplemented (modname, f, m)

    structure MR = MilRename.VarLabel

    datatype parameter = PVar of M.variable | PName of M.name | PStream of string | PCutSet of M.label

    datatype item =
        TiInstruction of M.instruction
      | TiStream of string
      | TiTransfer of M.transfer * M.label * M.variable Vector.t
      | TiApply of t * itemArg Vector.t
    and itemArg =
        IaVar of M.variable
      | IaName of M.name
      | IaStream of stream
      | IaCutSet of M.cuts
    and t = T of {
      name       : string,
      parameters : parameter Vector.t,
      locals     : M.variable Vector.t,
      items      : stream
    }
    withtype stream = item Vector.t

    datatype argument = AVar of M.variable | AName of M.name | AStream of MS.t | ACutSet of M.cuts

    fun remapVar (stm : M.symbolTableManager, vps : VS.t, r : Rename.t, v : M.variable) : Rename.t =
        if VS.member (vps, v) then
          r
        else
          let
            val v' = IM.variableClone (stm, v)
            val r = Rename.renameTo (r, v, v')
          in r
          end

    fun remapVars (stm : M.symbolTableManager, vps : VS.t, r : Rename.t, vs : M.variable Vector.t) : Rename.t =
        Vector.fold (vs, r, fn (v, r) => remapVar (stm, vps, r, v))

    fun remapItem (stm : M.symbolTableManager, vps : VS.t, r : MR.t, i : item) : MR.t =
        case i
         of TiInstruction i => 
            let
              val (vr, lr) = r
              val dests = MU.Instruction.dests i
              val vr = remapVars (stm, vps, vr, dests)
            in (vr, lr)
            end
          | TiStream _ => r
          | TiTransfer (t, l, vs) =>
            let
              val (vr, lr) = r
              val tvs = MU.Transfer.binds t
              val vr = remapVars (stm, vps, vr, tvs)
              val l' = IM.labelFresh stm
              val lr = LD.insert (lr, l, l')
              val vr = remapVars (stm, vps, vr, vs)
            in (vr, lr)
            end
          | TiApply (t, args) => Vector.fold (args, r, fn (a, r) => remapItemArg (stm, vps, r, a))
    and remapItemArg (stm : M.symbolTableManager, vps : VS.t, r : MR.t, ia : itemArg) : MR.t =
        case ia
         of IaVar _ => r
          | IaName _ => r
          | IaStream s => remapStream (stm, vps, r, s)
          | IaCutSet _ => r
    and remapStream (stm : M.symbolTableManager, vps : VS.t, r : MR.t, s : stream) : MR.t =
        Vector.fold (s, r, fn (i, r) => remapItem (stm, vps, r, i))

    type rewriteMaps = MR.t * M.name ND.t * MS.t SD.t * M.cuts LD.t

    fun buildMaps (stm : M.symbolTableManager, t : t, args : argument Vector.t) : rewriteMaps =
        let
          val T {name, parameters, locals, items, ...} = t
          val () =
              if Vector.length parameters <> Vector.length args then
                fail ("buildMaps", "argument number mismatch applying " ^ name)
              else
                ()
          fun matchOne (p, a, (vm, nm, sm, csm)) =
              case (p, a)
               of (PVar v1, AVar v2) => (Rename.renameTo (vm, v1, v2), nm, sm, csm)
                | (PName n1, AName n2) => (vm, ND.insert (nm, n1, n2), sm, csm)
                | (PStream s1, AStream s2) => (vm, nm, SD.insert (sm, s1, s2), csm)
                | (PCutSet l, ACutSet cs) => (vm, nm, sm, LD.insert (csm, l, cs))
                | _ => fail ("buildMaps.matchOne", "parameter/argument kind mismatch applying " ^ name)
          val (vm, nm, sm, csm) =
              Vector.fold2 (parameters, args, (Rename.none, ND.empty, SD.empty, LD.empty), matchOne)
          fun doOne (p, vps) = case p of PVar v => VS.insert (vps, v) | _ => vps
          val vps = Vector.fold (parameters, VS.empty, doOne)
          val vm = remapVars (stm, vps, vm, locals)
          val r = remapStream (stm, vps, (vm, LD.empty), items)
          val maps = (r, nm, sm, csm)
        in maps
        end

    (* What's going on with this?  Why doesn't it rewrite names in types?  -leaf *)
    structure RewriteNames =
    struct

      type t = M.name ND.t

      fun name (nm : t, n : M.name) : M.name = Utils.Option.get (ND.lookup (nm, n), n)

      fun constant (nm : t, c : M.constant) : M.constant =
          case c
           of M.CBoolean _ => c
            | M.CRat _ => c
            | M.CInteger _ => c
            | M.CName n => M.CName (name (nm, n))
            | M.CIntegral _ => c
            | M.CFloat _ => c
            | M.CDouble _ => c
            | M.CViMask _ => c
            | M.CPok _ => c
            | M.COptionSetEmpty => c
            | M.CRef _ => c
            | M.CTypePH => c

      fun simple (nm : t, s : M.simple) : M.simple =
          case s
           of M.SVariable _ => s
            | M.SConstant c => M.SConstant (constant (nm, c))

      val operand : t * M.operand -> M.operand = simple

      fun operands (nm : t, os : M.operand Vector.t) : M.operand Vector.t =
          Vector.map (os, fn opnd => operand (nm, opnd))

      fun fieldKindOperands (nm : t, fkos : (M.fieldKind * M.operand) Vector.t) : (M.fieldKind * M.operand) Vector.t =
          Vector.map (fkos, fn (fk, opnd) => (fk, operand (nm, opnd)))

      fun fieldIdentifier (nm : t, fi : M.fieldIdentifier) : M.fieldIdentifier =
          case fi
           of M.FiFixed _ => fi
            | M.FiVariable opnd => M.FiVariable (operand (nm, opnd))
            | M.FiVectorFixed {descriptor, mask, index} =>
              M.FiVectorFixed {descriptor = descriptor, mask = Option.map (mask, fn opnd => operand (nm, opnd)),
                               index = index}
            | M.FiVectorVariable {descriptor, base, mask, index, kind} =>
              M.FiVectorVariable {descriptor = descriptor, base = base,
                                  mask = Option.map (mask, fn opnd => operand (nm, opnd)), index = operand (nm, index),
                                  kind = kind}

      fun tupleField (nm : t, M.TF {tupDesc, tup, field} : M.tupleField) : M.tupleField =
          M.TF {tupDesc = tupDesc, tup = tup, field = fieldIdentifier (nm, field)}

      fun rhs (nm : t, r : M.rhs) : M.rhs =
          case r
           of M.RhsSimple s => M.RhsSimple (simple (nm, s))
            | M.RhsPrim {prim, createThunks, typs, args} =>
              M.RhsPrim {prim = prim, createThunks = createThunks, typs = typs, args = operands (nm, args)}
            | M.RhsTuple {mdDesc, inits} => M.RhsTuple {mdDesc = mdDesc, inits = operands (nm, inits)}
            | M.RhsTupleSub tf => M.RhsTupleSub (tupleField (nm, tf))
            | M.RhsTupleSet {tupField, ofVal} =>
              M.RhsTupleSet {tupField = tupleField (nm, tupField), ofVal = operand (nm, ofVal)}
            | M.RhsTupleInited _ => r
            | M.RhsIdxGet {idx, ofVal} => M.RhsIdxGet {idx = idx, ofVal = operand (nm, ofVal)}
            | M.RhsCont _ => r
            | M.RhsObjectGetKind _ => r
            | M.RhsThunkMk _ => r
            | M.RhsThunkInit {typ, thunk, fx, code, fvs} =>
              M.RhsThunkInit {typ = typ, thunk = thunk, fx = fx, code = code, fvs = fieldKindOperands (nm, fvs)}
            | M.RhsThunkGetFv _ => r
            | M.RhsThunkValue {typ, thunk, ofVal} =>
              M.RhsThunkValue {typ = typ, thunk = thunk, ofVal = operand (nm, ofVal)}
            | M.RhsThunkGetValue _ => r
            | M.RhsThunkSpawn _ => r
            | M.RhsClosureMk _ => r
            | M.RhsClosureInit {cls, code, fvs} =>
              M.RhsClosureInit {cls = cls, code = code, fvs = fieldKindOperands (nm, fvs)}
            | M.RhsClosureGetFv _ => r
            | M.RhsPSetNew opnd => M.RhsPSetNew (operand (nm, opnd))
            | M.RhsPSetGet _ => r
            | M.RhsPSetCond {bool, ofVal} => M.RhsPSetCond {bool = operand (nm, bool), ofVal = operand (nm, ofVal)}
            | M.RhsPSetQuery opnd => M.RhsPSetQuery (operand (nm, opnd))
            | M.RhsEnum {typ, tag} => M.RhsEnum {typ = typ, tag = operand (nm, tag)}
            | M.RhsSum {tag, typs, ofVals} => 
              M.RhsSum {tag = constant (nm, tag), typs = typs, ofVals = operands (nm, ofVals)}
            | M.RhsSumProj {typs, sum, tag, idx} => 
              M.RhsSumProj {typs = typs, sum = sum, tag = constant (nm, tag), idx = idx}
            | M.RhsSumGetTag _ => r

      fun instruction (nm : t, M.I {dests, n, rhs = r} : M.instruction) : M.instruction =
                                                                          M.I {dests = dests, n = n, rhs = rhs (nm, r)}

      fun target (nm : t, M.T {block, arguments} : M.target) : M.target =
          M.T {block = block , arguments = operands (nm, arguments)}

      fun switch (nm : t, {select, on, cases, default}) =
          {select = select, 
           on = operand (nm, on),
           cases = Vector.map (cases, fn (x, t) => (constant (nm, x), target (nm, t))),
           default = Option.map (default, fn t => target (nm, t))}

      fun interProc (nm : t, ip : M.interProc) : M.interProc =
          case ip
           of M.IpCall {call, args} => M.IpCall {call = call, args = operands (nm, args)}
            | M.IpEval _ => ip

      fun transfer (nm : t, t : M.transfer) : M.transfer =
        case t
         of M.TGoto t => M.TGoto (target (nm, t))
          | M.TCase sw => M.TCase (switch (nm, sw))
          | M.TInterProc {callee, ret, fx} => M.TInterProc {callee = interProc (nm, callee), ret = ret, fx = fx}
          | M.TReturn os => M.TReturn (operands (nm, os))
          | M.TCut {cont, args, cuts} => M.TCut {cont = cont, args = operands (nm, args), cuts = cuts}
          | M.THalt opnd => M.THalt (operand (nm, opnd))

    end (* RewriteNames *)

    structure RewriteCutSets =
    struct

      type t = M.cuts LD.t

      fun cuts (csm : t, M.C {exits, targets} : M.cuts) : M.cuts =
          let
            fun doOne (l, cs as M.C {exits, targets}) =
                case LD.lookup (csm, l)
                 of NONE => M.C {exits = exits, targets = LS.insert (targets, l)}
                  | SOME cs' => MU.Cuts.union (cs, cs')
          in
            LS.fold (targets, if exits then MU.Cuts.justExits else MU.Cuts.none, doOne)
          end

      fun return (csm : t, r : M.return) : M.return =
          case r
           of M.RNormal {rets, block, cuts = cs} => M.RNormal {rets = rets, block = block, cuts = cuts (csm, cs)}
            | M.RTail _ => r

      fun transfer (csm : t, t : M.transfer) : M.transfer =
          case t
           of M.TGoto _ => t
            | M.TCase _ => t
            | M.TInterProc {callee, ret, fx} => M.TInterProc {callee = callee, ret = return (csm, ret), fx = fx}
            | M.TReturn _ => t
            | M.TCut {cont, args, cuts = cs} => M.TCut {cont = cont, args = args, cuts = cuts (csm, cs)}
            | M.THalt _ => t

    end (* RewriteCutSets *)

    fun rewriteLabel (maps : rewriteMaps, l : M.label) : M.label =
        Utils.Option.get (LD.lookup (#2 (#1 maps), l), l)

    fun rewriteCutSet (maps : rewriteMaps, M.C {exits, targets} : M.cuts) : M.cuts =
        M.C {exits = exits,
             targets = LS.fold (targets, LS.empty, fn (l, ls) => LS.insert (ls, rewriteLabel (maps, l)))}

    fun rewriteTransfer (stm : M.symbolTableManager, config : Config.t, maps : rewriteMaps, t : M.transfer)
        : M.transfer =
        RewriteCutSets.transfer (#4 maps, RewriteNames.transfer (#2 maps, MR.transfer (config, #1 maps, t)))

    fun rewriteItem (stm : M.symbolTableManager, config : Config.t, maps : rewriteMaps, i : item) : MS.t =
        case i
         of TiInstruction i => MS.instruction (RewriteNames.instruction (#2 maps, MR.instruction (config, #1 maps, i)))
          | TiStream s =>
            (case SD.lookup (#3 maps, s)
              of NONE => fail ("rewriteItem", "stream " ^ s ^ " not bound")
               | SOME s => s)
          | TiTransfer (t, l, vs) =>
            let
              val t = rewriteTransfer (stm, config, maps, t)
              val l = Option.valOf (LD.lookup (#2 (#1 maps), l))
              val vs = Vector.map (vs, fn v => Rename.use (#1 (#1 maps), v))
              val s = MS.transfer (t, l, vs)
            in s
            end
          | TiApply (t, args) =>
            let
              val args = Vector.map (args, fn a => rewriteItemArg (stm, config, maps, a))
              val s = apply (stm, config, t, args)
            in s
            end

    and rewriteItemArg (stm : M.symbolTableManager, config : Config.t, maps : rewriteMaps, ia : itemArg) : argument =
        case ia
         of IaVar v => AVar (Rename.use (#1 (#1 maps), v))
          | IaName n => AName (RewriteNames.name (#2 maps, n))
          | IaStream s => AStream (rewriteStream (stm, config, maps, s))
          | IaCutSet (M.C {exits, targets}) =>
            let
              val lm = #2 (#1 maps)
              fun doOne (l, ls) = LS.insert (ls, Utils.Option.get (LD.lookup (lm, l), l))
              val cs = M.C {exits = exits, targets = LS.fold (targets, LS.empty, doOne)}
              val cs = RewriteCutSets.cuts (#4 maps, cs)
            in ACutSet cs
            end

    and rewriteStream (stm : M.symbolTableManager, config : Config.t, maps : rewriteMaps, s : stream) : MS.t =
        MS.seqnV (Vector.map (s, fn i => rewriteItem (stm, config, maps, i)))

    and applyR (stm : M.symbolTableManager, config : Config.t, t : t, args : argument Vector.t)
        : MS.t * rewriteMaps =
        let
          val T {items, ...} = t
          val maps = buildMaps (stm, t, args)
          val s = rewriteStream (stm, config, maps, items)
        in (s, maps)
        end

    and apply (stm : M.symbolTableManager, config : Config.t, t : t, args : argument Vector.t) : MS.t =
        let
          val (s, _) = applyR (stm, config, t, args)
        in s
        end

    fun applyIs (stm : M.symbolTableManager, config : Config.t, t : t, args : argument Vector.t)
        : M.instruction Vector.t =
        let
          val T {items, ...} = t
          val (r, nm, _, _) = buildMaps (stm, t, args)
          fun doOne i =
              case i
               of TiInstruction i => RewriteNames.instruction (nm, MR.instruction (config, r, i))
                | _ => fail ("applyIs.doOne", "bad template item")
          val is = Vector.map (items, doOne)
        in is
        end

    local
      structure L = Layout
      structure LU = LayoutUtils
      structure ML = MilLayout
    in
    fun layoutParameter (c, si, p) =
        case p
         of PVar v => ML.layoutVariable (c, si, v)
          | PName n => ML.layoutName (c, si, n)
          | PStream s => L.str s
          | PCutSet l => ML.layoutLabel (c, si, l)
    fun layoutTemplateItem (c, si, ti) =
        case ti
         of TiInstruction i => L.seq [L.str "i: ", ML.layoutInstruction (c, si, i)]
          | TiStream s => L.seq [L.str "s: ", L.str s]
          | TiTransfer (t, l, ps) =>
            L.seq [L.str "t: ",
                   L.mayAlign [ML.layoutTransfer (c, si, t),
                               L.seq [ML.layoutLabel (c, si, l),
                                      L.tuple (Vector.toListMap (ps, fn v => ML.layoutVariable (c, si, v)))]]]
          | TiApply (t, args) =>
            L.seq [L.str "a: ",
                   L.align [layoutTemplate (c, si, t),
                            L.tuple (Vector.toListMap (args, fn a => layoutTemplateItemArg (c, si, a)))]]
    and layoutTemplateItemArg (c, si, a) =
        case a
         of IaVar v => ML.layoutVariable (c, si, v)
          | IaName n => ML.layoutName (c, si, n)
          | IaStream s => layoutStream (c, si, s)
          | IaCutSet cs => ML.layoutCuts (c, si, cs)
    and layoutStream (c, si, s) =
        L.align [L.str "{",
                 LU.indent (L.align (Vector.toListMap (s, fn ti => layoutTemplateItem (c, si, ti)))),
                 L.str "}"]
    and layoutTemplate (c, si, T {name, parameters, locals, items}) =
        let
          val header = L.tuple (Vector.toListMap (parameters, fn p => layoutParameter (c, si, p)))
          val locals = L.seq [L.str "locals ",
                              L.sequence ("", ",", "")
                                         (Vector.toListMap (locals, fn v => ML.layoutVariable (c, si , v)))]
          val items = L.align (Vector.toListMap (items, fn ti => layoutTemplateItem (c, si, ti)))
          val body = L.align [L.str "{", LU.indent locals, LU.indent items, L.str "}"]
          val l = L.align [header, body]
        in l
        end
    end

  end

  (*** Parser Utilities ***)

  val || = P.||
  val && = P.&&
  infix 5 || &&

  val -&& = P.-&&
  val &&- = P.&&-
  infix  7 &&-
  infixr 6 -&&

  (* Convention:
   *   With an F suffix, a parser should fail if the first lexical item does not look like the beginning of thing
   *   being parsed.  If the first lexical item matches, then errors result if the subsequent parse cannot be
   *   completed.
   *   Without an F suffix, a parser should produce an error if it cannot parse something.
   *
   * The rationale between these conventions has to do with making choices between different forms and for combinators
   * like zeroOrMore.  The || operator will try the second parser if the first parser fails.  Similar the zeroOrMore
   * combinator needs to iterated parser to fail when there are no longer any items.  Thus we need parsers that
   * fail when the current input does not look like the item we are trying to parse, but that once enough is seen
   * to identify the item versus other alternatives then gives errors if the item is not completed correctly - so
   * that we get more specific errors.  For the parser so far, one token of look ahead in the toplevel parsers is
   * sufficient for this.  Some parser (e.g. typ) internally use multiple token look ahead to distinguish alternative
   * (e.g. code versus closure type), but this is not exposed at the top-level parser level.  When there are not
   * choices then we want to get an error even if the first token does not match.  Hence the different kinds of parsers
   * and the naming conventions for them.
   *)

  fun atEnd (e : string) = P.atEnd || P.error e

  fun whiteChar c = c = Char.space orelse c = Char.newline orelse c = #"\t" orelse c = #"\r"

  val whiteF = P.satisfy whiteChar

  val commentF =
      let
        fun match s = P.all (List.map (String.explode s, fn c => P.satisfy (fn c' => c = c')))
        val opn = match "(*"
        val cls = match "*)"
        fun prnChar c = let val c = Char.ord c in c >= 32 andalso c <= 126 end
        val commentChar = P.satisfy (fn c => prnChar c orelse whiteChar c)
        fun pr () =
            P.ignore cls ||
            P.ignore (opn && P.$ pr && P.$ pr) ||
            P.ignore (commentChar && P.$ pr) ||
            P.error "Unterminated comment"
        val p = P.map (opn && pr (), fn _ => #"c")
      in p
      end

  val whitespace = P.ignore (P.zeroOrMore (whiteF || commentF))

  (* Something is lexical if it matches exactly characters of the thing being parsed and not whitespace.
   * Something is syntactic if it parses surrounding whitespace as well as the thing being parsed.
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

  fun braceSeqF (pi : 'a P.t) : 'a Vector.t P.t =
      P.sequenceV
        {left = keycharSF #"{", sep = keycharSF #",", right = keycharSF #"}", err = "Expected } or ,", item = pi}

  fun angleBracketSeq (pi : 'a P.t) : 'a Vector.t P.t =
      P.sequenceV
        {left = keycharS #"<", sep = keycharSF #",", right = keycharSF #">", err = "Expected > or ,", item = pi}

  fun pair (p1 : 'a P.t, p2 : 'b P.t) : ('a * 'b) P.t =
      P.map (paren (p1 && keycharS #"," && p2), fn ((x, _), y) => (x, y))

  val identifierF : string P.t =
      syntax (P.map (P.satisfy Char.isAlpha && P.zeroOrMore (P.satisfy Char.isAlphaNum),
                     fn (c, cs) => String.implode (c::cs)))
      || P.succeed ""

  val digitsLF : string P.t =
      P.map (P.oneOrMore (P.satisfy Char.isDigit), String.implode)

  val optNegLF : bool P.t = P.map (P.optional (keycharLF #"~"), Option.isSome)

  val decimal : int P.t =
      syntax (P.map (digitsLF, fn s => Option.valOf (Int.fromString s))) || P.error "Expected nat"

  val int : int P.t =
      syntax (P.map (optNegLF && digitsLF,
                     fn (neg, s) => let val n = Option.valOf (Int.fromString s) in if neg then ~n else n end))
      || P.error "Expected int"

  val intInfLF = 
      P.map (optNegLF && digitsLF,
             fn (neg, s) => let val n = Option.valOf (IntInf.fromString s) in if neg then IntInf.~ n else n end)

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

  val float : Real32.t P.t =
      let
        val hex = P.satisfy Char.isHexDigit
        val p = hex && hex && hex && hex && hex && hex && hex && hex
        fun f (((((((c1, c2), c3), c4), c5), c6), c7), c8) =
            Utils.wordToReal32 (Option.valOf (LargeWord.fromString (String.implode [c1, c2, c3, c4, c5, c6, c7, c8])))
        val p = P.map (p, f)
        val p = syntax p || P.error "Expected float"
      in p
      end

  val double = P.$$ unimplemented ("constant", "double")

  (*** State ***)

  (* We use state even though we use a parsing monad.
   * To make this work, two things must be true of the state and the use of it:
   *   (1) The state update/lookup functions must be called during parsing and not during parser construction.
   *   (2) The state updates must not require rollback when a paser fails as we have no way to call rollback functions.
   *)

  datatype state = S of {
    stm       : M.symbolTableManager,
    gvars     : M.variable SD.t ref,
    lvars     : M.variable SD.t ref,
    labels    : M.label SD.t ref,
    typs      : M.typ SD.t ref,
    tds       : M.tupleDescriptor SD.t ref,
    mdds      : M.metaDataDescriptor SD.t ref,
    consts    : M.constant SD.t ref,
    fields    : M.fieldIdentifier SD.t ref,
    templates : Template.t SD.t ref
  }

  fun stateMk (stm : M.symbolTableManager) : state =
      S {stm = stm, gvars = ref SD.empty, lvars = ref SD.empty, labels = ref SD.empty, typs = ref SD.empty,
         tds = ref SD.empty, mdds = ref SD.empty, consts = ref SD.empty, fields = ref SD.empty,
         templates = ref SD.empty}

  fun getStm (S {stm, ...} : state) : M.symbolTableManager = stm

  fun getVariable (S {stm, gvars, lvars, ...} : state, pre : string, hint : string) : M.variable =
      let
        val full = pre ^ hint
        val vars = if String.length pre >= 2 andalso String.sub (pre, 1) = #"l" then lvars else gvars
      in
        case SD.lookup (!vars, full)
         of NONE => let val v = IM.variableFreshNoInfo (stm, hint) val () = vars := SD.insert (!vars, full, v) in v end
          | SOME v => v
      end

  fun getGlobalVars (S {gvars, ...} : state) : M.variable SD.t = !gvars

  fun getLabel (S {stm, labels, ...} : state, l : string) : M.label =
      case SD.lookup (!labels, l)
       of NONE => let val l' = IM.labelFresh stm val () = labels := SD.insert (!labels, l, l') in l' end
        | SOME l => l

  fun forkLocal (S {stm, gvars, lvars, labels, typs, tds, mdds, consts, fields, templates} : state) : state =
      S {stm = stm, gvars = gvars, lvars = ref SD.empty, labels = ref SD.empty, typs = typs, tds = tds, mdds = mdds,
         consts = consts, fields = fields, templates = templates}

  fun getNamedTyps (S {typs, ...} : state) : M.typ SD.t = !typs

  fun getNamedTyp (S {typs, ...} : state, s : string) : M.typ option = SD.lookup (!typs, s)

  fun addNamedTyp (S {typs, ...} : state, s : string, t : M.typ) : unit = typs := SD.insert (!typs, s, t)

  fun getNamedTDs (S {tds, ...} : state) : M.tupleDescriptor SD.t = !tds

  fun getNamedTD (S {tds, ...} : state, s : string) : M.tupleDescriptor option = SD.lookup (!tds, s)

  fun addNamedTD (S {tds, ...} : state, s : string, td : M.tupleDescriptor) : unit = tds := SD.insert (!tds, s, td)

  fun getNamedMDDs (S {mdds, ...} : state) : M.metaDataDescriptor SD.t = !mdds

  fun getNamedMDD (S {mdds, ...} : state, s : string) : M.metaDataDescriptor option = SD.lookup (!mdds, s)

  fun addNamedMDD (S {mdds, ...} : state, s : string, mdd : M.metaDataDescriptor) : unit =
      mdds := SD.insert (!mdds, s, mdd)

  fun getNamedConsts (S {consts, ...} : state) : M.constant SD.t = !consts

  fun getNamedConst (S {consts, ...} : state, s : string) : M.constant option = SD.lookup (!consts, s)

  fun addNamedConst (S {consts, ...} : state, s : string, c : M.constant) : unit = consts := SD.insert (!consts, s, c)

  fun getNamedFields (S {fields, ...} : state) : M.fieldIdentifier SD.t = !fields

  fun getNamedField (S {fields, ...} : state, s : string) : M.fieldIdentifier option = SD.lookup (!fields, s)

  fun addNamedField (S {fields, ...} : state, s : string, fi : M.fieldIdentifier) : unit =
      fields := SD.insert (!fields, s, fi)

  fun getTemplates (S {templates, ...} : state) : Template.t SD.t = !templates

  fun getTemplate (S {templates, ...}, s : string) : Template.t option = SD.lookup (!templates, s)

  fun addTemplate (S {templates, ...}, s : string, t : Template.t) : unit = templates := SD.insert (!templates, s, t)

  (*** Environment ***)

  datatype env = E of {config : Config.t}

  fun envMk (config : Config.t) : env = E {config = config}

  val ((_, getConfig)) =
      FunctionalUpdate.mk1 (fn (E {config}) => (config),
                            fn (c) => E {config = c})

  (*** The Parsers ***)

  val hintLF : string P.t =
      let
        fun hintChar c = Char.isAlphaNum c orelse String.contains("_\\-$#", c)
        val hintEscape = P.map (keycharLF #"^" && P.satisfy Char.isHexDigit && P.satisfy Char.isHexDigit,
                                fn ((_, c1), c2) => Char.chr (16 * Char.toHexDigit c1 + Char.toHexDigit c2))
        val hintItem = P.satisfy hintChar || hintEscape
        val p = P.map (P.zeroOrMore hintItem, String.implode)
      in p
      end

  fun variableF (state : state, env : env) : M.variable P.t =
      let
        val pre =
            keycharLF #"v" && P.optional (keycharLF #"l") && P.zeroOrMore (P.satisfy Char.isDigit) && keycharLF #"_"
        val pre =
            P.map (pre, fn (((_, l), cs), _) => "v" ^ (case l of NONE => "" | SOME _ => "l") ^ String.implode cs ^ "_")
        val p = syntax (P.map (pre && hintLF, fn (p, h) => getVariable (state, p, h)))
      in p
      end

  fun variable (state : state, env : env) : M.variable P.t = variableF (state, env) || P.error "Expected variable"

  fun nameF (state : state, env : env) : M.name P.t =
      let
        val pre = keycharLF #"n" && P.zeroOrMore (P.satisfy Char.isDigit) && keycharLF #"_"
        val p = P.map (pre && hintLF, fn (_, h) => MU.SymbolTableManager.nameMake (getStm state, h))
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

  fun abiCallConvF (state : state, env : env) : M.abiCallConv P.t =
      let
        fun doIt s =
            case s
             of "cdecl" => P.succeed M.AbiCdecl
              | "stdcall" => P.succeed M.AbiStdcall
              | _ => P.fail
        val p = P.bind identifierF doIt
      in p
      end

  fun abiCallConv (state : state, env : env) : M.abiCallConv P.t =
      abiCallConvF (state, env) || P.error "Expected ABI calling convention"

  fun callConvF (state : state, env : env, f : state * env -> 'a P.t) : 'a M.callConv P.t =
      let
        val doAux = parenSemiComma (P.$$ f (state, env), P.$$ f (state, env))
        fun doIt s =
            case s
             of "CcCode" => P.succeed M.CcCode
              | "CcUnmanaged" => P.map (abiCallConv (state, env), M.CcUnmanaged)
              | "CcClosure" => P.bind doAux (fn (x, y) => P.succeed (M.CcClosure {cls = x, fvs = y}))
              | "CcThunk" => P.bind doAux (fn (x, y) => P.succeed (M.CcThunk {thunk = x, fvs = y}))
              | _ => P.fail
        val p = P.bind identifierF doIt
      in p
      end

  fun callConv (state : state, env : env, f : state * env -> 'a P.t) : 'a M.callConv P.t =
      callConvF (state, env, f) || P.error "Expected calling convention"

  fun typKind (state : state, env : env) : M.typKind P.t =
      syntax (P.satisfyMap (MU.TypKind.fromChar)) || P.error "Expected type kind"

  fun pObjKind (state : state, env : env) : M.pObjKind P.t =
      syntax (P.satisfyMap (MU.PObjKind.fromChar)) || P.error "Expected P object kind"

  fun valueSize (state : state, env : env) : M.valueSize P.t =
      P.required (P.map (identifierF, MU.ValueSize.fromString), "Expected value size")

  fun fieldVariance (state : state, env : env) : M.fieldVariance P.t =
      syntax (P.satisfyMap (MU.FieldVariance.fromChar)) || P.error "Expected field variance"

  fun alignmentOpt (state : state, env : env) : M.valueSize P.t = 
      P.required (P.map (keycharSF #"-" -&& decimal, MU.ValueSize.fromBytes) || P.return (SOME M.Vs8), 
                  "Bad alignment")

  fun typNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s => if String.length s >= 1 andalso String.sub (s, 0) = #"t" then P.succeed s else P.fail)

  fun typName (state : state, env : env) : string P.t = typNameF (state, env) || P.error "Expected type name"

  fun vectorDescriptorF (state : state, env : env) : MP.vectorDescriptor P.t = 
      PU.Parse.vectorDescriptor (getConfig env)

  fun vectorDescriptor (state : state, env : env) : MP.vectorDescriptor P.t = 
      vectorDescriptorF (state, env) || P.error "Expected vector descriptor"
      
  fun vectorSizeF (state : state, env : env) : MP.vectorSize P.t = 
      PU.Parse.vectorSize (getConfig env)

  fun vectorSize (state : state, env : env) : MP.vectorSize P.t = 
      vectorSizeF (state, env) || P.error "Expected vector size"

  fun constantNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s => if String.length s >= 1 andalso String.sub (s, 0) = #"c" then P.succeed s else P.fail)

  fun constantName (state : state, env : env) : string P.t =
      constantNameF (state, env) || P.error "Expected constant name"

  fun constantF (state : state, env : env) : M.constant P.t =
      let
        val platSize = Config.targetWordSize' (getConfig env)
        fun intArb (sign, size) =
            P.map (paren intInf, fn i => M.CIntegral (IntArb.fromIntInf (IntArb.T (size, sign), i)))
        fun parseBool c = case c of #"0" => SOME false | #"1" => SOME true | _ => NONE
        val bools = syntax (P.zeroOrMoreV (P.satisfyMap parseBool))
        fun doIt s =
            case s
             of "Array" => P.succeed (M.CPok M.PokArray)
              | "Cell" => P.succeed (M.CPok M.PokCell)
              | "Dict" => P.succeed (M.CPok M.PokDict)
              | "Empty" => P.succeed M.COptionSetEmpty
              | "F" => P.map (paren float, M.CFloat)
              | "False" => P.succeed (M.CBoolean false)
              | "Float" => P.succeed (M.CPok M.PokFloat)
              | "Fun" => P.succeed (M.CPok M.PokFunction)
              | "D" => P.map (paren double, M.CDouble)
              | "Double" => P.succeed (M.CPok M.PokDouble)
              | "I" => P.map (paren intInf, M.CInteger)
              | "M" =>
                P.map (bracket (vectorDescriptor (state, env)) && angleBracket bools,
                       fn (vd, bs) => M.CViMask {descriptor = vd, elts = bs})
              | "MaxSIntp" => P.succeed (M.CIntegral (IntArb.maxValueT (IntArb.T (platSize, IntArb.Signed))))
              | "MaxUIntp" => P.succeed (M.CIntegral (IntArb.maxValueT (IntArb.T (platSize, IntArb.Unsigned))))
              | "MinSIntp" => P.succeed (M.CIntegral (IntArb.minValueT (IntArb.T (platSize, IntArb.Signed))))
              | "Name" => P.succeed (M.CPok M.PokName)
              | "None" => P.succeed (M.CPok M.PokNone)
              | "Ptr" => P.succeed (M.CPok M.PokPtr)
              | "R" => P.map (paren intInf, M.CRat)
              | "Rat" => P.succeed (M.CPok M.PokRat)
              | "Ref" => P.map (paren intInf, M.CRef)
              | "S8" => intArb (IntArb.Signed, IntArb.S8)
              | "S16" => intArb (IntArb.Signed, IntArb.S16)
              | "S32" => intArb (IntArb.Signed, IntArb.S32)
              | "S64" => intArb (IntArb.Signed, IntArb.S64)
              | "Set" => P.succeed (M.CPok M.PokOptionSet)
              | "SP" => intArb (IntArb.Signed, platSize)
              | "Tag" => P.succeed (M.CPok M.PokTagged)
              | "True" => P.succeed (M.CBoolean true)
              | "Type" => P.succeed (M.CPok M.PokType)
              | "TypePH" => P.succeed M.CTypePH
              | "U8" => intArb (IntArb.Unsigned, IntArb.S8)
              | "U16" => intArb (IntArb.Unsigned, IntArb.S16)
              | "U32" => intArb (IntArb.Unsigned, IntArb.S32)
              | "U64" => intArb (IntArb.Unsigned, IntArb.S64)
              | "UP" => intArb (IntArb.Unsigned, platSize)
              | _ => P.fail
        fun doNamed s =
            case getNamedConst (state, s)
             of NONE => P.error ("Constant " ^ s ^ " undefined")
              | SOME c => P.succeed c
        val named = P.bind (constantNameF (state, env)) doNamed
        val p = P.map (nameF (state, env), M.CName) || P.bind identifierF doIt || named
      in p
      end

  and constant (state : state, env : env) : M.constant P.t = constantF (state, env) || P.error "Expected constant"

  fun typ (state : state, env : env) : M.typ P.t =
      let
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
              | "Bitsp" => P.succeed (M.TBits (MU.ValueSize.ptrSize (getConfig env)))
              | "Boolean" => P.succeed M.TBoolean
              | "Cont" => P.map (parenSeq (typ (state, env)), fn ts => M.TContinuation ts)
              | "CStr" => P.succeed M.TCString
              | "Idx" => P.succeed M.TIdx
              | "Mask" => P.map (bracket (vectorDescriptor (state, env)), M.TViMask)
              | "Name" => P.succeed M.TName
              | "None" => P.succeed M.TNone
              | "NonRefPtr" => P.succeed M.TNonRefPtr
              | "PAny" => P.succeed M.TPAny
              | "PSet" => P.map (paren (typ (state, env)), fn t => M.TPType {kind = M.TkE, over = t})
              | "PRef" => P.map (paren (typ (state, env)), M.TPRef)
              | "PType" => P.map (paren (typ (state, env)), fn t => M.TPType {kind = M.TkI, over = t})
              | "Ref" => P.succeed M.TRef
              | "Sum" => 
                let
                  val arm = 
                      let
                        val k = constant (state, env)
                        val v = angleBracketSeq (P.$$ typ (state, env))
                        val p = k &&- keycharS #":" && v
                      in p
                      end
                  val fv = Utils.SortedVectorMap.fromVector MU.Constant.compare 
                  val p = brace (P.$$ typ (state, env)) && braceSeq arm
                  val f = fn (tag, arms) => M.TSum {tag = tag, arms = fv arms}
                in P.map (p, f)
                end
              | "Thunk" => P.map (paren (typ (state, env)), M.TThunk)
              | "Vec" => P.map (bracket (vectorSize (state, env)) && angleBracket (P.$$ typ (state, env)),
                                (fn (vs, t) => M.TViVector {vectorSize = vs, elementTyp = t}))
              | _ => P.fail
        val idBased = P.bind identifierF doId
        val tNumeric = syntax (P.map (PU.Parse.numericTyp (getConfig env), M.TNumeric))
        val code =
            P.map (parenSemiCommaF (callConvF (state, env, typ), P.$$ typ (state, env))
                   && keywordS "->"
                   && parenSeq (P.$$ typ (state, env)),
                   fn (((cc, args), _), ress) => M.TCode {cc = cc, args = args, ress = ress})
        val typAVar = P.map (P.$$ typ (state, env) && alignmentOpt (state, env) && fieldVariance (state, env), 
                             (fn ((a, b), c) => (a, b, c)))
        val tuple =
            P.map (semiCommaAux (keycharSF #"<", #"[", pObjKind (state, env), typAVar) &&
                   typAVar &&- keycharS #"]" &&- keycharS #">",
                   fn ((pok, tvs), tv) => M.TTuple {pok = pok, fixed = tvs, array = tv})
        val closure =
            P.map (parenSeqF (P.$$ typ (state, env))
                   && keywordS "=>"
                   && parenSeq (P.$$ typ (state, env)),
                   fn ((args, _), ress) => M.TClosure {args = args, ress = ress})
        fun doNamed s =
            case getNamedTyp (state, s) of NONE => P.error ("Type " ^ s ^ " undefined") | SOME t => P.succeed t
        val named = P.bind (typNameF (state, env)) doNamed
        val p = idBased || tNumeric || code || tuple || closure || named || P.error "Expected type"
      in p
      end

  fun binderF (state : state, env : env, k : M.variableKind) : M.variable P.t =
      let
        fun doIt ((v, _), t) =
            if MU.SymbolTableManager.variableHasInfo (getStm state,v) then
              P.error "Variable already bound"
            else
              let
                val () = MU.SymbolTableManager.variableSetInfo (getStm state, v, M.VI {typ = t, kind = k})
              in P.succeed v
              end
        val p = P.bind (variableF (state, env) && keycharS #":" && typ (state, env)) doIt
      in p
      end

  fun binder (state : state, env : env, k : M.variableKind) : M.variable P.t =
      binderF (state, env, k) || P.error "Expected variable binder"

  fun fieldKind (state : state, env : env) : M.fieldKind P.t =
      P.bind identifierF
             (fn s =>
                 case s
                  of "b8"  => P.succeed (M.FkBits M.Fs8)
                   | "b16" => P.succeed (M.FkBits M.Fs16)
                   | "b32" => P.succeed (M.FkBits M.Fs32)
                   | "b64" => P.succeed (M.FkBits M.Fs64)
                   | "bp"  => P.succeed (MU.FieldKind.nonRefPtr (getConfig env))
                   | "d"   => P.succeed M.FkDouble
                   | "f"   => P.succeed M.FkFloat
                   | "r"   => P.succeed M.FkRef
                   | _     => P.fail)
      || P.error "Expected field kind"

  fun fieldDescriptor (state : state, env : env) : M.fieldDescriptor P.t =
      let
        val alignment = alignmentOpt (state, env)
        val p = fieldKind (state, env) && alignment && fieldVariance (state, env)
        val f = fn ((k, a), v) => M.FD {kind = k, alignment = a, var = v}
      in P.map (p, f)
      end
          

  fun tupleDescriptorNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s =>
                 if String.length s >= 2 andalso String.sub (s, 0) = #"t" andalso String.sub (s, 1) = #"d"
                 then P.succeed s 
                 else P.fail)

  fun tupleDescriptorName (state : state, env : env) : string P.t =
      tupleDescriptorNameF (state, env) || P.error "Expected tuple descriptor name"

  fun tupleDescriptor (state : state, env : env) : M.tupleDescriptor P.t =
      let
        fun doNamed s =
            case getNamedTD (state, s)
             of NONE => P.error ("Tuple descriptor " ^ s ^ " undefined")
              | SOME td => P.succeed td
        val named = P.bind (tupleDescriptorNameF (state, env)) doNamed
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
        val p = P.map (keycharSF #"<" && p, #2) || named || P.error "Expected tuple descriptor"
      in p
      end

  fun metaDataDescriptorNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s =>
                 if String.length s >= 3 andalso String.sub (s, 0) = #"m" andalso String.sub (s, 1) = #"d" andalso
                    String.sub (s, 2) = #"d"
                 then P.succeed s 
                 else P.fail)

  fun metaDataDescriptorName (state : state, env : env) : string P.t =
      metaDataDescriptorNameF (state, env) || P.error "Expected metadata descriptor name"

  fun metaDataDescriptor (state : state, env : env) : M.metaDataDescriptor P.t =
      let
        fun doNamed s =
            case getNamedMDD (state, s)
             of NONE => P.error ("Metadata descriptor " ^ s ^ " undefined")
              | SOME mdd => P.succeed mdd
        val named = P.bind (metaDataDescriptorNameF (state, env)) doNamed
        val fd = fieldDescriptor (state, env)
        val array0 = keycharSF #"[" -&& fd &&- keycharS #"@" && decimal &&- keycharS #"]" &&- keycharS #">"
        val array = P.map (array0, fn (a, b) => (b, a))
        fun pr () =
            P.map (keycharSF #">", fn () => ([], NONE)) ||
            P.map (array, fn a => ([], SOME a)) ||
            P.map (keycharSF #"," -&& fd && P.$ pr, fn (fd, (fds, a)) => (fd::fds, a)) ||
            P.error "Expected , or [ or >"
        val p = P.map (keycharSF #">", fn () => (Vector.new0 (), NONE)) ||
                P.map (array, fn a => (Vector.new0 (), SOME a)) ||
                P.map (fd && P.$ pr, fn (fd, (fds, a)) => (Vector.fromList (fd::fds), a))
        val mk = fn ((pok, pO), (fixed, array)) => 
                    M.MDD {pok = pok, pinned = isSome pO, fixed = fixed, array = array}
        val p = 
            P.map (keycharSF #"<" -&& pObjKind (state, env) && P.optional (keycharSF #"!") &&- keycharS #";" && p, mk)
        val p = p || named || P.error "Expected metadata descriptor"
      in p
      end

  fun simpleF (state : state, env : env) : M.simple P.t =
      P.map (variableF (state, env), M.SVariable) ||
      P.map (constantF (state, env), M.SConstant)

  fun simple (state : state, env : env) : M.simple P.t = simpleF (state, env) || P.error "Expected simple"

  val operand : state * env -> M.operand P.t = simple

  fun fieldNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s => if String.length s >= 1 andalso String.sub (s, 0) = #"f" then P.succeed s else P.fail)

  fun fieldName (state : state, env : env) : string P.t =
      fieldNameF (state, env) || P.error "Expected field name"

  fun fieldIdentifier (state : state, env : env) : M.fieldIdentifier P.t =
      let
        val fiFixed = 
            let
              val fi = P.map (keywordLF "sf:" -&& decimal, M.FiFixed)
            in syntax fi
            end
        val fiVariable = 
            let
              val fv = P.map (keywordLF "sv:" -&& operand (state, env), M.FiVariable)
            in syntax fv
            end
        val fiVectorFixed = 
            let
              val mask = P.optional (keycharLF #"?" -&& operand (state, env))
              val p = keywordLF "vf" -&& bracket (vectorDescriptor (state, env)) &&- keycharLF #":" && decimal && mask
              val p = P.map (p, fn ((vd, i), mo) => M.FiVectorFixed {descriptor = vd, mask = mo, index = i})
            in syntax p
            end
        val fiVectorVariable = 
            let
              val mask = P.optional (keycharLF #"?" -&& operand (state, env))
              val base = P.optional (keycharLF #"^")
              val vd = bracket (vectorDescriptor (state, env))
              val vectorIndex = P.map (angleBracket (operand (state, env)), fn i => (M.VikVector, i))
              val stridedIndex = 
                  let
                    val p = angleBracket (operand (state, env) &&- keycharLF #":" &&
                                          operand (state, env) &&- keycharLF #"+" && 
                                          paren (decimal &&- keywordLF "*n"))
                  in P.map (p, fn ((idx, _), i) => (M.VikStrided i, idx))
                  end
              val index = vectorIndex || stridedIndex
              val p = keywordLF "vv" -&& vd &&- keycharLF #":" && base && index && mask
              val p = P.map (p, fn (((vd, base), (kind, idx)), mask) => 
                                   M.FiVectorVariable {descriptor = vd, 
                                                       base = case base of NONE => M.TbScalar | SOME _ => M.TbVector,
                                                       mask = mask,
                                                       index = idx,
                                                       kind = kind})
            in syntax p
            end
        fun doNamed s =
            case getNamedField (state, s)
             of NONE => P.error ("Field " ^ s ^ " undefined")
              | SOME c => P.succeed c
        val named = P.bind (fieldNameF (state, env)) doNamed
        val p =
            fiFixed || fiVariable || fiVectorFixed || fiVectorVariable || named || P.error "Expected field identifier"
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
        val primApp = 
            let
              val p = PU.Parse.t (getConfig env) && P.succeeds (bracketF (keycharLF #"T")) 
                   && P.optional (braceSeqF (typ (state, env))) && parenSeq (operand (state, env))
            in P.map (p, fn (((prim, ct), typsO), args) => M.RhsPrim {prim = prim, 
                                                                      createThunks = ct,
                                                                      typs = case typsO 
                                                                              of SOME typs => typs 
                                                                               | NONE => Vector.new0 (),
                                                                      args = args})
            end
        fun doKw s =
            case s
             of "ClosureGetFv" =>
                P.map (pair (variable (state, env) && keycharSF #":" && parenSeq (fieldKind (state, env)), decimal),
                       fn (((v, _), fks), i) => M.RhsClosureGetFv {fvs = fks, cls = v, idx = i})
              | "ClosureInit" => closureInit NONE
              | "ClosureMk" => P.map (parenSeq (fieldKind (state, env)), fn fks => M.RhsClosureMk {fvs = fks})
              | "Cont" => P.map (paren (label (state, env)), M.RhsCont)
              | "Enum" => 
                let
                  val p = paren (operand (state, env) &&- keycharSF #":" && fieldKind (state, env))
                in P.map (p, fn (tag, typ) => M.RhsEnum {tag = tag, typ = typ})
                end
              | "GetKind" => P.map (paren (variable (state, env)), M.RhsObjectGetKind)
              | "IdxGet" => P.map (pair (variable (state, env), operand (state, env)),
                                   fn (i, v) => M.RhsIdxGet {idx = i, ofVal = v})
              | "Inited" => P.map (pair (metaDataDescriptor (state, env), variable (state, env)),
                                   fn (mdd, tup) => M.RhsTupleInited {mdDesc = mdd, tup = tup})
              | "PSet" => P.map (paren (operand (state, env)), M.RhsPSetNew)
              | "PSetCond" => P.map (paren (operand (state, env) && keycharSF #"?" &&
                                           brace (operand (state, env)) && keycharSF #":" &&
                                           brace (P.succeed ())),
                                    fn ((((o1, _), o2), _), _) => M.RhsPSetCond {bool = o1, ofVal = o2})
              | "PSetGet" => P.map (paren (variable (state, env)), M.RhsPSetGet)
              | "Spawn" =>
                P.map (paren (variable (state, env) && keycharSF #":" && fieldKind (state, env)) &&
                       effects (state, env),
                       fn (((v, _), fk), fx) => M.RhsThunkSpawn {typ = fk, thunk = v, fx = fx})
              | "SumProj" => 
                let
                  val v = variable (state, env)
                  val k = constant (state, env)
                  val i = decimal
                  val fks = angleBracketSeq (fieldKind (state, env))
                  val p = paren (v &&- keycharSF #"." && k &&- keycharSF #"." && i &&- keycharSF #":" && fks)
                  val f = fn (((v, k), i), fks) => M.RhsSumProj {typs = fks, sum = v, tag = k, idx = i}
                in P.map (p, f)
                end
              | "SumGetTag" => 
                let
                  val v = variable (state, env)
                  val fk = fieldKind (state, env)
                  val p = brace fk && paren v
                  val f = fn (typ, sum) => M.RhsSumGetTag {typ = typ, sum = sum}
                in P.map (p, f)
                end
              | "Tagged" => 
                let
                  val k = constant (state, env)
                  val vs = angleBracketSeq (operand (state, env))
                  val fks = angleBracketSeq (fieldKind (state, env))
                  val p = pair (k, vs &&- keycharSF #":" && fks)
                  val f = fn (n, (opnds, fks)) => M.RhsSum {tag = n, typs = fks, ofVals = opnds}
                in P.map (p, f)
                end
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
              | _ => P.fail
        val kw = P.bind identifierF doKw
        val setQuery = P.map (keycharSF #"?" && operand (state, env), fn (_, opnd) => M.RhsPSetQuery opnd)
        val p = varStuff || tup || const || kw || setQuery || primApp || P.error "Expected right-hand side"
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
  fun switch (state : state, env : env) : M.transfer P.t =
      let
        val seSum = 
            let
              val p = operand (state, env) &&- keycharS #":" && fieldKind (state, env)
              val p = keywordSF "tagof" -&& paren p
            in P.map (p, fn (on, fk) => (M.SeSum fk, on))
            end
        val seConstant = P.map (operand (state, env), fn on => (M.SeConstant, on))
        val header = seSum || seConstant
        val case1 = constantF (state, env) &&- keywordS "=>" && target (state, env)
        val cases = P.zeroOrMoreV case1
        val default = keywordSF "Default" -&& keywordS "=>" -&& target (state, env)
        val p = header && brace (cases && P.optional default)
        val f = fn ((select, on), (cs, d)) => M.TCase {select = select, on = on, cases = cs, default = d}
      in P.map (p, f)
      end

  fun codesD (state : state, env : env, d : M.codes) : M.codes P.t =
      let
        val exhaustive = P.map (keywordSF "<=", fn _ => true) || P.map (keycharSF #"?", fn _ => false)
        val vs = P.map (braceSeq (variable (state, env)), VS.fromVector)
        val p = P.map (exhaustive && vs, fn (e, vs) => {possible = vs, exhaustive = e}) || P.succeed d
      in p
      end

  fun callA (state : state, env : env, s : string) : M.call P.t =
      case s
       of "Call" =>
          let
            fun doIt v =
                let
                  val d =
                      (case MU.SymbolTableManager.variableKind (getStm state, v)
                        of M.VkExtern => MU.Codes.all
                         | _ => {possible = VS.singleton v, exhaustive = true})
                      handle _ => {possible = VS.singleton v, exhaustive = true}
                  val p = P.map (codesD (state, env, d), fn cs => M.CCode {ptr = v, code = cs})
                in p
                end
            val p = P.bind (paren (variable (state, env))) doIt
          in p
          end
        | "CallClos" =>
          P.map (paren (variable (state, env)) && codesD (state, env, MU.Codes.all),
                 fn (v, cs) => M.CClosure {cls = v, code = cs})
        | "CallDir" =>
          P.map (pair (variable (state, env), variable (state, env)),
                 fn (v1, v2) => M.CDirectClosure {cls = v1, code = v2})
        | _ => P.error "Expected call"

  fun call (state : state, env : env) : M.call P.t = P.bind identifierF (fn s => callA (state, env, s))

  fun evalA (state : state, env : env, s : string) : M.eval P.t =
      case s
       of "Eval" =>
          P.map (paren (variable (state, env)) && codesD (state, env, MU.Codes.all),
                 fn (v, cs) => M.EThunk {thunk = v, code = cs})
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
      in p
      end

  fun cutsOpt (state : state, env : env) : M.cuts P.t = cuts (state, env) || P.succeed MU.Cuts.none

  fun return (state : state, env : env) : M.return P.t =
      let
        val normal =
            P.map (keywordSF "->" &&
                   parenSeq (binder (state, env, M.VkLocal)) &&
                   label (state, env) &&
                   cutsOpt (state, env),
                   fn (((_, vs), l), cs) => M.RNormal {rets = vs, block = l, cuts = cs})
        val tail =
            P.map (keywordSF "-|" && P.succeeds (keywordSF "/->/" && keywordS "Exit"),
                   fn (_, e) => M.RTail {exits = e})
        val p = normal || tail || P.error "Expected return"
      in p
      end

  fun transferF (state : state, env : env) : M.transfer P.t =
      let
        fun interProc s =
            P.map (interProcA (state, env, s) && return (state, env) && effects (state, env),
                   fn ((ip, r), fx) => M.TInterProc {callee = ip, ret = r, fx = fx})
        fun doIt s =
            case s
             of "Call" => interProc s
              | "CallClos" => interProc s
              | "CallDir" => interProc s
              | "Case" => switch (state, env)
              | "Cut" => P.map (variable (state, env) && parenSeq (operand (state, env)) && cutsOpt (state, env),
                                fn ((v, os), cs) => M.TCut {cont = v, args = os, cuts = cs})
              | "Eval" => interProc s
              | "EvalDir" => interProc s
              | "Goto" => P.map (target (state, env), M.TGoto)
              | "Halt" => P.map (paren (operand (state, env)), M.THalt)
              | "Return" => P.map (parenSeq (operand (state, env)), M.TReturn)
              | _ => P.fail
        val p = P.bind identifierF doIt
      in p
      end

  fun transfer (state : state, env : env) : M.transfer P.t =
      transferF (state, env) || P.error "Expected transfer"

  fun blockHeaderF (state : state, env : env) : (M.label * M.variable Vector.t) P.t =
      labelF (state, env) && parenSeq (binder (state, env, M.VkLocal))

  fun blockHeader (state : state, env : env) : (M.label * M.variable Vector.t) P.t =
      blockHeaderF (state, env) || P.error "Expected block header"

  fun blockF (state : state, env : env) : (M.label * M.block) P.t =
      let
        val p = blockHeaderF (state, env) && P.zeroOrMoreV (instructionF (state, env)) && transfer (state, env)
        val p = P.map (p, fn (((l, ps), is), t) => (l, M.B {parameters = ps, instructions = is, transfer = t}))
      in p
      end

  fun block (state : state, env : env) : (M.label * M.block) P.t = blockF (state, env) || P.error "Expected block"

  fun templateNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s => if String.length s >= 1 andalso String.sub (s, 0) = #"T" then P.succeed s else P.fail)

  fun templateName (state : state, env : env) : string P.t =
      templateNameF (state, env) || P.error "Expected template name"

  fun streamNameF (state : state, env : env) : string P.t =
      P.bind identifierF
             (fn s => if String.length s >= 1 andalso String.sub (s, 0) = #"s" then P.succeed s else P.fail)

  fun streamName (state : state, env : env) : string P.t =
      streamNameF (state, env) || P.error "Expected stream name"

  fun templateItemF (state : state, env : env) : Template.item P.t =
      let
        val instruction = P.map (instructionF (state, env), Template.TiInstruction)
        val namedStream = P.map (streamNameF (state, env), Template.TiStream)
        val header = blockHeader (state, env)
        fun doIt (t, (l, ps)) = Template.TiTransfer (t, l, ps)
        val transfer = P.map (transferF (state, env) && blockHeaderF (state, env), doIt)
        fun mkApply (t, args) =
            case getTemplate (state, t)
             of NONE => P.error ("Template " ^ t ^ " undefined")
              | SOME t => P.succeed (Template.TiApply (t, args))
        val apply = P.bind (templateNameF (state, env) && parenSeq (P.$$ templateArgumentF (state, env))) mkApply
        val p = instruction || namedStream || transfer || apply
      in p
      end

  and templateArgumentF (state : state, env : env) : Template.itemArg P.t =
      P.map (variableF (state, env), Template.IaVar) ||
      P.map (nameF (state, env), Template.IaName) ||
      P.map (P.$$ streamF (state, env), Template.IaStream) ||
      P.map (cuts (state, env), Template.IaCutSet)

  and streamF (state : state, env : env) : Template.stream P.t =
      let
        val p = keycharSF #"{" && P.zeroOrMoreV (P.$$ templateItemF (state, env)) && keycharS #"}"
        val p = P.map (p, fn ((_, is), _) => is)
      in p
      end

  fun localsF (state : state, env : env) : M.variable Vector.t P.t =
      let
        val p = keywordSF "Local" && P.seqSepV (binder (state, env, M.VkLocal), keycharSF #",") && keycharS #";"
        fun build x =
            case x
             of NONE => Vector.new0 ()
              | SOME ((_, ls), _) => ls
        val p = P.map (P.optional p, build)
      in p
      end

  fun codeBody (state : state, env : env) : M.codeBody P.t =
      let
        val entry = P.map (keywordS "Entry" && label (state, env), #2)
        val blks = blockHeader (state, env) && P.zeroOrMoreV (templateItemF (state, env)) && transfer (state, env)
        fun build ((e, ls), (((l, vs), is), t)) =
            let
              val stm = getStm state
              val c = getConfig env
              val temp = Template.T {name = "", parameters = Vector.new0 (), locals = ls, items = is}
              val (s, maps) = Template.applyR (stm, c, temp, Vector.new0 ())
              val t = Template.rewriteTransfer (stm, c, maps, t)
              val blks = MF.toBlocksD (MS.finish (l, vs, s, t))
              val cb = M.CB {entry = e, blocks = blks}
            in cb
            end
        val p = P.map (entry && localsF (state, env) && blks, build)
      in p
      end

  fun codeA (state : state, env : env) : M.code P.t =
      let
        val state = forkLocal state
        val binder' = fn (s, e) => binder (s, e, M.VkLocal)
        val args = parenSemiComma (callConv (state, env, binder'), binder (state, env, M.VkLocal))
        val header = P.map (optFlag #"^" && optFlag #"*" && args && effects (state, env),
                            fn (((e, r), (cc, vs)), fx) => (e, r, cc, vs, fx))
        val rets = P.map (keycharS #":" && parenSeq (typ (state, env)), #2)
        val body = brace (codeBody (state, env))
        fun mkIt (((e, r, cc, vs, fx), ts), cb) =
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
                let
                  val k = constant (state, env)
                  val vs = angleBracketSeq (operand (state, env))
                  val fks = angleBracketSeq (fieldKind (state, env))
                  val p = pair (k, vs &&- keycharSF #":" && fks)
                  val f = fn (n, (opnds, fks)) => M.GSum {tag = n, typs = fks, ofVals = opnds}
                in P.map (p, f)
                end
              | "ThunkValue" => P.map (paren (simple (state, env) && keycharS #":" && fieldKind (state, env)),
                                       fn ((s, _), fk) => M.GThunkValue {typ = fk, ofVal = s})
              | _ => P.fail
        val tup = P.map (tupleF (state, env), fn (mdd, os) => M.GTuple {mdDesc = mdd, inits = os})
        val p = P.bind identifierF doIt || tup || P.map (simpleF (state, env), M.GSimple) || P.error "Expected global"
      in p
      end

  fun varGlobalF (state : state, env : env) : (M.variable * M.global) P.t =
      P.map (binderF (state, env, M.VkGlobal) && keycharS #"=" && global (state, env), fn ((v, _), g) => (v, g))

  fun varGlobal (state : state, env : env) : (M.variable * M.global) P.t =
      varGlobalF (state, env) || P.error "Expected global binding"

  fun globals (state : state, env : env) : M.global VD.t P.t =
      P.map (P.zeroOrMore (varGlobalF (state, env)), VD.fromList)

  fun includeKindF (state : state, env : env) : M.includeKind P.t =
      P.bind (P.map (identifierF, MU.IncludeKind.fromString)) 
             (fn ko => case ko of SOME k => P.succeed k | NONE => P.fail)

  fun includeKind (state : state, env : env) : M.includeKind P.t =
      includeKindF (state, env) || P.error"Expected include kind"

  fun includeFileF (state : state, env : env) : M.includeFile P.t =
      P.map (cstringF && keycharS #":" && includeKind (state, env) && braceSeq (binder (state, env, M.VkExtern)),
             fn (((n, _), k), vs) => M.IF {name = n, kind = k, externs = VS.fromVector vs})

  fun includeFile (state : state, env : env) : M.includeFile P.t =
      includeFileF (state, env) || P.error "Expected include file"

  fun externGroupF (state : state, env : env) : M.externGroup P.t =
      P.map (includeKindF (state, env) && braceSeq (binder (state, env, M.VkExtern)),
             fn (k, vs) => M.EG {kind = k, externs = VS.fromVector vs})

  fun externGroup (state : state, env : env) : M.externGroup P.t =
      externGroupF (state, env) || P.error "Expected extern group"

  fun program (config : Config.t) : M.t P.t =
      let
        val stm = IM.new Prims.ordString
        val state = stateMk stm
        val env = envMk config
        val includes = P.map (keywordS "Includes:" && P.zeroOrMoreV (includeFileF (state, env)), #2)
        val externs = P.map (keywordS "Externs:" && P.zeroOrMoreV (externGroupF (state, env)), #2)
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

  fun templateParameterF (state : state, env : env) : Template.parameter P.t =
      P.map (variableF (state, env), Template.PVar) ||
      P.map (nameF (state, env), Template.PName) ||
      P.map (streamNameF (state, env), Template.PStream) ||
      P.map (labelF (state, env), Template.PCutSet)

  fun templateBody (state : state, env : env) : (M.variable Vector.t * Template.stream) P.t =
      let
        val p =
            keycharS #"{" && localsF (state, env) && P.zeroOrMoreV (P.$$ templateItemF (state, env)) && keycharS #"}"
        val p = P.map (p, fn (((_, ls), is), _) => (ls, is))
      in p
      end

  fun template (state : state, env : env) : (string * Template.t) P.t =
      let
        val state = forkLocal state
        val params = parenSeq (templateParameterF (state, env))
        val p = params && templateBody (state, env)
        fun doIt n (ps, (ls, is)) = (n, Template.T {name = n, parameters = ps, locals = ls, items = is})
        val p = P.bind (templateName (state, env)) (fn n => P.map (p, doIt n))
      in p
      end

  datatype declaration =
      DInclude of M.includeFile
    | DType of string * M.typ
    | DTupleDescriptor of string * M.tupleDescriptor
    | DMetaDataDescriptor of string * M.metaDataDescriptor
    | DConstant of string * M.constant
    | DFields of string Vector.t
    | DGlobal of M.variable * M.global
    | DTemplate of string * Template.t

  fun declarationF (state : state, env : env) : declaration P.t =
      let
        fun doConstant ((n, _), c) =
            let
              val () = addNamedConst (state, n, c)
            in DConstant (n, c)
            end
        fun doFields fs =
            let
              fun doOne (i, f) = addNamedField (state, f, M.FiFixed i)
              val () = Vector.foreachi (fs, doOne)
            in DFields fs
            end
        fun doMDD ((n, _), mdd) =
            let
              val () = addNamedMDD (state, n, mdd)
              val n' = "td" ^ String.substring (n, 3, String.length n - 3)
              val td = addNamedTD (state, n', MU.MetaDataDescriptor.toTupleDescriptor mdd)
            in DMetaDataDescriptor (n, mdd)
            end
        fun doTemplate (n, t) =
            let
              val () = addTemplate (state, n, t)
            in DTemplate (n, t)
            end
        fun doTD ((n, _), td) =
            let
              val () = addNamedTD (state, n, td)
            in DTupleDescriptor (n, td)
            end
        fun doTyp ((n, _), t) =
            let
              val () = addNamedTyp (state, n, t)
            in DType (n, t)
            end
        fun doIt s =
            case s
             of "Constant" => P.map (constantName (state, env) && keycharS #"=" && constant (state, env), doConstant)
              | "Fields" => P.map (angleBracketSeq (fieldName (state, env)), doFields)
              | "Global" => P.map (varGlobal (state, env), fn (v, d) => DGlobal (v, d))
              | "Include" => P.map (includeFile (state, env), DInclude)
              | "Metadata" =>
                P.map (metaDataDescriptorName (state, env) && keycharS #"=" && metaDataDescriptor (state, env), doMDD)
              | "Template" => P.map (template (state, env), doTemplate)
              | "TupleDescriptor" =>
                P.map (tupleDescriptorName (state, env) && keycharS #"=" && tupleDescriptor (state, env), doTD)
              | "Type" => P.map (typName (state, env) && keycharS #"=" && typ (state, env), doTyp)
              | _ => P.fail
        val p = P.bind identifierF doIt
      in p
      end

  datatype templateFile = TF of {
    includes            : M.includeFile Vector.t,
    typs                : M.typ SD.t,
    tupleDescriptors    : M.tupleDescriptor SD.t,
    metaDataDescriptors : M.metaDataDescriptor SD.t,
    constants           : M.constant SD.t,
    fields              : M.fieldIdentifier SD.t,
    templates           : Template.t SD.t,
    globals             : M.globals,
    globalVars          : M.variable SD.t
  }

  fun templateFile (config : Config.t, fname : string, stm : M.symbolTableManager) : templateFile =
      let
        val strm = Pervasive.TextIO.openIn fname
        val instrm = Pervasive.TextIO.getInstream strm
        val instrm = InStreamWithPos.mk instrm
        val state = stateMk stm
        val env = envMk config
        val parser =
            P.map (whitespace && P.zeroOrMore (declarationF (state, env)) && atEnd "Expected end of file",
                   fn ((_, ds), _) => ds)
        val ds =
            case P.parse (parser, instrm)
             of P.Success (_, ds) => ds
              | P.Failure => fail ("parseFile", "Parse failed")
              | P.Error ({line, col}, e) =>
                fail ("parseFile", "Parse error: line " ^ Int.toString line ^ " col " ^ Int.toString col ^ ": " ^ e)
        fun doOne (d, (is, gs)) =
            case d
             of DInclude i => (i::is, gs)
              | DGlobal (v, g) => (is, VD.insert (gs, v, g))
              | _ => (is, gs)
        val (is, gs) = List.fold (ds, ([], VD.empty), doOne)
        val typs = getNamedTyps state
        val tds = getNamedTDs state
        val mdds = getNamedMDDs state
        val cs = getNamedConsts state
        val fis = getNamedFields state
        val ts = getTemplates state
        val gvs = getGlobalVars state
        val tf = TF {includes = Vector.fromList (List.rev is), typs = typs, tupleDescriptors = tds,
                     metaDataDescriptors = mdds, constants = cs, fields = fis, templates = ts, globals = gs,
                     globalVars = gvs}
      in tf
      end

  (*** Pass Stuff ***)

  fun parseFile (config : Config.t, f : string) : M.t =
      let
        val strm = Pervasive.TextIO.openIn f
        val instrm = Pervasive.TextIO.getInstream strm
        val instrm = InStreamWithPos.mk instrm
        val parser = P.map (program config && atEnd "Expected end of file", #1)
        val p =
            case P.parse (parser, instrm)
             of P.Success (_, p) => p
              | P.Failure => fail ("parseFile", "Parse failed")
              | P.Error ({line, col}, e) =>
                fail ("parseFile", "Parse error: line " ^ Int.toString line ^ " col " ^ Int.toString col ^ ": " ^ e)
      in p
      end
                     
  fun readFile (() : unit, pd : PassData.t, basename : Path.t) : M.t * string VD.t =
      let
        val config = PassData.getConfig pd
        val basename = Config.pathToHostString (config, basename)
        val infile = basename ^ ".mil"
        fun cleanup () = ()
        val p = Exn.finally (fn () => parseFile (config, infile), cleanup)
      in (p, VD.empty)
      end

  fun wrapIR { printer, stater } = { printer = fn ((x, _), c) => printer (x, c)
                                   , stater  = fn ((x, _), c) => stater  (x, c) }

  val description = {name        = modname,
                     description = "Mil parser",
                     inIr        = Pass.unitHelpers,
                     outIr       = wrapIR MilUtils2.irHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkFilePass (description, associates, readFile)

end;

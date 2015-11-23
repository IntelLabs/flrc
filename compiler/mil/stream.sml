(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* This file contains two abstractions for building up code bodies for Mil code.
 * Both are intended to contain parts of a code body.
 * Fragments are just collections of complete blocks, complete in the sense of having a label, parameters,
 * instructions, and transfer.
 * Streams are collections of complete blocks plus a partial entry block and a partial exit block.  A partial entry
 * block is one without a label and paramters.  A partial exit block is one without a transfer.  Note that the entry
 * and exit blocks could be the same, and thus consist of just instructions.
 * Another way to view streams is that it is a collection of code that can be fallen into and that can fall out.
 * Two streams can be sequenced, the fall out of the first will fall into the second.  Similarly, code can be added
 * before the fall in, or after the fall out.
 * A fragment then is a collection code that has no fall in or fall out.
 * For convenience, we also supply sources and sinks that have just a fall out or fall in respectively; alternatively
 * that have a partial exit block or partial entry block respectively.
 *)

signature MIL_FRAGMENT =
sig

  type t

  val empty      : t
  val fromBlock  : Mil.label * Mil.block -> t
  val fromBlocks : (Mil.label * Mil.block) list -> t

  val merge      : t * t -> t
  val mergen     : t list -> t

  val toBlocksD  : t -> Mil.block Identifier.LabelDict.t
  val toBlocksL  : t -> (Mil.label * Mil.block) list
  val toBlocksV  : t -> (Mil.label * Mil.block) Vector.t

end;

structure MilFragment :> MIL_FRAGMENT =
struct

  structure AL = AppendList
  structure LD = Identifier.LabelDict
  structure M = Mil

  datatype t = F of (M.label * M.block) AL.t

  val empty : t = F AL.empty

  fun fromBlock (l : M.label, b : M.block) : t = F (AL.single (l, b))

  fun fromBlocks (lbs : (M.label * M.block) list) : t = F (AL.fromList lbs)

  fun merge (F f1 : t, F f2 : t) : t = F (AL.append (f1, f2))

  fun mergen (fs : t list) : t = F (AL.appends (List.map (fs, fn F f => f)))

  fun toBlocksD (F f : t) : M.block LD.t =
      AL.fold (f, LD.empty, fn ((l, b), blks) => LD.insert (blks, l, b))

  fun toBlocksL (F f : t) : (M.label * M.block) list = AL.toList f

  fun toBlocksV (F f : t) : (M.label * M.block) Vector.t = AL.toVector f

end;

signature MIL_STREAM =
sig

  type t
  type source = Mil.label * Mil.variable Vector.t * t
  type sink = t * Mil.transfer

  val empty        : t
  val instruction  : Mil.instruction -> t
  val instructions : Mil.instruction list -> t
  val bindsRhs     : Mil.variable Vector.t * Mil.rhs -> t
  val bindRhs      : Mil.variable * Mil.rhs -> t
  val doRhs        : Mil.rhs -> t
  val transfer     : Mil.transfer * Mil.label * Mil.variable Vector.t -> t

  val prependI  : Mil.instruction * t -> t
  val prependIs : Mil.instruction list * t -> t
  val prependTL : Mil.transfer * Mil.label * Mil.variable Vector.t * t -> t
  val appendI   : t * Mil.instruction -> t
  val appendIs  : t * Mil.instruction list -> t
  val appendTL  : t * Mil.transfer * Mil.label * Mil.variable Vector.t -> t
  val merge     : t * MilFragment.t -> t
  val mergen    : t * MilFragment.t list -> t

  val seq  : t * t -> t
  val seqn : t list -> t
  val seqnV : t Vector.t -> t

  val finish : Mil.label * Mil.variable Vector.t * t * Mil.transfer -> MilFragment.t

end;

structure MilStream :> MIL_STREAM =
struct

  structure AL = AppendList
  structure LD = Identifier.LabelDict
  structure M = Mil
  structure F = MilFragment

  datatype partials =
      PBB of M.instruction AL.t
    | PExt of {entry : M.instruction AL.t * M.transfer, exit : M.label * M.variable Vector.t * M.instruction AL.t}

  datatype t = S of {partials : partials, complete : F.t}

  type source = Mil.label * Mil.variable Vector.t * t
  type sink = t * Mil.transfer

  val empty : t = S {partials = PBB AL.empty, complete = F.empty}

  fun mkBlk (l : M.label, vs : M.variable Vector.t, is : M.instruction AL.t, t : M.transfer) : F.t =
      F.fromBlock (l, M.B {parameters = vs, instructions = AL.toVector is, transfer = t})

  fun seq (S {partials = p1, complete = c1} : t, S {partials = p2, complete = c2} : t) : t =
    let
      val (p, c') =
          case (p1, p2)
           of (PBB is1, PBB is2) => (PBB (AL.append (is1, is2)), F.empty)
            | (PBB is1, PExt {entry = (is2, t2), exit}) =>
              (PExt {entry = (AL.append (is1, is2), t2), exit = exit}, F.empty)
            | (PExt {entry, exit = (l, vs, is1)}, PBB is2) =>
              (PExt {entry = entry, exit = (l, vs, AL.append (is1, is2))}, F.empty)
            | (PExt {entry = e1, exit = (l, vs, is1)}, PExt {entry = (is2, t), exit = e2}) =>
              (PExt {entry = e1, exit = e2}, mkBlk (l, vs, AL.append (is1, is2), t))
      val c = F.mergen [c1, c', c2]
      val s = S {partials = p, complete = c}
    in s
    end

  fun seqn (ss : t list) : t = List.fold (ss, empty, seq o Utils.flip2)

  fun seqnV (ss : t Vector.t) : t = Vector.fold (ss, empty, seq o Utils.flip2)

  fun merge (S {partials, complete} : t, f : F.t) : t =
      S {partials = partials, complete = F.merge (complete, f)}

  fun mergen (S {partials, complete} : t, fs : F.t list) : t =
      S {partials = partials, complete = F.mergen (complete::fs)}

  fun instruction (i : M.instruction) : t = S {partials = PBB (AL.single i), complete = F.empty}

  fun instructions (is : M.instruction list) : t = S {partials = PBB (AL.fromList is), complete = F.empty}

  fun bindsRhs (vs : M.variable Vector.t, r : M.rhs) : t = instruction (M.I {dests = vs, n = 0, rhs = r})

  fun bindRhs (v : M.variable, r : M.rhs) : t = bindsRhs (Vector.new1 v, r)

  fun doRhs (r : M.rhs) : t = instruction (M.I {dests = Vector.new0 (), n = 0, rhs = r})

  fun transfer (t : M.transfer, l : M.label, vs : M.variable Vector.t) : t =
      S {partials = PExt {entry = (AL.empty, t), exit = (l, vs, AL.empty)}, complete = F.empty}

  fun prependI (i : Mil.instruction, s : t) : t = seq (instruction i, s)
  fun prependIs (is : Mil.instruction list, s : t) : t = seq (instructions is, s)
  fun prependTL (t : Mil.transfer, l : Mil.label, vs : Mil.variable Vector.t, s : t) = seq (transfer (t, l, vs), s)
  fun appendI (s : t, i : Mil.instruction) : t = seq (s, instruction i)
  fun appendIs (s : t, is : Mil.instruction list) : t = seq (s, instructions is)
  fun appendTL (s : t, t : Mil.transfer, l : Mil.label, vs : Mil.variable Vector.t) : t = seq (s, transfer (t, l, vs))

  fun finish (l : M.label, vs : M.variable Vector.t, S {partials, complete} : t, t : M.transfer) : F.t =
      case partials
       of PBB is => F.merge (mkBlk (l, vs, is, t), complete)
        | PExt {entry = (is1, t1), exit = (l2, vs2, is2)} =>
          F.mergen [mkBlk (l, vs, is1, t1), complete, mkBlk (l2, vs2, is2, t)]

end;

signature MIL_STREAM_UTILS =
sig

  type state
  type env

  type exp = MilStream.t * Mil.operand Vector.t

  val join : state * env * (Mil.label * Mil.label -> MilStream.sink) * exp * exp * Mil.variable Vector.t -> MilStream.t

  val joinn : state * env * (Mil.label Vector.t -> MilStream.sink) * exp Vector.t * Mil.variable Vector.t
              -> MilStream.t

  val goto : state * env * Mil.label * Mil.operand Vector.t -> MilStream.t

  val ifBool : state * env * Mil.operand * exp * exp * Mil.variable Vector.t -> MilStream.t

  val ifConst : state * env * Mil.operand * Mil.constant * exp * exp * Mil.variable Vector.t -> MilStream.t

  val ifConsts : state * env * Mil.operand * (Mil.constant * exp) Vector.t * exp option * Mil.variable Vector.t
                 -> MilStream.t

  val ifZero : state * env * Mil.operand * exp * exp * Mil.variable Vector.t -> MilStream.t

  val whenTrue : state * env * Mil.operand * MilStream.t -> MilStream.t

  val whenFalse : state * env * Mil.operand * MilStream.t -> MilStream.t

  val eval : state * env * Mil.fieldKind * Mil.eval * Mil.cuts * Mil.effects * Mil.variable -> MilStream.t

  val call : state * env * Mil.call * Mil.operand Vector.t * Mil.cuts * Mil.effects * Mil.variable Vector.t
             -> MilStream.t

  val tailcall : state * env * Mil.call * Mil.operand Vector.t * bool * Mil.effects -> MilStream.t

  val return : state * env * Mil.operand Vector.t -> MilStream.t

  val cut : state * env * Mil.variable * Mil.operand Vector.t * Mil.cuts -> MilStream.t

  val loop :
      state * env * (Mil.label * Mil.label -> MilStream.sink) * Mil.variable Vector.t *
      (Mil.label * Mil.label -> MilStream.sink) * Mil.variable Vector.t
      -> MilStream.t

  type carriedVars = {
    inits    : Mil.operand Vector.t,
    bodyVars : Mil.variable Vector.t,
    next     : Mil.operand Vector.t,
    outVars  : Mil.variable Vector.t
  }

  val noCarried : carriedVars

  val whileDo : state * env * carriedVars * (MilStream.t * Mil.operand) * MilStream.t -> MilStream.t

  val doWhile : state * env * carriedVars * (Mil.operand Vector.t -> MilStream.t * Mil.operand) * MilStream.t
                -> MilStream.t

  val uintpLoop : state * env * carriedVars * Mil.operand * Mil.operand * Mil.operand * (Mil.variable -> MilStream.t)
                  -> MilStream.t

  val withCont : state * env * (Mil.label -> exp) * Mil.variable Vector.t * exp * Mil.variable Vector.t
                 -> MilStream.t

end;

functor MilStreamUtilsF(type state
                        type env
                        val getStm : state -> Mil.symbolTableManager
                        val getConfig : env -> Config.t)
  :> MIL_STREAM_UTILS where type state = state and type env = env =
struct

  type state = state
  type env = env

  val modname = "MilStreamUtils"

  structure M = Mil
  structure MU = MilUtils
  structure MSTM = MU.SymbolTableManager
  structure MS = MilStream

  type exp = MS.t * M.operand Vector.t

  fun variableFresh (state : state, hint : string, t : M.typ) : M.variable =
      MSTM.variableFresh (getStm state, "m" ^ hint ^ "_#", t, M.VkLocal)

  fun variableClone (state : state, v : M.variable) : M.variable =
      MSTM.variableClone (getStm state, v)

  fun variableRelated (state : state, v : M.variable, hint : string, t : M.typ) : M.variable =
      MSTM.variableRelated (getStm state, v, hint, t, M.VkLocal)

  fun labelFresh (state : state) : M.label = MSTM.labelFresh (getStm state)

  fun join (state : state, env : env, f : M.label * M.label -> MS.sink, e1 : exp, e2 : exp, vs : M.variable Vector.t)
      : MS.t =
      let
        val l1 = labelFresh state
        val l2 = labelFresh state
        val lexit = labelFresh state
        val (s1, t1) = f (l1, l2)
        val (s2, os2) = e1
        val t2 = M.TGoto (M.T {block = lexit, arguments = os2})
        val (s3, os3) = e2
        val t3 = M.TGoto (M.T {block = lexit, arguments = os3})
        val s = MS.seqn [s1, MS.transfer (t1, l1, Vector.new0 ()), s2, MS.transfer (t2, l2, Vector.new0 ()), s3,
                         MS.transfer (t3, lexit, vs)]
      in s
      end

  fun joinn (state : state, env : env, f : M.label Vector.t -> MS.sink, exps : exp Vector.t, vs : M.variable Vector.t)
      : MS.t =
      let
        val n = Vector.length exps
        val ls = Vector.map (exps, fn _ => labelFresh state)
        val lexit = labelFresh state
        val (s1, t1) = f ls
        val (l1, vs1) = if n > 0 then (Vector.sub (ls, 0), Vector.new0 ()) else (lexit, vs)
        val s1 = MS.appendTL (s1, t1, l1, vs1)
        fun doOne (i, (s, os)) =
            let
              val (l, vs) = if i + 1 < n then (Vector.sub (ls, i + 1), Vector.new0 ()) else (lexit, vs)
              val s = MS.appendTL (s, M.TGoto (M.T {block = lexit, arguments = os}), l, vs)
            in s
            end
        val ss = Vector.mapi (exps, doOne)
        val s = MS.seq (s1, MS.seqnV ss)
      in s
      end

  fun goto (state : state, env : env, l : M.label, args : M.operand Vector.t) : MS.t =
      MS.transfer (M.TGoto (M.T {block = l, arguments = args}), labelFresh state, Vector.new0 ())

  fun ifBool (state : state, env : env, on : M.operand, t : exp, f : exp, vs : M.variable Vector.t) : MS.t =
      let
        val config = getConfig env
        val tc = MU.Bool.T config
        val fc = MU.Bool.F config
        fun genSwitch (lt, lf) = 
            (MS.empty, M.TCase {select = M.SeConstant, on = on, 
                                cases = Vector.new2 ((tc, MU.Target.mkNoArgs lt), 
                                                     (fc, MU.Target.mkNoArgs lf)), 
                                default = NONE})
        val s = join (state, env, genSwitch, t, f, vs)
      in s
      end

  fun ifConst (state : state, env : env, on : M.operand, c : M.constant, t : exp, f : exp, vs : M.variable Vector.t)
      : MS.t =
      let
        fun genSwitch (lt, lf) = 
            (MS.empty, M.TCase {select = M.SeConstant, on = on, 
                                cases = Vector.new1 (c, MU.Target.mkNoArgs lt), 
                                default = SOME (MU.Target.mkNoArgs lf)})
        val s = join (state, env, genSwitch, t, f, vs)
      in s
      end

  fun ifConsts (state   : state,
                env     : env,
                on      : M.operand,
                cexps   : (M.constant * exp) Vector.t,
                default : exp option,
                vs      : M.variable Vector.t)
      : MS.t =
      let
        val n = Vector.length cexps
        val (cs, exps) = Vector.unzip cexps
        fun genSwitch ls =
            let
              fun genCase (i, c) = (c, M.T {block = Vector.sub (ls, i), arguments = Vector.new0 ()})
              val cases = Vector.mapi (cs, genCase)
              fun genDefault _ = M.T {block = Vector.sub (ls, n), arguments = Vector.new0 ()}
              val default = Option.map (default, genDefault)
              val t = M.TCase {select = M.SeConstant, on = on, cases = cases, default = default}
            in (MS.empty, t)
            end
        val exps =
            case default
             of NONE => exps
              | SOME e => Vector.concat [exps, Vector.new1 e]
        val s = joinn (state, env, genSwitch, exps, vs)
      in s
      end

  fun ifZero (state : state, env : env, on : M.operand, t : exp, f : exp, vs : M.variable Vector.t) : MS.t =
      let
        val config = getConfig env
        val zero = MU.Uintp.zero config
        val s = ifConst (state, env, on, zero, t, f, vs)
      in s
      end

  fun whenTrue (state : state, env : env, on : M.operand, s : MS.t) : MS.t =
      ifBool (state, env, on, (s, Vector.new0 ()), (MS.empty, Vector.new0 ()), Vector.new0 ())

  fun whenFalse (state : state, env : env, on : M.operand, s : MS.t) : MS.t =
      ifBool (state, env, on, (MS.empty, Vector.new0 ()), (s, Vector.new0 ()), Vector.new0 ())

  fun eval (state : state,
            env   : env,
            typ   : M.fieldKind,
            e     : M.eval,
            cuts  : M.cuts,
            fx    : M.effects,
            res   : M.variable )
      : MS.t =
      let
        val lret = labelFresh state
        val e = M.IpEval {eval = e, typ = typ}
        val r = M.RNormal {rets = Vector.new1 res, block = lret, cuts = cuts}
        val t = M.TInterProc {callee = e, ret = r, fx = fx}
        val s = MS.transfer (t, lret, Vector.new0 ())
      in s
      end

  fun call (state : state,
            env   : env,
            c     : M.call,
            args  : M.operand Vector.t,
            cuts  : M.cuts,
            fx    : M.effects,
            ress  : M.variable Vector.t)
      : MS.t =
      let
        val lret = labelFresh state
        val c = M.IpCall {call = c, args = args}
        val r = M.RNormal {rets = ress, block = lret, cuts = cuts}
        val t = M.TInterProc {callee = c, ret = r, fx = fx}
        val s = MS.transfer (t, lret, Vector.new0 ())
      in s
      end

  fun tailcall (state : state,
                env   : env,
                c     : M.call,
                args  : M.operand Vector.t,
                exits : bool,
                fx    : M.effects)
      : MS.t =
      let
        val ldummy = labelFresh state
        val c = M.IpCall {call = c, args = args}
        val r = M.RTail {exits = exits}
        val t = M.TInterProc {callee = c, ret = r, fx = fx}
        val s = MS.transfer (t, ldummy, Vector.new0 ())
      in s
      end

  fun return (state : state, env : env, ress : M.operand Vector.t) : MS.t =
      let
        val ldummy = labelFresh state
        val t = M.TReturn ress
        val s = MS.transfer (t, ldummy, Vector.new0 ())
      in s
      end

  fun cut (state : state, env : env, c : M.variable, args : M.operand Vector.t, cuts : M.cuts) : MS.t =
      let
        val ldummy = labelFresh state
        val t = M.TCut {cont = c, args = args, cuts = cuts}
        val s = MS.transfer (t, ldummy, Vector.new0 ())
      in s
      end

  fun halt (state : state, env : env, arg : M.operand) : MS.t =
      let
        val ldummy = labelFresh state
        val t = M.THalt arg
        val s = MS.transfer (t, ldummy, Vector.new0 ())
      in s
      end

  fun loop (state    : state,
            env      : env,
            genEnter : M.label * M.label -> MS.sink,
            bodyVars : M.variable Vector.t,
            genBody  : M.label * M.label -> MS.sink,
            outVars  : M.variable Vector.t)
      : MS.t =
      let
        val lbody = labelFresh state
        val lexit = labelFresh state
        val (s1, t1) = genEnter (lbody, lexit)
        val (s2, t2) = genBody (lbody, lexit)
        val s = MS.seqn [s1, MS.transfer (t1, lbody, bodyVars), s2, MS.transfer (t2, lexit, outVars)]
      in s
      end

  type carriedVars = {
    inits    : M.operand Vector.t,
    bodyVars : M.variable Vector.t,
    next     : M.operand Vector.t,
    outVars  : M.variable Vector.t
  }

  val noCarried : carriedVars =
      {inits = Vector.new0 (), bodyVars = Vector.new0 (), next = Vector.new0 (), outVars = Vector.new0 ()}

  fun whileDo (state : state,
               env   : env,
               cvs   : carriedVars,
               test  : MS.t * M.operand,
               body  : MS.t)
      : MS.t =
      let
        val config = getConfig env
        val ct = MU.Bool.T config
        val cf = MU.Bool.F config
        val {inits, bodyVars, next, outVars} = cvs
        fun genEntry (lbody, lexit) = (MS.empty, M.TGoto (M.T {block = lbody, arguments = inits}))
        fun genBody (lbody, lexit) =
            let
              val lcont = labelFresh state
              val case1 = (ct, M.T {block = lcont, arguments = Vector.new0 ()})
              val case2 = (cf, M.T {block = lexit, arguments = Vector.map (bodyVars, M.SVariable)})
              val t1 = M.TCase {select = M.SeConstant, on = #2 test, cases = Vector.new2 (case1, case2), default = NONE}
              val s = MS.seqn [#1 test, MS.transfer (t1, lcont, Vector.new0 ()), body]
              val t2 = M.TGoto (M.T {block = lbody, arguments = next})
            in (s, t2)
            end
        val s = loop (state, env, genEntry, bodyVars, genBody, outVars)
      in s
      end

  fun doWhile (state   : state,
               env     : env,
               cvs     : carriedVars,
               genTest : M.operand Vector.t -> MS.t * M.operand,
               body    : MS.t)
      : MS.t =
      let
        val config = getConfig env
        val ct = MU.Bool.T config
        val cf = MU.Bool.T config
        val {inits, bodyVars, next, outVars} = cvs
        fun genEntry (lbody, lexit) =
            let
              val (s, b) = genTest inits
              val cases = ((ct, M.T {block = lbody, arguments = inits}), (cf, M.T {block = lexit, arguments = inits}))
              val t = M.TCase {select = M.SeConstant, on = b, cases = Vector.new2 cases, default = NONE}
            in (s, t)
            end
        fun genBody (lbody, lexit) =
            let
              val (s', b) = genTest next
              val cases = ((ct, M.T {block = lbody, arguments = next}), (cf, M.T {block = lexit, arguments = next}))
              val t = M.TCase {select = M.SeConstant, on = b, cases = Vector.new2 cases, default = NONE}
              val s = MS.seq (body, s')
            in (s, t)
            end
        val s = loop (state, env, genEntry, bodyVars, genBody, outVars)
      in s
      end

  fun uintpLoop (state   : state,
                 env     : env,
                 cvs     : carriedVars,
                 init    : M.operand,
                 limit   : M.operand,
                 step    : M.operand,
                 genBody : M.variable -> MS.t)
      : MS.t =
      let
        val config = getConfig env
        val index = variableFresh (state, "idx", MU.Uintp.t config)
        val index' = variableClone (state, index)
        val nexti = variableRelated (state, index, "next", MU.Uintp.t config)
        val {inits, bodyVars, next, outVars} = cvs
        val vcons = Utils.Vector.cons
        val cvs = {inits = vcons (init, inits), bodyVars = vcons (index, bodyVars),
                   next = vcons (M.SVariable nexti, next), outVars = vcons (index', outVars)}
        fun genTest os =
            let
              val nd = variableFresh (state, "ild", MU.Bool.t config)
              val s = MS.bindRhs (nd, MU.Uintp.lt (config, Vector.sub (os, 0), limit))
            in (s, M.SVariable nd)
            end
        val body = MS.seq (genBody index, MS.bindRhs (nexti, MU.Uintp.add (config, M.SVariable index, step)))
        val s = doWhile (state, env, cvs, genTest, body)
      in s
      end

  fun withCont (state : state,
                env   : env,
                e1    : M.label -> exp,
                args  : M.variable Vector.t,
                e2    : exp,
                vs    : M.variable Vector.t)
      : MS.t =
      let
        val lcont = labelFresh state
        val lexit = labelFresh state
        val (s1, os1) = e1 lcont
        val t1 = M.TGoto (M.T {block = lexit, arguments = os1})
        val (s2, os2) = e2
        val t2 = M.TGoto (M.T {block = lexit, arguments = os2})
        val s = MS.seqn [s1, MS.transfer (t1, lcont, args), s2, MS.transfer (t2, lexit, vs)]
      in s
      end

end;

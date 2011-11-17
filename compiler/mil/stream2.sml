(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_STREAM2 =
sig

  type state
  type env

  structure Fragment :
  sig

    (* A fragment is a collection of label & blocks pairs. *)
    type t

    val merge     : state * env * t * t -> t
    val mergen    : state * env * t list -> t
    val block     : state * env * Mil.label * Mil.block -> t
    val blocks    : state * env * t -> (Mil.label * Mil.block) List.t
    val codeBody  : state * env * t -> Mil.block Identifier.LabelDict.t

  end

  structure Stream :
  sig

    (* A stream has at most one designated entry point to which instructions
     * may be prepended, and at most one designated exit point to which 
     * instructions may be appended.  If there is no designated entry/exit
     * point, any prepended/appended instructions are dead.  Other side exits
     * and entries are expressible through labels, but are not tracked by the
     * stream abstraction.
     *)
    type t

    val new       : state * env -> t

    val seq       : state * env * t * t -> t
    val seqn      : state * env * t list -> t

    val append    : state * env * t * Mil.instruction -> t
    val appendl   : state * env * t * Mil.instruction list -> t
    val prepend   : state * env * Mil.instruction * t -> t
    val prependl  : state * env * Mil.instruction list * t -> t

    val instr     : state * env * Mil.instruction -> t
    val instrs    : state * env * Mil.instruction list -> t
    val instrMk   : state * env * Mil.variable vector * Mil.rhs -> t
    val bindRhs   : state * env * Mil.variable * Mil.rhs -> t
    val doRhs     : state * env * Mil.rhs -> t

    (* Label the entry, returning the label of the entry block.  The block
     * entry is closed.
     * 
     * label (state, vs, S) == (l1, S') 
     * where S' ==
     * l1 (vs):
     *    S;
     *)
    val label     : state 
                    * env
                    * (Mil.variable Vector.t)
                    * t
                    -> Mil.label * t
    val labelWith : state 
                    * env
                    * Mil.label 
                    * (Mil.variable Vector.t) 
                    * t
                    -> t

    (* Stream entry is left open
     * enterWith (state, S, parms, F) = (l1, S')
     * where S' = 
     *
     *    F (l1);
     * l1 (parms):
     *    S;
     * 
     *)
    val enterWith  : state 
                     * env
                     * t 
                     * Mil.variable Vector.t  (* Destinations *)
                     * (state * env * Mil.label -> Mil.transfer) 
                     -> Mil.label * t

    (* Stream exit is closed.  Any subsequently appended 
     * instructions are dead
     *)
    val terminate : state * env * t * Mil.transfer -> t

    (* Stream exit is closed. *)
    val transfer : state * env * Mil.transfer -> t

    (* Stream exit is left open. 
     * 
     * exitWith (state, S, parms, F) = (S', l1) 
     * where S' = 
     *    S;
     *    F (l1);
     * l1 (parms):
     * 
     *)
    val exitWith  : state 
                    * env
                    * t 
                    * Mil.variable Vector.t  (* Destinations *)
                    * (state * env * Mil.label -> Mil.transfer) 
                    -> t * Mil.label

    (* Given a loop body and functions enterF and exitF,
     * generate labels for the entry and exit points, 
     * and use enterF and exitF to generate the appropriate
     * transfers.
     * loop (state, enterF, inargs, S, exitF, outargs) = (entry, exit, S')
     * where S' =
     * 
     *   enterF (entry, exit)
     * entry (inargs):
     *   S
     *   exitF (entry, exit)
     * exit (outargs):
     *
     *)
    val loop : state
               * env
               * (state * env * Mil.label * Mil.label -> Mil.transfer)
               * Mil.variable Vector.t
               * t
               * (state * env * Mil.label * Mil.label -> Mil.transfer)
               * Mil.variable Vector.t
               -> Mil.label * Mil.label * t

     (*  Given two streams, merge them to a common exit point
     * (if they exit), and emit the two blocks.  The merge function 
     * is called  with the two labels to produce an entry transfer point.
     *  join (state, vs, (S1, rs1), (S2, rs2), F) ==
     *
     *       F (l1, l2):
     *    l1: 
     *       S1
     *       goto exit (rs1)
     *    l2:
     *       S2
     *       goto exit (rs2)
     *    exit (vs):
     *)
    val join : state
               * env
               * Mil.variable Vector.t
               * (t * (Mil.operand Vector.t))
               * (t * (Mil.operand Vector.t))
               * (state * env * Mil.label * Mil.label -> Mil.transfer)
               -> t * Mil.label * Mil.label

    (* Just like join, but for a list.  *)
    val joinn : state
               * env
               * Mil.variable Vector.t
               * (t * (Mil.operand Vector.t)) list
               * (state * env * Mil.label list -> Mil.transfer)
               -> t * Mil.label list
           
   (* Stream should already be terminated.
    * close (state, S, vs) = (l, f)
    * where f = 
    * l (vs):
    *   S;
    *)
    val close : state * env * t * Mil.variable Vector.t
                -> Mil.label * Fragment.t

   (* Stream will be terminated. 
    * closeWith (state, S, vs, t) = (l, f)
    * where f = 
    * l (vs):
    *   S;
    *   t;
    *)
    val closeWith : state 
                    * env
                    * t 
                    * Mil.variable Vector.t 
                    * Mil.transfer
                    -> Mil.label * Fragment.t

  end

  structure Utils :
  sig

    (* Stream is terminated *)
    val goto : state * env * Mil.label * Mil.operand Vector.t -> Stream.t

    val ifTrue : state
                 * env
                 * Mil.variable Vector.t
                 * Mil.operand
                 * (Stream.t * (Mil.operand Vector.t))  (* then / true  *)
                 * (Stream.t * (Mil.operand Vector.t))  (* else / false *)
                 -> Stream.t

    val ifFalse : state
                  * env
                  * Mil.variable Vector.t
                  * Mil.operand
                  * (Stream.t * (Mil.operand Vector.t)) (* then / false *)
                  * (Stream.t * (Mil.operand Vector.t)) (* else / true  *)
                  -> Stream.t

    val ifConst : state
                  * env
                  * Mil.variable Vector.t
                  * Mil.operand
                  * Mil.constant
                  * (Stream.t * (Mil.operand Vector.t))
                  * (Stream.t * (Mil.operand Vector.t))
                  -> Stream.t

    (* operand should be a signed platform sized machine integer *)
    val ifZero : state 
                 * env
                 * Mil.variable Vector.t
                 * Mil.operand
                 * (Stream.t * (Mil.operand Vector.t))
                 * (Stream.t * (Mil.operand Vector.t))
                 -> Stream.t

    (* operand should be a signed platform sized machine integer *)
    val ifNonZero : state 
                    * env
                    * Mil.variable Vector.t
                    * Mil.operand
                    * (Stream.t * (Mil.operand Vector.t))
                    * (Stream.t * (Mil.operand Vector.t))
                    -> Stream.t

    val whenTrue : state * env * Mil.operand * Stream.t -> Stream.t

    val whenFalse : state * env * Mil.operand * Stream.t -> Stream.t

    (* Stream remains open ended *)
    val call : state
               * env
               * Mil.variable Vector.t (* dests *)
               * Mil.call
               * Mil.operand Vector.t
               * Mil.cuts
               * Mil.effects
               -> Stream.t

    (* Stream is terminated *)
    val tailcall : state
                   * env
                   * Mil.call
                   * Mil.operand Vector.t
                   * bool (* exits *)
                   * Mil.effects
                   -> Stream.t

    (* Stream remains open ended *)
    val evalThunk : state 
                    * env
                    * Mil.variable (* dest *)
                    * Mil.fieldKind
                    * Mil.eval
                    * Mil.cuts
                    * Mil.effects
                    -> Stream.t

    (* Stream is terminated *)
    val return : state * env * Mil.operand Vector.t -> Stream.t

    (* Stream is terminated *)
    val cut : state * env * Mil.variable * Mil.operand Vector.t * Mil.cuts
              -> Stream.t
                     
    (* Loop over an unsigned integer.
     * intLoop (state, start, i, limit, incr, S) = S'
     * where i is an unsigned platformsized machine integer and S' = 
     * 
     *    if (limit == start) 
     *      goto exit ()
     *    else 
     *      goto loop (start)
     *  loop (i):
     *    S
     *    ni = i + incr 
     *    if ni < limit
     *       goto loop (ni)
     *    else
     *       goto exit ()
     *  exit ():
     *)

    val uintpLoop : state
                    * env
                    * Mil.operand
                    * Mil.variable
                    * Mil.operand
                    * Mil.operand
                    * Stream.t
                    -> Stream.t

    (* Loop over an unsigned integer with loop carried arguments.
     * intLoop (state, start, i, limit, incr, inits, inargs, S, ops, outargs) 
     *   = S'
     * where is an unsigned platformsized machine integer and S' = 
     * 
     *    if (limit == start) 
     *      goto exit (start, inits)
     *    else 
     *      goto loop (start, inits)
     *  loop (i, vs):
     *    S
     *    ni = i + incr 
     *    if ni < limit
     *       goto loop (ni, ops)
     *    else
     *       goto exit (ni, ops)
     *  exit (i', outargs):
     *)
    val uintpLoopCV : state
                     * env
                     * Mil.operand
                     * Mil.variable
                     * Mil.operand
                     * Mil.operand 
                     * Mil.operand Vector.t
                     * Mil.variable Vector.t
                     * Stream.t
                     * Mil.operand Vector.t
                     * Mil.variable Vector.t
                     -> Stream.t

  end

end;

functor MilStreamF (
  type state
  type env
  val toConfig   : env -> Config.t
  val getStm     : state -> Mil.symbolTableManager
  val indent : int
) :> MIL_STREAM2 where type state = state and type env = env =
struct

   structure AL = AppendList 
   structure LD = Identifier.LabelDict
   structure M = Mil
   structure MU = MilUtils

   structure Chat = ChatF (struct
                             type env = env
                             val extract = toConfig
                             val name = "MilStream"
                             val indent = indent
                           end)

   type state = state
   type env = env

   fun nextBlock state = MU.SymbolTableManager.labelFresh (getStm state)

   fun namedVar (state, hint, t, g) =
       MU.SymbolTableManager.variableFresh (getStm state, hint ^ "_#", t, g)

   fun relatedVar (state, v, hint, t, g) =
       MU.SymbolTableManager.variableRelated (getStm state, v, hint, t, g)

   structure Fragment =
   struct

     datatype t = F of (M.label * M.block) AL.t

     val empty = F AL.empty

     fun mergen (state, env, fs) = F (AL.appends (List.map (fs, fn F f => f)))

     fun merge (state, env, F f1, F f2) = F (AL.append (f1, f2))

     fun block (state, env, l, b) = F (AL.single (l, b))

     fun blocks (state, env, F f) = AL.toList f

     fun codeBody (state, env, F f) =
         AL.fold (f, LD.empty, fn ((l, b), blks) => LD.insert (blks, l, b))

     fun emitMilBlockI (F f, l, b) = F (AL.cons ((l, b), f))

     fun emitPhiBlockI (state, env, f, l, ps, is, t) =
         let
           val b = M.B {parameters = ps,
                        instructions = AL.toVector is,
                        transfer = t}
           val f = emitMilBlockI (f, l, b)
         in f
         end
       
     fun emitBlockI (state, env, f, l, is, t) =
         emitPhiBlockI (state, env, f, l, Vector.new0 (), is, t)

     fun emitPhiBlock (state, env, f, ps, is, t) =
         let
           val l = nextBlock state
           val f = emitPhiBlockI (state, env, f, l, ps, is, t)
         in (l, f)
         end

     fun emitBlock (state, env, f, is, t) =
         emitPhiBlock (state, env, f, Vector.new0 (), is, t)

   end

   structure Stream =
   struct

     structure F = Fragment

     datatype entry = 
              ELabeled of M.label
            | EBlock of (M.instruction AL.t * M.transfer) 

     datatype exit = 
              EClosed
            | EOpen of (M.label * M.variable Vector.t * M.instruction AL.t)
        
     datatype t = BB of M.instruction AL.t
                | EXT of {head : entry, tail : exit, frag : F.t}

     fun getFrag (state, env, s) = 
       (case s 
         of BB _ => F.empty
          | EXT {frag, ...} => frag)

     fun getTail (state, env, s) = 
         (case s 
           of BB l => EOpen (nextBlock state, Vector.new0 (), l)
            | EXT {tail, ...} => tail)

     fun new (state, env) = BB AL.empty

     fun seq (state, env, s1, s2) =
         case (s1, s2)
          of (EXT {head, tail = EClosed, frag = frag1}, 
              EXT {head = ELabeled l, tail, frag = frag2}) =>
             EXT {head = head, 
                  tail = tail, 
                  frag = F.merge (state, env, frag1, frag2)}
           | (EXT {head, tail = EClosed, frag = frag1}, s2) =>
             let
               val () = Chat.warn0 (env,
                                    "seq: Dropping dead code, tail is closed")
               val f2 = getFrag (state, env, s2)
               val f = F.merge (state, env, frag1, f2)
               val s = 
                   EXT {head = head, tail = getTail (state, env, s2), frag = f}
             in s
             end
           | (s1, EXT {head = ELabeled l, tail, frag = frag2}) =>
             let
               val () = Chat.warn0 (env,
                                    "seq: Dropping dead code, head is closed")
               val head =
                   case s1
                    of EXT {head, ...} => head
                     | _ => ELabeled l
               val f1 = getFrag (state, env, s1)
               val f = F.merge (state, env, f1, frag2)
               val s = EXT {head = head, tail = tail, frag = f}
             in s
             end
           | (BB l1, BB l2)  => BB (AL.append (l1, l2))
           | (BB l1, EXT {head = EBlock (l2, t), tail, frag}) =>
             let
               val head = EBlock (AL.append (l1, l2), t)
               val s = EXT {head = head, tail = tail, frag = frag}
             in s
             end
           | (EXT {head, tail = EOpen (b1, vs, l1), frag}, BB l2) => 
             let
               val tail = EOpen (b1, vs, AL.append (l1, l2))
               val s = EXT {head = head, tail = tail, frag = frag}
             in s
             end
           | (EXT {head = head1, tail = EOpen (b1, vs1, l1), frag = frag1},
              EXT {head = EBlock (l2, t2), tail = tail2, frag = frag2}) => 
             let
               val frag = F.merge (state, env, frag1, frag2)
               val l = AL.append (l1, l2)
               val frag = F.emitPhiBlockI (state, env, frag, b1, vs1, l, t2)
               val s = EXT {head = head1, tail = tail2, frag = frag}
             in s
             end

     fun seqn (state, env, ss) =
         case ss
          of [] => new (state, env)
           | (s::ss) => 
             List.fold (ss, s, fn (s, sofar) => seq (state, env, sofar, s))

     fun appendl (state, env, s, i) =
         let
           val il = AL.fromList i
           val res = 
               case s
                of BB l => BB (AL.append (l, il))
                 | EXT {head, tail = EOpen (bid, args, l), frag} => 
                   EXT {head = head, 
                        tail = EOpen (bid, args, AL.append (l, il)),
                        frag = frag}
                 | EXT {head, tail = EClosed, frag} => 
                   let
                     val () = Chat.warn2 (env, "append: dropping dead code")
                   in
                     s
                   end
         in res
         end


     fun append (state, env, s, i) = appendl (state, env, s, [i])
       
     fun prependl (state, env, i, s) = 
         let
           val il = AL.fromList i
           val res = 
               case s
                of BB l => BB (AL.append (il, l))
                 | EXT {head = EBlock (l, tfer), tail, frag} => 
                   EXT {head = EBlock (AL.append (il, l), tfer), 
                        tail = tail, 
                        frag = frag}
                 | EXT {head = ELabeled l, tail, frag} => 
                   let
                     val () = Chat.warn2 (env, "prepend: dropping dead code")
                   in s
                   end
         in res
         end
             
     fun prepend (state, env, i, s) = prependl (state, env, [i], s)

     fun instr (state, env, i) = append (state, env, new (state, env), i)

     fun instrs (state, env, is) = appendl (state, env, new (state, env), is)

     fun instrMk (state, env, dests, rhs) =
         instr (state, env, M.I {dests = dests, n = 0, rhs = rhs})

     fun bindRhs (state, env, x, rhs) = instrMk (state, env, Vector.new1 x, rhs)

     fun doRhs (state, env, rhs) = instrMk (state, env, Vector.new0 (), rhs)

     fun labelWith' (state, env, s, l, vs) =
         case s
          of BB is => 
             let
               val tail = EOpen (l, vs, is)
               val f = F.empty
             in (tail, f)
             end
           | EXT {head, tail, frag} => 
             let
               val (is, t) = 
                   case head
                    of ELabeled l => 
                       (AL.empty, M.TGoto (MU.Target.mkArgs (l, vs)))
                     | EBlock b => b
               val frag = F.emitPhiBlockI (state, env, frag, l, vs, is, t)
             in (tail, frag)
             end

     fun label' (state, env, s, vs) =
         case s
          of BB is => 
             let
               val l = nextBlock state
               val tail = EOpen (l, vs, is)
               val f = F.empty
             in (l, tail, f)
             end
           | EXT {head = ELabeled l, tail, frag} => (l, tail, frag)
           | EXT {head = EBlock (is, t), tail, frag} => 
             let
               val l = nextBlock state
               val frag = F.emitPhiBlockI (state, env, frag, l, vs, is, t)
             in (l, tail, frag)
             end

     fun label (state, env, vs, s) =
         let
           val (l, tail, frag) = label' (state, env, s, vs)
           val head = ELabeled l
           val s = EXT {head = head, tail = tail, frag = frag}
         in (l, s)
         end

     fun labelWith (state, env, l, vs, s) =
         let
           val (tail, frag) = labelWith' (state, env, s, l, vs)
           val head = ELabeled l
           val s = EXT {head = head, tail = tail, frag = frag}
         in s
         end

     fun enterWith (state, env, s, vs, f) =
         let
           val (l, tail, frag) = label' (state, env, s, vs)
           val t = f (state, env, l)
           val head = EBlock (AL.empty, t)
           val s = EXT {head = head, tail = tail, frag = frag}
         in (l, s)
         end

     fun terminate' (state, env, s, t) =
         case s
          of BB is => (EBlock (is, t), F.empty)
           | EXT {head, tail = EClosed, frag} => (head, frag)
           | EXT {head, tail = EOpen (bid, args, instrs), frag} => 
             let
               val f = F.emitPhiBlockI (state, env, frag, bid, args, instrs, t)
               val ef = (head, f)
             in ef
             end

     fun terminate (state, env, s, t) =
         let
           val (head, frag) = terminate' (state, env, s, t)
           val tail = EClosed
           val s = EXT {head = head, tail = tail, frag = frag}
         in s
         end

     fun transfer (state, env, t) = terminate (state, env, new (state, env), t)

     fun exitWith (state, env, s, vs, f) =
         let
           val bid = nextBlock state
           val t = f (state, env, bid)
           val (head, frag) = terminate' (state, env, s, t)
           val tail = EOpen (bid, vs, AL.empty)
           val s = EXT {head = head, tail = tail, frag = frag}
         in (s, bid)
         end

     fun loop (state, env, enterF, inargs, body, exitF, outargs) =
         let
           val entry = nextBlock state
           val exit  = nextBlock state
           val (tail, frag) = labelWith' (state, env, body, entry, inargs)
           val entryT = enterF (state, env, entry, exit)
           val exitT  = exitF (state, env, entry, exit)
           val head = EBlock (AL.empty, entryT)
           val s = EXT {head = head, tail = tail, frag = frag}
           val (head, frag) = terminate' (state, env, s, exitT)
           val tail = EOpen (exit, outargs, AL.empty)
           val s = EXT {head = head, tail = tail, frag = frag}
         in (entry, exit, s)
         end

     fun close (state, env, s, vs) =
         let
           (* s is closed by assumption, so ignore tail *)
           val (l, tail, frag) = label' (state, env, s, vs)
         in (l, frag)
         end

     fun closeWith (state, env, s, vs, t) =
         let
           val s = terminate (state, env, s, t)
           val (l, f) = close (state, env, s, vs)
         in (l, f)
         end

     fun joinn (state, env, vs, prs, mergefn) =
         let
           val mergeId = nextBlock state
           val (ps, rs) = List.unzip prs
           val gotos = List.map (rs, fn arg => M.TGoto (M.T {block = mergeId, arguments = arg}))
           val (is, fs) = List.unzip (List.map (List.zip (ps, gotos), 
                          fn (p, goto) => closeWith (state, env, p, Vector.new0 (), goto)))
           val frag = F.mergen (state, env, fs)
           val t = mergefn (state, env, is)
           val head = EBlock (AL.empty, t)
           val tail = EOpen (mergeId, vs, AL.empty)
           val s = EXT {head = head, tail = tail, frag = frag}
       in (s, is)
       end

     fun join (state, env, vs, (p1, rs1), (p2, rs2), mergefn) =
         let
           val mergeId = nextBlock state
           val goto1 = M.TGoto (M.T {block = mergeId, arguments = rs1})
           val goto2 = M.TGoto (M.T {block = mergeId, arguments = rs2})
           val (i1, f1) = closeWith (state, env, p1, Vector.new0 (), goto1)
           val (i2, f2) = closeWith (state, env, p2, Vector.new0 (), goto2)
           val frag = F.merge (state, env, f1, f2)
           val t = mergefn (state, env, i1, i2)
           val head = EBlock (AL.empty, t)
           val tail = EOpen (mergeId, vs, AL.empty)
           val s = EXT {head = head, tail = tail, frag = frag}
       in (s, i1, i2)
       end

   end

   structure Utils =
   struct

     open Stream

     fun goto (state, env, l, os) =
         terminate (state, env, new (state, env),
                    M.TGoto (M.T {block = l, arguments = os}))

     fun binConstSw (state, env, dests, opnd, c1, p1, c2, p2) =
         let
           fun doIt (state, env, i1, i2) =
               let
                 val cases = Vector.new2 ((c1, MU.Target.mkNoArgs i1), (c2, MU.Target.mkNoArgs i2))
                 val s = {select = M.SeConstant, on = opnd, cases = cases, default = NONE}
                 val t = M.TCase s
               in t
               end
         in 
           join (state, env, dests, p1, p2, doIt)
         end

     fun getBools (env, g) =
         let
           val c = toConfig env
           val t = MU.Bool.T c
           val f = MU.Bool.F c
         in g (t, f)
         end

     fun ifTrue (state, env, dests, opnd, p1, p2) =
         getBools (env,
                fn (t, f) =>
                   #1 (binConstSw (state, env, dests, opnd, t, p1, f, p2)))

     fun ifFalse (state, env, dests, opnd, p1, p2) =
         getBools (env,
                fn (t, f) =>
                   #1 (binConstSw (state, env, dests, opnd, f, p1, t, p2)))

     fun ifConst (state, env, dests, opnd, c1, p1, p2) =
         let
           fun doIt (state, env, i1, i2) =
               let
                 val t1 = MU.Target.mkNoArgs i1
                 val t2 = MU.Target.mkNoArgs i2
                 val cases = Vector.new1 ((c1, t1))
                 val s = {select = M.SeConstant, on = opnd, cases = cases, default = SOME t2}
                 val t = M.TCase s
               in t
               end
         in 
           #1 (join (state, env, dests, p1, p2, doIt))
         end

     fun zero env = MU.Sintp.zero (toConfig env)

     fun ifZero (state, env, dests, opnd, p1, p2) =
         ifConst (state, env, dests, opnd, zero env, p1, p2)

     fun ifNonZero (state, env, dests, opnd, p1, p2) =
         ifConst (state, env, dests, opnd, zero env, p2, p1)

     fun whenBinConst (state, env, opnd, c1, c2, p) =
         #1 (binConstSw (state, env, Vector.new0 (), opnd,
                         c1, (p, Vector.new0()),
                         c2, (new (state, env), Vector.new0 ())))

     fun whenTrue (state, env, opnd, p) =
         getBools (env,
                fn (t, f) =>
                   whenBinConst (state, env, opnd, t, f, p))

     fun whenFalse (state, env, opnd, p) =
         getBools (env,
                fn (t, f) =>
                   whenBinConst (state, env, opnd, f, t, p))

     fun call (state, env, dests, c, args, cuts, fx) =
         let
           fun doIt (state, env, t) =
               let
                 val c = M.IpCall {call = c, args = args}
                 val r = M.RNormal {rets = dests, block = t, cuts = cuts}
                 val t = M.TInterProc {callee = c, ret = r, fx = fx}
               in t
               end
           val code = new (state, env)
           val (code, _) = exitWith (state, env, code, Vector.new0 (), doIt)
         in code
         end

     fun tailcall (state, env, c, args, exits, fx) =
         let
           val c = M.IpCall {call = c, args = args}
           val r = M.RTail {exits = exits}
           val t = M.TInterProc {callee = c, ret = r, fx = fx}
         in
           terminate (state, env, new (state, env), t)
         end

     fun evalThunk (state, env, dest, t, e, cuts, fx) =
         let
           fun doIt (state, env, target) =
               let
                 val c = M.IpEval {typ = t, eval = e}
                 val dests = Vector.new1 dest
                 val r = M.RNormal {rets = dests, block = target, cuts = cuts}
                 val t = M.TInterProc {callee = c, ret = r, fx = fx}
               in t
               end
           val code = new (state, env)
           val (code, _) = exitWith (state, env, code, Vector.new0 (), doIt)
         in code
         end

     fun return (state, env, rs) =
         terminate (state, env, new (state, env), M.TReturn rs)

     fun cut (state, env, opnd, args, cuts) =
         terminate (state, env, new (state, env), M.TCut {cont = opnd, args = args, cuts = cuts})

     fun uintpLoopCV (state, env, start, i, limit, incr, inits, inargs, body, resv, outargs) =
       let
         val c = toConfig env
         val ni = relatedVar (state, i, "next", MU.Uintp.t c, M.VkLocal)
         val nd = namedVar (state, "mild", MU.Bool.t c, M.VkLocal)
         val body = 
             let
               val inc = MU.Uintp.add (c, M.SVariable i, incr)
               val inc = M.I {dests = Vector.new1 ni, n = 0, rhs = inc}
               val test = MU.Uintp.lt (c, M.SVariable ni, limit)
               val test = M.I {dests = Vector.new1 nd, n = 0, rhs = test}
               val body = appendl (state, env, body, [inc, test])
             in body
             end
         val inits' = Utils.Vector.cons (start, inits)
         val inargs = Utils.Vector.cons (i, inargs)
         val resv'  = Utils.Vector.cons (M.SVariable ni, resv)
         val nd' = namedVar (state, "mild", MU.Bool.t c, M.VkLocal)
         val preLoop = bindRhs (state, env, nd', MU.Uintp.lt (c, start, limit))
         fun enterF (state, env, entry, exit) = 
             let
               val tt = M.T {block = entry, arguments = inits'}
               val ft = M.T {block = exit,  arguments = inits}
             in
               MU.Bool.ifT (toConfig env, M.SVariable nd', {trueT = tt, falseT = ft})
             end
         fun exitF (state, env, entry, exit) = 
             let
               val tt = M.T {block = entry, arguments = resv'}
               val ft = M.T {block = exit,  arguments = resv}
             in
               MU.Bool.ifT (toConfig env, M.SVariable nd, {trueT = tt, falseT = ft})
             end
         val (_, _, loop) =
             loop (state, env, enterF, inargs, body, exitF, outargs)
       in
         seq (state, env, preLoop, loop)
       end

     fun uintpLoop (state, env, start, i, incr, limit, body) =
         uintpLoopCV (state, env, start, i, incr, limit, Vector.new0 (),
                      Vector.new0 (), body, Vector.new0 (), Vector.new0 ())

   end

end; (* MilStreamF *)

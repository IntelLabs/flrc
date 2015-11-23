(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_TRANSFORM = sig
  type state
  type env
  datatype order = ODfs | ODom | OAny
  val codeBody : state * env * order * Mil.codeBody -> Mil.codeBody
  val globals : state * env * order * Mil.globals -> Mil.globals
  val program : state * env * order * Mil.t -> Mil.t
end;

functor MilTransformF (
  type state
  type env
  val config : env -> Config.t
  val indent   : int

 (* For every block B such that B = 
  * 
  * l(vs):
  *   is
  *   t
  *
  * the transformer will apply label to l(vs), then instr to each of the is in
  * turn, and finally transfer to t.  Block B will be replaced by the fragment
  * that results from sequencing the individual streams, and closing the 
  * result.
  * 
  * For each of these functions, a return value of NONE indicates that no
  * change to the original object is desired.  A return value of SOME S
  * indicates that the object in question should be replaced with the stream
  * S.
  *)
  val label    : state * env * Mil.label * Mil.variable Vector.t -> env * MilStream.source option
  val instr    : state * env * Mil.instruction                   -> env * MilStream.t option
  val transfer : state * env * Mil.transfer                      -> env * MilStream.sink option
  val global   : state * env * Mil.variable * Mil.global         -> env * (Mil.variable * Mil.global) list option
) :> MIL_TRANSFORM where type state = state
                     and type env = env 
= struct

  structure VD = Identifier.VariableDict 
  structure LD = Identifier.LabelDict 
  structure M = Mil
  structure MF = MilFragment
  structure MS = MilStream

  val getConfig      = config
  val clientLabel    = fn (s, e, (l, vs)) => label (s, e, l, vs)
  val clientTransfer = transfer
  val clientInstr    = instr
  val clientGlobal   = fn (s, e, (v, g)) => global (s, e, v, g)

  fun fail (loc, msg) = Fail.fail ("MilTransform", loc, msg)
  
  type state = state
  type env = env
  datatype order = ODfs | ODom | OAny
                  
  fun rewrite (state, env, changed, try, ow, item) = 
      (case try (state, env, item)
        of (env, NONE) => (env, ow item)
         | (env, SOME res) => (changed := true; (env, res)))

  fun doLabel (state, env, changed, (bid, vs)) = 
      let
        fun ow (bid, vs) = (bid, vs, MS.empty)
        val (env, l) = rewrite (state, env, changed, clientLabel, ow, (bid, vs))
      in (env, l)
      end

  fun doInstrs (state, env, changed, is) = 
      let
        fun folder (item, env) = Utils.flip2 (rewrite (state, env, changed, clientInstr, MS.instruction, item))
        val (items, env) = Vector.mapAndFold (is, env, folder)
        val items = Vector.toList items
      in (env, items)
      end 

  fun doTransfer (state, env, changed, t) = 
      let
        fun ow t = (MS.empty, t)
        val (env, t) = rewrite (state, env, changed, clientTransfer, ow, t)
      in (env, t)
      end

  fun doBlock (state, env, changed, bid, b) = 
      let
        val M.B {parameters, instructions, transfer} = b
        val lchanged = ref false
        val (env, (l, vs, sl)) = doLabel (state, env, lchanged, (bid, parameters))
        val (env, iss) = doInstrs (state, env, lchanged, instructions)
        val (env, (st, t)) = doTransfer (state, env, lchanged, transfer)
        val frag =
            if !lchanged then 
              let
                val () = changed := true
                val s = MS.seqn [sl, MS.seqn iss, st]
                val frag = MS.finish (l, vs, s, t)
              in frag
              end
            else
              MF.fromBlock (bid, b)
      in (env, frag)
      end
      
  fun visitOrdered (state, env, order, cb, f) = 
      let
        fun folder ((l, b), (env, frags)) = 
            let
              val (env, frag) = f (state, env, l, b)
            in (env, frag::frags)
            end
        val frags = 
            (case order 
              of ODfs => fail ("visitOrdered", "DFS needs CFG!")
               | ODom => fail ("visitOrdered", "Dominator order not implemented")
               | OAny => 
                 let
                   val M.CB {blocks, ...} = cb
                   val blocks = LD.toList blocks
                   val (env, frags) = List.fold (blocks, (env, []), folder)
                 in frags
                 end)
      in frags
      end

  fun doCodeBody (state, env, order, changed, cb) = 
      let
        val M.CB {entry, ...} = cb
        fun visitor (state, env, l, b) = doBlock (state, env, changed,  l, b)
        val frags = visitOrdered (state, env, order, cb, visitor)
        val cb = 
            if !changed then
              let
                val frag   = MF.mergen frags
                val blocks = MF.toBlocksD frag
                val cb     = M.CB {entry = entry, blocks = blocks}
              in cb
              end
            else cb
      in cb
      end

  fun doFunction (state, env, order, changed, f) = 
      let
        val M.F {fx, escapes, recursive, cc, args, rtyps, body} = f
        val lchanged = ref false
        val body = doCodeBody (state, env, order, lchanged, body)
        val f = 
            if !lchanged then
              let
                val () = changed := true
                val f = 
                    M.F {fx        = fx,
                         escapes   = escapes,
                         recursive = recursive,
                         cc        = cc,
                         args      = args,
                         rtyps     = rtyps,
                         body      = body}
              in f
              end
            else 
              f
      in f
      end

  fun doCode (state, env, order, changed, (x, g)) = 
      let
        val g = 
            case g
             of M.GCode f => M.GCode (doFunction (state, env, order, changed, f))
              | _ => g
      in (x, g)
      end

  fun doCodes (state, env, order, changed, gs) = 
      List.map (gs, fn g => doCode (state, env, order, changed, g))

  fun doGlobal (state, env, order, changed, (x, g)) = 
      let
        val (env, xgs) = 
            case clientGlobal (state, env, (x, g))
             of (env, NONE) => (env, [(x, g)])
              | (env, SOME res) => let val () = changed := true in (env, res) end
        val xgs = doCodes (state, env, order, changed, xgs)
      in (env, xgs)
      end

  fun doGlobals (state, env, order, gs) = 
      let
        val changed = ref false
        fun doOne (x, g, (env, gs)) =
            let
              val (env, xgs) = doGlobal (state, env, order, changed, (x, g))
              val gs = VD.insertAll (gs, xgs)
            in (env, gs)
            end
        val (env, gs') = VD.fold (gs, (env, VD.empty), doOne)
        val gs = if !changed then gs' else gs
      in gs
      end

  fun doProgram (state, env, order, p) = 
      let
        val M.P {includes, externs, globals, symbolTable, entry} = p
        val globals = doGlobals (state, env, order, globals)
        val p = 
            M.P {includes = includes, externs = externs, globals = globals, symbolTable = symbolTable, entry = entry}
      in p
      end

  val codeBody = 
   fn (state, env, order, codeBody) => 
      let
        val changed = ref false
        val codeBody' = doCodeBody (state, env, order, changed, codeBody)
        val codeBody = if !changed then codeBody' else codeBody
      in codeBody
      end
  val globals = doGlobals
  val program = doProgram

end; (* Functor MilTransformF*)

(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_TRANSFORM = sig
  type state
  type env
  datatype order = ODfS | ODom | OAny
  val globals : state * env * order * Mil.globals -> Mil.globals
  val program : state * env * order * Mil.t -> Mil.t
end;

functor MilTransformF (
  type state
  type env
  structure MSS : MIL_STREAM where type state = state and type env = env
  val config : env -> Config.t

 (* For every block B such that B = 
  * 
  * l (vs):
  *   is
  * t
  *
  * the transformer will apply label to l (vs), then instr to each of the is in
  * turn, and finally transfer to t.  Block B will be replaced by the fragment
  * that results from sequencing the individual streams, and closing the 
  * result.
  * 
  * For each of these functions, a return value of NONE indicates that no
  * change to the original object is desired.  A return value of SOME S
  * indicates that the object in question should be replaced with the stream
  * S.  
  * 
  * If label (l, vs) => S_l and
  *    instr (is_i)  => S_i and
  *    transfer t    => S_t
  * 
  * Then seqn [S_l, S_i,..., S_t] should be a valid closed stream.  That is,
  * it should be labeled and terminated.  The most likely way to achieve this
  * is by ensuring the following:
  *   The stream returned by rewriting the label should be closed on its entry
  *   and open on its exit.
  *   The stream returned by rewriting each instruction should open on its
  *   entry and open on its exit.
  *   The stream returned by rewriting the transfer should be open on its 
  *   entry and closed on its exit.
  * However, other valid combinations are possible.
  *)
  val label     : state * env * (Mil.label * (Mil.variable Vector.t))
                  -> env * (MSS.Stream.t option)
  val instr     : state * env * Mil.instruction 
                  -> env * (MSS.Stream.t option)
  val transfer  : state * env * Mil.transfer
                  -> env * (MSS.Stream.t option)
  val global    : state * env * (Mil.variable * Mil.global)
                  -> env * ((Mil.variable * Mil.global) list option)
  val indent    : int
) :> MIL_TRANSFORM where type state = state
                     and type env = env 
= struct

  structure VD = Identifier.VariableDict 
  structure LD = Identifier.LabelDict 
  structure M = Mil
  structure MSF = MSS.Fragment
  structure MS = MSS.Stream

  val getConfig      = config
  val clientLabel    = label
  val clientTransfer = transfer
  val clientInstr    = instr
  val clientGlobal   = global

(*
  structure CFG = MilCfgF (
                  struct 
                    type env = env
                    val getConfig = getConfig
                    val passname = "MilTransform"
                    val indent = indent + 2
                  end
                  )
*)

  fun fail (loc, msg) = Fail.fail ("MilTransform", loc, msg)
  
  type state = state
  type env = env
  datatype order = ODfS | ODom | OAny
                  
  fun rewrite (state, env, changed, try, ow, item) = 
      (case try (state, env, item)
        of (env, NONE) => (env, ow item)
         | (env, SOME res) => (changed := true; (env, res)))

  fun doLabel (state, env, changed, (bid, vs)) = 
      let
        fun ow (bid, vs) =
            MS.labelWith (state, env, bid, vs, MS.new (state, env))
        val (env, l) =
            rewrite (state, env, changed, clientLabel, ow, (bid, vs))
      in (env, l)
      end

  fun doInstrs (state, env, changed, is) = 
      let
        fun ow i = MS.instr (state, env, i)
        fun folder (item, env) = 
            Utils.flip2 (rewrite (state, env, changed, clientInstr, ow, item))
        val (items, env) = Vector.mapAndFold (is, env, folder)
        val items = Vector.toList items
      in (env, items)
      end 

  fun doTransfer (state, env, changed, t) = 
      let
        fun ow t = MS.transfer (state, env, t)
        val (env, t) = rewrite (state, env, changed, clientTransfer, ow, t)
      in (env, t)
      end


  fun doBlock (state, env, changed, bid, b) = 
      let
        val M.B {parameters, instructions, transfer} = b
        val lchanged = ref false
        val (env, l) = doLabel (state, env, lchanged, (bid, parameters))
        val (env, iss) = doInstrs (state, env, lchanged, instructions)
        val (env, t) = doTransfer (state, env, lchanged, transfer)
        val frag = 
            if !lchanged then 
              let
                val () = changed := true
                val s = MS.seqn (state, env, [l, MS.seqn (state, env, iss), t])
                val (l, frag) = 
                    MS.close (state, env, s, Vector.new0 ())
              in frag
              end
            else
              MSF.block (state, env, bid, b)
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
              of ODfS => fail ("visitOrdered", "DFS needs CFG!")
(*                 let
                   val ts = CFG.Util.dfSTrees (env, cfg)
                   fun doTree (t, frags) = 
                       let
                         val (env, frags) = 
                             Tree.foldPre (t, (env, frags), folder)
                       in frags
                       end
                   val frags = List.fold (ts, [], doTree)
                 in frags
                 end*)
               | ODom => fail ("visitOrdered", 
                               "Dominator order not implemented")
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
                val frag   = MSF.mergen (state, env, frags)
                val blocks = MSF.blocks (state, env, frag)
                val blocks = LD.fromList blocks
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
             of M.GCode f => 
                M.GCode (doFunction (state, env, order, changed, f))
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
              | (env, SOME res) => (changed := true; 
                                    (env, res))
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
        val M.P {globals, symbolTable, entry} = p
        val globals = doGlobals (state, env, order, globals)
        val p = 
            M.P {globals = globals, symbolTable = symbolTable, entry = entry}
      in p
      end

  val globals = doGlobals
  val program = doProgram

end; (* Functor MilTransformF*)

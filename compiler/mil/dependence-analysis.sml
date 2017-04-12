(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


(* Comment by Leaf.
 * It's a little hard to follow exactly what's going on here, but basically,
 * I think this code is doing the following.
 * 1) Via dataflow analysis, computing an extended set of induction
 *   variables.  Essentially, it tries to extend the base set of
 *   induction variables with additional ones computed via conversions
 *   and things like RatToUInt32Checked.
 * 2) Via dataflow analysis, it does some constant propagation.  I don't
 *   know why it does this.  I think that if this is necessary, it
 *   is orthogonal and should be done elsewhere.  Am I missing something?
 * 3) It annotates variables which are defined by a read with the
 *   source, index and offset of the read.  This is done as part of the
 *   the dataflow pass, but is not propagated with the dataflow
 *   information.  I think this should be split out, as it's quite
 *   confusing to figure out what's going on here, and doesn't
 *   seem to have any dataflow properties.
 * 4) It annotates variables which are written into via a subscript
 *   with the source value, index and offset.  As with the reads,
 *   this is done as part of the dataflow pass, but the information
 *   is not propagated.
 *)
signature MIL_DEPENDENCE_ANALYSIS =
sig
  val debugs : Config.Debug.debug list

  type env

  datatype constant = CRat of Rat.t
                    | CIntegral of IntInf.t
                    | CFloat of Real32.t
                    | CDouble of Real64.t

  datatype bound = BRat of Rat.t
                 | BSymb of Mil.variable

  datatype ivinfo = IV of {base : Mil.variable,
                           trans : (Rat.t * Rat.t) option, (* m, c *)
                           range : (Rat.t * bound * Rat.t) option}
                                   (* The three values denote
                                      - lower bound
                                      - upper bound
                                      - step
                                      where the index ranges in [L, U).
                                      For the upper bound I allow symbolic
                                      values as this is advantageous for
                                      vectorisation. The other two values
                                      remain constants, as I as of now
                                      have no use for symbolic values there *)

  (* When variable v is annotated with:
   *  - R{source, iv = NONE, offset} it means that v is defined by a read from
   *    source at offset
   *  - R{source, iv = SOME v, offset} it means that v is defined by a read from
   *    source at offset + v
   *)
  datatype read = R of {source : Mil.variable,
                        iv : Mil.variable option,
                        offset : Rat.t}

  (* When variable v is annotated with:
   *  - W{source, iv = NONE, offset} it means that source was written into v
   *    at offset
   *  - W{source, iv = SOME x, offset} it means that source was written into v
   *    at offset + x
   *)
  datatype write = W of {source : Mil.simple,
                         iv : Mil.variable option,
                         offset : Rat.t}

  datatype 'a tristate = TNone
                       | TSome of 'a
                       | TUnknown

  datatype varinfo = VI of {deps : Identifier.VariableSet.t,  (* The SSA dependencies *)
                            iv : ivinfo tristate,
                            read : read option,
                            write : write list,
                            value : constant option}

  val loop : env
           * Mil.t     (* program *)
           * Mil.code  (* function *)
           * Mil.label (* loop *)
           -> varinfo Identifier.VariableDict.t option

  val loop'' : env
           * Mil.t     (* program *)
           * Mil.code  (* function *)
           * Mil.label (* loop *)
           * MilLoop.t (* loops *)
           -> varinfo Identifier.VariableDict.t option

  val loop' : env
            * Mil.t      (* program *)
            * Mil.code   (* function *)
            * Mil.label  (* loop *)
            * MilCfg.t  (* cfg XXX WL: useless? *)
            * MilCfg.LabelDominance.t (* dom info *)
            * Mil.block Identifier.LabelDict.t Identifier.LabelDict.t
            * MilLoop.inductionVariable list Identifier.LabelDict.t
            * MilLoop.tripCount Identifier.LabelDict.t
            * MilLoop.t
            -> varinfo Identifier.VariableDict.t option

  val function : env
               * Mil.t
               * Mil.code
               -> varinfo Identifier.VariableDict.t option
  val function' : env
                * Mil.t
                * Mil.code
                * MilCfg.t
                * MilCfg.LabelDominance.t
                * Mil.block Identifier.LabelDict.t Identifier.LabelDict.t
                * MilLoop.inductionVariable list Identifier.LabelDict.t
                * MilLoop.tripCount Identifier.LabelDict.t
                * MilLoop.t
                -> varinfo Identifier.VariableDict.t option

  val getCycles : varinfo Identifier.VariableDict.t -> Mil.variable list list
  val layoutDot : varinfo Identifier.VariableDict.t -> Layout.t
  val layoutVarInfo : env * varinfo -> Layout.t

  val tryIt : env * Mil.t -> Mil.t
end

functor MilDependenceAnalysisF (
  type env

  val getConfig : env -> Config.t
  val passname : string
  val indent : int
) :> MIL_DEPENDENCE_ANALYSIS where type env = env
                             =
struct
  val myPassname = passname ^ ":MDA"

  structure I  = Identifier
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure LU = LayoutUtils
  structure P  = Mil.Prims
  structure PU = MilUtils.Prims.Utils
  structure IPLG = ImpPolyLabeledGraph
  structure L  = Layout
  structure M  = Mil
  structure MU = MilUtils
  structure ML = MilLayout

  type variable = I.variable

  fun dbgLayout' (config, msg) = if Config.debug then LU.printLayout msg else ()

  (*
   * definitions for the environment
   *)

  type env = env

  datatype localenv = LE of {
                        env : env,
                        program : Mil.t,
                        blocks : Mil.block LD.t,       (* All of the blocks *)
                        ivs : VS.t LD.t,               (* A mapping from block labels to the set of
                                                        * MilLoop induction variables associated with the loop *)
                        tcs : MilLoop.tripCount LD.t,  (* The MilLoop tripCounts *)
                        loopBlocks : LS.t VD.t,        (* This maps induction variables discovered by MilLoop to
                                                        * the set of labels of the blocks of the loop with which
                                                        * the induction variable is associated *)
                        miloop : MilLoop.t             (* The MilLoop information *)
                      }

  fun mkLocalEnv (genv : env,
                  prog : Mil.t,
                  func as Mil.CB {entry, blocks},
                  nodes : Mil.block LD.t LD.t,
                  ivinfo : MilLoop.inductionVariable list LD.t,
                  tcs : MilLoop.tripCount LD.t,
                  miloop : MilLoop.t) =
      let
        (* {inductionVariable,...} list LD.t => variable list LD.t *)
        val ivs =
            let
              val getVar =
                  fn iv =>
                     (case iv
                       of MilLoop.BIV r => #variable r
                        | MilLoop.DIV r => #variable r)
              val ivs = LD.map (ivinfo, fn (k, ivl) => List.map (ivl, getVar))
            in ivs
            end

        (* variable list LD.t => VariableSet LD.t *)
        val ivs = LD.map (ivs, fn (k, e) => VS.fromList (e))

        (*
         * compute a mapping from induction variables to the blocks they
         * are valid in.
         *)
        fun doOne (l : Mil.label, ivl, r) =
            let
              val blockdict = Option.valOf (LD.lookup (nodes, l))
              val blockset = LS.fromList (LD.domain (blockdict))
            in
              VS.fold (ivl, r, fn (v, r) => VD.insert (r, v, blockset))
            end
        val lblocks = LD.fold (ivs, VD.empty, doOne)
      in
        LE {env = genv, program = prog, blocks = blocks, ivs = ivs, tcs = tcs, loopBlocks = lblocks, miloop = miloop}
      end

  fun getEnv label (LE x) = label x
  val getEnvConfig = getConfig o (getEnv #env)
  val getEnvProgram = getEnv #program
  val getEnvBlocks = getEnv #blocks
  val getEnvBlock = Option.valOf o
                    (fn (e, l) => LD.lookup (getEnvBlocks (e), l))
  val getEnvIvs = getEnv #ivs
  val getEnvTcs = getEnv #tcs
  val getEnvLoopBlocks = getEnv #loopBlocks

  (*
   * debugging output
   *)
  val (debugPassD, debugPass) = Config.Debug.mk (myPassname, "debug the dataflow-dependence module")

  structure Debug =
  struct
    fun dbgPrint (LE env, t) = if Config.debug andalso debugPass (getConfig (#env env)) then print t else ()

    fun printLayout (config, l) = if Config.debug andalso debugPass config then LU.printLayout l else ()

    fun addme (LE env, l) = dbgPrint (LE env, "ADDME:" ^ (LU.toString l) ^ "\n")

    fun abort (LE env, w, msg) =
        let
          val () = dbgPrint (LE env, "ABORT @ " ^ w ^ ": " ^ msg ^ "\n")
        in Try.fail ()
        end

    fun addmeAbort (LE env, w, l) = abort (LE env, w, "missing implementation for " ^ (LU.toString l))
  end


  (*
   * my own constants :)
   *)
  datatype constant = CRat of Rat.t
                    | CIntegral of IntInf.t
                    | CFloat of Real32.t
                    | CDouble of Real64.t

  fun equalConst (c1, c2) =
      case (c1, c2)
       of (CIntegral i1, CIntegral i2)     => IntInf.equals (i1, i2)
        | (CRat r1, CRat r2)               => Rat.equals (r1, r2)
        | (CFloat n1, CFloat n2)           => Real32.equals (n1, n2)
        | (CDouble n1, CDouble n2)         => Real64.equals (n1, n2)
        | _ => false

  fun constant (c1, c2) =
      case (c1, c2)
       of (CIntegral i1, CIntegral i2)     => IntInf.compare (i1, i2)
        | (CIntegral _, _)                 => GREATER
        | (_, CIntegral _)                 => LESS
        | (CRat r1, CRat r2)               => Rat.compare (r1, r2)
        | (CRat _, _)                      => GREATER
        | (_, CRat _)                      => LESS
        | (CFloat n1, CFloat n2)           => Real32.compare (n1, n2)
        | (CFloat _, _)                    => GREATER
        | (_, CFloat _)                    => LESS
        | (CDouble n1, CDouble n2)         => Real64.compare (n1, n2)

  fun constToRat (c) =
      case c
       of CIntegral i => SOME (Rat.fromIntInf i)
        | CRat r => SOME r
        | _ => NONE

  fun constToInt (c) =
      case c
       of CRat r => Option.map (Rat.toIntInf r, fn (x) => CIntegral x)
        | CIntegral i => SOME (CIntegral i)
        | _ => NONE

  fun layoutConstant (c) =
      case c
       of CIntegral i => IntInf.layout i
        | CRat r => Rat.layout r
        | CFloat f => Real32.layout f
        | CDouble d => Real64.layout d

  fun milToLocalConst (c) =
      case c
       of Mil.CIntegral v => SOME (CIntegral (IntArb.toIntInf v))
        | Mil.CFloat v    => SOME (CFloat v)
        | Mil.CDouble v   => SOME (CDouble v)
        | _ => NONE

  (*
   * definitions for the state
   *)

  datatype bound = BRat of Rat.t
                 | BSymb of Mil.variable

  fun boundEq (b1, b2) =
      case (b1, b2)
       of (BRat r1, BRat r2) => Rat.equals (r1, r2)
        | (BSymb s1, BSymb s2) => s1 = s2
        | _ => false

  datatype ivinfo = IV of {base : variable,
                           trans : (Rat.t * Rat.t) option, (* m, c *)
                           range : (Rat.t * bound * Rat.t) option}
                                   (* L, U, step *)

  datatype read = R of {source : variable,
                        iv : variable option,
                        offset : Rat.t}

  datatype write = W of {source : Mil.simple,
                         iv : variable option,
                         offset : Rat.t}

  (*
   * TNone means known to be no ind. var
   * TSome means possibly a known ind. var
   * TUnknown means possibly some kind of ind. var,
   *          i.e. a multi-subscript
   *)
  datatype 'a tristate = TNone
                       | TSome of 'a
                       | TUnknown

  fun tristateEqual (a, b, eq) =
    case (a, b)
     of (TNone, TNone) => true
      | (TSome x, TSome y) => eq (x, y)
      | (TUnknown, TUnknown) => true
      | _ => false

  datatype varinfo = VI of {deps : VS.t,
                            iv : ivinfo tristate,
                            read : read option,
                            write : write list,
                            value : constant option}

  (*
   * for enhanced readabilty some variations on nothing
   *)
  val noDeps = VS.empty
  val noWrites = []
  val noReads = NONE
  val noIvs = TNone
  val noValue = NONE

  type t = varinfo VD.t

  (*
   * construct a var info. Always use this function
   * as it ensures that the invariant that variables
   * with induction variable info do not have a
   * known value holds
   *)
  fun mkVarInfo (deps, iv, read, write, value) =
      VI {deps  = deps,
          iv    = iv,
          read  = read,
          write = write,
          value = (case iv
                    of TNone => value
                    | _ => NONE)}

  val emptyVarInfo = mkVarInfo (noDeps, noIvs, noReads, noWrites, noValue)

  (*
   * State manipulation functions
   *)
  local
    fun checkEqual (env, old, new, cmp) =
        (case (old, new)
          of (NONE, NONE) => NONE
           | (SOME od, SOME nd) => if cmp (od, nd) then old else Debug.abort (env, "checkEqual", "inconsistent read info")
           | _ => Debug.abort (env, "checkEqual", "inconsistent read info"))
    fun weakenEitherOrMerge (env, old, new, weaken, merge) =
        (case (old, new)
          of (TNone, TNone) => TNone
           | (TNone, TSome n) => TSome (weaken (n))
           | (TSome d, TNone) => TSome (weaken (d))
           | (TSome d, TSome n) => merge (env, d, n)
           | (TUnknown, _) => TUnknown
           | (_, TUnknown) => TUnknown)
    fun weakenNonEqual (env, old, new, cmp) =
        (case (old, new)
          of (SOME v1, SOME v2) => if (cmp (v1, v2)) then SOME v1 else NONE
           | _ => NONE)

    (* weaken info if different trans found, abort if
       different bases *)
    fun weakenIvInfo (IV x) = IV {base = #base x, trans = NONE, range = NONE}

    fun equalTrans ((a1, a2), (b1, b2)) = (Rat.equals (a1, b1)) andalso (Rat.equals (a2, b2))

    fun equalRange ((a1, a2, a3), (b1, b2, b3)) = (Rat.equals (a1, b1)) andalso (boundEq (a2, b2)) andalso (Rat.equals (a3, b3))

    fun mergeIvInfo (env, IV old, IV new) =
        if ((#base old) = (#base new)) then
          let
            val rt = weakenNonEqual (env, #trans old, #trans new, equalTrans)
            val rr = weakenNonEqual (env, #range old, #range new, equalRange)
          in
            TSome (IV {base = #base old, trans = rt, range = rr})
          end
        else
          let
            val () = Debug.dbgPrint (env, "merged IV to unknown")
          in TUnknown
          end

    fun equalIvInfo (IV a, IV b) = ((#base a) = (#base b)) andalso (Option.equals (#trans a, #trans b, equalTrans))

    (* compute union *)
    fun mergeDeps (env, old, new) = VS.union (old, new)

    fun equalRead (R a, R b) =
        Rat.equals (#offset a, #offset b) andalso
        (Option.equals (#iv a, #iv b, fn (a, b) => a = b)) andalso
        ((#source a) = (#source b))

    fun equalWrite (W a, W b) =
        Rat.equals (#offset a, #offset b) andalso
        (Option.equals (#iv a, #iv b, fn (a, b) => a = b)) andalso
        (MU.Compare.simple(#source a, #source b) = EQUAL)

    fun mergeWrite (env, old, new) =
        List.removeDuplicates (old @ new, equalWrite)
  in
    (*
     * Merging rules:
     *
     * deps: compute the union
     * value: weaken, i.e. keep the minimum of information available
     * write: join write information
     * read: ensure we found the same information. Due to SSA, a
     *       variable may only correspond to one single read
     * iv: if same ind. var used, keep and weaken the transformation
     *     if it differs. If the base differs, switch to TUnknown
     *)
    fun mergeVarInfo (env, VI old, VI new) =
        let
          val rd = mergeDeps (env, #deps old, #deps new)
          val ri = weakenEitherOrMerge (env, #iv old, #iv new, weakenIvInfo, mergeIvInfo)
          val rr = checkEqual (env, #read old, #read new, equalRead)
          val rw = mergeWrite (env, #write old, #write new)
          val rv = weakenNonEqual (env, #value old, #value new, equalConst)
        in
          mkVarInfo (rd, ri, rr, rw, rv)
        end

    fun fixedVarInfo (env, VI old, VI new) =
        let
          val fd = true (* dependencies are not considered for fixpoint *)
          val fi = tristateEqual (#iv old, #iv new, equalIvInfo)
          val fr = Option.equals (#read old, #read new, equalRead)
          (* write information cannot get lost, so it suffices to check
             whether new information has been found *)
          val fw = (List.length (#write new)) = (List.length (#write old))
          val fv = Option.equals (#value old, #value new, equalConst)
        in
          fd andalso fi andalso fr andalso fw andalso fv
        end
  end

  fun getSimpleVar (simple : Mil.simple) =
      (case simple
        of Mil.SVariable v => SOME v
         | Mil.SConstant c => NONE)

  fun getSimpleConst (simple) =
      (case simple
           of Mil.SConstant c =>
              (case c
                of Mil.CRat c1 => SOME (CIntegral(c1))
                 | Mil.CInteger i1 => SOME (CIntegral(i1))
                 | Mil.CFloat f1 => SOME (CFloat(f1))
                 | Mil.CDouble d1 => SOME (CDouble(d1))
                 | _ => NONE)
            | Mil.SVariable v => NONE)

  fun convertSimple (env, to, simple) =
      (case simple
        of Mil.SConstant c => Debug.abort (env, "convertSimple", "implement constant conversion")
         | other => other)

  fun getSimpleValue (dict : variable -> varinfo, simple) =
      (case simple
        of Mil.SVariable v => let
                                val VI {value = vl, ...} = dict (v)
                              in vl
                              end
         | Mil.SConstant c => milToLocalConst (c))

  fun deriveFromSimple (env, dict, simple) =
      case getSimpleVar (simple)
       of SOME v =>
          let
            val VI nfo = dict (v)
          in
            mkVarInfo (VS.fromList [v], #iv nfo, noReads, noWrites, #value nfo)
          end
        | NONE => mkVarInfo (noDeps, noIvs, noReads, noWrites, getSimpleValue (dict, simple))

  fun deriveGlobal (env, dict, global) =
      (case global
        of Mil.GSimple s => deriveFromSimple (env, dict, s)
         | Mil.GRat r => mkVarInfo (noDeps, noIvs, noReads, noWrites, SOME (CRat r))
         | _ => emptyVarInfo)

  (*
   * This function derives a new state from a set of dependencies.
   * As there are no assumptions on the computation that leads to
   * the result, it weakens all value/iv information.
   *)
  fun deriveFromVector (env, dict, ops) =
      let
        fun weaken (VI x) =
            let
              val iv = case #iv x
                        of TSome (IV i) => TSome (IV {base = #base i, trans = NONE, range = NONE})
                         | other => other
            in
              mkVarInfo (#deps x, iv, #read x, #write x, NONE)
            end
      in
        Vector.fold (ops, emptyVarInfo, fn (x, r) => mergeVarInfo (env, (weaken o deriveFromSimple) (env, dict, x), r))
      end

  fun deriveFromArith (env, dict, ops, arith) =
      let
        val deps = Vector.fold (ops, VS.empty,
                                fn (x, r) => (case getSimpleVar x
                                               of SOME v => VS.insert (r, v)
                                                | NONE => r))
        fun oneArg (ops) = getSimpleVar (Vector.sub (ops, 0))
        fun twoArg (ops) =
            let
              val vars = (getSimpleVar (Vector.sub (ops, 0)), getSimpleVar (Vector.sub (ops, 1)))
              val vals = (getSimpleValue (dict, Vector.sub (ops, 0)), getSimpleValue (dict, Vector.sub (ops, 1)))
            in
              (* for induction vars, the value cannot be known as it
                 is loop dependent. So we only have to check cases
                 where one value is not known but the other is *)
              case (vars, vals)
                of ((SOME v, _), (NONE, SOME x)) => SOME (v, x, false)
                 | ((_, SOME v), (SOME x, NONE)) => SOME (v, x, true)
                 | _ => NONE
            end
        fun negT (m, c) = SOME (Rat.~ m, Rat.~ c)
        fun negR (l, u, s) = SOME (Rat.~ l, Rat.~ u, Rat.~ s)
        fun addT (m, c, f, v) = SOME (m, Rat.+ (v, c))
        fun addR (l, u, s, f, v) = SOME (Rat.+ (l, v), Rat.+ (u, v), s)
        fun subT (m, c, f, v) = SOME (m, if f then (Rat.- (v, c))
                                              else (Rat.- (c, v)))
        fun subR (l, u, s, f, v) = SOME (if f
                                         then (Rat.- (l, v), Rat.- (u, v), s)
                                         else (Rat.- (v, l), Rat.- (v, u), s))
        fun mulT (m, c, f, v) = SOME (Rat.+ (v, m), Rat.* (v, c))
        fun mulR (l, u, s, f, v) = SOME (Rat.* (l, v), Rat.* (u, v),
                                         Rat.* (s, v))
        fun divideT (m, c, f, v) = if f then NONE
                                        else SOME (Rat.- (m, v), Rat./ (c, v))
        fun divideR (l, u, s, f, v) = if f
                                      then NONE
                                      else SOME (Rat./ (l, v), Rat./ (u, v),
                                                 Rat./ (s, v))
        fun r f v =
            case v
             of (a, BRat r, b) =>
                let
                  val r = f (a, r, b)
                in
                  Option.map (r, fn (a, b, c) => (a, BRat b, c))
                end
              | _ => NONE
        fun optAppOpt (v, f) =
            case f
              of SOME g => Option.map (v, g)
               | NONE => NONE
        fun derive (v, f, g) =
            case dict (v)
             of VI {iv = TSome (IV {base, trans, range}), ...} =>
                mkVarInfo (deps,
                           TSome (IV {base = base,
                                      trans = Utils.Option.bind (trans, f),
                                      range = Utils.Option.bind (range, r g)}),
                           noReads, noWrites, noValue)
              | VI {iv = other, ...} =>
                mkVarInfo (deps, other, noReads, noWrites, noValue)
        fun deriveOne (f, g) =
            case oneArg (ops)
             of SOME v => derive (v, f, g)
              | NONE => deriveFromVector (env, dict, ops)
        fun deriveBin (f, g) =
          case twoArg (ops)
            of SOME (v, x, fl) =>
               (case constToRat x
                 of SOME r =>
                    derive (v,
                            fn (m, c) => f (m, c, fl, r),
                            fn (l, u, s) => g (l, u, s, fl, r))
                  | NONE => deriveFromVector (env, dict, ops))
             | NONE => deriveFromVector (env, dict, ops)
      in
        (case arith
          of P.ANegate => deriveOne (negT, negR)
           | P.APlus => deriveBin (addT, addR)
           | P.AMinus => deriveBin (subT, subR)
           | P.ATimes => deriveBin (mulT, mulR)
           | P.ADiv _ => deriveBin (divideT, divideR)
           | _ => deriveFromVector (env, dict, ops))
      end

  fun deriveFromPrim (env, dict, dests, prim, ops) =
      case Utils.Vector.lookup (dests, 0)
        of SOME v =>
           (case prim
             of P.Prim p =>
                (case p
                  of P.PNumArith {operator = arith, ...} =>
                     SOME (deriveFromArith (env, dict, ops, arith))
                   | P.PNumCompare _  => (* result is no induction var *)
                     let
                       fun dropIv (VI x) =
                           mkVarInfo (#deps x, noIvs, #read x,
                                      #write x, #value x)
                     in
                       SOME ((dropIv o deriveFromVector) (env, dict, ops))
                     end
                   | P.PNumConvert {to, from} =>
                     let
                       val simp = convertSimple (env, to, Vector.sub (ops, 0))
                     in
                       SOME (deriveFromSimple (env, dict, simp))
                     end
                   | _ =>
                     let
                       val () = Debug.addme (env, PU.Layout.t (getEnvConfig env, prim))
                     in
                       SOME (deriveFromVector (env, dict, ops))
                     end)
                   (* TODO: check what these are about *)
              | x =>
                let
                  val () = Debug.addme (env, PU.Layout.t (getEnvConfig env, x))
                in
                  SOME (deriveFromVector (env, dict, ops))
                end)
         | NONE => NONE (* no target, nothing to do *)

  fun deriveFromRead (env, dict, source : variable, fixed : int, dyn : Mil.operand option) =
      let
        val m as Mil.P {globals, symbolTable, ...} = getEnvProgram env
        val config = getEnvConfig env
        val si = I.SymbolInfo.SiTable symbolTable
        val () = Debug.printLayout (config, L.seq [L.str "deriveFromRead ",
                                                L.str " source:",
                                                ML.layoutVariable (config, si, source),
                                                L.str " fixed:",
                                                Int.layout fixed,
                                                L.str " dyn:",
                                                (case dyn of SOME dyno => ML.layoutOperand (config, si, dyno)
                                                           | NONE => L.str "NONE")])
        val iv = case dyn
                  of SOME sdyn => getSimpleVar (sdyn)
                   | _ => NONE
        val doff = case dyn
                    of SOME sdyn => getSimpleValue (dict, sdyn)
                     | _ => NONE
        val ro = Rat.fromInt fixed
        val offset = case doff    (* TODO: can there ever be non Integers? *)
                      of SOME c => Rat.+ (Option.valOf (constToRat c), ro)
                       | NONE => ro
        (* FIXME WL: Code Review: offset = offset? *)
(*        val ri = R {source = source, iv = iv, offset = Rat.fromInt fixed}*)
        val ri = R {source = source, iv = iv, offset = offset}
      in
        mkVarInfo (VS.fromList [source], noIvs, SOME ri, noWrites, NONE)
      end

  fun deriveFromWrite (env, dict, array, fixed, dyn, value : Mil.simple) =
    let
      val config = getEnvConfig env
      val m as Mil.P {globals, symbolTable, ...} = getEnvProgram env
      val si = I.SymbolInfo.SiTable symbolTable
      val (doff, iv) = case dyn
                        of SOME s => (getSimpleValue (dict, s), getSimpleVar (s))
                         | _ => (NONE, NONE)
      val ro = Rat.fromInt fixed
      val offset = case doff     (* TODO: same here *)
                    of SOME c => Rat.+ (Option.valOf (constToRat c), ro)
                     | NONE => ro
    in
      (case value
        of M.SVariable v =>
           let
             (* FIXME WL: Code Review: offset = offset? *)
             val wi = W {source = value, iv = iv, offset = Rat.fromInt fixed}
           in
             mkVarInfo (VS.fromList [v], noIvs, noReads, [wi], NONE)
           end
(*         | M.SConstant c => deriveConstant (env, c))*)
	 | M.SConstant c =>
	   let
               val wi = W {source = value, iv = iv, offset = Rat.fromInt fixed}
	   in
	       mkVarInfo (noDeps, noIvs, noReads, [wi], milToLocalConst c)
	   end)
    end

  fun layoutRhsTyp rhs =
      (case rhs
        of Mil.RhsSimple s => L.str "RhsSimple"
         | Mil.RhsPrim p => L.str "RhsPrim"
         | Mil.RhsTuple _ => L.str "RhsTuple"
         | Mil.RhsTupleSub (ts as Mil.TF {tupDesc, tup, field}) => L.str "RhsTuplesub"
         | Mil.RhsTupleSet {tupField as Mil.TF {tupDesc, tup, field}, ofVal} => L.str "RhsTupleSet"
         | Mil.RhsTupleCAS {tupField as Mil.TF {tupDesc, tup, field}, cmpVal, newVal} => L.str "RhsTupleCAS"
         | Mil.RhsTupleWait {tupField as Mil.TF {tupDesc, tup, field}, pred} => L.str "RhsTupleWait"
         | Mil.RhsTupleInited _ => L.str "RhsTupleInited"
         | Mil.RhsIdxGet _ => L.str "RhsIdxGet"
         | Mil.RhsCont _ => L.str "RhsCont" 
         | Mil.RhsThunkMk _ => L.str "RhsThunMk"
         | Mil.RhsThunkInit _ => L.str "RhsThunkInit"
         | Mil.RhsThunkGetFv _ => L.str "RhsThunkGetFv"
         | Mil.RhsThunkValue _ => L.str "RhsThunkValue"
         | Mil.RhsThunkGetValue _ => L.str "RhsThunkGetValue"
         | Mil.RhsThunkSpawn _ => L.str "RhsThunkSpawn"
         | Mil.RhsClosureMk _ => L.str "RhsClosureMk"
         | Mil.RhsClosureInit _ => L.str "RhsClosureInit"
         | Mil.RhsClosureGetFv _ => L.str "RhsClosureGetFv"
         | Mil.RhsPSetNew _ => L.str "RhsPSetNew"
         | Mil.RhsPSetGet _ => L.str "RhsPSetGet"
         | Mil.RhsPSetCond _ => L.str "RhsPSetCond"
         | Mil.RhsPSetQuery _ => L.str "RhsPSetQuery"
         | Mil.RhsEnum _ => L.str "RhsEnum"
         | Mil.RhsSum _ => L.str "RhsSum"
         | Mil.RhsSumProj _ => L.str "RhsSumProj"
         | Mil.RhsSumGetTag _ => L.str "RhsSumGetTag")

  fun deriveInstr (env, dict, v, rhs) =
      let
        val config = getEnvConfig env
        val m as Mil.P {globals, symbolTable, ...} = getEnvProgram env
        val si = I.SymbolInfo.SiTable symbolTable
        val some = Vector.new1
        val none = Vector.new0 ()
        val dest = fn () => Vector.sub (v, 0)
	val () = Debug.printLayout (config, L.seq [L.str "deriveInstr: ",
					                           ML.layoutRhs (config, si, rhs)])
      in
      case rhs
       of Mil.RhsSimple s => some (dest (), deriveFromSimple (env, dict, s))
        | Mil.RhsPrim {prim, createThunks, typs, args} => (case deriveFromPrim (env, dict, v, prim, args)
                                                            of SOME x => some (dest (), x)
                                                             | NONE => none)
        | Mil.RhsTuple {mdDesc, inits} => some (dest (), deriveFromVector (env, dict, inits))
        | Mil.RhsTupleSub (tf as Mil.TF {tupDesc, tup, field}) =>
          (case field
            of Mil.FiFixed idx => some (dest (), deriveFromRead (env, dict, tup, idx, NONE))
             | Mil.FiVariable dyn => some (dest (),
                                           deriveFromRead (env, dict, tup,
                                                           MilUtils.TupleDescriptor.numFixed (tupDesc),
                                                           SOME dyn))
             | _ => none)
        | Mil.RhsTupleSet {tupField as Mil.TF {tupDesc, tup, field}, ofVal} =>
          let
          in
            (case field
              of Mil.FiFixed idx => some (tup, deriveFromWrite (env, dict, tup, idx, NONE, ofVal))
               | Mil.FiVariable dyn => some (tup, deriveFromWrite (env, dict, tup,
								   MilUtils.TupleDescriptor.numFixed (tupDesc),
								   SOME dyn, ofVal))
               | _ =>
		 let
		     val () = Debug.addmeAbort (env, "deriveInstr.RhsTupleSet", layoutRhsTyp rhs)
		 in
		     none
		 end)
          end
	| Mil.RhsSumProj {typs, sum, tag, idx} =>
	  let
	  in some (dest (), deriveFromSimple (env, dict, Mil.SVariable sum))
	  end
	| Mil.RhsSumGetTag {typ, sum} =>
	  let
	  in some (dest (), deriveFromSimple (env, dict, Mil.SVariable sum))
	  end
        | x =>
          let
            val () = Debug.addmeAbort (env, "deriveInstr", layoutRhsTyp rhs)
          in none
          end
      end

  fun deriveFunction (env, dict, ops, argno, cuts, effects) =
      let
        (* TODO: maybe this is a bit strict *)
        val () = Debug.dbgPrint (env, "deriveFunction attempt ")
        val () = Try.require (Effect.isEmpty (effects))
        val () = Debug.dbgPrint (env, "succeeds\n")
        val vi = deriveFromVector (env, dict, ops)
      in
        Vector.fromList (List.duplicate (argno, fn () => vi))
      end

  (* addIvInfo (env, dict, ivl, l, params, infos)
   * env is the environment
   * dict maps variables to their info
   * ivl is the induction variable list for the target loop
   * l is the target loop
   * params is the parameters to the target loop
   * infos is the info derived from the arguments *)
  fun addIvInfo (env, dict, ivl, l, params, infos) =
      let
        (* try and compute a range from the tripcount for this block *)
        fun symbolicRange (MilLoop.TC {flip1,
                                           comparison,
                                           flip2,
                                           init = (m, i, c),
                                           step,
                                           bound, ...}) =
            (* the support for symbolic ranges is very limited. It is
               somewhat handcrafted to handle some cases of vectorization.
               Only for iv in [...,var) with step = 1 a symbolic bound can
               be added. *)
            (* cmp, not, order, step 1, no mult, end of loop*)
            case (comparison, flip1, flip2, Rat.equals (step, Rat.one),
                  Rat.equals (m, Rat.one), Rat.equals (c, Rat.one))
              (* iv >= bound at end of loop *)
             of (CLt, true, false, true, true, true) =>
                Option.map (getSimpleVar bound, BSymb)
              (* iv == bound at end of loop *)
              | (Ceq, false, _, true, true, true) =>
                Option.map (getSimpleVar bound, BSymb)
              (* bound <= iv at end of loop *)
              | (CLt, true, true, true, true, true) =>
                Option.map (getSimpleVar bound, BSymb)
              | _ => NONE

        fun constantRange (MilLoop.TC {flip1,
                                           comparison,
                                           flip2,
                                           init = (m, i, c),
                                           step,
                                           bound, ...},
                           l) =
            Try.try (fn () =>
              let
                (* TODO: take a clooooose look at this again *)
                val bval = Try.<- (getSimpleValue (dict, bound))
                val brat = Try.<- (constToRat (bval))
                val inv = (Rat.compare (step, Rat.zero)) = LESS
                val u = case (comparison, flip1, flip2, inv)
                         of (P.CEq, false, _, _) => brat
                          | (P.CNe, true, _, _) => brat
                            (* brat < iv increasing *)
                          | (P.CLt, false, true, false) => Rat.+ (brat, Rat.one)
                            (* iv < brat decreasing *)
                          | (P.CLt, false, false, true) => Rat.- (brat, Rat.one)
                            (* brat >= iv decreasing *)
                          | (P.CLt, true, true, true) => brat
                            (* iv >= brat increasing *)
                          | (P.CLt, true, false, false) => brat
                            (* brat <= iv increasing *)
                          | (P.CLe, false, true, false) => brat
                            (* iv <= brat decreasing *)
                          | (P.CLe, false, false, true) => brat
                            (* brat > iv decreasing *)
                          | (P.CLe, true, true, true) => Rat.- (brat, Rat.one)
                            (* iv > brat increasing *)
                          | (P.CLe, true, false, false) => Rat.+ (brat, Rat.one)
                          | _ => Try.fail ()
                (* trim to step values *)
                val u = Rat.- (u, Rat.rem (Rat.- (u, l), step))
                (* wind back to initial value *)
                val u = Rat./ (Rat.- (u, c), m)
                (* the interval stored is exclusive of u *)
                val u = Rat.+ (u, Rat.one)
              in u
              end)

        fun tripCountToRange (tc as MilLoop.TC {flip1,
                                                     comparison,
                                                    flip2,
                                                    init,
                                                    step,
                                                    bound, ...}) =
            Try.try (fn () =>
              let
                val (m, i, c) = init
                val ival = Try.<- (getSimpleValue (dict, i))
                val irat = Try.<- (constToRat (ival))
                val u = constantRange (tc, irat)
                val u = case u
                         of SOME r => BRat r
                          | NONE => Try.<- (symbolicRange (tc))
              in (irat, u, step)
              end)

        (* See if we have a trip count associated with this loop,
         * and if so, turn it into a range *)
        val range = Utils.Option.bind (LD.lookup (getEnvTcs (env), l),
                                       tripCountToRange)

        (* For each parameter p and info i:
         * if p is an induction variable for the loop,
         * add some inductoin variable information for it.
         * Otherwise just keep the original information.*)
        val help =
         fn (p, VI i) =>
            if VS.member (ivl, p) then
              mkVarInfo (#deps i,
                         TSome (IV {base = p,
                                    trans = SOME (Rat.one,
                                                  Rat.zero),
                                    range = range}),
                         #read i,
                         #write i,
                         #value i)
            else VI i
      in
        Vector.map2 (params, infos, help)
      end

  (*
   * when entering a loop, initialise the induction variable information
   * when leaving a loop, strip the information again
   *)
  fun deriveBlock (env, dict, target, args) =
      let
        (* Derive the information for the arguments of the goto *)
        val infos = Vector.map (args, fn (arg) => deriveFromSimple (env, dict, arg))
        (* Maps induction variables to the set of blocks in their loop *)
        val loopBlocks = getEnvLoopBlocks (env)
        (* If the argument is an induction variable v:
         *    If the base variable for this info is associated with the loop
         *    block to which we are jumping, keep it.
         *    Otherwise, drop the ivInfo, keeping the rest.
         * If the argument is not an induction variable, keep the info *)
        fun filterArg (VI nfo) =
            case nfo
             of {iv=TSome (IV {base=b, ...}), ...} =>
                if Option.fold (VD.lookup (loopBlocks, b),
                                false,
                                fn (set, _) => LS.member (set, target))
                then VI nfo
                else mkVarInfo (#deps nfo, TNone, #read nfo, #write nfo, #value nfo)
              | _ => VI nfo
       (* The filterered parameter info *)
        val res = Vector.map (infos, filterArg)
        (* Get the induction variables associated with the target block *)
        val ivs = LD.lookup (getEnvIvs (env), target)
        (* Get the parameters of the target block *)
        val Mil.B {parameters, ...} = getEnvBlock (env, target)
      in
        case ivs
         of SOME ivl => addIvInfo (env, dict, ivl, target, parameters, res)  (* Loop header *)
          | NONE => res  (* Not a loop header, just keep the info *)
      end

  (*
   * layout functions
   *)
  fun layoutRead (read) =
      case read
       of SOME (R {source, iv, offset}) =>
          LayoutUtils.sequence (" R[", "]", "/")
                          [I.layoutVariable' source,
                           (case iv
                             of SOME v => I.layoutVariable' v
                              | NONE => Layout.str "--"),
                           Rat.layout offset]
        | NONE => Layout.str ""

  fun layoutWrite (write) =
      let
        fun writeOne (W {source, iv, offset}, r) =
            Layout.seq [LayoutUtils.sequence (" W[", "]", "/")
                                        [(case source
					   of Mil.SVariable v => I.layoutVariable' v
					    | _ => L.str "constant "),
                                         (case iv
                                           of SOME v => I.layoutVariable' v
                                            | NONE => Layout.str "--"),
                                         Rat.layout offset],
                        r]
      in
        List.fold (write, Layout.str "", writeOne)
      end

  fun layoutIv (iv) =
      case iv
       of TSome (IV idx) =>
          LayoutUtils.sequence (" I[", "]", "/")
                           [I.layoutVariable' (#base idx),
                            (case (#trans idx)
                              of SOME (m, c) =>
                                 Layout.seq [Rat.layout m,
                                             Layout.str "*#+",
                                             Rat.layout c]
                               | NONE => Layout.str "--"),
                            (case (#range idx)
                              of SOME (l, BRat u, s) =>
                                 Layout.seq [Layout.str "[",
                                             Rat.layout l,
                                             Layout.str ",",
                                             Rat.layout u,
                                             Layout.str ")/+",
                                             Rat.layout s]
                               | SOME (l, BSymb u, s) =>
                                 Layout.seq [Layout.str "[",
                                             Rat.layout l,
                                             Layout.str ",",
                                             I.layoutVariable' u,
                                             Layout.str ")/+",
                                             Rat.layout s]
                               | NONE => Layout.str "--")]
        | TNone => Layout.str ""
        | TUnknown => Layout.str "I[???]"

  fun layoutValue (v) =
      case v
       of SOME c => Layout.seq [Layout.str " V:", layoutConstant c]
        | NONE => Layout.str ""

  fun layoutVarInfo (env, VI x) =
      let
        val ri = layoutRead (#read x)
        val wi = layoutWrite (#write x)
        val ii = layoutIv (#iv x)
        val vi = layoutValue (#value x)
        val deps = VS.layout (#deps x, I.layoutVariable')
      in
        LayoutUtils.sequence ("(", ")", "") [ri, wi, ii, vi, deps]
      end

  (*
   * the actual analysis
   *)
  structure DependenceAnalysis = MilDataFlowAnalysisF (
                                   type env = localenv
                                   type info = varinfo
                                   val getConfig = getEnvConfig
                                   val passname = myPassname
                                   val indent = indent + 2
                                   val deriveInstr = deriveInstr
                                   val deriveGlobal = deriveGlobal
                                   val deriveFunction = deriveFunction
                                   val deriveBlock = deriveBlock
                                   val emptyInfo = fn (_) => emptyVarInfo
                                   val mergeInfo = mergeVarInfo
                                   val equalInfo = fixedVarInfo
                                   val layoutInfo = layoutVarInfo
                                 )

  (* Loop is currently broken *)
  fun loop (env,
            m as Mil.P {globals, symbolTable, ...},
            f as Mil.F {body, ...},
            loop : Mil.label) =
      let
        val config = getConfig env
        val si = I.SymbolInfo.SiTable symbolTable
        val cfg = MilCfg.build (config, si, body)
        val lbdomtree = MilCfg.getLabelBlockDomTree cfg
        val loops = MilLoop.build (config, si, cfg, lbdomtree)

        val entryL = MilLoop.getEntry loops
(*        val nodes = MilLoop.getAllNodes (loops, entryL)*)
        val loops = MilLoop.genAllNodes loops
        val nodes = MilLoop.allNodes loops
        val exits = MilLoop.allExits loops
        val ivs = MilLoop.inductionVars loops
        val tcs = MilLoop.allTripCounts loops

        val ldomtree = Tree.map (lbdomtree, fn (l, b) => l)
        val ldominfo = MilCfg.LabelDominance.new ldomtree

      in
        loop' (env, m, f, loop, cfg, ldominfo, nodes, ivs, tcs, loops)
      end
  (* loop'' requires that all the exits, induction variables, nodes, and trip
     counts have already been set up *)
  and loop'' (env,
              m as Mil.P {globals, symbolTable, ...},
              f as Mil.F {body, ...},
              loop : Mil.label,
              loops : MilLoop.t) =
      let
        val config = getConfig env
        val si = I.SymbolInfo.SiTable symbolTable
        val cfg = MilCfg.build (config, si, body)
        val lbdomtree = MilCfg.getLabelBlockDomTree cfg
        val () = Debug.printLayout (config, L.seq [L.str "MDA.loop''"])
        val entryL = MilLoop.getEntry loops
(*        val nodes = MilLoop.getAllNodes (loops, entryL)*)
        val nodes = MilLoop.allNodes loops
        val exits = MilLoop.allExits loops
        val ivs = MilLoop.inductionVars loops
        val tcs = MilLoop.allTripCounts loops

        val ldomtree = Tree.map (lbdomtree, fn (l, b) => l)
        val ldominfo = MilCfg.LabelDominance.new ldomtree
      in
        loop' (env, m, f, loop, cfg, ldominfo, nodes, ivs, tcs, loops)
      end

  and loop' (env,
             m as Mil.P {globals, symbolTable, ...},
             f as Mil.F {body, ...},
             loop : Mil.label,
             cfg,
             ldominfo,
             nodes,
             ivs,
             tcs,
             milloop : MilLoop.t) =
      Try.try (fn () =>
        let
          val config = getConfig env
          val si = I.SymbolInfo.SiTable symbolTable
          val lenv = mkLocalEnv (env, m, body, nodes, ivs, tcs, milloop)
          val restrD = Try.<- (LD.lookup (nodes, loop))
          val restr = LS.fromList (LD.domain (restrD))
          fun initf (env, dict, target) =
              let
                val Mil.B {parameters, ...} = getEnvBlock (env, target)
                val infos = Vector.map (parameters, fn (x) => emptyVarInfo)
                val ivs = Try.<- (LD.lookup (getEnvIvs (env), target))
              in
                addIvInfo (env, dict, ivs, target, parameters, infos)
              end

          val () = Debug.printLayout (config, L.seq [L.str "MDA.loop'", MilLoop.layout (config, si, milloop)])
        in
          DependenceAnalysis.blocks' (lenv, m, f, loop, initf, restr, ldominfo)
        end)

  fun function (env, m as Mil.P {globals, symbolTable, ...}, f as Mil.F {body, ...}) =
      let
        val config = getConfig env
        val si = I.SymbolInfo.SiTable symbolTable
        val cfg = MilCfg.build (config, si, body)
        val lbdomtree = MilCfg.getLabelBlockDomTree cfg
        val milloop = MilLoop.build (config, si, cfg, lbdomtree)
        val nodes = MilLoop.allNodes milloop
        val exits = MilLoop.allExits milloop
        val ivs = MilLoop.inductionVars milloop
        val tcs = MilLoop.allTripCounts milloop

        val ldomtree = Tree.map (lbdomtree, fn (l, b) => l)
        val ldominfo = MilCfg.LabelDominance.new ldomtree
      in
        function' (env, m, f, cfg, ldominfo, nodes, ivs, tcs, milloop)
      end

  and function' (env, m, f as Mil.F {body, ...}, cfg, ldominfo, nodes, ivs, tcs, milloop) =
      Try.try (fn () =>
        let
          val lenv = mkLocalEnv (env, m, body, nodes, ivs, tcs, milloop)
        in
          DependenceAnalysis.function' (lenv, m, f, ldominfo)
        end)

  (*
   * Routines to extract information from a dependence graph
   *)

  (*
   * Compute all cycles in the graphs using the classical dfs algorithm.
   * Returns a list of list of nodes for each cycle.
   *)
  fun getCycles (dg) =
      let
        val done = ref VS.empty
        fun markDone (n) =
            if VS.member (!done, n) then ()
            else
              let
                val () = done := VS.insert (!done, n)
                val () =
                  case VD.lookup (dg, n)
                   of SOME (VI {deps, ...}) => VS.foreach (deps, markDone)
                    | NONE => ()
              in ()
              end
        fun doSons (n, p) =
            case VD.lookup (dg, n)
             of SOME (VI {deps, ...}) =>
                VS.fold (deps, [], fn (k, r) =>
                                     if VS.member (!done, k) then []
                                     else (doNode (k, p)) @ r)
              | NONE => []
        and doNode (n, p) =
            if (VS.member (!done, n)) then []
            else
              (case (List.index (p, fn (e) => e = n))
                of SOME i => [n :: (List.firstN (p, i))]
                 | NONE =>
                   (case doSons (n, n::p)
                     of [] => let
                                val () = markDone (n)
                              in []
                              end
                      | ls => ls))
        fun trav (n, _, r) =
            if (VS.member (!done, n)) then r
            else
              let
                val res = (doNode (n, [])) @ r
                val () = markDone (n)
              in res
              end
      in VD.fold (dg, [], trav)
      end

  (*
   * Debugging routines.
   *
   * TODO: make the graph conversion a bit less of a quick hack.
   *)

  val debugs = [debugPassD] @ DependenceAnalysis.debugs

  datatype gnode = N of {
           var   : variable,
           iv    : ivinfo tristate,
           read  : read option,
           write : write list,
           value : constant option
  }

  fun toGraph (ddg) =
      let
        val graph = IPLG.new ()
        fun addNode (v, VI {iv, read, write, value, ...}) =
            let
              val lnode = N {var = v, iv = iv, read = read, write = write,
                             value = value}
            in
              IPLG.newNode (graph, lnode)
            end
        val nodes = VD.map (ddg, addNode)
        fun addEdges (v, VI {deps, ...}, nodes) =
            let
              val vn = Option.valOf (VD.lookup (nodes, v))
              fun addEdge (dep, nodes) =
                  case VD.lookup (nodes, dep)
                   of SOME node =>
                      let
                        val _ = IPLG.addEdge (graph, vn, node, ref NONE)
                      in nodes
                      end
                    | NONE =>
                      let
                        val n = N {var = dep, iv = noIvs, read = noReads, write = noWrites, value = noValue}
                        val n = IPLG.newNode (graph, n)
                        val _ = IPLG.addEdge (graph, vn, n, ref NONE)
                      in
                        VD.insert (nodes, dep, n)
                      end
            in
              VS.fold (deps, nodes, addEdge)
            end
        val _ = VD.fold (ddg, nodes, addEdges)
      in graph
      end

  fun layoutDot (ddg) =
      let
        val g = toGraph (ddg)
        fun layoutNode (n) =
            case IPLG.Node.getLabel (n)
             of N x =>
                let
                  val vn = I.layoutVariable' (#var x)
                  val ri = layoutRead (#read x)
                  val wi = layoutWrite (#write x)
                  val ii = layoutIv (#iv x)
                  val vi = layoutValue (#value x)
                in
                  LayoutUtils.sequence ("(", ")", "") [vn, ri, wi, ii, vi]
                end
      in
        IPLG.layoutDot (g, {nodeName = layoutNode, graphTitle = Layout.str "DDG"})
      end

  fun tryIt (env, program as Mil.P {globals, entry, ...}) =
      let
        val code = case Option.valOf (VD.lookup (globals, entry))
                    of Mil.GCode code => SOME code
                     | _ => NONE
        val ddg = Utils.Option.bind (code, fn (c) => function (env, program, c))
        val () = Option.app (ddg, fn (d) => LU.printLayout (layoutDot (d)))
      in program
      end
end

(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_TYPE =
sig

  structure Type :
  sig
    type t = Mil.typ
    val equal : t * t -> bool
    val subtype : Config.t * t * t -> bool
    val lub : Config.t * t * t -> t
    val glb : Config.t * t * t -> t
  end


  structure PrimsTyper :
  sig
    val prim    : Config.t * Mil.Prims.prim -> (Mil.typ Vector.t * Mil.typ Vector.t)
    val runtime : Config.t * Mil.symbolInfo * Mil.Prims.runtime -> (Mil.typ Vector.t * Mil.typ Vector.t)
    val vector  : Config.t * Mil.Prims.vector * Mil.typ Vector.t -> (Mil.typ Vector.t * Mil.typ Vector.t)
    val t       : Config.t * Mil.symbolInfo * Mil.Prims.t * Mil.typ Vector.t -> (Mil.typ Vector.t * Mil.typ Vector.t)
  end

  structure Typer :
  sig
    type ('a, 'b) typer = Config.t * Mil.symbolInfo * 'a -> 'b
    val callConv  : (Mil.variable Mil.callConv, Mil.typ Mil.callConv) typer
    val variable  : (Mil.variable             , Mil.typ             ) typer
    val variables : (Mil.variable Vector.t    , Mil.typ Vector.t    ) typer
    val constant  : (Mil.constant             , Mil.typ             ) typer
    val simple    : (Mil.simple               , Mil.typ             ) typer
    val operand   : (Mil.operand              , Mil.typ             ) typer
    val operands  : (Mil.operand Vector.t     , Mil.typ Vector.t    ) typer
    val global    : (Mil.global               , Mil.typ             ) typer
  end

end;

structure MilType :> MIL_TYPE =
struct

   structure IA = IntArb
   structure ID = IntDict
   structure IS = IntSet
   structure I = Identifier
   structure ND = I.NameDict
   structure M = Mil
   structure MU = MilUtils

   val <- = Try.<-
   val <@ = Try.<@
   val <! = Try.<!
   val << = Try.<<
   val oo = Try.oo
   val om = Try.om
   val or = Try.or
   val || = Try.||
   val @@ = Utils.Function.@@

   infix 3 << @@ oo om <! <\ 
   infixr 3 />
   infix 4 or || 

   structure Chat = ChatF (type env = Config.t
                           fun extract x = x
                           val name = "MilType"
                           val indent = 0)

   structure Type =
   struct

     type t = Mil.typ

     structure TS = MU.TraceabilitySize
     structure MUT = MU.Typ

     fun equal (t1, t2) = MUT.compare (t1, t2) = EQUAL

     fun subtype (c, t1, t2) =
         case (t1, t2)
          of (_, M.TAny) => true
           | (M.TNone, _) => true
           | (_, M.TAnyS vs) =>
             (case MUT.valueSize (c, t1)
               of NONE => false
                | SOME vs' => MU.ValueSize.compare (vs, vs') = EQUAL)
           | (_, M.TNonRefPtr) => MU.Typ.isNonRefPtr t1
           | (_, M.TRef) => MU.Typ.isRef t1
           | (_, M.TBits vs) =>
             (case MUT.traceabilitySize (c, t1)
               of TS.TsAny       => false
                | TS.TsAnyS _    => false
                | TS.TsBits vs'  => MU.ValueSize.compare (vs, vs') = EQUAL
                | TS.TsFloat     => false
                | TS.TsDouble    => false
                | TS.TsRef       => false
                | TS.TsNone      => true
                | TS.TsMask _    => false)
           | (M.TName, M.TName) => true
           | (M.TNumeric nt1, M.TNumeric nt2) => MU.Prims.Utils.NumericTyp.eq (nt1, nt2)
           | (M.TBoolean, M.TBoolean) => true
           | (M.TViVector vet1, M.TViVector vet2) =>
             MU.Prims.Utils.VectorSize.eq (#vectorSize vet1, #vectorSize vet2) andalso
             subtype (c, #elementTyp vet1, #elementTyp vet2)
           | (M.TViMask vet1, M.TViMask vet2) => MU.Prims.Utils.VectorDescriptor.eq (vet1, vet2)
           | (M.TCode {cc = cc1, args = args1, ress = ress1}, M.TCode {cc = cc2, args = args2, ress = ress2}) =>
             (* Technically code might be covariant in free variable types.
              * But it probably does not hurt to make them invariant instead.
              *)
             MU.CallConv.compare MUT.compare (cc1, cc2) = EQUAL andalso
             subtypes (c, args2, args1) andalso
             subtypes (c, ress1, ress2)
           | (M.TTuple {pok = pok1, fixed = ftvs1, array = a1}, M.TTuple {pok = pok2, fixed = ftvs2, array = a2}) =>
             MU.PObjKind.compare (pok1, pok2) = EQUAL andalso
             Vector.size ftvs1 >= Vector.size ftvs2 andalso
             let
               fun checkField ((t1, vs1, v1), (t2, vs2, v2)) =
                   MU.ValueSize.eq (vs1, vs2) andalso 
                   case (v1, v2)
                    of (M.FvReadOnly , M.FvReadOnly ) => subtype (c, t1, t2)
                     | (M.FvReadOnly , M.FvReadWrite) => false
                     | (M.FvReadWrite, M.FvReadOnly ) => subtype (c, t1, t2)
                     | (M.FvReadWrite, M.FvReadWrite) => equal (t1, t2)
               fun checkFields idx =
                   if idx < Vector.size ftvs2 then
                     checkField (Vector.sub (ftvs1, idx), Vector.sub (ftvs2, idx)) andalso
                     checkFields (idx + 1)
                   else
                     checkArray idx
               and checkArray idx =
                   if idx < Vector.size ftvs1 then
                     checkField (Vector.sub (ftvs1, idx), a2) andalso
                     checkArray (idx + 1)
                   else
                     checkField (a1, a2)
             in checkFields 0
             end
           | (M.TCString, M.TCString) => true
           | (M.TIdx, M.TIdx) => true
           | (M.TContinuation ts1, M.TContinuation ts2) => subtypes (c, ts2, ts1)
           | (M.TThunk t1, M.TThunk t2) => subtype (c, t1, t2)
           | (_, M.TPAny) => MU.Typ.isP t1
           | (M.TClosure {args = args1, ress = ress1}, M.TClosure {args = args2, ress = ress2}) =>
             subtypes (c, args2, args1) andalso subtypes (c, ress1, ress2)
           | (M.TSum {tag = t1, arms = a1}, M.TSum {tag = t2, arms = a2}) =>
             let
               (* Seems reasonable to allow subtyping on tag types, but I'm not sure this
                * is a good idea. -leaf 
                *)
               val rec loop =
                fn (a1, a2) =>
                   case (a1, a2)
                    of ([], _)                      => true
                     | (_, [])                      => false
                     | ((c1, v1)::a12, (c2, v2)::a22) => 
                       (case MU.Constant.compare (c1, c2)
                         of LESS    => loop (a12, a2)
                          | GREATER => loop (a1, a22)
                          | EQUAL   => subtypes (c, Vector.prefix (v1, Vector.length v2), v2))
             in equal (t1, t2) andalso loop (Vector.toList a1, Vector.toList a2)
             end
           | (M.TPType {kind = tk1, over = t1}, M.TPType {kind = tk2, over = t2}) =>
             MU.TypKind.compare (tk1, tk2) = EQUAL andalso subtype (c, t1, t2)
           | (M.TPRef t1, M.TPRef t2) => equal (t1, t2)
           | _ => false
     and subtypes (c, ts1, ts2) =
         Vector.size ts1 = Vector.size ts2 andalso
         Vector.forall2 (ts1, ts2, fn (t1, t2) => subtype (c, t1, t2))

     structure Lub =
     struct

     val pObjKind =
      fn (pok1, pok2) => 
         if MU.PObjKind.eq (pok1, pok2) then
           SOME pok1
         else 
           NONE

     val typKind = 
      fn (tk1, tk2) => 
         if MU.TypKind.eq (tk1, tk2) then
           SOME tk1
         else 
           NONE

     val vector = 
         Try.lift 
         (fn (v1, v2, doit) => 
             let
               val () = Try.require (Vector.length v1 = Vector.length v2)
               val v = Vector.map2 (v1, v2, doit)
             in v
             end)

     (* The Lub/Glb code is split into three pieces.  The first function handles
      * the purely structural rules (i.e. no width subtyping, and only
      * comparing like to like) which are dual.  This function is parameterized
      * over the full lub/glb functions which are used to compute lubs/glbs of
      * the sub-components.  By instantiating up/down with lub/glb or glb/lub, 
      * the structural lub rules/glb rules are obtained.
      * 
      * The second two functions implement the non-structural rules for lubs/glbs
      * respectively.  These functions assume that none of the non-structural rules
      * apply (that is, that the structural lub/glb has failed).
      *
      * N.B.  The structural code is deliberately written to cover all cases so that 
      * exhaustiveness checking will catch any missed cases.  Please preserve 
      * this. The other functions are not necessarily of this form, so a match
      * non-exhaustive warning in the structural code should prompt an inspection
      * of the non-structural code as well.
      *)
     val rec structural = 
      fn (up, down) => 
      fn (config, t1, t2) => 
         let
           val up = fn (t1, t2) => up (config, t1, t2)
           val down = fn (t1, t2) => down (config, t1, t2)
           val eq = fn (t1, t2) => if MU.Typ.eq (t1, t2) then SOME t1 else NONE
           val cc = (* Not sure what the right thing is here, equality is safe *)
               fn (cc1, cc2) => if MU.CallConv.eq MU.Typ.eq (cc1, cc2) then SOME cc1 else NONE
           val to = 
               Try.try 
               (fn () => 
                   (case (t1, t2)
                     of (M.TAny, M.TAny) => M.TAny
                      | (M.TAnyS _, M.TAnyS _) => <@ eq (t1, t2)
                      | (M.TNonRefPtr, M.TNonRefPtr) => M.TNonRefPtr
                      | (M.TRef, M.TRef) => M.TRef
                      | (M.TBits _, M.TBits _) => <@ eq (t1, t2)
                      | (M.TNone, M.TNone) => M.TNone
                      | (M.TName, M.TName) => M.TName
                      | (M.TNumeric nt1, M.TNumeric nt2) => <@ eq (t1, t2)
                      | (M.TBoolean, M.TBoolean) => M.TBoolean
                      | (M.TViVector vit1, M.TViVector vit2) => <@ eq (t1, t2)
                      | (M.TViMask vit1, M.TViMask vit2) => <@ eq (t1, t2)
                      | (M.TCode {cc = cc1, args = args1, ress = ress1},
                         M.TCode {cc = cc2, args = args2, ress = ress2}) => 
                        let
                          val cc = <@ cc (cc1, cc2)
                          val args = <@ vector (args1, args2, down)
                          val ress = <@ vector (ress1, ress2, up)
                        in M.TCode {cc = cc, args = args, ress = ress}
                        end
                      | (M.TTuple _, M.TTuple _) => Try.fail () (* handled elsewhere *)
                      | (M.TCString, M.TCString) => M.TCString
                      | (M.TIdx, M.TIdx) => M.TIdx
                      | (M.TContinuation ts1, M.TContinuation ts2) => 
                       let
                         val ts = <@ vector (ts1, ts2, up)
                       in M.TContinuation ts
                       end
                      | (M.TThunk t1, M.TThunk t2) => M.TThunk (up (t2, t2))
                      | (M.TPAny, M.TPAny) => M.TPAny
                      | (M.TClosure {args = args1, ress = ress1}, M.TClosure {args = args2, ress = ress2}) => 
                        let
                          val args = <@ vector (args1, args2, down)
                          val ress = <@ vector (ress1, ress2, up)
                        in M.TClosure {args = args, ress = ress}
                        end
                      | (M.TSum ts1, M.TSum ts2) => Try.fail () (* handled elsewhere *)
                      | (M.TPType {kind = kind1, over = over1}, M.TPType {kind = kind2, over = over2}) => 
                        let
                          val kind = <@ typKind (kind1 ,kind2)
                          val over = up (over1, over2)
                        in M.TPType {kind = kind, over = over}
                        end
                      | (M.TPRef _, M.TPRef _) => <@ eq (t1, t2)
                      | (M.TAny, _) => Try.fail ()           | (M.TAnyS _, _) => Try.fail ()
                      | (M.TNonRefPtr, _) => Try.fail ()     | (M.TRef, _) => Try.fail ()
                      | (M.TBits _, _) => Try.fail ()        | (M.TNone, _) => Try.fail ()
                      | (M.TNumeric _, _) => Try.fail ()     | (M.TBoolean, _) => Try.fail ()
                      | (M.TName, _) => Try.fail ()     
                      | (M.TViVector vit1, _) => Try.fail () | (M.TViMask vit1, _) => Try.fail ()
                      | (M.TCode _, _) => Try.fail ()        | (M.TTuple _, _) => Try.fail ()
                      | (M.TCString, _) => Try.fail ()
                      | (M.TIdx, _) => Try.fail ()           | (M.TContinuation _, _) => Try.fail ()
                      | (M.TThunk _, _) => Try.fail ()       | (M.TPAny, _)   => Try.fail ()
                      | (M.TClosure _, _) => Try.fail ()     | (M.TSum _, _)  => Try.fail ()
                      | (M.TPType _, _) => Try.fail ()       | (M.TPRef _, _) => Try.fail ()
                   )
               )
         in to
         end

     (* In order to make the non-structural glb/lub code more compact, we partition 
      * the types into classes which behave identically wrt lub/glb.  There are 
      * individual partitions for TAny, Mask types, the TAnyS _ types, and TNone. 
      * The TRef, TPtr, TPAny, and TBits types each define a partition which includes
      * themselves and their exact immediate sub-types.  So for example, the SRef
      * class contains TRef, as well as TRat, TInteger, etc; but not TPAny nor any of
      * its immediate sub-types (e.g. TClosure, etc).  The boolean argument to these
      * classes indicate whether or not the summarized type is exact.  So for example,
      * TRef is classified by SRef false, whereas TRat is classified by SRef true.
      *)      
     datatype summary = 
              SRef of bool        (* false => TRef, true => An exact immediate subtype of TRef except TPAny *)
            | SNonRefPtr of bool  (* false => TNonRefPtr, true => An exact immediate subtype of TNonRefPtr *)
            | SPAny of bool       (* false => TPAny, true => An exact immediate subtype of TPAny *)
            | SBits of bool       (* false => TBits, true => An exact immediate subtype of TBits except TNonRefPtr *)
            | SFloat              (* TFloat *)
            | SDouble             (* TDouble *)
            | SMask               (* TMask *)
            | SAny                (* TAny *)
            | SSized              (* TAnyS *)
            | SNone               (* TNone *)

     val summarizeNumericTyp = 
      fn (config, nt) => 
         (case nt
           of M.Prims.NtRat            => SRef true
            | M.Prims.NtInteger ip     => 
              (case ip
                of M.Prims.IpArbitrary => SRef true
                 | M.Prims.IpFixed ia  => SBits true)
            | M.Prims.NtFloat fp       => 
              (case fp
                of M.Prims.FpSingle    => SFloat
                 | M.Prims.FpDouble    => SDouble))

     val summarize =
      fn (config, t) => 
         (case t
           of M.TAny                       => SAny
            | M.TAnyS vs                   => SSized
            | M.TNonRefPtr                 => SNonRefPtr false
            | M.TRef                       => SRef false
            | M.TBits vs                   => SBits false
            | M.TNone                      => SNone
            | M.TName                      => SPAny true
            | M.TNumeric nt                => summarizeNumericTyp (config, nt)
            | M.TBoolean                   => SBits true
            | M.TViVector et               => SBits true
            | M.TViMask et                 => SMask
            | M.TCode {cc, args, ress}     => SNonRefPtr true
            | M.TTuple {pok, fixed, array} => if MU.Typ.isP t then SPAny true else SRef true
            | M.TCString                   => SNonRefPtr true
            | M.TIdx                       => SRef true
            | M.TContinuation ts           => SNonRefPtr true
            | M.TThunk t                   => SRef true
            | M.TPAny                      => SPAny false
            | M.TClosure {args, ress}      => SPAny true
            | M.TSum nts                   => SPAny true
            | M.TPType {kind, over}        => SPAny true
            | M.TPRef t                    => SPAny true)

     (* This code handles the non-structural lub cases *)
     val rec lubLossy = 
      fn (config, t1, t2) => 
         let
           val lub = fn (t1, t2) => lub (config, t1, t2)
           val glb = fn (t1, t2) => glb (config, t1, t2)
           val eq = fn (t1, t2) => if MU.Typ.eq (t1, t2) then SOME t1 else NONE
           val sub = fn (t1, t2) => if subtype (config, t1, t2) then SOME t2 else NONE

           val field = 
               (fn ((t1, vs1, fv1), (t2, vs2, fv2)) => 
                   if MU.ValueSize.eq (vs1, vs2) then 
                     SOME (case (fv1, fv2)
                            of (M.FvReadOnly, M.FvReadOnly) => (lub (t1, t2), vs1, M.FvReadOnly)
                             | (M.FvReadOnly, M.FvReadWrite) => (lub (t1, t2), vs1, M.FvReadOnly)
                             | (M.FvReadWrite, M.FvReadOnly) => (lub (t2, t1), vs1, M.FvReadOnly)
                             | (M.FvReadWrite, M.FvReadWrite) =>
                               if MU.Typ.eq (t1, t2) then (t1, vs1, M.FvReadWrite) 
                               else (lub (t1, t2), vs1, M.FvReadOnly))
                   else NONE)

          (*
           * LUB: t1 v t2
           *
           * <a1, ..., an, R1> v <b1, ..., bn, ..., bm, R2> = <a1 v b1, ..., an v bn, R3>
           * where
           *  R3 = [] if R1 = [] or R2 = []
           *     = [ak v bk v bn+1 v ... v bm] if R1 = [ak] and R2 = [bk]
           *)
           val tTupleWidth =
               Try.lift 
               (fn ({pok = pok1, fixed = fixed1, array = array1},
                    {pok = pok2, fixed = fixed2, array = array2}) => 
                   let
                     val pok = <@ pObjKind (pok1, pok2)
                     val len = Int.min (Vector.length fixed1, Vector.length fixed2)
                     val (fixed1, extras1) = Utils.Vector.split (fixed1, len)
                     val (fixed2, extras2) = Utils.Vector.split (fixed2, len)
                     (* One is empty *)
                     val extras = Vector.concat [extras1, extras2]
                     val fixed = Vector.map2 (fixed1, fixed2, <@ field)
                     val array = Vector.fold (extras, <@ field (array1, array2), <@ field)
                   in M.TTuple {pok = pok, fixed = fixed, array = array}
                   end)
               
           val t = 
               (case (t1, t2)
                  (* Width subtyping on tuples *)
                 of (M.TTuple r1, M.TTuple r2) =>
                     (case tTupleWidth (r1, r2)
                       of SOME t => t
                        | NONE => if MU.Typ.isP t1 andalso MU.Typ.isP t2 then M.TPAny else M.TRef)

                  (* Sum widening *)
                  | (M.TSum {tag = t1, arms = a1}, M.TSum {tag = t2, arms = a2}) => 
                    if not (equal (t1, t2)) then if MU.Typ.isP t1 andalso MU.Typ.isP t2 then M.TPAny else M.TRef
                    else
                      let
                        val combine = 
                         fn (v1, v2) => 
                            let
                              val n = Int.min (Vector.length v1, Vector.length v2)
                              val v1 = Vector.prefix (v1, n)
                              val v2 = Vector.prefix (v2, n)
                              val v = Vector.map2 (v1, v2, lub)
                            in v
                            end
                        val tag = t1
                        val arms = Utils.SortedVectorMap.unionWith MU.Constant.compare (a1, a2, combine)
                      in M.TSum {tag = tag, arms = arms}
                      end

                  (* All other combinations.  Note that by assumption, 
                   * t1 and t2 have different top level structure.
                   *)
                  | _ => 
                    (case (MU.Typ.valueSize (config, t1), MU.Typ.valueSize (config, t2))
                      of (SOME sz1, SOME sz2) => 
                         if MU.ValueSize.eq (sz1, sz2) then  (* Both same sized *)
                           (case (summarize (config, t1), summarize (config, t2))
                             of (SNone, _) => t2
                              | (_, SNone) => t1
                              | (SAny, _) => M.TAny
                              | (_, SAny) => M.TAny
                              | (SMask, _) => M.TAny
                              | (_, SMask) => M.TAny 
                              | (SSized, _) => M.TAnyS sz1 (* Same size *)
                              | (_, SSized) => M.TAnyS sz1 (* Same size *)
                              | (SBits _, SBits _) => M.TBits sz1 (* Same size, both <= Bits *)
                              | (SNonRefPtr _, SBits _) => M.TBits sz1 (* Same size, both <= Bits *)
                              | (SBits _, SNonRefPtr _) => M.TBits sz1 (* Same size, both <= Bits *)
                              | (SBits _, _) => M.TAnyS sz1 (* Same size, t2 not <= Bits *)
                              | (_, SBits _) => M.TAnyS sz1 (* Same size, t1 not <= Bits *)
                              | (SNonRefPtr _, SNonRefPtr _) => M.TNonRefPtr (* Same size, both <= NonRefPtr *)
                              | (SNonRefPtr _, _) => M.TAnyS sz1 (* Same size, t2 not <= Bits *)
                              | (_, SNonRefPtr _) => M.TAnyS sz1 (* Same size, t1 not <= Bits *)
                              | (SFloat, _) => M.TAnyS M.Vs32 (* Same size, t2 not <= TFloat *)
                              | (_, SFloat) => M.TAnyS M.Vs32 (* Same size, t1 not <= TFloat *)
                              | (SDouble, _) => M.TAnyS M.Vs64 (* Same size, t2 not <= TDouble *)
                              | (_, SDouble) => M.TAnyS M.Vs64 (* Same size, t1 not <= TDouble *)
                              | (SRef _, _) => M.TRef (* Same size, t2 must be <= TRef *)
                              | (_, SRef _) => M.TRef (* Same size, t1 must be <= TRef *)
                              | (SPAny _, _) => M.TPAny (* Same size, t2 must be <= TPAny *)
(*                            | (_, SPAny _) => M.TPAny (* Same size, t2 must be <= TPAny *) *)
                           )
                         else
                           M.TAny (* Different sizes, not equal *)
                       | _ => (* At least one is not sized, and they are not equal *)
                         (case (t1, t2)
                           of (M.TNone, _) => t2
                            | (_, M.TNone) => t1
                            | _ => M.TAny (* Both TMask or TAny *)
                         )
                    )
               )
         in t
         end

     (* This code handles the non-structural glb cases *)
     and rec glbLossy = 
      fn (config, t1, t2) => 
         let
           val lub = fn (t1, t2) => lub (config, t1, t2)
           val glb = fn (t1, t2) => glb (config, t1, t2)
           val eq = fn (t1, t2) => if MU.Typ.eq (t1, t2) then SOME t1 else NONE
           val sub = fn (t1, t2) => if subtype (config, t1, t2) then SOME t1 else NONE

           val field = 
               Try.lift 
               (fn ((t1, vs1, fv1), (t2, vs2, fv2)) => 
                   if MU.ValueSize.eq (vs1, vs2) then 
                     (case (fv1, fv2)
                       of (M.FvReadOnly, M.FvReadOnly) => (glb (t1, t2), vs1, M.FvReadOnly)
                        | (M.FvReadOnly, M.FvReadWrite) => (<@ sub (t2, t1), vs1, M.FvReadWrite)
                        | (M.FvReadWrite, M.FvReadOnly) => (<@ sub (t1, t2), vs1, M.FvReadWrite)
                        | (M.FvReadWrite, M.FvReadWrite) => (<@ eq (t1, t2), vs1, M.FvReadWrite))
                   else Try.fail ())
               
           (* GLB: t1 ^ t2
            * <a1, ..., an, R1> ^ <b1, ..., bn, ..., bm, R2> = <a1 ^ b1, ..., an ^ bn, cn+1, ..., cm, R3>
            * where
            *   R3 = [] if R1 = [] and R2 = []
            *      = [aa] if R1 = [aa] and R2 = [] or vice versa
            *      = [aa ^ bb] if R1=[aa] and R2=[bb]
            * and
            *   ci = bi ^ aa if R1 = [aa]
            *      = bi otherwise
            *)
           val rec tTupleWidth =
            fn (r1 as {pok = pok1, fixed = fixed1, array = array1},
                r2 as {pok = pok2, fixed = fixed2, array = array2}) => 
               if (Vector.length fixed1 > Vector.length fixed2) then
                 tTupleWidth (r2, r1)
               else 
                 Try.try
                   (fn () => 
                       let 
                         val pok = <@ pObjKind (pok1, pok2)
                         val (fixed2, extras2) = Utils.Vector.split (fixed2, Vector.length fixed1)
                         val fixedA = Vector.map2 (fixed1, fixed2, <@ field)
                         val fixedB = Vector.map (extras2, fn f2 => (<@ field (array1, f2)))
                         val fixed = Vector.concat [fixedA, fixedB]
                         val array = <@ field (array1, array2)
                       in M.TTuple {pok = pok, fixed = fixed, array = array}
                       end)
               
           val t = 
               (case (t1, t2)
                  (* Width subtyping on tuples *)
                 of (M.TTuple r1, M.TTuple r2) =>
                     (case tTupleWidth (r1, r2)
                       of SOME t => t
                        | NONE => M.TNone)

                  (* Sum narrowing *)
                  | (M.TSum {tag = t1, arms = a1}, M.TSum {tag = t2, arms = a2}) => 
                    if not (equal (t1, t2)) then M.TNone
                    else
                      let
                        val combine = 
                         fn (v1, v2) => 
                            let
                              val n = Int.min (Vector.length v1, Vector.length v2)
                              val (v1, extra1) = Utils.Vector.split (v1, n)
                              val (v2, extra2) = Utils.Vector.split (v2, n)
                              val v = Vector.map2 (v1, v2, glb)
                              val v = Vector.concat [v, extra1, extra2]
                            in v
                            end
                        val tag = t1
                        val arms = Utils.SortedVectorMap.intersectWith MU.Constant.compare (a1, a2, combine)
                    in M.TSum {tag = tag, arms = arms}
                    end

                  (* All other combinations.  Note that by assumption, 
                   * t1 and t2 have different top level structure.
                   *)
                  | _ => 
                    (case (MU.Typ.valueSize (config, t1), MU.Typ.valueSize (config, t2))
                      of (SOME sz1, SOME sz2) => 
                         if MU.ValueSize.eq (sz1, sz2) then  (* Both same sized *)
                           (case (summarize (config, t1), summarize (config, t2))
                             of (SNone, _) => M.TNone
                              | (_, SNone) => M.TNone
                              | (SAny, _) => t2
                              | (_, SAny) => t1
                              | (SMask, _) => M.TNone
                              | (_, SMask) => M.TNone
                              | (SFloat, _) => M.TNone
                              | (_, SFloat) => M.TNone
                              | (SDouble, _) => M.TNone
                              | (_, SDouble) => M.TNone
                              | (SSized, _) => t2 (* Same size *)
                              | (_, SSized) => t1 (* Same size *)
                              (* On a bits branch *)
                              | (SBits true, SBits true) => M.TNone (* Unequal exact bit types *)
                              | (SBits false, SBits _) => t2
                              | (SBits _, SBits false) => t1
                              | (SBits true, SNonRefPtr _) => M.TNone
                              | (SNonRefPtr _, SBits true) => M.TNone
                              | (SBits false, SNonRefPtr _) => t2
                              | (SNonRefPtr _, SBits false) => t1
                              | (SBits _, _) => M.TNone
                              | (_, SBits _) => M.TNone
                              | (SNonRefPtr true, SNonRefPtr true) => M.TNone (* Unequal exact non-ref ptrs *)
                              | (SNonRefPtr false, SNonRefPtr _) => t2
                              | (SNonRefPtr _, SNonRefPtr false) => t1
                              | (SNonRefPtr _, _) => M.TNone
                              | (_, SNonRefPtr _) => M.TNone
                              (* On the Ref branch *)
                              | (SPAny true, SPAny true) => M.TNone (* Unequal, exact *)
                              | (SRef true, SRef true)   => M.TNone (* Unequal, exact *)
                              | (SPAny _, SRef true)  => M.TNone (* Unequal, exact *)
                              | (SRef true, SPAny _)  => M.TNone (* Unequal, exact *)
                              | (SPAny false, _) => t1
                              | (_, SPAny false) => t2
                              | (SRef false, _)  => t1
                              | (_, SRef false)  => t2
                           )
                         else
                           M.TNone (* Different sizes *)
                       | _ => 
                         (case (t1, t2)
                           of (_, M.TAny) => t1
                            | (M.TAny, _) => t2
                            | _ => M.TNone (* At least one is not sized, and they are not equal *)
                         )
                    )
               )
         in t
         end

     and rec lub = 
      fn args =>           
         (case structural (lub, glb) args
           of NONE => lubLossy args
            | SOME t => t)

     and rec glb = 
      fn args => 
         (case structural (glb, lub) args
           of NONE => glbLossy args
            | SOME t => t)

     end (* structure Lub  *)

     val lub = Lub.lub
     val glb = Lub.glb

   end


   structure PrimsTyper =
   struct
     structure M = Mil
     structure MP = Mil.Prims
     structure OA = MU.OrdinalArray
     structure POM = PObjectModelHigh
     structure MU = MilUtils
     structure MUP = MilUtils.Prims
     structure PU = MilUtils.Prims.Utils

     val fail = fn (f, m) => Fail.fail ("type.sml", "PrimsTyper."^f, m)

     val tFloat = MUP.NumericTyp.tFloat
     val tRat = MUP.NumericTyp.tRat
     val tInt = MUP.NumericTyp.tIntegerArbitrary
     val tPAny = M.TPAny
     val tSet = POM.OptionSet.typ
     val tArrayV = fn (c, t) => OA.varTyp (c, M.PokArray, t)
     val tUint8 = MUP.NumericTyp.tIntegerFixed (IntArb.T (IntArb.S8, IntArb.Unsigned))
     val tUintp = 
      fn c => MUP.NumericTyp.tIntegerFixed (IntArb.T (Config.targetWordSize' c, IntArb.Unsigned))
     val tSintp = 
      fn c => MUP.NumericTyp.tIntegerFixed (IntArb.T (Config.targetWordSize' c, IntArb.Signed))
     val tMask = fn vd => M.TViMask vd
     val tVector = fn (sz, t) => M.TViVector {vectorSize = sz, elementTyp = t}
     val tCString = M.TCString
     val tString = 
      fn (c, si) =>
         let
           val ord = M.CName (Prims.getOrd si)
           val char = MU.Boxed.t (M.PokRat, tRat)
           val sum = POM.Sum.typ (M.TName, Vector.new1 (ord, Vector.new1 char))
           val str = OA.varTyp (c, M.PokArray, char)
         in str
         end
     val tBoolean = M.TBoolean
     val nullary = Vector.new0 
     val unary = Vector.new1 
     val binary = Vector.new2
     val trinary = Vector.new3
     val mk00 = fn () => (nullary (), nullary ())
     val mk10 = fn a1 => (unary a1, nullary ())
     val mk01 = fn r1 => (nullary (), unary r1)
     val mk11 = fn (a1, r1) => (unary a1, unary r1)
     val mk12 = fn (a1, r1, r2) => (unary a1, binary (r1, r2))
     val mk20 = fn (a1, a2) => (binary (a1, a2), nullary ())
     val mk21 = fn (a1, a2, r1) => (binary (a1, a2), unary r1)
     val mk22 = fn (a1, a2, r1, r2) => (binary (a1, a2), binary (r1, r2))
     val mk30 = fn (a1, a2, a3) => (trinary (a1, a2, a3), nullary ())
     val mk31 = fn (a1, a2, a3, r1) => (trinary (a1, a2, a3), unary r1)


     val rec stringOp = 
      fn (c, p) => 
         let
           val res = 
	       case p
	        of MP.SAllocate      => mk11 (tUintp c, tCString)
	         | MP.SDeallocate    => mk10 tCString
	         | MP.SGetLen        => mk11 (tCString, tUintp c)
	         | MP.SGetChar       => mk21 (tCString, tUintp c, tUint8)
	         | MP.SSetChar       => mk30 (tCString, tUintp c, tUint8)
	         | MP.SEqual         => mk21 (tCString, tCString, tBoolean)
         in
           res
         end

     val logicOp = 
      fn (c, operator, t) => 
         let
           val res = 
               case PU.Arity.logicOp operator
                of PU.Arity.ArAtoA    => mk11 (t, t)
                 | PU.Arity.ArAAtoA   => mk21 (t, t, t)
                 | _                  => fail ("logicOp", "Unexpected logicOp arity")
         in res
         end

     fun nameOp (c : Config.t, no : MP.nameOp) =
         case no
          of MP.NGetString => mk11 (M.TName, M.TCString)
           | MP.NGetHash => mk11 (M.TName, tUintp c)
         
     val rec prim = 
      fn (c, p) => 
         let
           val res = 
	       case p
	        of MP.PNumArith r1   => 
                   let
                     val t = M.TNumeric (#typ r1)
                     val res = 
                         case PU.Arity.arithOp (#operator r1)
                          of PU.Arity.ArAtoA    => mk11 (t, t)
                           | PU.Arity.ArAAtoA   => mk21 (t, t, t)
                           | PU.Arity.ArAAtoB   => fail ("prim", "Unexpected arithOp arity")
                           | PU.Arity.ArOther _ => 
                             (case #operator r1
                               of MP.ADivMod _  => mk22 (t, t, t, t)
                                | _             => fail ("prim", "Unexpected arithOp arity"))
                   in res
                   end
	         | MP.PFloatOp r1    => 
                   let
                     val t = M.TNumeric (MP.NtFloat (#typ r1))
                     val res = 
                         case PU.Arity.floatOp (#operator r1)
                          of PU.Arity.ArAtoA    => mk11 (t, t)
                           | PU.Arity.ArAAtoA   => mk21 (t, t, t)
                           | _                  => fail ("prim", "Unexpected floatOp arity")
                   in res
                   end
	         | MP.PNumCompare r1 => 
                   let
                     val t1 = M.TNumeric (#typ r1)
                     val t2 = M.TBoolean
                     val t = mk21 (t1, t1, t2)
                   in t
                   end
	         | MP.PNumConvert r1 => mk11 (M.TNumeric (#from r1), M.TNumeric (#to r1))
	         | MP.PNumCast r1 => mk11 (M.TNumeric (#from r1), M.TNumeric (#to r1))
	         | MP.PBitwise r1    => 
                   let
                     val t1 = M.TNumeric (MP.NtInteger (#typ r1))
                     val doRotShift =
                      fn () => mk21 (t1, tUint8, t1)
                     val res = 
                         case PU.Arity.bitwiseOp (#operator r1)
                          of PU.Arity.ArAtoA    => mk11 (t1, t1)
                           | PU.Arity.ArAAtoA   => mk21 (t1, t1, t1)
                           | PU.Arity.ArAAtoB   => fail ("prim", "Unexpected bitwiseOp arity")
                           | PU.Arity.ArOther _ => 
                             (case #operator r1
                               of MP.BRotL   => doRotShift ()
                                | MP.BRotR   => doRotShift ()
                                | MP.BShiftL => doRotShift ()
                                | MP.BShiftR => doRotShift ()
                                | _          => fail ("prim", "Unexpected bitwiseOp arity"))
                   in res
                   end
	         | MP.PBoolean r1    => logicOp (c, r1, M.TBoolean)
                 | MP.PName r1       => nameOp (c, r1)
	         | MP.PCString r1    => stringOp (c, r1)
                 | MP.PPtrEq => mk21 (M.TRef, M.TRef, M.TBoolean)
                 | MP.PCondMov => mk31 (M.TBoolean, M.TAny, M.TAny, M.TAny)
         in
           res
         end
         
     val dataOp = 
      fn (c, p, desc, targs) => 
         let
           val t = if Vector.length targs > 0 then 
                     Vector.sub (targs, 0)
                   else
                     fail ("dataOp", "Type argument required for dataOp typing")
           val size = PU.VectorDescriptor.vectorSize desc
           val tv = tVector (size, t)
           val res = 
	       case p
	        of MP.DBroadcast    => mk11 (t, tv)
	         | MP.DVector       => 
                   let
                     val n = PU.VectorDescriptor.elementCount desc
                   in (Vector.new (n, t), unary tv)
                   end
	         | MP.DSub r1       => mk11 (tv, t)
	         | MP.DPermute r1   => mk11 (tv, tv)
	         | MP.DBlend        => mk31 (tMask desc, tv, tv, tv)
	         | MP.DSplit        => 
                   let
                     val size2 = 
                         (case PU.VectorSize.halfSize size 
                           of SOME size => size
                            | NONE => fail ("dataOp", "Vector size can't be split"))
                     val tv2 = tVector (size, t)
                   in mk12 (tv, tv2, tv2)
                   end
	         | MP.DConcat       => 
                   let
                     val size2 = 
                         (case PU.VectorSize.doubleSize size 
                           of SOME size => size
                            | NONE => fail ("dataOp", "Vector size can't be doubled"))
                     val tv2 = tVector (size, t)
                   in mk21 (tv, tv, tv2)
                   end
         in
           res
         end
         
     val vector = 
      fn (c, p, targs) => 
         let
           val unBin = 
            fn (ats, rts) => 
               (case (Vector.length ats, Vector.length rts)
                 of (2, 1) => (Vector.sub (ats, 0), Vector.sub (ats, 1), Vector.sub (rts, 0))
                  | _ => fail ("vector", "Not a binary operator"))

           val unUn = 
            fn (ats, rts) => 
               (case (Vector.length ats, Vector.length rts)
                 of (1, 1) => (Vector.sub (ats, 0), Vector.sub (rts, 0))
                  | _ => fail ("vector", "Not a unary operator"))
               
           val res = 
	       case p
	        of MP.ViPointwise r1   => 
                   let
                     val (ats, rts) = prim (c, #operator r1)
                     val size = PU.VectorDescriptor.vectorSize (#descriptor r1)
                     val ats = Vector.map (ats, fn t => tVector (size, t))
                     val rts = Vector.map (rts, fn t => tVector (size, t))
                     val ats = 
                         if #masked r1 then 
                           Utils.Vector.snoc (ats, tMask (#descriptor r1))
                         else 
                           ats
                   in (ats, rts)
                   end
	         | MP.ViConvert r1     => 
                   let
                     val {to, from} = r1
                     val p = MP.PNumConvert {to = #typ to, from = #typ from}
                     val (t1, t2) = unUn (prim (c, p))
                     val size1 = PU.VectorDescriptor.vectorSize (#descriptor from)
                     val size2 = PU.VectorDescriptor.vectorSize (#descriptor to)
                     val t1 = tVector (size1, t1)
                     val t2 = tVector (size2, t2)
                     val t = mk11 (t1, t2)
                   in t
                   end
	         | MP.ViCast r1     => 
                   let
                     val {to, from} = r1
                     val p = MP.PNumCast {to = #typ to, from = #typ from}
                     val (t1, t2) = unUn (prim (c, p))
                     val size1 = PU.VectorDescriptor.vectorSize (#descriptor from)
                     val size2 = PU.VectorDescriptor.vectorSize (#descriptor to)
                     val t1 = tVector (size1, t1)
                     val t2 = tVector (size2, t2)
                     val t = mk11 (t1, t2)
                   in t
                   end
	         | MP.ViCompare r1     => 
                   let
                     val t1 = M.TNumeric (#typ r1)
                     val size = PU.VectorDescriptor.vectorSize (#descriptor r1)
                     val t1 = tVector (size, t1)
                     val t2 = tMask (#descriptor r1)
                     val t = mk21 (t1, t1, t2)
                   in t
                   end
	         | MP.ViReduction r1   => 
                   let
                     val (t1, _, _) = unBin (prim (c, #operator r1))
                     val size = PU.VectorDescriptor.vectorSize (#descriptor r1)
                     val t2 = tVector (size, t1)
                     val t = mk21 (t1, t2, t1)
                   in t
                   end
	         | MP.ViData r1        => dataOp (c, #operator r1, #descriptor r1, targs)
	         | MP.ViMaskData r1    => dataOp (c, #operator r1, #descriptor r1, targs)
	         | MP.ViMaskBoolean r1 => logicOp (c, #operator r1, tMask (#descriptor r1))
	         | MP.ViMaskConvert r1 => mk11 (tMask (#from r1), tMask (#to r1))
         in
           res
         end

     val runtime = 
      fn (c, si, p) => 
         let
           val res = 
	       case p
	        of MP.RtFloatMk              => mk21 (tRat, tRat, tFloat)
	         | MP.RtWriteln              => mk10 tPAny
	         | MP.RtReadln               => mk01 (tString (c, si))
	         | MP.RtAssert               => mk11 (tSet tPAny, tPAny)
	         | MP.RtError                => mk11 (tPAny, tPAny)
	         | MP.RtDebug                => mk10 tPAny
	         | MP.RtOpenOut              => mk11 ((tString (c, si)), tRat)
	         | MP.RtGetStdout            => mk01 tRat
	         | MP.RtOutputByte           => mk20 (tRat, tRat)
	         | MP.RtCloseOut             => mk10 tRat
	         | MP.RtOpenIn               => mk11 ((tString (c, si)), tRat)
	         | MP.RtGetStdin             => mk01 tRat
	         | MP.RtInputByte            => mk11 (tRat, tRat)
	         | MP.RtInputString          => mk21 (tRat, (tString (c, si)), (tString (c, si)))
	         | MP.RtInputAll             => mk11 (tRat, (tString (c, si)))
	         | MP.RtIsEOF                => mk11 (tRat, tBoolean)
	         | MP.RtCloseIn              => mk10 tRat
	         | MP.RtCommandLine          => mk01 tPAny
	         | MP.RtStringToNat          => mk11 ((tString (c, si)), tRat)
	         | MP.RtStringToFloat        => mk11 ((tString (c, si)), tFloat)
	         | MP.RtFloatToString        => mk21 (tFloat, tRat, (tString (c, si)))
	         | MP.RtFloatToStringI       => mk21 (tFloat, tSintp c, (tString (c, si)))
	         | MP.RtRatNumerator         => mk11 (tRat, tRat)
	         | MP.RtRatDenominator       => mk11 (tRat, tRat)
	         | MP.RtEqual                => mk21 (tRat, tRat, tBoolean)
	         | MP.RtDom                  => mk11 (tArrayV (c, tPAny), tSet tPAny)
	         | MP.RtNub                  => mk11 (tSet tPAny, tRat)
	         | MP.RtRatToUIntpChecked    => mk11 (tRat, tUintp c)
	         | MP.RtRatToString          => mk11 (tRat, (tString (c, si)))
	         | MP.RtStringToRat          => mk11 ((tString (c, si)), tRat)
	         | MP.RtResetTimer           => mk10 tRat
	         | MP.RtGetTimer             => mk11 (tRat, tFloat)
	         | MP.RtVtuneAttach          => mk00 ()
	         | MP.RtVtuneDetach          => mk00 ()
	         | MP.RtArrayEval            => mk10 (tArrayV (c, tPAny))
                 | MP.RtIntegerHash          => mk11 (tInt, tUintp c)
         in
           res
         end
         
     val t = 
      fn (c, si, p, targs) => 
         let
           val res = 
	       case p
	        of MP.Prim r1    => prim (c, r1)
	         | MP.Runtime r1 => runtime (c, si, r1)
	         | MP.Vector r1  => vector (c, r1, targs)
         in
           res
         end

   end (* structure PrimsTyper *)

   structure Typer =
   struct

     structure M = Mil
     structure MP = Mil.Prims
     structure MU = MilUtils
     structure MUP = MilUtils.Prims
     structure PU = MilUtils.Prims.Utils

     type ('a, 'b) typer = Config.t * Mil.symbolInfo * 'a -> 'b

     fun variable (config, si, v) = MU.SymbolInfo.variableTyp (si, v)

     fun variables (c, si, vs) = Vector.map (vs, fn v => variable (c, si, v))

     fun callConv (config, si, cc) =
         case cc
          of M.CcCode => M.CcCode
           | M.CcUnmanaged abi => M.CcUnmanaged abi
           | M.CcClosure {cls, fvs} =>
             M.CcClosure {cls = variable (config, si, cls),
                          fvs = variables (config, si, fvs)}
           | M.CcThunk {thunk, fvs} =>
             M.CcThunk {thunk = variable (config, si, thunk),
                        fvs = variables (config, si, fvs)}

     fun constant (config, si, c) = MU.Constant.typOf (config, c)

     fun simple (config, si, s) =
         case s
          of M.SVariable v => variable (config, si, v)
           | M.SConstant c => constant (config, si, c)

     fun simples (config, si, ss) =
         Vector.map (ss, fn s => simple (config, si, s))

     val operand = simple

     fun operands (config, si, os) =
         Vector.map (os, fn opnd => operand (config, si, opnd))

     fun global (config, si, g) =
         case g
          of M.GCode f =>
             M.TCode {cc = callConv (config, si, MU.Code.cc f),
                      args = variables (config, si, MU.Code.args f),
                      ress = MU.Code.rtyps f}
           | M.GErrorVal t => t
           | M.GIdx _ => M.TIdx
           | M.GTuple {mdDesc, inits} => 
             let
               val pok = MU.MetaDataDescriptor.pok mdDesc
               val getFD = fn i => MU.MetaDataDescriptor.fixedField (mdDesc, i)
               val get = fn i => let val fd = getFD i 
                                 in (MU.FieldDescriptor.alignment fd, MU.FieldDescriptor.var fd) 
                                 end
               val s = simples (config, si, inits)
               val l = Vector.mapi (s, fn (i, t) => let val (vs, vr) = get i in (t, vs, vr) end)
             in  MU.Typ.fixedArray (pok, l)
             end
           | M.GRat _ => MUP.NumericTyp.tRat
           | M.GInteger _ => MUP.NumericTyp.tIntegerArbitrary
           | M.GCString _ => M.TCString
           | M.GThunkValue {typ, ofVal} =>
             M.TThunk (simple (config, si, ofVal))
           | M.GSimple s => simple (config, si, s)
           | M.GClosure {code = NONE, ...} => M.TPAny
           | M.GClosure {code = SOME f, ...} => 
             (case variable (config, si, f)
               of M.TCode {args, ress, ...} =>
                  M.TClosure {args = args, ress = ress}
                | _ => M.TPAny)
           | M.GSum {tag, typs, ofVals} =>
             M.TSum {tag = constant (config, si, tag),
                     arms = Vector.new1 (tag, simples (config, si, ofVals))}
           | M.GPSet s =>
             M.TPType {kind = M.TkE, over = simple (config, si, s)}

   end

end

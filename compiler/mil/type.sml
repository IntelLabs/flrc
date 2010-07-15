(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_TYPE =
sig

  structure Type :
  sig
    type t = Mil.typ
    val isPPObjKind : Config.t * Mil.pObjKind -> bool
    val isPType : Config.t * Mil.typ -> bool
    val equal : t * t -> bool
    val subtype : Config.t * t * t -> bool
    val lub : Config.t * t * t -> t
    val glb : Config.t * t * t -> t
    val equalVectorElemType : Config.t * t * VectorInstructions.elemType -> bool
  end


  structure Prim :
  sig
    val typToMilTyp : Config.t * Mil.name (* ord *) * Prims.typ -> Mil.typ
    val subtypeIn : Config.t * Mil.name (* ord *) * Mil.typ * Prims.typ -> bool
    val subtypeOut : Config.t * Mil.name (* ord *) * Prims.typ * Mil.typ
                     -> bool

    val resultTypFromPrimTyp : Config.t * Mil.symbolInfo * Prims.primTyp -> Mil.typ option 
    val resultTypOf : Config.t * Mil.symbolInfo * Prims.t -> Mil.typ option option
    val resultTypOfPrim : Config.t * Mil.symbolInfo * Prims.prim -> Mil.typ option 
    val resultTypOfRuntime : Config.t * Mil.symbolInfo * Prims.runtime -> Mil.typ option option
    val resultTypOfVi : Config.t * Mil.symbolInfo * VectorInstructions.prim -> Mil.typ option
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
   structure VI = VectorInstructions
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

     fun isPPObjKind (c, pok) =
         case pok
          of M.PokNone      => false
           | M.PokRat       => true
           | M.PokFloat     => true
           | M.PokDouble    => true
           | M.PokName      => true
           | M.PokFunction  => true
           | M.PokArray     => true
           | M.PokDict      => true
           | M.PokTagged    => true
           | M.PokOptionSet => true
           | M.PokType      => true
           | M.PokPtr       => true
           | M.PokCell      => false

     fun isPType (c, t) =
         case t
          of M.TName             => true
           | M.TTuple {pok, ...} => isPPObjKind (c, pok)
           | M.TPAny             => true
           | M.TPFunction _      => true
           | M.TPSum _           => true
           | M.TPType _          => true
           | M.TPRef _           => true
           | _                   => false

     fun subtype (c, t1, t2) =
         case (t1, t2)
          of (_, M.TAny) => true
           | (M.TNone, _) => true
           | (_, M.TAnyS vs) =>
             (case MUT.valueSize (c, t1)
               of NONE => false
                | SOME vs' => MU.ValueSize.compare (vs, vs') = EQUAL)
           | (_, M.TPtr) =>
             (case MUT.traceabilitySize (c, t1)
               of TS.TsAny       => false
                | TS.TsAnyS _    => false
                | TS.TsBits _    => false
                | TS.TsFloat     => false
                | TS.TsDouble    => false
                | TS.TsPtr       => true
                | TS.TsNonRefPtr => true
                | TS.TsRef       => true
                | TS.TsNone      => true
                | TS.TsMask _    => false)
           | (_, M.TRef) =>
             (case MUT.traceabilitySize (c, t1)
               of TS.TsAny       => false
                | TS.TsAnyS _    => false
                | TS.TsBits _    => false
                | TS.TsFloat     => false
                | TS.TsDouble    => false
                | TS.TsPtr       => false
                | TS.TsNonRefPtr => false
                | TS.TsRef       => true
                | TS.TsNone      => true
                | TS.TsMask _    => false)
           | (_, M.TBits vs) =>
             (case MUT.traceabilitySize (c, t1)
               of TS.TsAny       => false
                | TS.TsAnyS _    => false
                | TS.TsBits vs'  => MU.ValueSize.compare (vs, vs') = EQUAL
                | TS.TsFloat     => false
                | TS.TsDouble    => false
                | TS.TsPtr       => false
                | TS.TsNonRefPtr => false
                | TS.TsRef       => false
                | TS.TsNone      => false
                | TS.TsMask _    => false)
           | (M.TRat, M.TRat) => true
           | (M.TInteger, M.TInteger) => true
           | (M.TName, M.TName) => true
           | (M.TIntegral t1, M.TIntegral t2) => IntArb.compareTyps (t1, t2) = EQUAL
           | (M.TFloat, M.TFloat) => true
           | (M.TDouble, M.TDouble) => true
           | (M.TViVector vet1, M.TViVector vet2) => VI.Compare.elemType (vet1, vet2) = EQUAL
           | (M.TViMask vet1, M.TViMask vet2) => VI.Compare.elemType (vet1, vet2) = EQUAL
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
               fun checkField ((t1, v1), (t2, v2)) =
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
           | (M.TIdx, M.TIdx) => true
           | (M.TContinuation ts1, M.TContinuation ts2) => subtypes (c, ts2, ts1)
           | (M.TThunk t1, M.TThunk t2) => subtype (c, t1, t2)
           | (_, M.TPAny) => isPType (c, t1)
           | (M.TPFunction {args = args1, ress = ress1}, M.TPFunction {args = args2, ress = ress2}) =>
             subtypes (c, args2, args1) andalso subtypes (c, ress1, ress2)
           | (M.TPSum nts1, M.TPSum nts2) =>
             let
               fun checkArm (n, t1) =
                   case ND.lookup (nts2, n)
                    of NONE => false
                     | SOME t2 => subtype (c, t1, t2)
             in
               ND.forall (nts1, checkArm)
             end
           | (M.TPType {kind = tk1, over = t1}, M.TPType {kind = tk2, over = t2}) =>
             MU.TypKind.compare (tk1, tk2) = EQUAL andalso subtype (c, t1, t2)
           | (M.TPRef t1, M.TPRef t2) => equal (t1, t2)
           | _ => false
     and subtypes (c, ts1, ts2) =
         Vector.size ts1 = Vector.size ts2 andalso
         Vector.forall2 (ts1, ts2, fn (t1, t2) => subtype (c, t1, t2))

     fun equalVectorElemType (config, t1, t2) =
         case (t1, t2)
          of (M.TIntegral (IA.T (IA.S8 , IA.Unsigned)), VI.ViUInt8  ) => true
           | (M.TIntegral (IA.T (IA.S16, IA.Unsigned)), VI.ViUInt16 ) => true
           | (M.TIntegral (IA.T (IA.S32, IA.Unsigned)), VI.ViUInt32 ) => true
           | (M.TIntegral (IA.T (IA.S64, IA.Unsigned)), VI.ViUInt64 ) => true
           | (M.TIntegral (IA.T (IA.S8 , IA.Signed  )), VI.ViSInt8  ) => true
           | (M.TIntegral (IA.T (IA.S16, IA.Signed  )), VI.ViSInt16 ) => true
           | (M.TIntegral (IA.T (IA.S32, IA.Signed  )), VI.ViSInt32 ) => true
           | (M.TIntegral (IA.T (IA.S64, IA.Signed  )), VI.ViSInt64 ) => true
           | (M.TFloat                                , VI.ViFloat32) => true
           | (M.TDouble                               , VI.ViFloat64) => true
           | _                                                        => false

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
                      | (M.TPtr, M.TPtr) => M.TPtr
                      | (M.TRef, M.TRef) => M.TRef
                      | (M.TBits _, M.TBits _) => <@ eq (t1, t2)
                      | (M.TNone, M.TNone) => M.TNone
                      | (M.TRat, M.TRat) => M.TRat
                      | (M.TInteger, M.TInteger) => M.TInteger
                      | (M.TName, M.TName) => M.TName
                      | (M.TIntegral _, M.TIntegral _) => <@ eq (t1, t2)
                      | (M.TFloat, M.TFloat) => M.TFloat
                      | (M.TDouble, M.TDouble) => M.TDouble
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
                      | (M.TIdx, M.TIdx) => M.TIdx
                      | (M.TContinuation ts1, M.TContinuation ts2) => 
                       let
                         val ts = <@ vector (ts1, ts2, up)
                       in M.TContinuation ts
                       end
                      | (M.TThunk t1, M.TThunk t2) => M.TThunk (up (t2, t2))
                      | (M.TPAny, M.TPAny) => M.TPAny
                      | (M.TPFunction {args = args1, ress = ress1}, M.TPFunction {args = args2, ress = ress2}) => 
                        let
                          val args = <@ vector (args1, args2, down)
                          val ress = <@ vector (ress1, ress2, up)
                        in M.TPFunction {args = args, ress = ress}
                        end
                      | (M.TPSum ts1, M.TPSum ts2) => Try.fail () (* handled elsewhere *)
                      | (M.TPType {kind = kind1, over = over1}, M.TPType {kind = kind2, over = over2}) => 
                        let
                          val kind = <@ typKind (kind1 ,kind2)
                          val over = up (over1, over2)
                        in M.TPType {kind = kind, over = over}
                        end
                      | (M.TPRef _, M.TPRef _) => <@ eq (t1, t2)
                      | (M.TAny, _) => Try.fail ()           | (M.TAnyS _, _) => Try.fail ()
                      | (M.TPtr, _) => Try.fail ()           | (M.TRef, _) => Try.fail ()
                      | (M.TBits _, _) => Try.fail ()        | (M.TNone, _) => Try.fail ()
                      | (M.TRat, _) => Try.fail ()           | (M.TInteger, _) => Try.fail ()
                      | (M.TName, _) => Try.fail ()          | (M.TIntegral _, _) => Try.fail ()
                      | (M.TFloat, _) => Try.fail ()         | (M.TDouble, _) => Try.fail ()
                      | (M.TViVector vit1, _) => Try.fail () | (M.TViMask vit1, _) => Try.fail ()
                      | (M.TCode _, _) => Try.fail ()        | (M.TTuple _, _) => Try.fail ()
                      | (M.TIdx, _) => Try.fail ()           | (M.TContinuation _, _) => Try.fail ()
                      | (M.TThunk _, _) => Try.fail ()       | (M.TPAny, _) => Try.fail ()
                      | (M.TPFunction _, _) => Try.fail ()   | (M.TPSum _, _) => Try.fail ()
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
      * its immediate sub-types (e.g. TPFunction, etc).  The boolean argument to these
      * classes indicate whether or not the summarized type is exact.  So for example,
      * TRef is classified by SRef false, whereas TRat is classified by SRef true.
      *)      
     datatype summary = 
              SRef of bool  (* false => TRef, true => An exact immediate subtype of TRef *)
            | SPtr of bool  (* false => TPtr, true => An exact immediate subtype of TPtr *)
            | SPAny of bool (* false => TPAny, true => An exact immediate subtype of TPAny *)
            | SBits of bool (* false => TBits, true => An exact immediate subtype of TBits *)
            | SFloat  (* TFloat *)
            | SDouble (* TDouble *)
            | SMask   (* TMask *)
            | SAny    (* TAny *)
            | SSized  (* TAnyS *)
            | SNone   (* TNone *)

     val summarize =
      fn (config, t) => 
         (case t
           of M.TAny                       => SAny
            | M.TAnyS vs                   => SSized
            | M.TPtr                       => SPtr false
            | M.TRef                       => SRef false
            | M.TBits vs                   => SBits false
            | M.TNone                      => SNone
            | M.TRat                       => SRef true
            | M.TInteger                   => SRef true
            | M.TName                      => SRef true
            | M.TIntegral sz               => SBits true
            | M.TFloat                     => SFloat
            | M.TDouble                    => SDouble
            | M.TViVector et               => SBits true
            | M.TViMask et                 => SMask
            | M.TCode {cc, args, ress}     => SPtr true
            | M.TTuple {pok, fixed, array} => if isPType (config, t) then 
                                                SPAny true
                                              else
                                                SRef true
            | M.TIdx                       => SRef true
            | M.TContinuation ts           => SPtr true
            | M.TThunk t                   => SRef true
            | M.TPAny                      => SPAny false
            | M.TPFunction {args, ress}    => SPAny true
            | M.TPSum nts                  => SPAny true
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
               (fn ((t1, fv1), (t2, fv2)) => 
                   (case (fv1, fv2)
                     of (M.FvReadOnly, M.FvReadOnly) => (lub (t1, t2), M.FvReadOnly)
                      | (M.FvReadOnly, M.FvReadWrite) => (lub (t1, t2), M.FvReadOnly)
                      | (M.FvReadWrite, M.FvReadOnly) => (lub (t2, t1), M.FvReadOnly)
                      | (M.FvReadWrite, M.FvReadWrite) =>
                        if MU.Typ.eq (t1, t2) then (t1, M.FvReadWrite) else (lub (t1, t2), M.FvReadOnly)))

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
                     val fixed = Vector.map2 (fixed1, fixed2, field)
                     val array = Vector.fold (extras, field (array1, array2), field)
                   in M.TTuple {pok = pok, fixed = fixed, array = array}
                   end)
               
           val t = 
               (case (t1, t2)
                  (* Width subtyping on tuples *)
                 of (M.TTuple r1, M.TTuple r2) =>
                     (case tTupleWidth (r1, r2)
                       of SOME t => t
                        | NONE => 
                          if isPType (config, t1) andalso isPType (config, t2) then 
                            M.TPAny
                          else
                            M.TRef)

                  (* Sum widening *)
                  | (M.TPSum d1, M.TPSum d2) => 
                    let
                      val combine = (fn (nm, op1, op2) => 
                                        (case (op1, op2)
                                          of (NONE, _) => op2
                                           | (_, NONE) => op1
                                           | (SOME t1, SOME t2) => SOME (lub (t1, t2))))
                      val d = ND.map2 (d1, d2, combine)
                    in M.TPSum d
                    end

                  (* All other combinations.  Note that by assumption, 
                   * t1 and t2 have different top level structure, and neither is TNone. *)
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
                              | (SBits ex1, SBits ex2) => M.TBits sz1 (* Same size, both <= Bits *)
                              | (SBits _, _) => M.TAnyS sz1 (* Same size, t2 not <= Bits *)
                              | (_, SBits _) => M.TAnyS sz1 (* Same size, t1 not <= Bits *)
                              | (SFloat, _) => M.TAnyS M.Vs32 (* Same size, t2 not <= TFloat *)
                              | (_, SFloat) => M.TAnyS M.Vs32 (* Same size, t1 not <= TFloat *)
                              | (SDouble, _) => M.TAnyS M.Vs64 (* Same size, t2 not <= TDouble *)
                              | (_, SDouble) => M.TAnyS M.Vs64 (* Same size, t1 not <= TDouble *)
                              | (SPtr _, _) => M.TPtr (* Same size, t2 <> TAny, TAnyS, TBits, FP so t2 <= t1*)
                              | (_, SPtr _) => M.TPtr (* Same size, t1 <> TAny, TAnyS, TBits, FP so t1 <= t2*)
                              | (SRef _, _) => M.TRef (* Same size, t2 <> TAny, TAnyS, TBits, TPtr, FP so t2 <= t1*)
                              | (_, SRef _) => M.TRef (* Same size, t1 <> TAny, TAnyS, TBits, TPtr, FP so t1 <= t2*)
                              | (SPAny _, _) => M.TPAny
                                                  (* Same size, t2 <> TAny, TAnyS, TBits, TPtr, TRef, FP so t2 <= t1*)
(*                            | (_, SPAny _) => M.TPAny
                                                  (* Same size, t1 <> TAny, TAnyS, TBits, TPtr, TRef, FP so t1 <= t2*)
*)
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
               (fn ((t1, fv1), (t2, fv2)) => 
                   (case (fv1, fv2)
                     of (M.FvReadOnly, M.FvReadOnly) => (glb (t1, t2), M.FvReadOnly)
                      | (M.FvReadOnly, M.FvReadWrite) => (<@ sub (t2, t1), M.FvReadWrite)
                      | (M.FvReadWrite, M.FvReadOnly) => (<@ sub (t1, t2), M.FvReadWrite)
                      | (M.FvReadWrite, M.FvReadWrite) => (<@ eq (t1, t2), M.FvReadWrite)))

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
                  | (M.TPSum d1, M.TPSum d2) => 
                    let
                      val combine = (fn (nm, op1, op2) => 
                                        (case (op1, op2)
                                          of (NONE, _) => NONE
                                           | (_, NONE) => NONE
                                           | (SOME t1, SOME t2) => SOME (glb (t1, t2))))
                      val d = ND.map2 (d1, d2, combine)
                    in M.TPSum d
                    end

                  (* All other combinations.  Note that by assumption, 
                   * t1 and t2 have different top level structure *)
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
                              | (SBits _, _) => M.TNone
                              | (_, SBits _) => M.TNone

                              (* On the Ptr branch *)

                              | (SPAny true, SPAny true) => M.TNone (* Unequal, exact *)
                              | (SRef true, SRef true)   => M.TNone (* Unequal, exact *)
                              | (SPtr true, SPtr true)   => M.TNone (* Unequal, exact *)

                              | (SPAny true, SRef true)  => M.TNone (* Unequal, exact *)
                              | (SRef true, SPAny true)  => M.TNone (* Unequal, exact *)

                              | (SPAny true, SPtr true)  => M.TNone (* Unequal, exact *)
                              | (SPtr true, SPAny true)  => M.TNone (* Unequal, exact *)

                              | (SRef true, SPtr true)  => M.TNone (* Unequal, exact *)
                              | (SPtr true, SRef true)  => M.TNone (* Unequal, exact *)

                              | (SPAny false, _) => t1
                              | (_, SPAny false) => t2

                              | (SRef false, _)  => t1
                              | (_, SRef false)  => t2

                              | (SPtr false, _)  => t1
                              | (_, SPtr false)  => t2
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

   structure Prim =
   struct

      structure P = Prims
      structure OA = MU.OrdinalArray

      fun typToMilTyp (c, ord, t) =
          case t
           of P.TAny => M.TPAny
            | P.TNum P.NtRat => M.TRat
            | P.TNum P.NtInteger => M.TInteger
            | P.TNum (P.NtIntegral t) => M.TIntegral t
            | P.TNum P.NtFloat => M.TFloat
            | P.TNum P.NtDouble => M.TDouble
            | P.TString =>
              let
                val char = MU.Boxed.t (M.PokRat, M.TRat)
                val sum = M.TPSum (ND.singleton (ord, char))
                val str = OA.varTyp (c, M.PokArray, char)
              in str
              end
            | P.TBool => MU.Uintp.t c
            | P.TArrayF ts =>
              let
                fun doOne t =  typToMilTyp (c, ord, t)
                val ts = Vector.fromListMap (ts, doOne)
                val t = OA.fixedTyp (c, M.PokArray, ts)
              in t
              end
            | P.TArrayV t =>
              OA.varTyp (c, M.PokArray, typToMilTyp (c, ord, t))
            | P.TRef t => M.TPRef (typToMilTyp (c, ord, t))
            | P.TSet t =>
              M.TPType {kind = M.TkE, over = typToMilTyp (c, ord, t)}
            | P.TViVector et => M.TViVector et
            | P.TViMask et => M.TViMask et
            | P.TVoid =>
              Fail.fail ("MilTyp.Prim", "typToMilTyp",
                         "void is not a Mil type")


      (* XXX NG: this doesn't work after lowering *)
      fun equalTyp (config, ord, t1, t2) =
          Type.equal (t1, typToMilTyp (config, ord, t2))

      (* XXX NG: this doesn't work after lowering *)
      fun subtypeIn (config, ord, t1, t2) =
          Type.subtype (config, t1, typToMilTyp (config, ord, t2))

      (* XXX NG: this doesn't work after lowering *)
      fun subtypeOut (config, ord, t1, t2) =
          Type.subtype (config, typToMilTyp (config, ord, t1), t2)

      val resultTypFromPrimTyp = 
       fn (config, si, t) => 
          (case t
            of (_, _, P.TVoid) => NONE
             | (_, _, t) => SOME (typToMilTyp (config, Prims.getOrd si, t)))

      val resultTypOfPrim = 
       fn (config, si, p) => 
          let
            val t = Prims.typeOfPrim (config, p) 
            val to = resultTypFromPrimTyp (config, si, t)
          in to
          end

      val resultTypOfRuntime =
       fn (config, si, rt) => 
          let
            val to = 
                (case Prims.typeOfRuntime (config, rt) 
                  of SOME t => SOME (resultTypFromPrimTyp (config, si, t))
                   | NONE => NONE)
          in to
          end

      val resultTypOfVi = 
       fn (config, si, vi) => 
          let
            val t = Prims.typeOfVi (config, vi) 
            val to = resultTypFromPrimTyp (config, si, t)
          in to
          end

      val resultTypOf =
       fn (config, si, p) => 
          (case p
            of P.Prim p => SOME (resultTypOfPrim (config, si, p))
             | P.Runtime rt => resultTypOfRuntime (config, si, rt)
             | P.Vi vi => SOME (resultTypOfVi (config, si, vi)))

   end

   structure Typer =
   struct

     type ('a, 'b) typer = Config.t * Mil.symbolInfo * 'a -> 'b

     fun variable (config, si, v) = MU.SymbolInfo.variableTyp (si, v)

     fun variables (c, si, vs) = Vector.map (vs, fn v => variable (c, si, v))

     fun callConv (config, si, cc) =
         case cc
          of M.CcCode => M.CcCode
           | M.CcClosure {cls, fvs} =>
             M.CcClosure {cls = variable (config, si, cls),
                          fvs = variables (config, si, fvs)}
           | M.CcThunk {thunk, fvs} =>
             M.CcThunk {thunk = variable (config, si, thunk),
                        fvs = variables (config, si, fvs)}

     fun constant (config, si, c) =
         case c
          of M.CRat _          => M.TRat
           | M.CInteger _      => M.TInteger
           | M.CName _         => M.TName
           | M.CIntegral i     => M.TIntegral (IntArb.typOf i)
           | M.CFloat _        => M.TFloat
           | M.CDouble _       => M.TDouble
           | M.CViVector x     => M.TViVector (#typ x)
           | M.CViMask x       => M.TViMask (#typ x)
           | M.CPok _          => MU.Uintp.t config
           | M.COptionSetEmpty => M.TPType {kind = M.TkE, over = M.TNone}
           | M.CTypePH         => M.TPType {kind = M.TkI, over = M.TNone}

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
           | M.GTuple {vtDesc, inits} => 
             MU.Typ.fixedArray (MU.VTableDescriptor.pok vtDesc,
                                Vector.map (simples (config, si, inits), fn t => (t, M.FvReadWrite)))
           | M.GRat _ => M.TRat
           | M.GInteger _ => M.TInteger
           | M.GThunkValue {typ, ofVal} =>
             M.TThunk (simple (config, si, ofVal))
           | M.GSimple s => simple (config, si, s)
           | M.GPFunction {code = NONE, ...} => M.TPAny
           | M.GPFunction {code = SOME f, ...} => 
             (case variable (config, si, f)
               of M.TCode {args, ress, ...} =>
                  M.TPFunction {args = args, ress = ress}
                | _ => M.TPAny)
           | M.GPSum {tag, typ, ofVal} =>
             M.TPSum (ND.singleton (tag, simple (config, si, ofVal)))
           | M.GPSet s =>
             M.TPType {kind = M.TkE, over = simple (config, si, s)}

   end

(*
   fun variable (st, v) = getTyp (st, v)

   fun variables (st, vs) = 
       Vector.map (vs,
                  (fn v => variable (st, v)))


   fun constant (st, c) = 
       (case c
         of M.CIntegral i      => M.TIntegral (IntArb.typOf i)
          | M.CName n          => MD.T.ptName
          | M.CFloat f         => M.TFloat
          | M.CDouble d        => M.TDouble
          | M.COptionSetEmpty  => M.TPObj (M.PtType (M.TkE, M.TPBottom))
          | M.CTypePH          => M.TPObj (M.PtType (M.TkI, M.TPBottom))
          | M.CViVector (t, _) => M.TViVector t
          | M.CViMask (t, _)   => M.TViMask t)


   fun simple (st, s)   = 
       (case s
         of M.SConstant c => constant (st, c)
          | M.SVariable v => variable (st, v)
          | M.SCoerce (t, _) => t)

   fun operand (st, s)  = simple (st, s)

   fun operands (st, ops) = 
       Vector.map (ops,
                  (fn oper => operand (st, oper)))


   local
     fun warn (state, s) = 
         (Chat.warn1 
            (state, "Strange elim type: " ^ s);
          M.TPAny)
     fun fail s = 
         Fail.fail ("typeof.sml", "fail", "Strange elim type: " ^ s)
   in

   fun pObjElim (state, t) = 
       (case t
         of M.PtRat    => warn (state, "PtRat")
          | M.PtName   => warn (state, "PtName")
          | M.PtFloat  => warn (state, "PtFloat")
          | M.PtDouble => warn (state, "PtDouble")
          | M.PtFunction (_, rt) =>
            if (Vector.length rt) = 1 then
              Vector.sub (rt, 0)
            else
              warn (state, "TCode")
          | M.PtSum _   => warn (state, "PtSum")
          | M.PtArray t => t
          | M.PtArrayFixed _ => warn (state, "PtFixedArray")
          | M.PtArrayIdx t => t
          | M.PtArrayIdxFixed _ => warn (state, "PtFixedIdxArray")
          | M.PtType (_, t) => t)
       
                     
   fun elim (state, t) = 
       let
         
         val res = 
             case t
              of M.TPAny      => warn (state, "TPAny")
               | M.TPBottom   => warn (state, "TPBottom")
               | M.TPUnion vs => warn (state, "TPUnion")
               | M.TPObj po   => pObjElim (state, po)
               | M.TCode (_, _, ts) => 
                 if (Vector.length ts) = 1 then
                   Vector.sub (ts, 0)
                 else
                   fail "TCode"
               | M.TThunk t   => t

               (* Unboxed things have no safe fallback *)
               | M.TIntegral _     => fail "TIntegral"
               | M.TFloat          => fail "TFloat"
               | M.TDouble         => fail "TDouble"
               | M.TTuple _        => fail "TTuple"
               | M.TSum _          => fail "TSum"
               | M.TIdx            => fail "TIdx"
               | M.TContinuation _ => fail "TContinuation"
               | M.TViVector _     => fail "TViVector"
               | M.TViMask _       => fail "TViMask"
       in 
           res
       end

   end

   fun call (st, s)     = 
       let
         val t = 
             case s
              of M.CCode v               => elim (st, variable (st, v))
               | M.CClosure v            => elim (st, variable (st, v))
               | M.CExtern n             => Fail.fail ("MilTypeOf", "call",
                                                       "extern")
               | M.CDirectClosure (v, _) => elim (st, variable (st, v))

       in t
       end

   fun callConv (state, c) = 
       let
         val t = 
             case c
              of M.CcCode => M.CcCode
               | M.CcExtern => M.CcExtern
               | M.CcClosure (v, vs) => M.CcClosure (variable (state, v), 
                                                     variables (state, vs))
               | M.CcThunk (v, vs) => M.CcThunk (variable (state, v), 
                                                 variables (state, vs))
               | M.CcBulkSpawn => M.CcBulkSpawn
       in t
       end

   val global =
    fn (state, g) => 
       case g
        of M.GSimple oper => operand (state, oper)
         | M.GCode (M.F {effects, 
                         escapes, 
                         recursive,
                         conv,
                         args,
                         rtyps,
                         body}) => M.TCode (callConv (state, conv),
                                            variables (state, args),
                                            rtyps)
         | M.GIdx _ => M.TIdx
         | M.GTuple (po, opers) => M.TTuple (po, 
                                             operands (state, opers),
                                             NONE)
         | M.GThunkValue s => M.TThunk (operand (state, s))
         | M.GPFunction NONE => M.TPAny
         | M.GPFunction (SOME f) => 
           (case variable (state, f)
             of M.TCode (_, args, rtyps) => MD.T.ptFunction (args, rtyps)
              | _ => M.TPAny)
         | M.GPRat r => MD.T.ptRat
         | M.GPSum (nm, opers) => 
           MD.T.pSum (ND.singleton (nm, 
                                    operands (state, 
                                              opers)))
         | M.GPSet oper => MD.T.pType (M.TkE, 
                                       operand (state, 
                                                oper))

   fun eqTK (state, tk1, tk2) = tk1 = tk2

   fun eqTV (state, tv1, tv2) = 
       Vector.equals (tv1, tv2, (fn (t1, t2) => eqT (state, t1, t2)))
   and eqTL (state, tv1, tv2) = 
       List.equals (tv1, tv2, (fn (t1, t2) => eqT (state, t1, t2)))
   and eqTCC (state, tcc1, tcc2) =
       (case (tcc1, tcc2)
         of (M.CcCode, M.CcCode) => true
          | (M.CcExtern, M.CcExtern) => true
          | (M.CcClosure (t1, tv1), M.CcClosure (t2, tv2)) =>
            eqT (state, t1, t2) andalso
            eqTV (state, tv1, tv2)
          | (M.CcThunk (t1, tv1), M.CcThunk (t2, tv2)) =>
            eqT (state, t1, t2) andalso
            eqTV (state, tv1, tv2)
          | _ => false)
   and eqPObj (state, po1, po2) = 
       (case (po1, po2)
         of (M.PtRat, M.PtRat) => true
          | (M.PtName, M.PtName) => true
          | (M.PtFloat, M.PtFloat) => true
          | (M.PtDouble, M.PtDouble) => true
          | (M.PtFunction (ts11, ts12), 
             M.PtFunction (ts21, ts22)) => 
            eqTV (state, ts11, ts21) andalso eqTV (state, ts12, ts22)
          | (M.PtSum tvd1, M.PtSum tvd2) => 
            List.equals (ND.toList tvd1, ND.toList tvd2,
                         (fn ((v1, tv1), (v2, tv2)) => 
                             (v1 = v2 andalso
                              eqTV (state, tv1, tv2))))
          | (M.PtArray t1, M.PtArray t2) => eqT (state, t1, t2)
          | (M.PtArrayFixed tv1, M.PtArrayFixed tv2) => 
            eqTV (state, tv1, tv2)
          | (M.PtArrayIdx t1, M.PtArrayIdx t2) => eqT (state, t1, t2)
          | (M.PtArrayIdxFixed (d1, tv1), M.PtArrayIdxFixed (d2, tv2)) => 
            List.equals (ND.toList d1, ND.toList d2, op =) andalso
            eqTV (state, tv1, tv2)
          | (M.PtType (tk1, t1), M.PtType (tk2, t2)) => 
            eqTK (state, tk1, tk2) andalso
            eqT (state, t1, t2)
          | _ => false)
   and eqT (state, t1, t2) = 
       (case (t1, t2) 
         of (M.TPAny, M.TPAny) => true
          | (M.TPBottom , M.TPBottom ) => true
          | (M.TPUnion tv1, M.TPUnion tv2) => eqTV (state, tv1, tv2)
          | (M.TPObj po1, M.TPObj po2) => eqPObj (state, po1, po2)
          | (M.TIntegral t1, M.TIntegral t2) => IntArb.equalTyps (t1, t2)
          | (M.TFloat, M.TFloat) => true
          | (M.TDouble, M.TDouble) => true
          | (M.TTuple (poo1, tv1, to1), 
             M.TTuple (poo2, tv2, to2)) => 
            Option.equals (poo1, poo2, 
                        fn (po1, po2) => eqPObj (state, po1, po2)) andalso
            eqTV (state, tv1, tv2) andalso
            Option.equals (to1, to2, fn (t1, t2) => eqT (state, t1, t2))
          | (M.TSum (s1, poo1, tvv1), M.TSum (s2, poo2, tvv2)) => 
            List.equals (IS.toList s1, IS.toList s2, op =) andalso
            Option.equals (poo1, poo2, 
                        fn (po1, po2) => eqPObj (state, po1, po2)) andalso
            Vector.equals (tvv1, tvv2, fn (tv1, tv2) => eqTV (state, tv1, tv2))
          | (M.TCode (tcc1, tv11, tv12), M.TCode (tcc2, tv21, tv22)) => 
            eqTCC (state, tcc1, tcc2) andalso
            eqTV (state, tv11, tv21) andalso
            eqTV (state, tv12, tv22)
          | (M.TIdx , M.TIdx ) => true
          | (M.TThunk t1, M.TThunk t2) => eqT (state, t1, t2)
          | (M.TContinuation tv1, M.TContinuation tv2) => 
            eqTV (state, tv1, tv2)
          | _ => false)
*)

end

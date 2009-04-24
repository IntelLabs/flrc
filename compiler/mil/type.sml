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
(*    val lub : Config.t * t * t -> t
    val glb : Config.t * t * t -> t*)
    val equalVectorElemType :
        Config.t * t * VectorInstructions.elemType -> bool
  end

  structure Prim :
  sig
    val typToMilTyp : Config.t * Mil.name (* ord *) * Prims.typ -> Mil.typ
    val subtypeIn : Config.t * Mil.name (* ord *) * Mil.typ * Prims.typ -> bool
    val subtypeOut : Config.t * Mil.name (* ord *) * Prims.typ * Mil.typ
                     -> bool
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
(*
  val call      : state * Mil.call -> Mil.typ        
  val elim      : state * Mil.typ -> Mil.typ
*)
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

   structure Chat = ChatF (type env = Config.t
                           fun extract x = x
                           val name = "MilType"
                           val indent = 0)

   structure Type =
   struct

     type t = Mil.typ

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
           | M.PokOArray    => true
           | M.PokIArray    => true
           | M.PokSum       => true
           | M.PokOptionSet => true
           | M.PokType      => true
           | M.PokThunk     => false
           | M.PokRef       => true

     fun isPType (c, t) =
         case t
          of M.TName => true
           | M.TTuple {pok, ...} => isPPObjKind (c, pok)
           | M.TPAny => true
           | M.TPFunction _ => true
           | M.TPSum _ => true
           | M.TPType _ => true
           | M.TPRef _ => true
           | _ => false

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
               of MUT.TsAny       => false
                | MUT.TsAnyS _    => false
                | MUT.TsBits _    => false
                | MUT.TsPtr       => true
                | MUT.TsNonRefPtr => true
                | MUT.TsRef       => true
                | MUT.TsNone      => true
                | MUT.TsMask _    => false)
           | (_, M.TRef) =>
             (case MUT.traceabilitySize (c, t1)
               of MUT.TsAny       => false
                | MUT.TsAnyS _    => false
                | MUT.TsBits _    => false
                | MUT.TsPtr       => false
                | MUT.TsNonRefPtr => false
                | MUT.TsRef       => true
                | MUT.TsNone      => true
                | MUT.TsMask _    => false)
           | (_, M.TBits vs) =>
             (case MUT.traceabilitySize (c, t1)
               of MUT.TsAny       => false
                | MUT.TsAnyS _    => false
                | MUT.TsBits vs'  => MU.ValueSize.compare (vs, vs') = EQUAL
                | MUT.TsPtr       => false
                | MUT.TsNonRefPtr => false
                | MUT.TsRef       => false
                | MUT.TsNone      => false
                | MUT.TsMask _    => false)
           | (M.TRat, M.TRat) => true
           | (M.TInteger, M.TInteger) => true
           | (M.TName, M.TName) => true
           | (M.TIntegral t1, M.TIntegral t2) =>
             IntArb.compareTyps (t1, t2) = EQUAL
           | (M.TFloat, M.TFloat) => true
           | (M.TDouble, M.TDouble) => true
           | (M.TViVector vet1, M.TViVector vet2) =>
             VI.Compare.elemType (vet1, vet2) = EQUAL
           | (M.TViMask vet1, M.TViMask vet2) =>
             VI.Compare.elemType (vet1, vet2) = EQUAL
           | (M.TCode {cc = cc1, args = args1, ress = ress1},
              M.TCode {cc = cc2, args = args2, ress = ress2}) =>
             (* Technically code might be covariant in free variable types.
              * But it probably does not hurt to make them invariant instead.
              *)
             MU.CallConv.compare MUT.compare (cc1, cc2) = EQUAL andalso
             subtypes (c, args2, args1) andalso
             subtypes (c, ress1, ress2)
           | (M.TTuple {pok = pok1, fixed = ftvs1, array = a1},
              M.TTuple {pok = pok2, fixed = ftvs2, array = a2}) =>
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
                     checkField (Vector.sub (ftvs1, idx),
                                 Vector.sub (ftvs2, idx)) andalso
                     checkFields (idx + 1)
                   else
                     checkArray idx
               and checkArray idx =
                   case a2
                    of NONE => true
                     | SOME tv => checkArray' (idx, tv)
               and checkArray' (idx, tv) =
                   if idx < Vector.size ftvs1 then
                     checkField (Vector.sub (ftvs1, idx), tv) andalso
                     checkArray' (idx + 1, tv)
                   else
                     case a1
                      of NONE => false
                       | SOME tv' => checkField (tv', tv)
             in checkFields 0
             end
           | (M.TIdx, M.TIdx) => true
           | (M.TContinuation ts1, M.TContinuation ts2) =>
             subtypes (c, ts2, ts1)
           | (M.TThunk t1, M.TThunk t2) => subtype (c, t1, t2)
           | (_, M.TPAny) => isPType (c, t1)
           | (M.TPFunction {args = args1, ress = ress1},
              M.TPFunction {args = args2, ress = ress2}) =>
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
           | (M.TPType {kind = tk1, over = t1},
              M.TPType {kind = tk2, over = t2}) =>
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
                val str = OA.varTyp (c, M.PokOArray, char)
              in str
              end
            | P.TBool => MU.Uintp.t c
            | P.TArrayF ts =>
              let
                fun doOne t =  typToMilTyp (c, ord, t)
                val ts = Vector.fromListMap (ts, doOne)
                val t = OA.fixedTyp (c, M.PokOArray, ts)
              in t
              end
            | P.TArrayV t =>
              OA.varTyp (c, M.PokOArray, typToMilTyp (c, ord, t))
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
             M.TTuple {pok = MU.VtableDescriptor.pok vtDesc,
                       fixed = Vector.map (simples (config, si, inits),
                                           fn t => (t, M.FvReadWrite)),
                       array = NONE}
           | M.GRat _ => M.TRat
           | M.GInteger _ => M.TInteger
           | M.GThunkValue {typ, ofVal} =>
             M.TThunk (simple (config, si, ofVal))
           | M.GSimple s => simple (config, si, s)
           | M.GPFunction NONE => M.TPAny
           | M.GPFunction (SOME f) => 
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

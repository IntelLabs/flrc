(* COPYRIGHT_NOTICE_1 *)
(* 
 * Definition of primitive types from GHC.Prim.
 *)
signature GHC_PRIM_TYPE = 
sig
  datatype 'a primTy = Bool | Int | Int64 | Word | Word64 | Char | Addr | Double | 
                    Float | ByteArray | MutableByteArray of 'a | Array of 'a | MutableArray of 'a * 'a | 
                    ImmutableArray of 'a | StrictImmutableArray of 'a |
                    UnboxedWordArray | UnboxedWord8Array | UnboxedWord16Array | UnboxedWord32Array |
                    UnboxedWord64Array | UnboxedIntArray | UnboxedInt8Array | UnboxedInt16Array | UnboxedInt32Array |
                    UnboxedInt64Array | UnboxedFloatArray | UnboxedDoubleArray | UnboxedCharArray | UnboxedAddrArray |
                    WeakPtr of 'a | StablePtr of 'a | StableName | State of 'a | MutVar of 'a * 'a | MVar of 'a * 'a | 
                    ThreadId | Tuple of 'a list | Integer | Ref | LiftedKind | EqTy of 'a * 'a * 'a 

  val hsToPrimTy : ((CoreHs.ty -> 'a) * CoreHs.ty) -> 'a primTy option
  val mapPrimTy : ('a primTy * ('a -> 'b)) -> 'b primTy 
  val eqPrimTy : ('a * 'a -> bool) -> 'a primTy * 'a primTy -> bool
  val hashPrimTy : ('a -> word) -> 'a primTy -> word
end

structure GHCPrimType :> GHC_PRIM_TYPE =
struct
  structure CH = CoreHs
  structure CHU = CoreHsUtils
  structure CHP = CoreHsPrims
  structure CL = CoreHsLayout

  datatype 'a primTy = Bool | Int | Int64 | Word | Word64 | Char | Addr | Double | 
                    Float | ByteArray | MutableByteArray of 'a | Array of 'a | MutableArray of 'a * 'a | 
                    ImmutableArray of 'a | StrictImmutableArray of 'a |
                    UnboxedWordArray | UnboxedWord8Array | UnboxedWord16Array | UnboxedWord32Array |
                    UnboxedWord64Array | UnboxedIntArray | UnboxedInt8Array | UnboxedInt16Array | UnboxedInt32Array |
                    UnboxedInt64Array | UnboxedFloatArray | UnboxedDoubleArray | UnboxedCharArray | UnboxedAddrArray |
                    WeakPtr of 'a | StablePtr of 'a | StableName | State of 'a | MutVar of 'a * 'a | MVar of 'a * 'a | 
                    ThreadId | Tuple of 'a list | Integer | Ref | LiftedKind | EqTy of 'a * 'a * 'a 

  val hsPrimTy0 = [ (CHP.tIntzh,                Int)
                  , (CHP.tInt64zh,              Int64)
                  , (CHP.tAddrzh,               Addr)
                  , (CHP.tWordzh,               Word)
                  , (CHP.tWord64zh,             Word64)
                  , (CHP.tCharzh,               Char)
                  , (CHP.tFloatzh,              Float)
                  , (CHP.tDoublezh,             Double)
                  , (CHP.tIntegerzh,            Integer)
                  , (CHP.tThreadIdzh,           ThreadId)
                  , (CHP.tByteArrayzh,          ByteArray)
                  , (CHP.tUnboxedWordArrayzh,   UnboxedWordArray)
                  , (CHP.tUnboxedWord8Arrayzh,  UnboxedWord8Array)
                  , (CHP.tUnboxedWord16Arrayzh, UnboxedWord16Array)
                  , (CHP.tUnboxedWord32Arrayzh, UnboxedWord32Array)
                  , (CHP.tUnboxedWord64Arrayzh, UnboxedWord64Array)
                  , (CHP.tUnboxedIntArrayzh,    UnboxedIntArray)
                  , (CHP.tUnboxedInt8Arrayzh,   UnboxedInt8Array)
                  , (CHP.tUnboxedInt16Arrayzh,  UnboxedInt16Array)
                  , (CHP.tUnboxedInt32Arrayzh,  UnboxedInt32Array)
                  , (CHP.tUnboxedInt64Arrayzh,  UnboxedInt64Array)
                  , (CHP.tUnboxedFloatArrayzh,  UnboxedFloatArray)
                  , (CHP.tUnboxedDoubleArrayzh, UnboxedDoubleArray)
                  , (CHP.tUnboxedCharArrayzh,   UnboxedCharArray)
                  , (CHP.tUnboxedAddrArrayzh,   UnboxedAddrArray)
                  , (CHP.tLiftedzh,             LiftedKind)
(*                  , (CHP.tAny,                  Ref) *)
                  ]
  val hsPrimTy1 = [ (CH.Tcon CHP.tcArrayzh,                Array)
                  , (CH.Tcon CHP.tcImmutableArrayzh,       ImmutableArray)
                  , (CH.Tcon CHP.tcStrictImmutableArrayzh, StrictImmutableArray)
                  , (CH.Tcon CHP.tcStatezh,                State)
                  , (CH.Tcon CHP.tcWeakPtrzh,              WeakPtr)
                  , (CH.Tcon CHP.tcStablePtrzh,            StablePtr)
                  , (CH.Tcon CHP.tcMutableByteArrayzh,     MutableByteArray)
(*                  , (CH.Tcon CHP.tcAny,                    fn _ => Ref) *)
                  ]
  val hsPrimTy2 = [ (CH.Tcon CHP.tcMVarzh,         MVar)
                  , (CH.Tcon CHP.tcTVarzh,         MVar)
                  , (CH.Tcon CHP.tcMutVarzh,       MutVar)
                  , (CH.Tcon CHP.tcMutableArrayzh, MutableArray)
                  ]
  val hsPrimTy3 = [ (CH.Tcon CHP.tcEqzh, EqTy) ]

  fun hsToPrimTy (toTy, t) = 
      let
        fun lookup (((k,v)::xs), w) = if k = w then SOME v else lookup (xs, w)
          | lookup ([], w)          = NONE
      in
        case t
          of CH.Tapp (CH.Tapp (CH.Tapp (f, a), b), c) =>
            (case lookup (hsPrimTy3, f)
              of SOME f => SOME (f (toTy a, toTy b, toTy c))
               | _ => NONE)
           | CH.Tapp (CH.Tapp (f, a), b) =>
            (case lookup (hsPrimTy2, f)
              of SOME f => SOME (f (toTy a, toTy b))
              | _ => NONE)
           | CH.Tapp (f, a) => 
            (case lookup (hsPrimTy1, f)
              of SOME f => SOME (f (toTy a))
               | _ => NONE)
           | _ => 
            (case lookup (hsPrimTy0, t)
              of SOME t => SOME t
               | _ => NONE)
      end

  fun mapPrimTy (ty, f) =    
      (case ty 
        of Int                    => Int
        |  Int64                  => Int64
        |  Integer                => Integer
        |  Bool                   => Bool
        |  Word                   => Word
        |  Word64                 => Word64
        |  Char                   => Char
        |  Addr                   => Addr
        |  Double                 => Double
        |  Float                  => Float
        |  Array a                => Array (f a)
        |  ImmutableArray a       => ImmutableArray (f a)
        |  StrictImmutableArray a => StrictImmutableArray (f a)
        |  UnboxedWordArray       => UnboxedWordArray
        |  UnboxedWord8Array      => UnboxedWord8Array
        |  UnboxedWord16Array     => UnboxedWord16Array
        |  UnboxedWord32Array     => UnboxedWord32Array
        |  UnboxedWord64Array     => UnboxedWord64Array
        |  UnboxedIntArray        => UnboxedIntArray
        |  UnboxedInt8Array       => UnboxedInt8Array
        |  UnboxedInt16Array      => UnboxedInt16Array
        |  UnboxedInt32Array      => UnboxedInt32Array
        |  UnboxedInt64Array      => UnboxedInt64Array
        |  UnboxedFloatArray      => UnboxedFloatArray
        |  UnboxedDoubleArray     => UnboxedDoubleArray
        |  UnboxedCharArray       => UnboxedCharArray
        |  UnboxedAddrArray       => UnboxedAddrArray
        |  ByteArray              => ByteArray
        |  MutableArray (a,b)     => MutableArray (f a, f b)
        |  MutableByteArray a     => MutableByteArray (f a)
        |  ThreadId               => ThreadId 
        |  State a                => State (f a)
        |  WeakPtr a              => WeakPtr (f a)
        |  StablePtr a            => StablePtr (f a)
        |  StableName             => StableName
        |  MVar (a, b)            => MVar (f a, f b)
        |  MutVar (a, b)          => MutVar (f a, f b)
        |  Tuple tys              => Tuple (List.map (tys, f))
        |  Ref                    => Ref
        |  LiftedKind             => LiftedKind
        |  EqTy (a, b, c)         => EqTy (f a, f b, f c))

fun eqPrimTy f (ty1, ty2) =    
      (case (ty1, ty2)
        of (Int                    , Int) => true
        |  (Int64                  , Int64) => true
        |  (Integer                , Integer) => true
        |  (Bool                   , Bool) => true
        |  (Word                   , Word) => true
        |  (Word64                 , Word64) => true
        |  (Char                   , Char) => true
        |  (Addr                   , Addr) => true
        |  (Double                 , Double) => true
        |  (Float                  , Float) => true
        |  (Array a                , Array u) => f (a, u)
        |  (ImmutableArray a       , ImmutableArray u) => f (a, u)
        |  (StrictImmutableArray a , StrictImmutableArray u) => f (a, u)
        |  (UnboxedWordArray       , UnboxedWordArray) => true
        |  (UnboxedWord8Array      , UnboxedWord8Array) => true
        |  (UnboxedWord16Array     , UnboxedWord16Array) => true
        |  (UnboxedWord32Array     , UnboxedWord32Array) => true
        |  (UnboxedWord64Array     , UnboxedWord64Array) => true
        |  (UnboxedIntArray        , UnboxedIntArray) => true
        |  (UnboxedInt8Array       , UnboxedInt8Array) => true
        |  (UnboxedInt16Array      , UnboxedInt16Array) => true
        |  (UnboxedInt32Array      , UnboxedInt32Array) => true
        |  (UnboxedInt64Array      , UnboxedInt64Array) => true
        |  (UnboxedFloatArray      , UnboxedFloatArray) => true
        |  (UnboxedDoubleArray     , UnboxedDoubleArray) => true
        |  (UnboxedCharArray       , UnboxedCharArray) => true
        |  (UnboxedAddrArray       , UnboxedAddrArray) => true
        |  (ByteArray              , ByteArray) => true
        |  (MutableArray (a,b)     , MutableArray (u,v)) => f (a, u) andalso f (b, v)
        |  (MutableByteArray a     , MutableByteArray u) => f (a, u)
        |  (ThreadId               , ThreadId ) => true
        |  (State a                , State u) => f (a, u)
        |  (WeakPtr a              , WeakPtr u) => f (a, u)
        |  (StablePtr a            , StablePtr u) => f (a, u)
        |  (StableName             , StableName) => true
        |  (MVar (a, b)            , MVar (u, v)) => f (a, u) andalso f (b, v)
        |  (MutVar (a, b)          , MutVar (u, v)) => f (a, u) andalso f (b, v)
        |  (Tuple tys1             , Tuple tys2) => List.forall (List.zip (tys1, tys2), f)
        |  (Ref                    , Ref) => true
        |  (LiftedKind             , LiftedKind) => true
        |  (EqTy (a, b, c)         , EqTy (u, v, w)) => f (a, u) andalso f (b, v) andalso f (c, w)
        |  _ => false)

  val hashPrimTy = fn f => fn ty =>
      (case ty 
        of Int                    => 0w1
        |  Int64                  => 0w2
        |  Integer                => 0w3
        |  Bool                   => 0w4
        |  Word                   => 0w5
        |  Word64                 => 0w6
        |  Char                   => 0w7
        |  Addr                   => 0w8
        |  Double                 => 0w9
        |  Float                  => 0w10
        |  Array a                => TypeRep.hash2 (0w11, f a)
        |  ImmutableArray a       => TypeRep.hash2 (0w12, f a)
        |  StrictImmutableArray a => TypeRep.hash2 (0w13, f a)
        |  UnboxedWordArray       => 0w14
        |  UnboxedWord8Array      => 0w15
        |  UnboxedWord16Array     => 0w16
        |  UnboxedWord32Array     => 0w17
        |  UnboxedWord64Array     => 0w18
        |  UnboxedIntArray        => 0w19
        |  UnboxedInt8Array       => 0w20
        |  UnboxedInt16Array      => 0w21
        |  UnboxedInt32Array      => 0w22
        |  UnboxedInt64Array      => 0w23
        |  UnboxedFloatArray      => 0w24
        |  UnboxedDoubleArray     => 0w25
        |  UnboxedCharArray       => 0w26
        |  UnboxedAddrArray       => 0w27
        |  ByteArray              => 0w28
        |  MutableArray (a,b)     => TypeRep.hash3 (0w29, f a, f b)
        |  MutableByteArray a     => TypeRep.hash2 (0w30, f a)
        |  ThreadId               => 0w31
        |  State a                => TypeRep.hash2 (0w32, f a)
        |  WeakPtr a              => TypeRep.hash2 (0w33, f a)
        |  StablePtr a            => TypeRep.hash2 (0w34, f a)
        |  StableName             => 0w35
        |  MVar (a, b)            => TypeRep.hash3 (0w36, f a, f b)
        |  MutVar (a, b)          => TypeRep.hash3 (0w37, f a, f b)
        |  Tuple tys              => TypeRep.hashList (0w38 :: List.map (tys, f))
        |  Ref                    => 0w39
        |  LiftedKind             => 0w40
        |  EqTy (a, b, c)         => TypeRep.hash4 (0w41, f a, f b, f c))

end

signature GHC_PRIM_TYPE_LAYOUT =
sig
  val layoutPrimTy : ('a -> Layout.t) -> 'a GHCPrimType.primTy -> Layout.t
end

structure GHCPrimTypeLayout :> GHC_PRIM_TYPE_LAYOUT =
struct
  open GHCPrimType
  structure L = Layout

  fun layoutPrimTy layoutTy 
    = fn Int                    => L.str "#int"
      |  Int64                  => L.str "#int64"
      |  Integer                => L.str "#integer"
      |  Bool                   => L.str "#bool"
      |  Word                   => L.str "#word"
      |  Word64                 => L.str "#word64"
      |  Char                   => L.str "#char"
      |  Addr                   => L.str "#addr"
      |  Double                 => L.str "#double"
      |  Float                  => L.str "#float"
      |  Ref                    => L.str "#ref"
      |  LiftedKind             => L.str "*"
      |  Array a                => L.seq [L.str "#array ", layoutTy a]
      |  ImmutableArray a       => L.seq [L.str "#immutablearray ", layoutTy a]
      |  StrictImmutableArray a => L.seq [L.str "#strictimmutablearray ", layoutTy a]
      |  UnboxedWordArray       => L.str "#unboxedwordarray"
      |  UnboxedWord8Array      => L.str "#unboxedword8array"
      |  UnboxedWord16Array     => L.str "#unboxedword16array"
      |  UnboxedWord32Array     => L.str "#unboxedword32array"
      |  UnboxedWord64Array     => L.str "#unboxedword64array"
      |  UnboxedIntArray        => L.str "#unboxedintarray"
      |  UnboxedInt8Array       => L.str "#unboxedint8array"
      |  UnboxedInt16Array      => L.str "#unboxedint16array"
      |  UnboxedInt32Array      => L.str "#unboxedint32array"
      |  UnboxedInt64Array      => L.str "#unboxedint64array"
      |  UnboxedFloatArray      => L.str "#unboxedfloatarray"
      |  UnboxedDoubleArray     => L.str "#unboxeddoublearray"
      |  UnboxedCharArray       => L.str "#unboxedchararray"
      |  UnboxedAddrArray       => L.str "#unboxedaddrarray"
      |  ByteArray              => L.str "#bytearray"
      |  MutableArray (a, b)    => L.seq [L.str "#mutablearray ", layoutTy a, layoutTy b]
      |  MutableByteArray a     => L.seq [L.str "#mutablebytearray ", layoutTy a]
      |  State a                => L.seq [L.str "#state ", layoutTy a]
      |  WeakPtr a              => L.seq [L.str "#weakptr ", layoutTy a]
      |  StablePtr a            => L.seq [L.str "#stableptr ", layoutTy a]
      |  StableName             => L.str "#stablename"
      |  MVar (a, b)            => L.seq [L.str "#mvar ", layoutTy a, layoutTy b]
      |  MutVar (a, b)          => L.seq [L.str "#mutvar ", layoutTy a, layoutTy b]
      |  ThreadId               => L.str "#threadId"
      |  EqTy (a, b, c)         => L.seq [L.str "#eq ", layoutTy a, layoutTy b, layoutTy c]
      |  Tuple tys              => L.seq [L.str "#tuple ", L.sequence ("(", ")", ",") (List.map (tys, layoutTy))]

end 

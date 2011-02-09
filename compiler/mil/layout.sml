(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_LAYOUT =
sig

    val controls : Config.Control.control list
    val debugs   : Config.Debug.debug list

    type 'a layout = Config.t * Mil.symbolInfo * 'a -> Layout.t

    val layoutVariable             : Mil.variable layout
    val layoutName                 : Mil.name layout
    val layoutLabel                : Mil.label layout
    val layoutEffects              : Mil.effects layout
    val layoutCallConv             : 'a layout -> 'a Mil.callConv layout
    val layoutTypKind              : Mil.typKind layout
    val layoutPObjKind             : Mil.pObjKind layout
    val layoutPObjKindShort        : Mil.pObjKind layout
    val layoutValueSize            : Mil.valueSize layout
    val layoutFieldVariance        : Mil.fieldVariance layout
    val layoutFieldVarianceShort   : Mil.fieldVariance layout
    val layoutTyp                  : Mil.typ layout
    val layoutFieldSize            : Mil.fieldSize layout
    val layoutFieldKind            : Mil.fieldKind layout
    val layoutFieldKinds           : Mil.fieldKind vector layout
    val layoutFieldKindShort       : Mil.fieldKind layout
    val layoutFieldDescriptor      : Mil.fieldDescriptor layout
    val layoutFieldDescriptorShort : Mil.fieldDescriptor layout
    val layoutTupleDescriptor      : Mil.tupleDescriptor layout
    val layoutMetaDataDescriptor   : Mil.metaDataDescriptor layout
    val layoutConstant             : Mil.constant layout
    val layoutSimple               : Mil.simple layout
    val layoutOperand              : Mil.operand layout
    val layoutFieldIdentifier      : Mil.fieldIdentifier layout
    val layoutTupleField           : Mil.tupleField layout
    val layoutRhs                  : Mil.rhs layout
    val layoutInstruction          : Mil.instruction layout
    val layoutTarget               : Mil.target layout
    val layoutSwitch               : string * 'a layout -> 'a Mil.switch layout
    val layoutCodes                : Mil.codes layout
    val layoutCall                 : Mil.call layout
    val layoutEval                 : Mil.eval layout
    val layoutInterProc            : Mil.interProc layout
    val layoutCuts                 : Mil.cuts layout
    val layoutReturn               : Mil.return layout
    val layoutTransfer             : Mil.transfer layout
    val layoutBlock                : (Mil.label * Mil.block) layout
    val layoutCodeBody             : Mil.codeBody layout
    val layoutCode                 : Mil.code layout
    val layoutGlobal               : (Mil.variable * Mil.global) layout
    val layoutIncludeKind          : Mil.includeKind layout
    val layoutIncludeFile          : Mil.includeFile layout
    val layoutVariableKind         : Mil.variableKind layout

    val layout           : Config.t * Mil.t -> Layout.t
    val layoutParseable  : Config.t * Mil.t -> Layout.t
    val print            : Config.t * Mil.t -> unit
    val printGlobalsOnly : Config.t * Mil.t -> unit

    type 'a printer = Config.t * Mil.symbolInfo * 'a -> unit

    val printVariable    : Mil.variable printer
    val printLabel       : Mil.label printer
    val printRhs         : Mil.rhs printer
    val printInstruction : Mil.instruction printer
    val printTransfer    : Mil.transfer printer
    val printBlock       : (Mil.label * Mil.block) printer
    val printCode        : Mil.code printer
    val printGlobal      : (Mil.variable * Mil.global) printer

    structure Helpers :
    sig

      datatype items =
          ILayout of Layout.t
        | IIndent of items
        | IItems of items list
        | IBlock of Mil.label * Mil.block

      type t = {
        varBind : (Mil.variable -> Layout.t list) option,
        block   : (Mil.label -> Layout.t) option,
        edge    : (Mil.label * Mil.label -> Layout.t) option,
        cb      : (Config.t * Mil.codeBody -> items) option
      }

      val default : t

    end

    structure General :
    sig

      type 'a layout = Config.t * Mil.symbolInfo * Helpers.t * 'a -> Layout.t

      val layoutCodeBody : Mil.codeBody layout
      val layoutCode     : Mil.code layout
      val layoutGlobal   : (Mil.variable * Mil.global) layout
      val layoutProgram  : Config.t * Helpers.t * Mil.t -> Layout.t

    end

end;

structure MilLayout :> MIL_LAYOUT =
struct

   type 'a layout = Config.t * Mil.symbolInfo * 'a -> Layout.t

   structure VI = VectorInstructions
   structure I = Identifier
   structure VS = I.VariableSet
   structure VD = I.VariableDict
   structure ND = I.NameDict
   structure LS = I.LabelSet
   structure LD = I.LabelDict
   structure L = Layout
   structure LU = LayoutUtils
   structure M = Mil
   structure MU = MilUtils
   structure MPU = MU.Prims.Utils

   val modulename = "MilLayout"

   fun layoutVector (env, f, v) = Vector.toListMap (v, fn x => f (env, x))

   fun semiCommaL (i, l) = (L.seq [i, L.str ";"])::(L.separateRight (l, ","))

   fun semiCommaV (i, v) = semiCommaL (i, Vector.toList v)

   fun semiCommaLP (i, l) = LU.paren (L.mayAlign (semiCommaL (i, l)))
   fun semiCommaVP (i, l) = LU.paren (L.mayAlign (semiCommaV (i, l)))

   fun cstring s = L.str ("\"" ^ String.escapeC s ^ "\"")

   structure Helpers =
   struct

      datatype items =
          ILayout of L.t
        | IIndent of items
        | IItems of items list
        | IBlock of M.label * M.block

      type t = {
        varBind : (Mil.variable -> Layout.t list) option,
        block   : (Mil.label -> Layout.t) option,
        edge    : (Mil.label * Mil.label -> Layout.t) option,
        cb      : (Config.t * Mil.codeBody -> items) option
      }

      val default = {varBind = NONE, block = NONE, edge = NONE, cb = NONE}

   end

   type options = {
     supVarKind : bool,
     escVarName : bool,
     binFpConst : bool,
     binderTyps : bool,
     viElemType : bool,
     numbers    : bool,
     tupDesc    : bool,
     thunkTyp   : bool,
     thunkFvs   : bool,
     pFunFvs    : bool,
     pSumTyp    : bool,
     codes      : bool
   }

   datatype env = E of {
     config  : Config.t,
     si      : M.symbolInfo,
     helpers : Helpers.t,
     options : options,
     block   : Mil.label option
   }

   fun getConfig  (E {config,  ...}) = config
   fun getSI      (E {si,      ...}) = si
   fun getHelpers (E {helpers, ...}) = helpers
   fun getOptions (E {options, ...}) = options
   fun getBlock   (E {block,   ...}) = block

   fun suppressVarKinds e = #supVarKind (getOptions e)
   fun escapeVarNames   e = #escVarName (getOptions e)
   fun showBinFpConsts  e = #binFpConst (getOptions e)
   fun showBinderTyps   e = #binderTyps (getOptions e)
   fun showViElemType   e = #viElemType (getOptions e)
   fun showNumbers      e = #numbers    (getOptions e)
   fun showTupDesc      e = #tupDesc    (getOptions e)
   fun showThunkTyp     e = #thunkTyp   (getOptions e)
   fun showThunkFvs     e = #thunkFvs   (getOptions e)
   fun showClosureFvs   e = #pFunFvs    (getOptions e)
   fun showPSumTyp      e = #pSumTyp    (getOptions e)
   fun showCodes        e = #codes      (getOptions e)

   fun setBlock (E {config, si, helpers, options, block}, b) =
       E {config = config, si = si, helpers = helpers, options = options, block = b}

   fun layoutVariable (env, v) =
       let
         val si = getSI env
         val l =
             if escapeVarNames env
             then MU.SymbolInfo.layoutVariableEscaped (si, v)
             else MU.SymbolInfo.layoutVariable (si, v)
         val l = 
             if suppressVarKinds env then
               l
             else
               if MU.SymbolInfo.variableExists (si, v) then
                 L.seq [Char.layout (MU.VariableKind.toChar (MU.SymbolInfo.variableKind (si, v))), l]
               else
                 L.seq [L.str "BAD_VAR_", l]
       in l
       end

   fun layoutName (env, n) =
       if escapeVarNames env
       then MU.SymbolInfo.layoutNameEscaped (getSI env, n)
       else MU.SymbolInfo.layoutName (getSI env, n)

   fun layoutLabel (env, l) = MU.SymbolInfo.layoutLabel (getSI env, l)

   fun layoutEffects (env, fx) = Effect.layout fx

   fun layoutCallConv f (env, cc) =
       let
         fun ct (s, v, vs) = L.seq [L.str s, semiCommaLP (f (env, v), layoutVector (env, f, vs))]
       in
         case cc
          of M.CcCode => L.str "Code"
           | M.CcClosure {cls, fvs} => ct ("Closure", cls, fvs)
           | M.CcThunk {thunk, fvs} => ct ("Thunk", thunk, fvs)
       end

   fun layoutTypKind (env, tk) = LU.char (MU.TypKind.toChar tk)

   fun layoutPObjKind (env, pok) = L.str (MU.PObjKind.toString pok)

   fun layoutPObjKindShort (env, pok) = LU.char (MU.PObjKind.toChar pok)

   fun layoutValueSize (env, vs) = L.str (MU.ValueSize.toString vs)

   fun layoutFieldVariance (env, fv) = L.str (MU.FieldVariance.toString fv)

   fun layoutFieldVarianceShort (env, fv) =
       LU.char (MU.FieldVariance.toChar fv)

   fun layoutTyp (env, t) =
       case t
        of M.TAny => L.str "Any"
         | M.TAnyS vs => L.str ("Any" ^ Int.toString (MU.ValueSize.numBits vs))
         | M.TPtr => L.str "Ptr"
         | M.TRef => L.str "Ref"
         | M.TBits vs => L.str ("Bits" ^ Int.toString (MU.ValueSize.numBits vs))
         | M.TNone => L.str "None"
         | M.TNumeric nt => MPU.Layout.numericTyp (getConfig env, nt)
         | M.TBoolean => L.str "Boolean"
         | M.TName => L.str "Name"
         | M.TViVector {vectorSize, elementTyp} => 
           L.seq [L.str "Vec", 
                  LU.bracket (MPU.Layout.vectorSize (getConfig env, vectorSize)), 
                  LU.angleBracket (layoutTyp (env, elementTyp))]
         | M.TViMask vd => L.seq [L.str "Mask", LU.bracket (MPU.Layout.vectorDescriptor (getConfig env, vd))]
         | M.TCode {cc, args, ress} =>
           let
             val args = layoutTyps (env, args)
             val args = semiCommaLP (layoutCallConv layoutTyp (env, cc), args)
             val a = L.seq [args, L.str " ->"]
             val r = LU.parenSeq (layoutTyps (env, ress))
             val l = L.mayAlign [a, LU.indent r]
           in l
           end
         | M.TTuple {pok, fixed, array} =>
           let
             val pok = layoutPObjKindShort (env, pok)
             fun layoutTypVar (env, (t, fv)) = L.seq [layoutTyp (env, t), layoutFieldVarianceShort (env, fv)]
             val fixed = layoutVector (env, layoutTypVar, fixed)
             val is = semiCommaL (pok, fixed)
             val array = [LU.bracket (layoutTypVar (env, array))]
             val is = is @ array
             val l = LU.angleBracket (L.mayAlign is)
           in l
           end
         | M.TCString => L.str "CStr"
         | M.TIdx => L.str "Idx"
         | M.TContinuation ts => L.seq [L.str "Cont", LU.parenSeq (layoutTyps (env, ts))]
         | M.TThunk t => L.seq [L.str "Thunk", LU.paren (layoutTyp (env, t))]
         | M.TPAny => L.str "PAny"
         | M.TClosure {args, ress} =>
           let
             val a = L.seq [LU.parenSeq (layoutTyps (env, args)), L.str " =>"]
             val r = LU.parenSeq (layoutTyps (env, ress))
             val l = L.mayAlign [a, LU.indent r]
           in l
           end
         | M.TPSum nts =>
           let
             val is = ND.toList nts
             fun doOne (n, t) = L.mayAlign [L.seq [layoutName (env, n), L.str ":"], LU.indent (layoutTyp (env, t))]
             val is = List.map (is, doOne)
             val l = L.seq [L.str "PSum", LU.braceSeq is]
           in l
           end
         | M.TPType {kind, over} =>
           let
             val kw =
                 case kind
                  of M.TkI => "PType"
                   | M.TkE => "PSet"
             val over = layoutTyp (env, over)
             val l = L.seq [L.str kw, LU.paren over]
           in l
           end
         | M.TPRef t => L.seq [L.str "PRef", LU.paren (layoutTyp (env, t))]
   and layoutTyps (env, ts) = layoutVector (env, layoutTyp, ts)

   fun layoutBinder (env, v) =
       let
         val si = getSI env
         val vl = layoutVariable (env, v)
         val tl = 
             if showBinderTyps env then
               if MU.SymbolInfo.variableExists (si, v) then
                 let
                   val t = MU.SymbolInfo.variableTyp (getSI env, v)
                   val t = layoutTyp (env, t)
                   val l = L.seq [L.str ": ", t]
                 in [l]
                 end
               else
                 [L.str ": BAD_VAR_NO_TYPE"]
             else
               []
         val el =
             case #varBind (getHelpers env)
              of NONE => []
               | SOME f => f v
         val l = L.mayAlign (vl::(List.map (tl @ el, LU.indent)))
       in l
       end

   fun layoutBinders (env, vs) =
       LU.parenSeq (layoutVector (env, layoutBinder, vs))

   fun layoutFieldSize (env, fs) = L.str (MU.FieldSize.toString fs)

   fun layoutFieldKind (env, fk) = L.str (MU.FieldKind.toString fk)

   fun layoutFieldKindShort (env, fk) =
       case fk
        of M.FkRef => L.str "r"
         | M.FkBits fs => L.str ("b" ^ Int.toString (MU.FieldSize.numBits fs))
         | M.FkFloat => L.str "f"
         | M.FkDouble => L.str "d"

   fun layoutFieldKinds (env, fks) = 
       LU.parenSeq (layoutVector (env, layoutFieldKindShort, fks))

   fun layoutFieldDescriptor (env, M.FD {kind, var}) =
       L.seq [layoutFieldKind (env, kind), layoutFieldVarianceShort (env, var)]

   fun layoutFieldDescriptorShort (env, M.FD {kind, var}) =
       L.seq [layoutFieldKindShort (env, kind), layoutFieldVarianceShort (env, var)]

   fun layoutTupleDescriptor (env, M.TD {fixed, array}) =
       let
         val fixed = layoutVector (env, layoutFieldDescriptorShort, fixed)
         val array =
             case array
              of NONE => []
               | SOME fd => [LU.bracket (layoutFieldDescriptorShort (env, fd))]
         val l = LU.angleBracket (L.mayAlign (L.separateRight (fixed, ",") @ array))
       in l
       end

   fun layoutMetaDataDescriptor (env, M.MDD {pok, fixed, array}) =
       let
         val pok = layoutPObjKindShort (env, pok)
         val fixed = layoutVector (env, layoutFieldDescriptorShort, fixed)
         val array =
             case array
              of NONE => []
               | SOME (li, fd) =>
                 let
                   val fd = layoutFieldDescriptorShort (env, fd)
                   val l = L.seq [fd, L.str "@", Int.layout li]
                   val l = [LU.bracket l]
                 in l
                 end
         val l = LU.angleBracket (L.mayAlign (semiCommaL (pok, fixed) @ array))
       in l
       end

   fun layoutConstant (env, c) =
       case c
        of M.CRat r => L.seq [L.str "R", L.paren (IntInf.layout r)]
         | M.CInteger i => L.seq [L.str "I", L.paren (IntInf.layout i)]
         | M.CName n => layoutName (env, n)
         | M.CIntegral i => IntArb.layout i
         | M.CBoolean b => if b then L.str "True" else L.str "False"
         | M.CFloat f =>
           let
             val f =
                 if showBinFpConsts env then
                   let
                     val lw = Utils.real32ToWord f
                     val s = LargeWord.toString lw
                     val s = s ^ String.make (8 - String.length s, #"0")
                     val l = L.str s
                   in l
                   end
                 else
                   Real32.layout f
             val l = L.seq [L.str "F", L.paren f]
           in l
           end
         | M.CDouble d =>
           let
             val d =
                 if showBinFpConsts env then
                   Fail.unimplemented (modulename, "layoutConstant", "binary double")
                 else
                   Real64.layout d
             val l = L.seq [L.str "D", L.paren d]
           in l
           end
         | M.CViMask {descriptor, elts} =>
           let
             val desc = MPU.Layout.vectorDescriptor (getConfig env, descriptor)
             val bs = L.seq (Vector.toListMap (elts, LU.layoutBool'))
             val l = L.seq [L.str "M", LU.bracket desc, LU.angleBracket bs]
           in l
           end
         | M.CPok pok => layoutPObjKind (env, pok)
         | M.COptionSetEmpty => L.str "Empty"
         | M.CTypePH => L.str "TypePH"
   and layoutConstants (env, cs) = layoutVector (env, layoutConstant, cs)

   fun layoutSimple (env, s) =
       case s
        of M.SVariable v => layoutVariable (env, v)
         | M.SConstant c => layoutConstant (env, c)

   fun layoutSimples (env, ss) = layoutVector (env, layoutSimple, ss)

   fun layoutOperand (env, opnd) = layoutSimple (env, opnd)

   fun layoutOperands (env, os) = layoutSimples (env, os)

   fun layoutFieldIdentifier (env, fi) =
       let
         fun wrapViElemType (l, t) =
             if showViElemType env
             then L.mayAlign [L.seq [l, L.str ":"], LU.indent (VI.layoutElemTypeShort t)]
             else l
         val l =
             case fi
              of M.FiFixed idx => 
                 L.seq [L.str "sf:", Int.layout idx]
               | M.FiVariable opnd => 
                 L.seq [L.str "sv:", layoutOperand (env, opnd)]
               | M.FiVectorFixed {descriptor, mask, index} => 
                 let
                   val d = MPU.Layout.vectorDescriptor (getConfig env, descriptor)
                   val m = case mask
                            of SOME oper => L.seq [L.str "?", layoutOperand (env, oper)]
                             | NONE      => L.empty
                   val i = Int.layout index
                   val l = L.seq [L.str "vf", LU.bracket d, L.str ":", i, m]
                 in l
                 end
               | M.FiVectorVariable {descriptor, base, mask, index, kind} =>
                 let
                   val d = MPU.Layout.vectorDescriptor (getConfig env, descriptor)
                   val m = case mask
                            of SOME oper => L.seq [L.str "?", layoutOperand (env, oper)]
                             | NONE      => L.empty
                   val idx = layoutOperand (env, index)
                   val i = case kind
                            of M.VikStrided i => 
                               LU.angleBracket (L.seq [idx, L.str ":", idx, L.str "+(", Int.layout i, L.str "*n)"])
                             | M.VikVector => 
                               LU.angleBracket idx
                   val v = case base
                            of M.TbScalar => L.empty
                             | M.TbVector => L.str "^"
                   val l = L.seq [L.str "vv", LU.bracket d, L.str ":", v, i, m]
                 in l
                 end
       in l
       end

   fun layoutTupleField (env, M.TF {tupDesc, tup, field}) =
       let
         val tup = layoutVariable (env, tup)
         val tup =
             if showTupDesc env then
               let
                 val td = layoutTupleDescriptor (env, tupDesc)
                 val l = L.mayAlign [tup, LU.indent (L.seq [L.str ":", td])]
               in l
               end
             else
               tup
         val field = LU.bracket (layoutFieldIdentifier (env, field))
         val l = L.mayAlign [tup, LU.indent field]
       in l
       end

   fun layoutTuple (env, mdDesc, inits) =
       let
         val mdd = layoutMetaDataDescriptor (env, mdDesc)
         val inits = layoutOperands (env, inits)
         val l = LU.angleBracket (L.mayAlign (semiCommaL (mdd, inits)))
       in l
       end

   fun addInit (env, l, vo) =
       case vo
        of NONE => l
         | SOME v => L.mayAlign [L.seq [layoutVariable (env, v), L.str " <-"], LU.indent l]

   fun layoutCodeOption (env, codeo) =
       case codeo
        of NONE => L.str "-"
         | SOME v => layoutVariable (env, v)

   fun layoutFvsInits (env, fvs, s) =
       let
         fun doOne (fk, opnd) =
             let
               val opnd = layoutOperand (env, opnd)
               val opnd =
                   if s then
                     L.seq [opnd, L.str ":", layoutFieldKindShort (env, fk)]
                   else
                     opnd
             in opnd
             end
         val fvs = L.separateRight (Vector.toListMap (fvs, doOne), ",")
       in fvs
       end

   fun layoutThunkTyp (env, l, typ) =
       if showThunkTyp env then
         L.seq [l, L.str ":", layoutFieldKindShort (env, typ)]
       else
         l

   fun layoutThunkVar (env, t, typ) =
       if showThunkTyp env then
         let
           val t = layoutVariable (env, t)
           val typ = layoutFieldKindShort (env, typ)
           val l = L.seq [t, L.str ":", typ]
         in l
         end
       else
         layoutVariable (env, t)

   fun layoutThunkFvs (env, fvs) =
       if showThunkFvs env then
         L.separateRight (layoutVector (env, layoutFieldKindShort, fvs), ",")
       else
         [Int.layout (Vector.size fvs)]

   fun layoutClosureFvs (env, fvs) =
       if showClosureFvs env then
         LU.parenSeq (layoutVector (env, layoutFieldKindShort, fvs))
       else
         LU.paren (Int.layout (Vector.size fvs))

   fun layoutPSumTyp (env, l, fk) =
       if showPSumTyp env then
         L.seq [l, L.str ":", layoutFieldKindShort (env, fk)]
       else
         l

   fun layoutRhs (env, rhs) =
       case rhs
        of M.RhsSimple s => layoutSimple (env, s)
         | M.RhsPrim {prim, createThunks, typs, args} =>
           L.seq [L.seq [MPU.Layout.t (getConfig env, prim), if createThunks then LU.bracket (L.str "T") else L.str ""],
                  if Vector.length typs > 0 then LU.braceSeq (layoutTyps (env, typs)) else L.empty,
                  LU.parenSeq (layoutOperands (env, args))]
         | M.RhsTuple {mdDesc, inits} => layoutTuple (env, mdDesc, inits)
         | M.RhsTupleSub tf => layoutTupleField (env, tf)
         | M.RhsTupleSet {tupField, ofVal} =>
           L.mayAlign [L.seq [layoutTupleField (env, tupField), L.str " <-"], LU.indent (layoutOperand (env, ofVal))]
         | M.RhsTupleInited {mdDesc, tup} =>
           let
             val mdDesc = layoutMetaDataDescriptor (env, mdDesc)
             val tup = layoutVariable (env, tup)
             val l = L.seq [L.str "Inited", L.tuple [mdDesc, tup]]
           in l
           end
         | M.RhsIdxGet {idx, ofVal} =>
           L.seq [L.str "IdxGet", L.tuple [layoutVariable (env, idx), layoutOperand (env, ofVal)]]
         | M.RhsCont l => L.seq [L.str "Cont", LU.paren (layoutLabel (env, l))]
         | M.RhsObjectGetKind v => L.seq [L.str "GetKind", LU.paren (layoutVariable (env, v))]
         | M.RhsThunkMk {typ, fvs} =>
           let
             val fvs = layoutThunkFvs (env, fvs)
             val l =
                 if showThunkTyp env
                 then (L.seq [layoutFieldKindShort (env, typ), L.str ";"])::fvs
                 else fvs
             val l = L.seq [L.str "ThunkMk", LU.paren (L.mayAlign l)]
           in l
           end
         | M.RhsThunkInit {typ, thunk, fx, code, fvs} =>
           let
             val typ =
                 if showThunkTyp env
                 then [L.seq [layoutFieldKindShort (env, typ), L.str ";"]]
                 else []
             val code = L.seq [layoutCodeOption (env, code), L.str ";"]
             val fx = L.seq [layoutEffects (env, fx), L.str ";"]
             val fvs = layoutFvsInits (env, fvs, showThunkFvs env)
             val l = typ @ (code::fx::fvs)
             val l = L.seq [L.str "ThunkInit", LU.paren (L.mayAlign l)]
             val l = addInit (env, l, thunk)
           in l
           end
         | M.RhsThunkGetFv {typ, fvs, thunk, idx} =>
           let
             val thunk = layoutVariable (env, thunk)
             val thunk =
                 if showThunkTyp env orelse showThunkFvs env then
                   let
                     val typ = layoutFieldKindShort (env, typ)
                     val fvs = layoutVector (env, layoutFieldKindShort, fvs)
                     val l =
                         if showThunkTyp env then
                           if showThunkFvs env then
                             LU.angleBracket (L.mayAlign (semiCommaL (typ, fvs)))
                           else
                             typ
                         else
                           LU.angleSeq fvs
                     val l = L.seq [thunk, L.str ":", l]
                   in l
                   end
                 else
                   thunk
             val l = L.seq [L.str "ThunkGetFv", L.tuple [thunk, Int.layout idx]]
           in l
           end
         | M.RhsThunkValue {typ, thunk, ofVal} =>
           let
             val ofVal = layoutOperand (env, ofVal)
             val ls =
                 if showThunkTyp env
                 then semiCommaL (layoutFieldKindShort (env, typ), [ofVal])
                 else [ofVal]
             val l = L.seq [L.str "ThunkMkVal", L.paren (L.mayAlign ls)]
             val l = addInit (env, l, thunk)
           in l
           end
         | M.RhsThunkGetValue {typ, thunk} => L.seq [L.str "ThunkGetValue", L.tuple [layoutThunkVar (env, thunk, typ)]]
         | M.RhsThunkSpawn {typ, thunk, fx} =>
           L.seq [L.str "Spawn", LU.paren (layoutThunkVar (env, thunk, typ)), layoutEffects (env, fx)]
         | M.RhsClosureMk {fvs} => L.seq [L.str "ClosureMk", layoutClosureFvs (env, fvs)]
         | M.RhsClosureInit {cls, code, fvs} =>
           let
             val code = L.seq [layoutCodeOption (env, code), L.str ";"]
             val fvs = layoutFvsInits (env, fvs, showClosureFvs env)
             val l = code::fvs
             val l = L.seq [L.str "ClosureInit", LU.paren (L.mayAlign l)]
             val l = addInit (env, l, cls)
           in l
           end
         | M.RhsClosureGetFv {fvs, cls, idx} =>
           let
             val cls = layoutVariable (env, cls)
             val cls =
                 if showClosureFvs env then
                   L.mayAlign [cls, L.seq [L.str ":", layoutClosureFvs (env, fvs)]]
                 else
                   cls
             val l = L.seq [L.str "ClosureGetFv", L.tuple [cls, Int.layout idx]]
           in l
           end
         | M.RhsPSetNew oper => L.seq [L.str "PSet", L.paren (layoutOperand (env, oper))]
         | M.RhsPSetGet v => L.seq [L.str "PSetGet", L.paren (layoutVariable (env, v))]
         | M.RhsPSetCond {bool, ofVal} =>
           let
             val ts = LU.brace (layoutOperand (env, ofVal))
             val l = L.seq [layoutOperand (env, bool), L.str " ? ", ts, L.str " : {}"]
             val l = L.seq [L.str "PSetCond", L.paren l]
           in l
           end
         | M.RhsPSetQuery oper => L.seq [L.str "?", layoutOperand (env, oper)]
         | M.RhsPSum {tag, typ, ofVal} =>
           L.seq [L.str "Tagged",
                  L.tuple [layoutName (env, tag), layoutPSumTyp (env, layoutOperand (env, ofVal), typ)]]
         | M.RhsPSumProj {typ, sum, tag} =>
           let
             val l = L.seq [layoutVariable (env, sum), L.str ".", layoutName (env, tag)]
             val l = layoutPSumTyp (env, l, typ)
             val l = L.seq [L.str "SumProj", L.paren l]
           in l
           end

   fun layoutInstruction (env, M.I {dests, n, rhs}) =
       let
         val rhs = layoutRhs (env, rhs)
         val l =
             case Vector.length dests
              of 0 => L.seq [L.str "!", rhs]
               | 1 => L.mayAlign [layoutBinder (env, Vector.sub (dests, 0)), LU.indent (L.seq [L.str "= ", rhs])]
               | _ =>
                 L.mayAlign [L.tuple (layoutVector (env, layoutBinder, dests)), LU.indent (L.seq [L.str "= ", rhs])]
         val l = L.seq [l, L.str ";"]
         val l = if showNumbers env then L.mayAlign [L.str ("i" ^ Int.toString n ^ ":"), LU.indent l] else l
       in l
       end

   fun layoutTarget (env, M.T {block, arguments}) =
       let
         val anot =
             case (#edge (getHelpers env), getBlock env)
              of (SOME get, SOME b) => LU.bracket (get (b, block))
               | _ => L.empty
       in
         L.seq [layoutLabel (env, block), LU.parenSeq (layoutOperands (env, arguments)), anot]
       end

   fun layoutSwitch (kw, f) (env, {on, cases, default} : 'a Mil.switch) =
       let
         val header = L.seq [L.str kw, LU.space, layoutOperand (env, on), L.str " {"]
         fun doOne (k, t) =
             let
               val k = f (env, k)
               val t = layoutTarget (env, t)
               val l = L.mayAlign [L.seq [k, L.str " =>"], LU.indent t]
             in LU.indent l
             end
         val cases = Vector.toListMap (cases, doOne)
         val default =
             case default
              of NONE => []
               | SOME t =>
                 let
                   val t = layoutTarget (env, t)
                   val l = LU.indent (L.mayAlign [L.str "Default =>", LU.indent t])
                 in [l]
                 end
         val trailer = L.str "}"
         val l = L.mayAlign (header::cases @ default @ [trailer])
       in l
       end

   fun layoutCodes (env, {possible, exhaustive} : M.codes) =
       L.seq [L.str (if exhaustive then "<=" else "?"), VS.layout (possible, fn v => layoutVariable (env, v))]

   fun addCodes (env, l, codes) =
       if showCodes env then
         L.mayAlign [l, LU.indent (layoutCodes (env, codes))]
       else
         l

   fun layoutCall (env, c) = 
       case c
        of M.CCode {ptr, code} => addCodes (env, L.seq [L.str "Call", LU.paren (layoutVariable (env, ptr))], code)
         | M.CClosure {cls, code} =>
           addCodes (env, L.seq [L.str "CallClos", LU.paren (layoutVariable (env, cls))], code)
         | M.CDirectClosure {cls, code} =>
           L.seq [L.str "CallDir", LU.parenSeq [layoutVariable (env, cls), layoutVariable (env, code)]]

   fun layoutEval (env, e) = 
       case e
        of M.EThunk {thunk, code} => addCodes (env, L.seq [L.str "Eval", LU.paren (layoutVariable (env, thunk))], code)
         | M.EDirectThunk {thunk, code} =>
           L.seq [L.str "EvalDir", LU.parenSeq [layoutVariable (env, thunk), layoutVariable (env, code)]]

   fun layoutInterProc (env, ip) =
       case ip
        of M.IpCall {call, args} =>
           L.mayAlign [layoutCall (env, call), LU.indent (LU.parenSeq (layoutOperands (env, args)))]
         | M.IpEval {typ, eval} =>
           let
             val l = layoutEval (env, eval)
             val l =
                 if showThunkTyp env then
                   L.seq [l, L.str ":", layoutFieldKindShort (env, typ)]
                 else
                   l
           in l
           end

   fun layoutCutsA (env, M.C {exits, targets}) = 
       if exits orelse (not (LS.isEmpty targets)) then
         let
           val ts = LS.toList targets
           val l = List.map (ts, fn l => layoutLabel (env, l))
           val l = if exits then L.str "Exit"::l else l
         in
           [L.seq [L.str "/->/ ", LU.braceSeq l]]
         end
       else
         []

   fun layoutCuts (env, cuts) = L.seq (layoutCutsA (env, cuts))

   fun layoutReturn (env, r) =
       case r
        of M.RNormal {rets, block, cuts} =>
           let
             val rets = layoutBinders (env, rets)
             val block = layoutLabel (env, block)
             val cuts = layoutCutsA (env, cuts)
             val l = L.mayAlign ((L.seq [L.str "-> ", L.mayAlign [rets, LU.indent block]])::cuts)
           in l
           end
         | M.RTail {exits} => L.str ("-|" ^ (if exits then " /->/ Exit" else ""))

   fun layoutTransfer (env, t) = 
       case t
        of M.TGoto tg => L.seq [L.str "Goto ", layoutTarget (env, tg)]
         | M.TCase s => layoutSwitch ("Case", layoutConstant) (env, s)
         | M.TInterProc {callee, ret, fx} =>
           L.mayAlign [layoutInterProc (env, callee), layoutReturn (env, ret), layoutEffects (env, fx)]
         | M.TReturn os => L.seq [L.str "Return", LU.parenSeq (layoutOperands (env, os))]
         | M.TCut {cont, args, cuts} =>
           L.mayAlign [L.seq [L.str "Cut ", layoutVariable (env, cont)],
                       LU.indent (LU.parenSeq (layoutOperands (env, args))),
                       LU.indent (layoutCuts (env, cuts))]
         | M.THalt opnd => L.seq [L.str "Halt", L.paren (layoutOperand (env, opnd))]
         | M.TPSumCase s => layoutSwitch ("PSumCase", layoutName) (env, s)

   fun layoutBlock (env, (l, M.B {parameters, instructions, transfer})) =
       let
         val parameters = layoutBinders (env, parameters)
         val anot =
             case #block (getHelpers env)
              of NONE => L.empty
               | SOME get => LU.bracket (get l)
         val header = L.seq [layoutLabel (env, l), parameters, anot]
         val instructions = layoutVector (env, layoutInstruction, instructions)
         val transfer = layoutTransfer (env, transfer)
         val l = L.align [header, LU.indent (L.align instructions), LU.indent transfer]
       in l
       end

   fun layoutItems (env, items) =
       case items
        of Helpers.ILayout l       => l
         | Helpers.IIndent is      => LU.indent (layoutItems (env, is))
         | Helpers.IItems is       => L.align (List.map (is, fn i => layoutItems (env, i)))
         | Helpers.IBlock (l, blk) => layoutBlock (env, (l, blk))

   fun layoutCodeBody (env, cb as M.CB {entry, blocks}) =
       let
         val entry = L.seq [L.str "Entry ", layoutLabel (env, entry)]
         val config = getConfig env
         val blks =
             case #cb (getHelpers env)
              of NONE       => Helpers.IItems (List.map (MU.CodeBody.listRPO (config, cb), Helpers.IBlock))
               | SOME order => order (config, cb)
         val items = Helpers.IItems [Helpers.ILayout entry, blks]
         val l = layoutItems (env, items)
       in l
       end

   fun layoutCode (env, M.F {fx, escapes, recursive, cc, args, rtyps, body}) =
       let
         val fx        = layoutEffects (env, fx)
         val escapes   = L.str (if escapes then "^" else "")
         val recursive = L.str (if recursive then "*" else "")
         val cc        = layoutCallConv layoutBinder (env, cc)
         val args      = layoutVector (env, layoutBinder, args)
         val args      = LU.paren (L.mayAlign (semiCommaL (cc, args)))
         val rtyps     = LU.parenSeq (layoutTyps (env, rtyps))
         val header    = L.seq [L.str "Code", escapes, recursive, args, fx]
         val header    = L.mayAlign [header, L.seq [L.str ": ", rtyps]]
         val body      = layoutCodeBody (env, body)
       in
         L.align [header, L.str "{", LU.indent body, L.str "}"]
       end

   fun layoutIdx (env, idx) =
       let
         val idx = ND.toList idx
         fun doOne (n, i) = L.seq [layoutName (env, n), L.str " -> ", Int.layout i]
         val l = L.seq [L.str "Idx", LU.braceSeq (List.map (idx, doOne))]
       in l
       end

   fun layoutGlobalOnly (env, global) = 
       case global
        of M.GCode code => layoutCode (env, code)
         | M.GErrorVal t => L.seq [L.str "ErrorVal : ", layoutTyp (env, t)]
         | M.GIdx d => layoutIdx (env, d)
         | M.GTuple {mdDesc, inits} => layoutTuple (env, mdDesc, inits)
         | M.GRat r => L.seq [L.str "R", L.paren (Rat.layout r)]
         | M.GInteger i => L.seq [L.str "I", L.paren (IntInf.layout i)]
         | M.GCString s => L.seq [L.str "CString", L.paren (cstring s)]
         | M.GThunkValue {typ, ofVal} =>
           let
             val ofVal = layoutSimple (env, ofVal)
             val ofVal = layoutThunkTyp (env, ofVal, typ)
             val l = L.seq [L.str "ThunkValue", LU.paren ofVal]
           in l
           end
         | M.GSimple simple => layoutSimple (env, simple)
         | M.GClosure {code, fvs} => 
           let
             val code = L.seq [layoutCodeOption (env, code), L.str ";"]
             val fvs = layoutFvsInits (env, fvs, showClosureFvs env)
             val l = code::fvs
             val l = L.seq [L.str "Closure", LU.paren (L.mayAlign l)]
           in l
           end
         | M.GPSum {tag, typ, ofVal} =>
           let
             val ofVal = layoutSimple (env, ofVal)
             val ofVal = layoutPSumTyp (env, ofVal, typ)
             val tag = layoutName (env, tag)
             val l = L.seq [L.str "Tagged", L.tuple [tag, ofVal]]
           in l
           end
         | M.GPSet opnd => L.seq [L.str "PSet", L.paren (layoutSimple (env, opnd))]

   fun layoutGlobal (env, (v, g)) =
       L.mayAlign [layoutBinder (env, v), LU.indent (L.seq [L.str "= ", layoutGlobalOnly (env, g)])]
       
   fun layoutGlobals (env, globals) = 
       let
         val gs = VD.toList globals
         val ls = List.map (gs, fn g => layoutGlobal (env, g))
         val l = L.align ls
       in l
       end

   fun layoutIncludeKind (env, k) = L.str (MU.IncludeKind.toString k)

   fun layoutIncludeFile (env, M.IF {name, kind, externs}) =
       let
         val k = L.seq [L.str ": ", layoutIncludeKind (env, kind)]
         val externs = VS.layout (externs, fn v => layoutBinder (env, v))
         val l = L.mayAlign [cstring name, LU.indent k, LU.indent externs]
       in l
       end

   fun layoutVariableKind (env, k) = L.str (MU.VariableKind.toString k)

   fun layoutSymbolTable (env, st, allInfo) = 
       let
         fun layout v =  
             let
               val k = MU.SymbolTable.variableKind (st, v)
               val k = MU.VariableKind.toString k
               val prefix = L.str (k ^ String.make (7 - String.length k, #" "))
               val name   = layoutVariable (env, v)
               val typ    = MU.SymbolTable.variableTyp (st, v)
               val typ    = layoutTyp (env, typ)
             in
               L.seq [prefix, name, L.str ": ", typ]
             end
         val vs = I.listVariables st
         val vars = L.align [L.str "Variables:", LU.indent (L.align (List.map (vs, layout)))]
         val l =
             if allInfo then
               L.align [vars, L.str "Names:",
                        LU.indent (L.align (List.map (I.listNames st, fn n => layoutName (env, n))))]
             else
               vars
       in l
       end

   fun layoutProgramAux (env, symbols, fullSymTab, M.P {includes, externs, globals, symbolTable, entry}) =
       let
         val includes =
             [L.str "Includes:", LU.indent (L.align (Vector.toListMap (includes, fn i => layoutIncludeFile (env, i))))]
         val externs = [L.str "Externs:", LU.indent (VS.layout (externs, fn v => layoutBinder (env, v)))]
         val symtab =
             if symbols then
               [L.str "Symbols:", LU.indent (layoutSymbolTable (env, symbolTable, fullSymTab))]
             else
               []
         val globals = [L.str "Globals:", LU.indent (layoutGlobals (env, globals))]
         val entry = [L.seq [L.str "Entry: ", layoutVariable (env, entry)]]
         val l = L.align (includes @ externs @ symtab @ globals @ entry)
       in l
       end

   fun layoutParseable (c : Config.t, p : M.t) : Layout.t =
       let
         val M.P {symbolTable, ...} = p
         val si = I.SymbolInfo.SiTable symbolTable
         val hs = Helpers.default
         val opts = {supVarKind = true, escVarName = true, binFpConst = true, binderTyps = true,
                     viElemType = true, numbers = false, tupDesc = true,
                     thunkTyp = true, thunkFvs = true, pFunFvs = true, pSumTyp = true, codes = true}
         val env = E {config = c, si = si, helpers = hs, options = opts, block = NONE}
         val l = layoutProgramAux (env, false, false, p)
       in l
       end

   fun describe () =
       L.align [L.str (modulename ^ " control string consists of:"),
                LU.indent (L.align [L.str "b => show types on variable binders",
                                    L.str "B => binary floating-point constants",
                                    L.str "c => show codes",
                                    L.str "e => escape variable and name strings",
                                    L.str "f => show P Function free variable field kinds",
                                    L.str "F => show full symbol table information",
                                    L.str "k => suppress kinds on variables",
                                    L.str "n => show instruction number",
                                    L.str "s => show sum types",
                                    L.str "S => show symbol table",
                                    L.str "t => show metadata/tuple descriptors",
                                    L.str "T => show thunk types",
                                    L.str "U => show thunk free variable field kinds",
                                    L.str "v => show vector element types",
                                    L.str "w => set options for parsing",
                                    L.str "% => show a reasonable subset of information (cfFktUv)",
                                    L.str "+ => show all of the above except suppress kinds and binary FP constants"]),
                L.str "default is c"]

   fun parse str =
       let
         val supVarKind = ref false
         val escVarName = ref false
         val binFpConst = ref false
         val binderTyps = ref false
         val viElemType = ref false
         val tupDesc    = ref false
         val thunkTyp   = ref false
         val thunkFvs   = ref false
         val pFunFvs    = ref false
         val pSumTyp    = ref false
         val codes      = ref false
         val symbols    = ref false
         val fullSymTab = ref false
         val numbers    = ref false
         fun doOne c =
             case c
              of #"b" => let val () = binderTyps := true in true end
               | #"B" => let val () = binFpConst := true in true end
               | #"c" => let val () = codes := true in true end
               | #"e" => let val () = escVarName := true in true end
               | #"f" => let val () = pFunFvs := true in true end
               | #"F" => let val () = fullSymTab := true val () = symbols := true in true end
               | #"k" => let val () = supVarKind := true in true end
               | #"n" => let val () = numbers := true in true end
               | #"s" => let val () = pSumTyp := true in true end
               | #"S" => let val () = symbols := true in true end
               | #"t" => let val () = tupDesc := true in true end
               | #"T" => let val () = thunkTyp := true in true end
               | #"U" => let val () = thunkFvs := true in true end
               | #"v" => let val () = viElemType := true in true end
               | #"w" =>
                 let
                   val () = supVarKind := true
                   val () = escVarName := true
                   val () = binFpConst := true
                   val () = binderTyps := true
                   val () = viElemType := true
                   val () = tupDesc    := true
                   val () = thunkTyp   := true
                   val () = thunkFvs   := true
                   val () = pFunFvs    := true
                   val () = pSumTyp    := true
                   val () = codes      := true
                   val () = symbols    := false
                   val () = fullSymTab := false
                   val () = numbers    := false
                 in true
                 end
               | #"%" =>
                 let
                   val () = codes := true 
                   val () = pFunFvs := true 
                   val () = fullSymTab := true 
                   val () = symbols := true 
                   val () = supVarKind := true 
                   val () = tupDesc := true 
                   val () = thunkFvs := true 
                   val () = viElemType := true 
                 in true
                 end
               | #"+" =>
                 let
                   val () = binderTyps := true
                   val () = viElemType:= true
                   val () = tupDesc := true
                   val () = thunkTyp := true
                   val () = thunkFvs := true
                   val () = pFunFvs := true
                   val () = pSumTyp := true
                   val () = codes := true
                   val () = symbols := true
                   val () = fullSymTab := true
                   val () = numbers := true
                 in true
                 end
               | _    => false
       in
         if List.forall (String.explode str, doOne) then
           SOME ({supVarKind = !supVarKind, escVarName = !escVarName,
                  binFpConst = !binFpConst, binderTyps = !binderTyps,
                  viElemType = !viElemType, tupDesc = !tupDesc,
                  thunkTyp = !thunkTyp, thunkFvs = !thunkFvs, pFunFvs = !pFunFvs, pSumTyp = !pSumTyp, codes = !codes,
                  numbers = !numbers},
                 !symbols,
                 !fullSymTab)
         else
           NONE
       end

   fun dft _ = ({supVarKind = false, escVarName = false, binFpConst = false, binderTyps = false,
                 viElemType = false, tupDesc = false, thunkTyp = false,
                 thunkFvs = false, pFunFvs = false, pSumTyp = false, codes = true, numbers = false},
                false,
                false)

   val (control, controlGet) = Config.Control.mk (modulename, describe, parse, dft)

   fun envMk (c, si, hs) =
       let
         val (options, symbols, fullSymTab) = controlGet c
         val env = E {config = c, si = si, helpers = hs, options = options, block = NONE}
       in (env, (symbols, fullSymTab))
       end
       
   fun layoutProgram (c, hs, p) =
       let
         val M.P {symbolTable, ...} = p
         val (env, (symbols, fullSymTab)) = envMk (c, I.SymbolInfo.SiTable symbolTable, hs)
         val l = layoutProgramAux (env, symbols, fullSymTab, p)
       in l
       end

   fun layoutGlobalsOnly (c, hs, M.P {symbolTable, globals, ...}) =
       let
         val (env, _) = envMk (c, I.SymbolInfo.SiTable symbolTable, hs)
         val globals = [L.str "Globals:", LU.indent (layoutGlobals (env, globals))]
         val l = L.align globals
       in l
       end

   structure General =
   struct

     type 'a layout = Config.t * Mil.symbolInfo * Helpers.t * 'a -> Layout.t

     fun wrap f = fn (config, si, hs, x) => f (#1 (envMk (config, si, hs)), x)

     val layoutCodeBody = wrap layoutCodeBody
     val layoutCode     = wrap layoutCode
     val layoutGlobal   = wrap layoutGlobal
     val layoutGlobals  = wrap layoutGlobals
     
     val layoutProgram = layoutProgram

   end

   fun wrap f = fn (config, si, x) => f (#1 (envMk (config, si, Helpers.default)), x)

   val layoutVariable             = wrap layoutVariable
   val layoutName                 = wrap layoutName
   val layoutLabel                = wrap layoutLabel
   val layoutEffects              = wrap layoutEffects
   val layoutTypKind              = wrap layoutTypKind
   val layoutPObjKind             = wrap layoutPObjKind
   val layoutPObjKindShort        = wrap layoutPObjKindShort
   val layoutValueSize            = wrap layoutValueSize
   val layoutFieldVariance        = wrap layoutFieldVariance
   val layoutFieldVarianceShort   = wrap layoutFieldVarianceShort
   val layoutTyp                  = wrap layoutTyp
   val layoutFieldSize            = wrap layoutFieldSize
   val layoutFieldKind            = wrap layoutFieldKind
   val layoutFieldKinds           = wrap layoutFieldKinds
   val layoutFieldKindShort       = wrap layoutFieldKindShort
   val layoutFieldDescriptor      = wrap layoutFieldDescriptor
   val layoutFieldDescriptorShort = wrap layoutFieldDescriptorShort
   val layoutTupleDescriptor      = wrap layoutTupleDescriptor
   val layoutMetaDataDescriptor   = wrap layoutMetaDataDescriptor
   val layoutConstant             = wrap layoutConstant
   val layoutSimple               = wrap layoutSimple
   val layoutOperand              = wrap layoutOperand
   val layoutFieldIdentifier      = wrap layoutFieldIdentifier
   val layoutTupleField           = wrap layoutTupleField
   val layoutRhs                  = wrap layoutRhs
   val layoutInstruction          = wrap layoutInstruction
   val layoutTarget               = wrap layoutTarget
   val layoutCodes                = wrap layoutCodes
   val layoutCall                 = wrap layoutCall
   val layoutEval                 = wrap layoutEval
   val layoutInterProc            = wrap layoutInterProc
   val layoutCuts                 = wrap layoutCuts
   val layoutReturn               = wrap layoutReturn
   val layoutTransfer             = wrap layoutTransfer
   val layoutBlock                = wrap layoutBlock
   val layoutCodeBody             = wrap layoutCodeBody
   val layoutCode                 = wrap layoutCode
   val layoutGlobal               = wrap layoutGlobal
   val layoutGlobals              = wrap layoutGlobals
   val layoutIncludeKind          = wrap layoutIncludeKind
   val layoutIncludeFile          = wrap layoutIncludeFile
   val layoutVariableKind         = wrap layoutVariableKind

   val layoutCallConv =
    fn g =>
    fn (config, si, cc) =>
       layoutCallConv (fn (env, y) => g (getConfig env, getSI env, y))
                      (#1 (envMk (config, si, Helpers.default)), cc)

   val layoutSwitch =
    fn (kw, g) =>
    fn (config, si, s) =>
       layoutSwitch (kw, fn (env, y) => g (getConfig env, getSI env, y))
                    (#1 (envMk (config, si, Helpers.default)), s)

   fun layout (c, p) = layoutProgram (c, Helpers.default, p)

   fun print (c, p) = LU.printLayout (layout (c, p))

   fun printGlobalsOnly (c, p) = LU.printLayout (layoutGlobalsOnly (c, Helpers.default, p))

   type 'a printer = Config.t * Mil.symbolInfo * 'a -> unit

   fun wrap f = LU.printLayout o f

   val printVariable    = wrap layoutVariable
   val printLabel       = wrap layoutLabel
   val printRhs         = wrap layoutRhs
   val printInstruction = wrap layoutInstruction
   val printTransfer    = wrap layoutTransfer
   val printBlock       = wrap layoutBlock
   val printCode        = wrap layoutCode
   val printGlobal      = wrap layoutGlobal

   val controls = [control]
   val debugs = []

end;

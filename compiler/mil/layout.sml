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

    val layout           : Config.t * Mil.t -> Layout.t
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
        block : (Mil.label -> string option) option,
        edge  : (Mil.label * Mil.label -> string option) option,
        cb    : (Config.t * Mil.codeBody -> items) option
      }

      val default : t

    end

    structure General :
    sig

      type 'a layout = Config.t * Mil.symbolInfo * Helpers.t * 'a -> Layout.t

      val layoutCodeBody  : Mil.codeBody layout
      val layoutCode      : Mil.code layout
      val layoutGlobal    : (Mil.variable * Mil.global) layout
      val layoutProgram   : Config.t * Helpers.t * Mil.t -> Layout.t

    end

end;

structure MilLayout :> MIL_LAYOUT =
struct

   type 'a layout = Config.t * Mil.symbolInfo * 'a -> Layout.t

   structure VI = VectorInstructions
   structure VS = Identifier.VariableSet
   structure VD = Identifier.VariableDict
   structure ND = Identifier.NameDict
   structure LS = Identifier.LabelSet
   structure LD = Identifier.LabelDict
   structure L = Layout
   structure LU = LayoutUtils
   structure M = Mil
   structure MU = MilUtils

   val modulename = "MilLayout"

   fun layoutVector (env, f, v) = 
       Vector.toListMap (v, fn x => f (env, x))

   fun semiCommaL (i, l) =
       (L.seq [i, L.str ";"])::(L.separateRight (l, ","))

   fun semiCommaV (i, v) = semiCommaL (i, Vector.toList v)

   fun semiCommaLP (i, l) = LU.paren (L.mayAlign (semiCommaL (i, l)))
   fun semiCommaVP (i, l) = LU.paren (L.mayAlign (semiCommaV (i, l)))

   structure Helpers =
   struct

      datatype items =
          ILayout of L.t
        | IIndent of items
        | IItems of items list
        | IBlock of M.label * M.block

      type t = {
        block : (Mil.label -> string option) option,
        edge  : (Mil.label * Mil.label -> string option) option,
        cb    : (Config.t * Mil.codeBody -> items) option
      }

      val default = {block = NONE, edge = NONE, cb = NONE}

   end

   type options = {
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

   fun showBinderTyps   e = #binderTyps (getOptions e)
   fun showViElemType   e = #viElemType (getOptions e)
   fun showTupDesc      e = #tupDesc    (getOptions e)
   fun showThunkTyp     e = #thunkTyp   (getOptions e)
   fun showThunkFvs     e = #thunkFvs   (getOptions e)
   fun showClosureFvs e = #pFunFvs    (getOptions e)
   fun showPSumTyp      e = #pSumTyp    (getOptions e)
   fun showCodes        e = #codes      (getOptions e)
   fun showNumbers      e = #numbers    (getOptions e)

   fun setBlock (E {config, si, helpers, options, block}, b) =
       E {config = config, si = si, helpers = helpers, options = options, block = b}

   fun layoutVariable (env, v) =
       let
         val si = getSI env
         val l = 
             if MU.SymbolInfo.variableExists (si, v) then
               L.seq [Char.layout (MU.VariableKind.toChar (MU.SymbolInfo.variableKind (si, v))),
                      MU.SymbolInfo.layoutVariable (si, v)]
             else
               L.seq [L.str "BAD_VAR_", MU.SymbolInfo.layoutVariable (si, v)]
       in l
       end

   fun layoutName  (env, n) = MU.SymbolInfo.layoutName  (getSI env, n)
   fun layoutLabel (env, l) = MU.SymbolInfo.layoutLabel (getSI env, l)

   fun layoutEffects (env, fx) = Effect.layout fx

   fun layoutCallConv f (env, cc) =
       let
         fun ct (s, v, vs) =
             L.seq [L.str s,
                    semiCommaLP (f (env, v), layoutVector (env, f, vs))]
       in
         case cc
          of M.CcCode => L.str "CcCode"
           | M.CcClosure {cls, fvs} => ct ("CcClosure", cls, fvs)
           | M.CcThunk {thunk, fvs} => ct ("CcThunk", thunk, fvs)
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
         | M.TBits vs =>
           L.str ("Bits" ^ Int.toString (MU.ValueSize.numBits vs))
         | M.TNone => L.str "None"
         | M.TRat => L.str "Rat"
         | M.TInteger => L.str "Integer"
         | M.TName => L.str "Name"
         | M.TIntegral t => IntArb.layoutTyp t
         | M.TFloat => L.str "Float"
         | M.TDouble => L.str "Double"
         | M.TViVector et =>
           L.seq [L.str "Vector", LU.paren (VI.layoutElemType et)]
         | M.TViMask et =>
           L.seq [L.str "Mask", LU.paren (VI.layoutElemType et)]
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
             val pok = layoutPObjKind (env, pok)
             fun layoutTypVar (env, (t, fv)) =
                 L.seq [layoutTyp (env, t), layoutFieldVarianceShort (env, fv)]
             val fixed = layoutVector (env, layoutTypVar, fixed)
             val is = semiCommaL (pok, fixed)
             val array = [LU.bracket (layoutTypVar (env, array))]
             val is = is @ array
             val l = LU.brace (L.mayAlign is)
           in l
           end
         | M.TCString => L.str "CString"
         | M.TIdx => L.str "Idx"
         | M.TContinuation ts =>
           L.seq [L.str "Cont", LU.parenSeq (layoutTyps (env, ts))]
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
             fun doOne (n, t) =
                 L.mayAlign [L.seq [layoutName (env, n), L.str ":"],
                             LU.indent (layoutTyp (env, t))]
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
         | M.TPRef t => L.seq [L.str "Ref", LU.paren (layoutTyp (env, t))]
   and layoutTyps (env, ts) = layoutVector (env, layoutTyp, ts)

   fun layoutBinder (env, v) =
       let
         val si = getSI env
         val vl = layoutVariable (env, v)
         val l =
             if showBinderTyps env then
               if MU.SymbolInfo.variableExists (si, v) then
                 let
                   val t = MU.SymbolInfo.variableTyp (getSI env, v)
                   val t = layoutTyp (env, t)
                   val l = L.mayAlign [L.seq [vl, L.str ":"], LU.indent t]
                 in l
                 end
               else
                 L.mayAlign [L.seq [vl, L.str ":"], L.str " BAD_VAR_NO_INFO"]
             else
               vl
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
       L.seq [layoutFieldKindShort (env, kind),
              layoutFieldVarianceShort (env, var)]

   fun layoutTupleDescriptor (env, M.TD {fixed, array}) =
       let
         val fixed = layoutVector (env, layoutFieldDescriptorShort, fixed)
         val array =
             case array
              of NONE => []
               | SOME fd => [LU.bracket (layoutFieldDescriptorShort (env, fd))]
         val l = LU.brace (L.mayAlign (L.separateRight (fixed, ",") @ array))
       in l
       end

   fun layoutMetaDataDescriptor (env, M.MDD {pok, fixed, array}) =
       let
         val pok = layoutPObjKind (env, pok)
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
         val l = LU.brace (L.mayAlign (semiCommaL (pok, fixed) @ array))
       in l
       end

   fun layoutConstant (env, c) =
       case c
        of M.CRat r => L.seq [IntInf.layout r, L.str "R"]
         | M.CInteger i => L.seq [IntInf.layout i, L.str "I"]
         | M.CName n => layoutName (env, n)
         | M.CIntegral i => IntArb.layout i
         | M.CFloat f => L.seq [Real32.layout f, L.str "F"]
         | M.CDouble d => L.seq [Real64.layout d, L.str "D"]
         | M.CViVector {typ, elts} =>
           let
             val typ = VI.layoutElemType typ
             val cs = layoutConstants (env, elts)
             val args = semiCommaL (typ, cs)
             val l = L.seq [L.str "Vector", LU.paren (L.mayAlign args)]
           in l
           end
         | M.CViMask {typ, elts} =>
           let
             val typ = VI.layoutElemType typ
             val bs = L.seq (Vector.toListMap (elts, LU.layoutBool'))
             val l = L.seq [L.str "Mask", LU.parenSeq [typ, bs]]
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
             then L.mayAlign [L.seq [l, L.str ":"],
                              LU.indent (VI.layoutElemType t)]
             else l
         val l =
             case fi
              of M.FiFixed idx => L.seq [L.str "sf:", Int.layout idx]
               | M.FiVariable opnd =>
                 L.seq [L.str "sv:", layoutOperand (env, opnd)]
               | M.FiViFixed {typ, idx} =>
                 wrapViElemType (L.seq [L.str "vf:", Int.layout idx], typ)
               | M.FiViVariable {typ, idx} =>
                 let
                   val l = L.seq [L.str "vv:", layoutOperand (env, idx)]
                   val l = wrapViElemType (l, typ)
                 in l
                 end
               | M.FiViIndexed {typ, idx} =>
                 let
                   val l = L.seq [L.str "vi:", layoutOperand (env, idx)]
                   val l = wrapViElemType (l, typ)
                 in l
                 end
         val l = LU.bracket l
       in l
       end

   fun layoutTupleField (env, M.TF {tupDesc, tup, field}) =
       let
         val tup = layoutVariable (env, tup)
         val tup =
             if showTupDesc env then
               let
                 val td = layoutTupleDescriptor (env, tupDesc)
                 val l = LU.paren (L.mayAlign [L.seq [tup, L.str ":"],
                                               LU.indent td])
               in l
               end
             else
               tup
         val field = layoutFieldIdentifier (env, field)
         val l = L.seq [tup, field]
       in l
       end

   fun layoutTuple (env, mdDesc, inits) =
       let
         val vtd = layoutMetaDataDescriptor (env, mdDesc)
         val inits = layoutOperands (env, inits)
         val l = LU.brace (L.mayAlign (semiCommaL (vtd, inits)))
       in l
       end

   fun addInit (env, l, vo) =
       case vo
        of NONE => l
         | SOME v => 
           L.mayAlign [L.seq [layoutVariable (env, v), L.str " <-"],
                       LU.indent l]

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
                     L.seq [opnd, L.str ": ", layoutFieldKindShort (env, fk)]
                   else
                     opnd
             in opnd
             end
         val fvs = L.separateRight (Vector.toListMap (fvs, doOne), ",")
       in fvs
       end

   fun layoutThunkTyp (env, l, typ) =
       if showThunkTyp env then
         L.seq [l, L.str ": ", layoutFieldKindShort (env, typ)]
       else
         l

   fun layoutThunkVar (env, t, typ, p) =
       if showThunkTyp env then
         let
           val t = layoutVariable (env, t)
           val typ = layoutFieldKindShort (env, typ)
           val l = L.seq [t, L.str ": ", typ]
           val l = if p then LU.paren l else l
         in l
         end
       else
         layoutVariable (env, t)

   fun layoutThunkVarO (env, t, typ) =
       case t
        of NONE =>
           if showThunkTyp env then [layoutFieldKindShort (env, typ)] else []
         | SOME t => [layoutThunkVar (env, t, typ, false)]

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
         L.seq [l, L.str ": ", layoutFieldKindShort (env, fk)]
       else
         l

   fun layoutRhs (env, rhs) =
       case rhs
        of M.RhsSimple s => layoutSimple (env, s)
         | M.RhsPrim {prim, createThunks, args} =>
           L.seq [L.seq [Prims.layout prim,
                         L.str (if createThunks then "T" else "D")],
                  LU.parenSeq (layoutOperands (env, args))]
         | M.RhsTuple {mdDesc, inits} => layoutTuple (env, mdDesc, inits)
         | M.RhsTupleSub tf => layoutTupleField (env, tf)
         | M.RhsTupleSet {tupField, ofVal} =>
           L.mayAlign [L.seq [layoutTupleField (env, tupField), L.str " <-"],
                       LU.indent (layoutOperand (env, ofVal))]
         | M.RhsTupleInited {mdDesc, tup} =>
           let
             val mdDesc = layoutMetaDataDescriptor (env, mdDesc)
             val tup = layoutVariable (env, tup)
             val l = L.seq [L.str "Inited", L.tuple [mdDesc, tup]]
           in l
           end
         | M.RhsIdxGet {idx, ofVal} =>
           L.seq [layoutVariable (env, idx),
                  LU.bracket (L.seq [L.str "i", layoutOperand (env, ofVal)])]
         | M.RhsCont l => L.seq [L.str "Cont", LU.paren (layoutLabel (env, l))]
         | M.RhsObjectGetKind v =>
           L.seq [L.str "GetKind", LU.paren (layoutVariable (env, v))]
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
                             LU.brace (L.mayAlign (semiCommaL (typ, fvs)))
                           else
                             typ
                         else
                           LU.braceSeq fvs
                     val l =
                         LU.paren (L.mayAlign [L.seq [thunk, L.str ":"], l])
                   in l
                   end
                 else
                   thunk
             val l = L.seq [thunk, L.str ("tfv" ^ Int.toString idx)]
           in l
           end
         | M.RhsThunkValue {typ, thunk, ofVal} =>
           let
             val typ =
                 if showThunkTyp env
                 then L.seq [layoutFieldKindShort (env, typ), L.str ";"]
                 else L.empty
             val ofVal = L.seq [typ, layoutOperand (env, ofVal)]
             val l = L.seq [L.str "ThunkMkVal", LU.parenSeq ([ofVal])]
             val l = addInit (env, l, thunk)
           in l
           end
         | M.RhsThunkGetValue {typ, thunk} =>
           L.seq [layoutThunkVar (env, thunk, typ, true), L.str ".tval"]
         | M.RhsThunkSpawn {typ, thunk, fx} =>
           L.seq [L.str "Spawn",
                  LU.paren (layoutThunkVar (env, thunk, typ, false)),
                  layoutEffects (env, fx)]
         | M.RhsClosureMk {fvs} =>
           L.seq [L.str "ClosureMk", layoutClosureFvs (env, fvs)]
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
                   LU.paren (L.mayAlign [L.seq [cls, L.str ":"],
                                         layoutClosureFvs (env, fvs)])
                 else
                   cls
             val l = L.seq [cls, L.str (".pffv" ^ Int.toString idx)]
           in l
           end
         | M.RhsPSetNew oper => 
           L.seq [L.str "Set", L.paren (layoutOperand (env, oper))]
         | M.RhsPSetGet v =>
           L.seq [layoutVariable (env, v), L.str ".elt"]
         | M.RhsPSetCond {bool, ofVal} =>
           L.seq [layoutOperand (env, bool),
                  L.str " ? ", 
                  LU.brace (layoutOperand (env, ofVal)),
                  L.str " : {}"]
         | M.RhsPSetQuery oper => L.seq [layoutOperand (env, oper), L.str "?"]
         | M.RhsPSum {tag, typ, ofVal} =>
           L.seq [layoutName (env, tag),
                  LU.angleBracket
                    (layoutPSumTyp (env, layoutOperand (env, ofVal), typ))]
         | M.RhsPSumProj {typ, sum, tag} =>
           layoutPSumTyp (env,
                          L.seq [layoutVariable (env, sum),
                                 L.str ".",
                                 layoutName (env, tag)],
                          typ)

   fun layoutInstruction (env, M.I {dests, n, rhs}) =
       let
         val rhs = layoutRhs (env, rhs)
         val num = 
             if showNumbers env then
               L.seq [L.str " > ", Int.layout n]
             else
               L.empty
         val l = 
             L.mayAlign [L.seq [LU.parenSeq (layoutVector (env, layoutBinder, dests)), num, L.str " ="],
                         LU.indent rhs]
       in l
       end

   fun layoutTarget (env, M.T {block, arguments}) =
       let
         val anot =
             case (#edge (getHelpers env), getBlock env)
              of (SOME get, SOME b) =>
                 (case get (b, block)
                   of SOME s => L.seq [LU.space, LU.bracket (L.str s)]
                    | NONE => L.empty)
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
                   val l = LU.indent (L.mayAlign [L.str "DEFAULT =>", LU.indent t])
                 in [l]
                 end
         val trailer = L.str "}"
         val l = L.mayAlign (header::cases @ default @ [trailer])
       in l
       end

   fun layoutCodes (env, {possible, exhaustive} : M.codes) =
       L.seq [L.str (if exhaustive then "<=" else "?"),
              VS.layout (possible, fn v => layoutVariable (env, v))]

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
        of M.EThunk {thunk, code} =>
           addCodes (env,
                     L.seq [L.str "Eval",
                            LU.paren (layoutVariable (env, thunk))],
                     code)
         | M.EDirectThunk {thunk, code} =>
           L.seq [L.str "EvalDir", LU.parenSeq [layoutVariable (env, thunk),
                                                layoutVariable (env, code)]]

   fun layoutInterProc (env, ip) =
       case ip
        of M.IpCall {call, args} =>
           L.mayAlign [layoutCall (env, call),
                       LU.indent (LU.parenSeq (layoutOperands (env, args)))]
         | M.IpEval {typ, eval} =>
           let
             val l = layoutEval (env, eval)
             val l =
                 if showThunkTyp env then
                   L.seq [l, L.str " :", layoutFieldKindShort (env, typ)]
                 else
                   l
           in l
           end

   fun layoutCutsA (env, M.C {exits, targets}) = 
       if exits orelse (not (LS.isEmpty targets)) then
         let
           val ts = LS.toList targets
           val l = List.map (ts, fn l => layoutLabel (env, l))
           val l = if exits then L.str "EXIT"::l else l
         in
           [L.seq [L.str "/->/ ", LU.bracketSeq l]]
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
             val l = L.mayAlign (L.seq [L.str "-> ", L.mayAlign [rets, block]]
                                 ::cuts)
           in l
           end
         | M.RTail {exits} => L.str ("-| /->/ " ^ (if exits then "EXIT" else "NONE"))

   fun layoutTransfer (env, t) = 
       case t
        of M.TGoto tg => L.seq [L.str "Goto ", layoutTarget (env, tg)]
         | M.TCase s => layoutSwitch ("Case", layoutConstant) (env, s)
         | M.TInterProc {callee, ret, fx} =>
           L.mayAlign [layoutInterProc (env, callee), 
                       L.mayAlign [layoutReturn (env, ret), layoutEffects (env, fx)]]
         | M.TReturn vs => L.seq [L.str "Return", LU.parenSeq (layoutOperands (env, vs))]
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
               | SOME get =>
                 case get l
                  of NONE => L.empty
                   | SOME s => L.seq [LU.space, LU.bracket (L.str s)]
         val header = L.seq [layoutLabel (env, l), parameters, anot]
         val instructions = layoutVector (env, layoutInstruction, instructions)
         val transfer = layoutTransfer (env, transfer)
         val l = L.align [header, LU.indent (L.align instructions), LU.indent transfer]
       in l
       end

   fun layoutBlock (env, (l, M.B {parameters, instructions, transfer})) =
       let
         val parameters = layoutBinders (env, parameters)
         val anot =
             case #block (getHelpers env)
              of NONE => L.empty
               | SOME get =>
                 case get l
                  of NONE => L.empty
                   | SOME s => L.seq [LU.space, LU.bracket (L.str s)]
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
         val entry = L.seq [L.str "ENTRY ", layoutLabel (env, entry)]
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
         val cc        = layoutCallConv layoutVariable (env, cc)
         val args      = layoutVector (env, layoutBinder, args)
         val args      = LU.paren (L.mayAlign (semiCommaL (cc, args)))
         val rtyps     = LU.parenSeq (layoutTyps (env, rtyps))
         val header    = L.seq [L.str "fun", escapes, recursive, args, fx]
         val header    = L.mayAlign [header, L.seq [L.str ": ", rtyps]]
         val body      = layoutCodeBody (env, body)
       in
         L.align [header, L.str "{", LU.indent body, L.str "}"]
       end

   fun layoutIdx (env, idx) =
       let
         val idx = ND.toList idx
         fun doOne (n, i) = L.seq [layoutName (env, n), L.str " -> ", Int.layout i]
         val l = LU.bracketSeq (List.map (idx, doOne))
       in l
       end

   fun layoutGlobalOnly (env, global) = 
       case global
        of M.GCode code => layoutCode (env, code)
         | M.GErrorVal t => L.seq [L.str "ERRORVAL : ", layoutTyp (env, t)]
         | M.GIdx d => layoutIdx (env, d)
         | M.GTuple {mdDesc, inits} => layoutTuple (env, mdDesc, inits)
         | M.GRat r => L.seq [Rat.layout r, L.str "R"]
         | M.GInteger i => L.seq [IntInf.layout i, L.str "I"]
         | M.GCString s => L.seq [L.str "CString", L.paren (L.str s)]
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
             val l = L.seq [tag, LU.angleBracket ofVal]
           in l
           end
         | M.GPSet opnd => L.seq [L.str "Set", L.paren (layoutOperand (env, opnd))]

   fun layoutGlobal (env, (v, g)) =
       let
         val header = L.seq [L.str "G ", layoutBinder (env, v), L.str " ="]
         val res =  L.mayAlign [header, LU.indent (layoutGlobalOnly (env, g))]
       in res
       end
       
   fun layoutGlobals (env, globals) = 
       let
         val gs = VD.toList globals
         val ls = List.map (gs, fn g => layoutGlobal (env, g))
         val l = L.align ls
       in l
       end

   fun layoutIncludeFile (env, M.IF {name, kind, externs}) =
       let
         val k = L.seq [L.str ": ", L.str (MU.IncludeKind.toString kind)]
         val externs = VS.layout (externs, fn v => layoutVariable (env, v))
         val l = L.mayAlign [L.str name, LU.indent k, LU.indent externs]
       in l
       end

   fun layoutSymbolTable (env, st) = 
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
         val vs = Identifier.listVariables st
         val entries = List.map (vs, layout)
       in
         L.align entries
       end

   fun describe () =
       L.align [L.str (modulename ^ " control string consists of:"),
                LU.indent (L.align [L.str "b => show types on variable binders",
                                    L.str "c => show codes in CcClosure",
                                    L.str "f => show P Function free variable field kinds",
                                    L.str "n => show instruction number",
                                    L.str "s => show sum types",
                                    L.str "S => show symbol table",
                                    L.str "t => show metadata/tuple descriptors",
                                    L.str "T => show thunk types",
                                    L.str "U => show thunk free variable field kinds",
                                    L.str "v => show vector element types",
                                    L.str "+ => show all of the above"]),
                L.str "default is c"]

   fun parse str =
       let
         val binderTyps = ref false
         val viElemType = ref false
         val tupDesc    = ref false
         val thunkTyp   = ref false
         val thunkFvs   = ref false
         val pFunFvs    = ref false
         val pSumTyp    = ref false
         val codes      = ref false
         val symbols    = ref false
         val numbers    = ref false
         fun doOne c =
             case c
              of #"b" => let val () = binderTyps := true in true end
               | #"c" => let val () = codes := true in true end
               | #"f" => let val () = pFunFvs := true in true end
               | #"n" => let val () = numbers := true in true end
               | #"s" => let val () = pSumTyp := true in true end
               | #"S" => let val () = symbols := true in true end
               | #"t" => let val () = tupDesc := true in true end
               | #"T" => let val () = thunkTyp := true in true end
               | #"U" => let val () = thunkFvs := true in true end
               | #"v" => let val () = viElemType := true in true end
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
                   val () = numbers := true
                 in true
                 end
               | _    => false
       in
         if List.forall (String.explode str, doOne) then
           SOME ({binderTyps = !binderTyps, viElemType = !viElemType, tupDesc = !tupDesc, thunkTyp = !thunkTyp,
                  thunkFvs = !thunkFvs, pFunFvs = !pFunFvs, pSumTyp = !pSumTyp, codes = !codes, numbers = !numbers},
                 !symbols)
         else
           NONE
       end

   fun dft _ = ({binderTyps = false, viElemType = false, tupDesc = false, thunkTyp = false, thunkFvs = false,
                 pFunFvs = false, pSumTyp = false, codes = true, numbers = false},
                false)

   val (control, controlGet) = Config.Control.mk (modulename, describe, parse, dft)

   fun envMk (c, si, hs) =
       let
         val (options, symbols) = controlGet c
         val env = E {config = c, si = si, helpers = hs, options = options, block = NONE}
       in (env, symbols)
       end
       
   fun layoutProgram (c, hs, M.P {includes, externs, globals, symbolTable, entry}) =
       let
         val (env, symbols) = envMk (c, Identifier.SymbolInfo.SiTable symbolTable, hs)
         val includes =
             [L.str "INCLUDES:", LU.indent (L.align (Vector.toListMap (includes, fn i => layoutIncludeFile (env, i))))]
         val externs = [L.str "EXTERNS:", LU.indent (VS.layout (externs, fn v => layoutVariable (env, v)))]
         val symtab =
             if symbols then
               [L.str "SYMBOLS:", LU.indent (layoutSymbolTable (env, symbolTable))]
             else
               []
         val globals = [L.str "GLOBALS:", LU.indent (layoutGlobals (env, globals))]
         val entry = [L.seq [L.str "ENTRY: ", layoutVariable (env, entry)]]
         val l = L.align (includes @ externs @ symtab @ globals @ entry)
       in l
       end

   fun layoutGlobalsOnly (c, hs, M.P {symbolTable, globals, ...}) =
       let
         val (env, _) = envMk (c, Identifier.SymbolInfo.SiTable symbolTable, hs)
         val globals = [L.str "GLOBALS:", LU.indent (layoutGlobals (env, globals))]
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

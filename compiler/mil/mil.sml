(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

structure Mil =
struct

  structure VD = Identifier.VariableDict
  structure VS = Identifier.VariableSet
  structure ND = Identifier.NameDict
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict

  type variable = Identifier.variable
  type name = Identifier.name
  type label = Identifier.label
  type effects = Effect.set

  datatype abiCallConv =
      AbiCdecl
    | AbiStdcall

  datatype 'a callConv =
      CcCode
    | CcUnmanaged of abiCallConv
    | CcClosure of {cls   : 'a, fvs : 'a Vector.t}
    | CcThunk   of {thunk : 'a, fvs : 'a Vector.t}

  datatype typKind = TkI | TkE

  datatype pObjKind =
      PokNone
    | PokRat
    | PokFloat
    | PokDouble
    | PokName
    | PokFunction
    | PokArray
    | PokDict
    | PokTagged
    | PokOptionSet
    | PokPtr
    | PokType
    | PokCell

  datatype valueSize = Vs8 | Vs16 | Vs32 | Vs64 | Vs128 | Vs256 | Vs512 | Vs1024

  datatype fieldVariance = FvReadOnly | FvReadWrite

  datatype fieldSize = Fs8 | Fs16 | Fs32 | Fs64

  structure Prims = MilPrimsF(struct
                                type fieldSize = fieldSize
                              end)

  (* Vector indexing conventions:
   *   The element at index 0 is the same element that a vector load will load from the address loaded from.
   *   The element at the highest index is the same element that a vector load will load from the address furtherest
   *   away from the addresse loaded from.
   *)

  (* Unboxed constants *)
  datatype constant =
    (* Core *)
      CBoolean  of bool
    | CRat      of IntInf.t   (* Only optimised reps *)
    | CInteger  of IntInf.t   (* Only optimised reps *)
    | CName     of name
    | CIntegral of IntArb.t
    | CFloat    of Real32.t
    | CDouble   of Real64.t
    | CViMask   of {descriptor : Prims.vectorDescriptor, elts : bool Vector.t} (* see vector indexing conventions *)
    | CPok      of pObjKind
    | CRef      of IntInf.t   (* Only gc-ignored refs *)
    (* HL *)
    | COptionSetEmpty
    | CTypePH 
 
  datatype typ =
    (* Core *)
      TAny                        (* All values *)
    | TAnyS         of valueSize  (* All values of given size *)
    | TNonRefPtr                  (* All non-GC pointers *)
    | TRef                        (* All GC valid pointers to object starts *)
    | TBits         of valueSize  (* All values of given size except heap pointers, floats, and doubles *)
    | TNone                       (* No values *)
    | TName
    | TNumeric      of Prims.numericTyp
    | TBoolean
    (* elementTyp cannot be vector or mask
     * elementType must have definite fieldSize
     * can't appear in other code types
     *)
    | TViVector     of {vectorSize : Prims.vectorSize, 
                        elementTyp : typ} 
    (* Does not have definite size *)
    | TViMask       of Prims.vectorDescriptor     

    | TCode         of {cc : typ callConv, 
                        args : typ Vector.t, 
                        ress : typ Vector.t}
    | TTuple        of {pok : pObjKind, 
                        fixed : (typ * valueSize * fieldVariance) Vector.t, 
                        array : (typ * valueSize * fieldVariance)}
    | TCString
    | TIdx
    | TContinuation of typ Vector.t
    | TThunk        of typ
    (* HL *)
    | TPAny
    | TClosure      of {args : typ Vector.t, 
                        ress : typ Vector.t}
    | TSum          of {tag : typ, 
                        arms : (constant * (typ Vector.t)) Vector.t} 
                        (* No duplicates, sorted. *)
    | TPType        of {kind : typKind, 
                        over : typ}
    | TPRef         of typ


  (* FkFloat and FkDouble are separate from bits, as backend casting is not preserving *)
  datatype fieldKind = FkRef | FkBits of fieldSize | FkFloat | FkDouble

  datatype fieldDescriptor = FD of {kind : fieldKind, alignment : valueSize, var : fieldVariance}

  datatype tupleDescriptor = TD of {
    fixed : fieldDescriptor Vector.t,
    array : fieldDescriptor option
  }

  datatype metaDataDescriptor = MDD of {
    pok    : pObjKind,
    pinned : bool, 
    fixed  : fieldDescriptor Vector.t,
    array  : (int * fieldDescriptor) option
  }

  datatype simple = 
      SVariable of variable
    | SConstant of constant

  type operand = simple

  datatype tupleBase = 
      TbScalar      (* Operation is on an array *)
    | TbVector      (* Operation is on a vector of arrays *)

  datatype vectorIndexKind = 
      VikStrided of int  (* Strided load:      X[i:i+(s*n)]. n *)
    | VikVector          (* General load:      X[av] *)

  datatype fieldIdentifier = 
      FiFixed          of int                                   (* a.i *)
    | FiVariable       of operand                               (* a[i] *)
    | FiVectorFixed    of {descriptor : Prims.vectorDescriptor, 
                           mask : operand option,
                           index : int}  
    | FiVectorVariable of {descriptor : Prims.vectorDescriptor,  (* index[Y]. the tupleBase describes X *)
                           base : tupleBase,  
                           mask : operand option,
                           index : operand,
                           kind: vectorIndexKind}          (* and the vectorIndexKind describes Y*)

  datatype tupleField = TF of {
    tupDesc : tupleDescriptor,
    tup     : variable,
    field   : fieldIdentifier
  }

  datatype rhs =
    (* Core *)
      RhsSimple        of simple
    | RhsPrim          of {prim : Prims.t, 
                           createThunks : bool, 
                           typs : typ Vector.t, 
                           args : operand Vector.t}
    | RhsTuple         of {mdDesc : metaDataDescriptor,  (* Length field must be initialised *)
                           inits  : operand Vector.t}    (* Initialises a prefix of the fields;
                                                          * can be less than all fixed fields;
                                                          * can include some/all array elements
                                                          *)
    | RhsTupleSub      of tupleField
    | RhsTupleSet      of {tupField : tupleField, 
                           ofVal : operand}
    | RhsTupleCAS      of {tupField : tupleField,        (* Only pointer sized fields allowed. *)
                           cmpVal : operand,             (* Returns the field's old value.     *)
                           newVal : operand}
    | RhsTupleInited   of {mdDesc : metaDataDescriptor, 
                           tup : variable}
    | RhsIdxGet        of {idx : variable, 
                           ofVal : operand}
    | RhsCont          of label
    | RhsObjectGetKind of variable
    | RhsThunkMk       of {typ : fieldKind, 
                           fvs : fieldKind Vector.t}
    | RhsThunkInit     of {typ   : fieldKind,
                           thunk : variable option,   (* if absent then create *)
                           fx    : effects,
                           code  : variable option,   (* Must be function name. If absent then this is an environment, 
                                                       * which can only be projected from but not evaled.
                                                       *)
                           fvs   : (fieldKind * operand) Vector.t}
    | RhsThunkGetFv    of {typ   : fieldKind,
                           fvs   : fieldKind Vector.t,
                           thunk : variable,
                           idx   : int}
    | RhsThunkValue    of {typ    : fieldKind,
                           thunk  : variable option, (* if absent then create *)
                           ofVal : operand}
    | RhsThunkGetValue of {typ : fieldKind, 
                           thunk : variable} (* thunk must be evaled *)
    | RhsThunkSpawn    of {typ : fieldKind, 
                           thunk : variable, 
                           fx : effects}
    (* HL *)
    | RhsClosureMk     of {fvs : fieldKind Vector.t}
    | RhsClosureInit   of {cls  : variable option,                (* if absent, create;
                                                                   * if present, must be from ClosureMk
                                                                   *)
                           code : variable option,   (* Must be function name. If absent then this is an environment, 
                                                      * which can only be projected from but not called.  *)
                           fvs  : (fieldKind * operand) Vector.t}
    | RhsClosureGetFv  of {fvs : fieldKind Vector.t, 
                           cls : variable, 
                           idx : int}
    | RhsPSetNew       of operand
    | RhsPSetGet       of variable
    | RhsPSetCond      of {bool : operand, 
                           ofVal : operand} (* if bool then {ofVal} else {} *)
    | RhsPSetQuery     of operand (* {} => false | _ => true *)
    | RhsEnum          of {tag : operand, typ : fieldKind}  (* non value carrying sum object *)
    | RhsSum           of {tag : constant, 
                           ofVals : operand Vector.t,
                           typs   : fieldKind Vector.t}
    | RhsSumProj       of {typs : fieldKind Vector.t, 
                           sum : variable,
                           tag : constant, 
                           idx : int}
    | RhsSumGetTag     of {typ : fieldKind, 
                           sum : variable}

  datatype instruction = I of {
    dests : variable vector,     (* arity must match rhs *)
    n     : int,                 (* scratch info, pass specific *)
    rhs   : rhs
  }

  datatype target = T of {block : label, 
                          arguments : operand Vector.t}
                      
  datatype selector = 
           SeSum of fieldKind (*HL*) (* dispatch on sum tag of the operand.  fieldKind is size of tag *)
         | SeConstant (* Core *)     (* dispatch on the operand itself *)

  (* The variables in "possible" are always function names *)
  type codes = {possible : VS.t, exhaustive : bool}

  (* The code variable in CDirectClosure and in EDirectThunk must be
   * a function name.
   *) 
  datatype call =
      CCode          of {ptr : variable, code : codes}
    | CClosure       of {cls : variable, code : codes}
    | CDirectClosure of {cls : variable, code : variable}

  (* The codes/code describe the set of code pointers that can possibly 
   * reach here, or produce a value which reaches here.  
   * Value is only guaranteed to be correct if the codes are exhaustive.
   * Value indicates whether or not a value reaching this point could have
   * been created by a thunkvalue instruction.  
   * If value is false, then everything which reaches here is either unevaluated
   * or is a thunk value produced by running one of the code pointers in code.
   *)
  datatype eval =
      EThunk       of {thunk : variable, value : bool, code : codes}
    | EDirectThunk of {thunk : variable, value : bool, code : variable}

  datatype interProc =
      IpCall of {call : call, args : operand Vector.t}
    | IpEval of {typ : fieldKind, eval : eval}

  datatype cuts = C of {exits : bool, targets : LS.t}  

  datatype return =
      RNormal of {rets : variable Vector.t, block : label, cuts : cuts}
    | RTail of {exits : bool}

  datatype transfer =
    (* Core *)
      TGoto      of target
    | TCase      of {select  : selector,
                     on      : operand, 
                     cases   : (constant * target) Vector.t, 
                     default : target option}
    | TInterProc of {callee : interProc, ret : return, fx : effects}
    | TReturn    of operand Vector.t
    | TCut       of {cont : variable, args : operand Vector.t, cuts : cuts}
    | THalt      of operand

  datatype block = B of {
    parameters   : variable Vector.t,
    instructions : instruction Vector.t,
    transfer     : transfer  
  }

  datatype codeBody = CB of {entry : label, blocks : block LD.t}

  (* f not escapes => no unknown calls
   * f not recursive => 
   *  calling f never leads to another call to f before the 
   *  return of the first call.
   *)
  datatype code = F of {
    fx        : effects,
    escapes   : bool,
    recursive : bool,
    cc        : variable callConv,
    args      : variable Vector.t,
    rtyps     : typ Vector.t,
    body      : codeBody
  }

  (* Invariant: Before lowering, code pointers should never escape except through
   * closures.
   *)
  datatype global =
    (* Core *)
      GCode       of code
    | GErrorVal   of typ
    | GIdx        of int ND.t
    | GTuple      of {mdDesc : metaDataDescriptor,
                      inits  : simple Vector.t}      (* must be all fields *)
    | GRat        of Rat.t
    | GInteger    of IntInf.t
    | GCString    of string
    | GThunkValue of {typ : fieldKind, 
                      ofVal : simple}
    (* HL *)
    | GSimple     of simple
    | GClosure    of {code : variable option, 
                      fvs : (fieldKind * simple) Vector.t}
    | GSum       of {tag    : constant, 
                     ofVals : simple Vector.t,
                     typs   : fieldKind Vector.t}
    | GPSet       of simple

  type globals = global VD.t

  (* An IkTarget include file is of the same type (C/Pillar) as we are generating from P.
   * An IkC include file is always a C file.
   *)
  datatype includeKind = IkC | IkTarget
  datatype includeFile = IF of {name : string, kind : includeKind, externs : VS.t}
  datatype externGroup = EG of {kind : includeKind, externs : VS.t}

  datatype variableKind = VkExtern | VkGlobal | VkLocal
  datatype variableInfo = VI of {typ : typ, kind : variableKind}
  type symbolTable = variableInfo Identifier.symbolTable
  type symbolTableManager = variableInfo Identifier.Manager.t
  type symbolInfo = variableInfo Identifier.SymbolInfo.t

  datatype t = P of {
    includes    : includeFile Vector.t,
    externs     : externGroup Vector.t,
    globals     : globals,
    symbolTable : symbolTable,
    entry       : variable
  }

end (* structure Mil *)

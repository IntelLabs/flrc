(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* The main IL *)

structure Mil =
struct

  structure VI = VectorInstructions
  structure VD = Identifier.VariableDict
  structure VS = Identifier.VariableSet
  structure ND = Identifier.NameDict
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict

  type variable = Identifier.variable
  type name = Identifier.name
  type label = Identifier.label
  type effects = Effect.set

  datatype 'a callConv =
      CcCode
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

  datatype valueSize = Vs8 | Vs16 | Vs32 | Vs64 | Vs128 | Vs256 | Vs512 

  datatype fieldVariance = FvReadOnly | FvReadWrite

  datatype typ =
    (* Core *)
      TAny                (* All values *)
    | TAnyS of valueSize  (* All values of given size *)
    | TPtr                (* All pointers; traceability unknown *)
    | TRef                (* All GC valid pointers to object starts *)
    | TBits of valueSize  (* All values of given size except heap pointers, floats, and doubles *)
    | TNone               (* No values *)
    | TRat
    | TInteger
    | TName
    | TIntegral of IntArb.typ
    | TFloat
    | TDouble
    | TViVector of VI.elemType
    | TViMask of VI.elemType
    | TCode of {cc : typ callConv, args : typ Vector.t, ress : typ Vector.t}
    | TTuple of {pok : pObjKind, fixed : (typ * fieldVariance) Vector.t, array : (typ * fieldVariance)}
    | TCString
    | TIdx
    | TContinuation of typ Vector.t
    | TThunk of typ
    (* HL *)
    | TPAny
    | TClosure of {args : typ Vector.t, ress : typ Vector.t}
    | TPSum of typ ND.t
    | TPType of {kind : typKind, over : typ}
    | TPRef of typ

  datatype fieldSize = Fs8 | Fs16 | Fs32 | Fs64

  datatype fieldKind = FkRef | FkBits of fieldSize | FkFloat | FkDouble

  datatype fieldDescriptor = FD of {kind : fieldKind, var : fieldVariance}

  datatype tupleDescriptor = TD of {
    fixed : fieldDescriptor Vector.t,
    array : fieldDescriptor option
  }

  datatype metaDataDescriptor = MDD of {
    pok   : pObjKind,
    fixed : fieldDescriptor Vector.t,
    array : (int * fieldDescriptor) option
  }

  (* Vector indexing conventions:
   *   The element at index 0 is the same element that a vector load will load from the address loaded from.
   *   The element at the highest index is the same element that a vector load will load from the address furtherest
   *   away from the addresse loaded from.
   *)

  (* Unboxed constants *)
  datatype constant =
    (* Core *)
      CRat      of IntInf.t   (* Only optimised reps *)
    | CInteger  of IntInf.t   (* Only optimised reps *)
    | CName     of name
    | CIntegral of IntArb.t
    | CFloat    of Real32.t
    | CDouble   of Real64.t
    | CViVector of {typ : VI.elemType, elts : constant Vector.t}  (* see vector indexing conventions above *)
    | CViMask   of {typ : VI.elemType, elts : bool Vector.t}      (* see vector indexing conventions above *)
    | CPok      of pObjKind
    (* HL *)
    | COptionSetEmpty
    | CTypePH 
 
  datatype simple =
      SVariable of variable
    | SConstant of constant

  type operand = simple

  datatype fieldIdentifier =
      FiFixed      of int
    | FiVariable   of operand
    | FiViFixed    of {typ : VI.elemType, idx : int}       (* the fields starting at this fixed offset *)
    | FiViVariable of {typ : VI.elemType, idx : operand}   (* the array portion fields starting at this index *)
    | FiViIndexed  of {typ : VI.elemType, idx : operand}   (* the array portion fields given by this vector index *)
                                                           (* Missing vector of tuples, scalar offset into the tuples *)
                                                           (* Missing vector of tuples, vector of offests into the tuples? *)

  datatype tupleField = TF of {
    tupDesc : tupleDescriptor,
    tup     : variable,
    field   : fieldIdentifier
  }

  datatype rhs =
    (* Core *)
      RhsSimple of simple
    | RhsPrim of {prim : Prims.t, createThunks : bool, args : operand Vector.t}
    | RhsTuple of {
        mdDesc : metaDataDescriptor,  (* Length field must be initialised *)
        inits  : operand Vector.t     (* Initialises a prefix of the fields;
                                       * can be less than all fixed fields;
                                       * can include some/all array elements
                                       *)
      }
    | RhsTupleSub of tupleField
    | RhsTupleSet of {tupField : tupleField, ofVal : operand}
    | RhsTupleInited of {mdDesc : metaDataDescriptor, tup : variable}
    | RhsIdxGet of {idx : variable, ofVal : operand}
    | RhsCont of label
    | RhsObjectGetKind of variable
    | RhsThunkMk of {typ : fieldKind, fvs : fieldKind Vector.t}
    | RhsThunkInit of {
        typ   : fieldKind,
        thunk : variable option,                   (* if absent then create *)
        fx    : effects,
        code  : variable option,                   (* Must be function name. If absent then this is an environment, 
                                                    * which can only be projected from but not evaled.
                                                    *)
        fvs   : (fieldKind * operand) Vector.t
      }
    | RhsThunkGetFv of {
        typ   : fieldKind,
        fvs   : fieldKind Vector.t,
        thunk : variable,
        idx   : int
      }
    | RhsThunkValue of {
        typ    : fieldKind,
        thunk  : variable option, (* if absent then create *)
        ofVal : operand
      }
    | RhsThunkGetValue of {typ : fieldKind, thunk : variable} (* thunk must be evaled *)
    | RhsThunkSpawn of {typ : fieldKind, thunk : variable, fx : effects}
    (* HL *)
    | RhsClosureMk of {fvs : fieldKind Vector.t}
    | RhsClosureInit of {
        cls  : variable option,                (* if absent, create;
                                                * if present, must be from ClosureMk
                                                *)
        code : variable option,                (* Must be function name. If absent then this is an environment, 
                                                * which can only be projected from but not called.  *)
        fvs  : (fieldKind * operand) Vector.t
      }
    | RhsClosureGetFv of {fvs : fieldKind Vector.t, cls : variable, idx : int}
    | RhsPSetNew of operand
    | RhsPSetGet of variable
    | RhsPSetCond of {bool : operand, ofVal : operand} (* if bool then {ofVal} else {} *)
    | RhsPSetQuery of operand (* {} => false | _ => true *)
    | RhsPSum of {tag : name, typ : fieldKind, ofVal : operand}
    | RhsPSumProj of {typ : fieldKind, sum : variable, tag : name}

  datatype instruction = I of {
    dests : variable vector,     (* arity must match rhs *)
    n     : int,                 (* scratch info, pass specific *)
    rhs   : rhs
  }

  datatype target = T of {block : label, arguments : operand Vector.t}

  type 'a switch = {
    on      : operand,
    cases   : ('a * target) Vector.t,
    default : target option
  }

  (* The variables in "possible" are always function names *)
  type codes = {possible : VS.t, exhaustive : bool}

  (* The variable in CCode must be a function name pre-lowering,
   * but after lowering it may be bound to an arbitary instruction. 
   * The code variable in CDirectThunk and in EDirectThunk must be
   * a function name. *) 
  datatype call =
      CCode          of {ptr : variable, code : codes}
    | CClosure       of {cls : variable, code : codes}
    | CDirectClosure of {cls : variable, code : variable}

  datatype eval =
      EThunk       of {thunk : variable, code : codes}
    | EDirectThunk of {thunk : variable, code : variable}

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
    | TCase      of constant switch
    | TInterProc of {callee : interProc, ret : return, fx : effects}
    | TReturn    of operand Vector.t
    | TCut       of {cont : variable, args : operand Vector.t, cuts : cuts}
    | THalt      of operand
    (* HL *)
    | TPSumCase  of name switch

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
    | GTuple      of {
        mdDesc : metaDataDescriptor,
        inits  : simple Vector.t      (* must be all fields *)
      }
    | GRat        of Rat.t
    | GInteger    of IntInf.t
    | GCString    of string
    | GThunkValue of {typ : fieldKind, ofVal : simple}
    (* HL *)
    | GSimple     of simple
    | GClosure    of {code : variable option, fvs : (fieldKind * simple) Vector.t}
    | GPSum       of {tag : name, typ : fieldKind, ofVal : simple}
    | GPSet       of simple

  type globals = global VD.t

  (* An IkTarget include file is of the same type (C/Pillar) as we are generating from P.
   * An IkC include file is always a C file.
   *)
  datatype includeKind = IkC | IkTarget
  datatype includeFile = IF of {name : string, kind : includeKind, externs : VS.t}

  datatype variableKind = VkExtern | VkGlobal | VkLocal
  datatype variableInfo = VI of {typ : typ, kind : variableKind}
  type symbolTable = variableInfo Identifier.symbolTable
  type symbolTableManager = variableInfo Identifier.Manager.t
  type symbolInfo = variableInfo Identifier.SymbolInfo.t

  datatype t = P of {
    includes    : includeFile Vector.t,
    externs     : VS.t,
    globals     : globals,
    symbolTable : symbolTable,
    entry       : variable
  }

end

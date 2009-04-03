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
    | PokOArray
    | PokIArray
    | PokSum
    | PokOptionSet
    | PokRef
    | PokType
    | PokThunk

  datatype valueSize = Vs8 | Vs16 | Vs32 | Vs64 | Vs128 | Vs256 | Vs512

  datatype fieldVariance = FvReadOnly | FvReadWrite

  datatype typ =
    (* Core *)
      TAny                (* All values *)
    | TAnyS of valueSize  (* All values of given size *)
    | TPtr                (* All pointers; traceability unknown *)
    | TRef                (* All GC valid pointers to object starts *)
    | TBits of valueSize  (* All values of given size except heap pointers *)
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
    | TTuple of {
        pok   : pObjKind,
        fixed : (typ * fieldVariance) Vector.t,
        array : (typ * fieldVariance) option
      }
    | TIdx
    | TContinuation of typ Vector.t
    | TThunk of typ
    (* HL *)
    | TPAny
    | TPFunction of {args : typ Vector.t, ress : typ Vector.t}
    | TPSum of typ ND.t
    | TPType of {kind : typKind, over : typ}
    | TPRef of typ

  datatype fieldSize = Fs8 | Fs16 | Fs32 | Fs64

  datatype fieldKind = FkRef | FkBits of fieldSize

  datatype fieldDescriptor = FD of {kind : fieldKind, var : fieldVariance}

  datatype tupleDescriptor = TD of {
    fixed : fieldDescriptor Vector.t,
    array : fieldDescriptor option
  }

  datatype vtableDescriptor = VTD of {
    pok   : pObjKind,
    fixed : fieldDescriptor Vector.t,
    array : (int * fieldDescriptor) option
  }

  (* Unboxed constants *)
  datatype constant =
    (* Core *)
      CRat      of IntInf.t   (* Only optimised reps *)
    | CInteger  of IntInf.t   (* Only optimised reps *)
    | CName     of name
    | CIntegral of IntArb.t
    | CFloat    of Real32.t
    | CDouble   of Real64.t
    | CViVector of {typ : VI.elemType, elts : constant Vector.t}
    | CViMask   of {typ : VI.elemType, elts : bool Vector.t}
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
    | FiViFixed    of {typ : VI.elemType, idx : int}
                      (* the fields starting at this fixed offset *)
    | FiViVariable of {typ : VI.elemType, idx : operand}
                      (* the array portion fields starting at this index *)
    | FiViIndexed  of {typ : VI.elemType, idx : operand}
                      (* the array portion fields given by this vector index *)

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
        vtDesc : vtableDescriptor,  (* Length field must be initialised *)
        inits  : operand Vector.t   (* Initialises a prefix of the fields;
                                     * can be less than all fixed fields;
                                     * can include some/all array elements
                                     *)
      }
    | RhsTupleSub of tupleField
    | RhsTupleSet of {tupField : tupleField, ofVal : operand}
    | RhsTupleInited of {vtDesc : vtableDescriptor, tup : variable}
    | RhsIdxGet of {idx : variable, ofVal : operand}
    | RhsCont of label
    | RhsObjectGetKind of variable
    | RhsThunkMk of {typ : fieldKind, fvs : fieldKind Vector.t}
    | RhsThunkInit of {
        typ   : fieldKind,
        thunk : variable option, (* if absent then create *)
        fx    : effects,
        code  : variable option, (* if absent then cannot eval indirectly *)
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
    | RhsThunkGetValue of {typ : fieldKind, thunk : variable}
                          (* thunk must be evaled *)
    | RhsThunkSpawn of {typ : fieldKind, thunk : variable, fx : effects}
    (* HL *)
    | RhsPFunctionMk of {fvs : fieldKind Vector.t}
    | RhsPFunctionInit of {
        cls  : variable option, (* if absent, create;
                                 * if present, must be from PFunctionMk
                                 *)
        code : variable option, (* if absent, cannot be called indirectly *)
        fvs  : (fieldKind * operand) Vector.t
      }
    | RhsPFunctionGetFv of
        {fvs : fieldKind Vector.t, cls : variable, idx : int}
    | RhsPSetNew of operand
    | RhsPSetGet of variable
    | RhsPSetCond of {bool : operand, ofVal : operand}
                     (* if bool then {ofVal} else {} *)
    | RhsPSetQuery of operand (* {} => false | _ => true *)
    | RhsPSum of {tag : name, typ : fieldKind, ofVal : operand}
    | RhsPSumProj of {typ : fieldKind, sum : variable, tag : name}

  datatype instruction = I of {dest : variable option, rhs : rhs}

  datatype target = T of {block : label, arguments : operand Vector.t}

  type 'a switch = {
    on      : operand,
    cases   : ('a * target) Vector.t,
    default : target option
  }

  type codes = {possible : VS.t, exhaustive : bool}

  datatype call =
      CCode          of variable
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
    | RTail

  datatype transfer =
    (* Core *)
      TGoto      of target
    | TCase      of constant switch
    | TInterProc of {callee : interProc, ret : return, fx : effects}
    | TReturn    of operand Vector.t
    | TCut       of {cont : variable, args : operand Vector.t, cuts : cuts}
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

  (* Invariant: code pointers should never escape except through
   * closures.
   *)
  datatype global =
    (* Core *)
      GCode       of code
    | GIdx        of int ND.t
    | GTuple      of {
        vtDesc : vtableDescriptor,
        inits  : simple Vector.t   (* must be all fields *)
      }
    | GRat        of Rat.t
    | GInteger    of IntInf.t
    | GThunkValue of {typ : fieldKind, ofVal : simple}
    (* HL *)
    | GSimple     of simple
    | GPFunction  of variable option (* code pointer, no fvs *)
    | GPSum       of {tag : name, typ : fieldKind, ofVal : simple}
    | GPSet       of simple

  type globals = global VD.t

  datatype variableInfo = VI of {typ : typ, global : bool}
  type symbolTable = variableInfo Identifier.symbolTable
  type symbolTableManager = variableInfo Identifier.Manager.t
  type symbolInfo = variableInfo Identifier.SymbolInfo.t

  datatype t = P of {
    globals     : globals,
    symbolTable : symbolTable,
    entry       : variable
  }

end

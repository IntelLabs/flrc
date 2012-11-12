(* COPYRIGHT_NOTICE_1 *)
(*
 * A strict variation of A-normal form that has explicit thunks.
 *)
structure ANormStrict =
struct
  type var   = Identifier.variable
  type con   = Identifier.name * int
  type lit   = CoreHs.coreLit
  type cc    = CoreHs.callconv
  type mname = string
  type pname = string
  type 'a primTy = 'a GHCPrimType.primTy

  datatype ty 
      = Boxed                            (* unknown boxed type *)
      | Prim  of ty primTy               (* primitive type *)
      | Arr   of ty list * ty list       (* function type *)
      | Sum   of (con * ty list) list    (* (boxed) Sum type *)
      | Tname of var                     (* named type *)
      | Thunk of ty                      (* thunk type *)

  type vbinds = (var * ty) list

  datatype cast 
      = FromAddr of var 
      | ToAddr   of var
      | NullRef
      | Bottom   of var                  (* keep the var live *)

  datatype exp 
      = Return   of var List.t
      | PrimApp  of GHCPrimOp.primOp * var list          (* saturated prim application *)
      | ExtApp   of pname * cc * string * ty * var list  (* saturated extern application *)
      | ConApp   of con * var list                       (* saturated data constructor *)
      | App      of var * var list                       (* multi-param application *)
      | Let      of vDefg * exp
      | Case     of var * alt list
      | Lit      of lit * ty
      | Cast     of cast
      | Eval     of var           (* thunk evaluation *)

  and alt
      = Acon of con * vbinds * exp
      | Alit of lit * ty * exp
      | Adefault of exp

  (* f not escapes => every use is syntactically as the callee in a call
   * f not recursive => 
   *  calling f never leads to another call to f before the 
   *  return of the first call.
   *)
  and vDef
      (* lambda must be bound to variable, where the first ty is type of var, not exp *)
      = Vfun of {name : var, ty : ty, 
                 escapes : bool, recursive : bool, 
                 fvs : var list, args : vbinds, body : exp}
      (* thunks must also be bound to variable *)
      | Vthk of {name : var, ty : ty, 
                 escapes : bool, recursive : bool, 
                 fvs : var list, body : exp}

  and vDefg 
      = Rec of vDef list
      | Nonrec of vDef 
      | Vdef of (var * ty) list * exp

  type tDef = var * ty

  datatype variableKind = VkGlobal | VkLocal 

  type im = (ty * variableKind) Identifier.symbolTable

  type symbolInfo = (ty * variableKind) Identifier.SymbolInfo.t

  type symbolTableManager = (ty * variableKind) Identifier.Manager.t

  datatype module 
      = Module of var * vDefg list 

  type t = module * im
end (* structure ANormStrict *)


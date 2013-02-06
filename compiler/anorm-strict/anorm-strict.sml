(* COPYRIGHT_NOTICE_1 *)
(*
 * ANormStrict is a strict variation of A-normal form with
 * explicit thunks and effect annotation.
 *
 * In addition to side effects in ANormLazy, non-termination
 * (or partial) is also considered as an effect. But we only 
 * annotate each App because:
 * 1. The effect of PrimApp can be looked up from GhcPrimOp.
 * 2. The effect of ExtApp is assumed to any.
 * 3. The effect of thunk Eval is assumed to be partial.
 *)
structure ANormStrict =
struct
  type effect = Effect.set
  type name  = Identifier.name
  type var   = Identifier.variable
  type con   = Identifier.name * int
  type lit   = CoreHs.coreLit
  type cc    = CoreHs.callconv
  type mname = string
  type pname = string
  type 'a primTy = 'a GHCPrimType.primTy

  datatype ty_
      = Boxed                               (* unknown boxed type *)
      | Prim  of ty primTy                  (* primitive type *)
      | Arr   of ty list * ty list * effect (* function type *)
      | Sum   of (con * ty list) list       (* (boxed) Sum type *)
      | Thunk of ty                         (* thunk type *)

  withtype ty = ty_ TypeRep.rep

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
      | App      of var * var list * effect              (* multi-param application *)
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

  type symbolTable = (ty * variableKind) Identifier.symbolTable

  type symbolInfo = (ty * variableKind) Identifier.SymbolInfo.t

  type symbolTableManager = (ty * variableKind) Identifier.Manager.t

  type typeManager = ty_ TypeRep.manager

  datatype module 
      = Module of var * vDefg list 

  type t = module * symbolTable * typeManager

  val hashTy_ : symbolInfo -> ty_ TypeRep.baseHash
    = fn st => fn (tm, t) => 
      let
        fun hashRep t = TypeRep.hashRep (tm, t)
      in
        case t 
          of Boxed => "B"
           | Prim p => 
            let
              val l = GHCPrimTypeLayout.layoutPrimTy (Layout.str o hashRep) p
            in
              "P(" ^ Layout.toString l ^ ")"
            end
           | Arr (t1, t2, _) => "A(" ^ List.toString hashRep t1 ^ "," ^ List.toString hashRep t2 ^ ")"
           | Sum arms => 
            let
              fun doTys tys = List.toString hashRep tys
              fun doArm ((n, _), tys) = Identifier.SymbolInfo.nameString (st, n) ^ doTys tys
            in
              "S" ^ List.toString doArm arms
            end
           | Thunk t => "T(" ^ hashRep t ^ ")"
      end

end (* structure ANormStrict *)


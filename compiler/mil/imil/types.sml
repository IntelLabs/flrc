(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(***************** Implementation notes ***************************************
 *
 * This comment explains the basic architecture of the IMil
 * implementation.  The goal of this design is to allow the
 * implementation to be split up into multiple files with well-defined
 * interfaces to make the implementation easier to deal with.  However,
 * the various implementation files need access to a somewhat richer
 * interface to each other than the should be exported to clients.  This
 * design provides such an interface while still ensuring that clients
 * are restricted to the safer public interface.
 *                  
 * All of the basic IMil types are defined in the structure IMilTypes.
 * This structure also defines various set and get operations, and few
 * other derived operations.  This structure is sealed to hide the
 * primitive implementation of the imil types, so no other piece of the
 * imil implementation needs to know how the basic imil types are
 * represented.
 *
 * Each sub structure implementation (e.g. use) is defined in its own
 * file, along with its public signature.  The public signature includes
 * all of the publicly exported types (listed in IMIL_PUBLIC_TYPES), but
 * does not expose them as equal to the types defined in IMilTypes.  The
 * implementation structure is transparently sealed with this public
 * signature, extended with additional functionality needed by other
 * implementation modules, but not intended for export.  Think of this
 * like "protected" or "friend" methods in C++.  The transparent sealing
 * exposes the fact that the types in the signature are equal to the
 * types defined in IMilTypes .  So for example, we have something like:
 *
 * signature IMIL_VAR = 
 * sig
 *   include IMIL_PUBLIC_TYPES
 *  (* All of the publicly exported functions here*)
 * end
 *
 * structure IMilVar :  (* transparent ascription: we're not abstracting the types yet *)
 * sig
 *   (* All of the public functions are implemented by this structure*)
 *   include IMIL_VAR
 *   (* Some additional private functions go here *)
 * end = 
 * struct
 *   open IMilPublicTypes (* include the public types *)
 *  (* implementation goes here*)
 * end
 *
 * Other implementation modules access IMilVar directly, with access to
 * the extended signature.  Because of the transparent ascription, the
 * types exported by IMilVar are known to be equal to those defined in
 * IMilTypes.
 *
 * The IMil structure collects up all of these sub structures and
 * exports them opaquely sealed with the public signature.  The opaque
 * ascription ensures that the various types defined in IMilTypes are
 * not equal to the types with the same name exported by IMil.
 * Consequently, the type checker will reject any attempt to mix uses of
 * the IMil operations and the IMilXXX implementation operations.  So
 * for example, IMilT.getStm (IMil.T.build (config, p)) will be rejected
 * because the types IMilT.t and IMil.t are not equal.
 *
 *****************************************************************************)


signature IMIL_TYPES = 
sig

  structure Graph : IMP_POLY_LABELED_GRAPH  
        where type ('a, 'b) t = ('a, 'b) ImpPolyLabeledGraph.t
        and type ('a, 'b) node = ('a, 'b) ImpPolyLabeledGraph.node
        and type ('a, 'b) edge = ('a, 'b) ImpPolyLabeledGraph.edge


  structure VS : SET where type element = Identifier.VariableSet.element
                       and type t = Identifier.VariableSet.t
  structure LS : SET where type element = Identifier.LabelSet.element
                       and type t = Identifier.LabelSet.t
  structure VD : DICT where type key = Identifier.VariableDict.key
                       and type 'a t = 'a Identifier.VariableDict.t
  structure LD : DICT where type key = Identifier.LabelDict.key
                       and type 'a t = 'a Identifier.LabelDict.t
  structure IVD : DICT_IMP where type key = Identifier.ImpVariableDict.key
                             and type 'a t = 'a Identifier.ImpVariableDict.t
  structure ILD : DICT_IMP where type key = Identifier.ImpLabelDict.key
                             and type 'a t = 'a Identifier.ImpLabelDict.t

  eqtype iInstr
  eqtype iBlock
  eqtype iFunc
  eqtype iGlobal

  type variable = Mil.variable
  type label = Mil.label

  type node = (iBlock option, unit) Graph.node
  type edge = (iBlock option, unit) Graph.edge
  type graph = (iBlock option, unit) Graph.t

  datatype mInstr = 
           MInstr of Mil.instruction
         | MTransfer of Mil.transfer
         | MLabel of Mil.label * Mil.variable Vector.t
         | MDead

  datatype mGlobal = 
           GGlobal of Mil.variable * Mil.global
         | GDead

  datatype iFunc' = F of {
           id          : int,
           fname       : Mil.variable,
           size        : int ref,
           effects     : Mil.effects,
           escapes     : bool,
           recursive   : bool,
           conv        : Mil.variable Mil.callConv,
           args        : Mil.variable Vector.t,
           rtyps       : Mil.typ Vector.t,
           start       : Mil.label,
           iBlocks     : iBlock ILD.t, 
           entry       : node,
           exit        : node,
           cfg         : graph}

       and iBlock'  = B of {id    : int,
                            label : iInstr,
                            code  : iInstr DList.t,
                            trans : iInstr,
                            iFunc : iFunc,
                            node  : node}

       and iInstr'  = I of {id     : int,
                            mil    : mInstr, 
                            vars   : use DList.cursor IVD.t,
                            loc    : iInstr DList.cursor option,
                            iBlock : iBlock}

       and iGlobal' = G of {id    : int,
                            mil   : mGlobal,
                            vars  : use DList.cursor IVD.t}

       and use = 
           Used
         | UseInstr  of iInstr
         | UseGlobal of iGlobal

  datatype def = 
           DefUnk
         | DefExtern
         | DefInstr of iInstr
         | DefGlobal of iGlobal
         | DefFunc of iFunc
         | DefParameter of iFunc

  datatype t = 
           P of {
           includes : Mil.includeFile Vector.t,
           externs  : Mil.externGroup Vector.t,
           nextId   : int ref,
           config   : Config.t,
           stm      : Mil.symbolTableManager,
           entry    : Mil.variable,
           iGlobals : iGlobal IVD.t,
           defs     : def IVD.t,
           uses     : use DList.t IVD.t,  
           iFuncs   : iFunc IVD.t
  }
                
  datatype item = ItemInstr of iInstr
                | ItemGlobal of iGlobal
                | ItemFunc of iFunc


  val iFuncGetIFunc' : iFunc -> iFunc'
  val iFuncSetIFunc' : iFunc * iFunc' -> unit

  val iFuncSetId : iFunc * int -> unit
  val iFuncGetId : iFunc -> int
  val iFuncSetFName : iFunc * Mil.variable -> unit
  val iFuncGetFName : iFunc -> Mil.variable
  val iFuncSetSize : iFunc * int ref -> unit
  val iFuncGetSize : iFunc -> int ref
  val iFuncSetEffects : iFunc * Mil.effects -> unit
  val iFuncGetEffects : iFunc -> Mil.effects
  val iFuncSetEscapes : iFunc * bool -> unit
  val iFuncGetEscapes : iFunc -> bool
  val iFuncSetRecursive : iFunc * bool -> unit
  val iFuncGetRecursive : iFunc -> bool
  val iFuncSetConv : iFunc * Mil.variable Mil.callConv -> unit
  val iFuncGetConv : iFunc -> Mil.variable Mil.callConv 
  val iFuncSetArgs : iFunc * Mil.variable Vector.t -> unit
  val iFuncGetArgs : iFunc -> Mil.variable Vector.t
  val iFuncSetRtyps : iFunc * Mil.typ Vector.t -> unit
  val iFuncGetRtyps : iFunc -> Mil.typ Vector.t
  val iFuncSetStart : iFunc * Mil.label -> unit
  val iFuncGetStart : iFunc -> Mil.label
  val iFuncSetIBlocks : iFunc * iBlock ILD.t -> unit
  val iFuncGetIBlocks : iFunc -> iBlock ILD.t
  val iFuncSetEntry : iFunc * node -> unit
  val iFuncGetEntry : iFunc -> node
  val iFuncSetExit : iFunc * node-> unit
  val iFuncGetExit : iFunc -> node
  val iFuncSetCfg : iFunc * graph -> unit
  val iFuncGetCfg : iFunc -> graph

  val iBlockGetIBlock' : iBlock -> iBlock'
  val iBlockSetIBlock' : iBlock * iBlock' -> unit

  val iBlockSetId : iBlock * int -> unit
  val iBlockGetId : iBlock -> int
  val iBlockSetLabel : iBlock * iInstr -> unit
  val iBlockGetLabel : iBlock -> iInstr
  val iBlockSetCode : iBlock * iInstr DList.t -> unit
  val iBlockGetCode : iBlock -> iInstr DList.t
  val iBlockSetTrans : iBlock * iInstr -> unit
  val iBlockGetTrans : iBlock -> iInstr
  val iBlockSetIFunc : iBlock * iFunc -> unit
  val iBlockGetIFunc : iBlock -> iFunc 
  val iBlockSetNode : iBlock * node -> unit
  val iBlockGetNode : iBlock -> node

  val iInstrGetIInstr' : iInstr -> iInstr'
  val iInstrSetIInstr' : iInstr * iInstr' -> unit

  val iInstrSetId : iInstr * int -> unit 
  val iInstrGetId : iInstr -> int
  val iInstrSetMil : iInstr * mInstr -> unit
  val iInstrGetMil : iInstr -> mInstr
  val iInstrSetVars : iInstr * use DList.cursor IVD.t -> unit
  val iInstrGetVars : iInstr -> use DList.cursor IVD.t
  val iInstrSetLoc : iInstr * iInstr DList.cursor option -> unit
  val iInstrGetLoc : iInstr -> iInstr DList.cursor option
  val iInstrSetIBlock : iInstr * iBlock -> unit
  val iInstrGetIBlock : iInstr -> iBlock

  val iGlobalGetIGlobal' : iGlobal -> iGlobal'
  val iGlobalSetIGlobal' : iGlobal * iGlobal' -> unit

  val iGlobalSetId : iGlobal * int -> unit
  val iGlobalGetId : iGlobal -> int
  val iGlobalSetMil : iGlobal * mGlobal -> unit
  val iGlobalGetMil : iGlobal -> mGlobal
  val iGlobalSetVars : iGlobal * use DList.cursor IVD.t -> unit
  val iGlobalGetVars : iGlobal -> use DList.cursor IVD.t

  val tSetNextId : t * int ref -> t
  val tGetNextId : t -> int ref
  val tSetConfig : t * Config.t -> t
  val tGetConfig : t -> Config.t
  val tSetStm : t * Mil.symbolTableManager -> t
  val tGetStm : t -> Mil.symbolTableManager
  val tSetEntry : t * Mil.variable -> t
  val tGetEntry : t -> Mil.variable
  val tSetIncludes : t * Mil.includeFile Vector.t -> t
  val tGetIncludes : t -> Mil.includeFile Vector.t
  val tSetExterns : t * Mil.externGroup Vector.t -> t
  val tGetExterns : t -> Mil.externGroup Vector.t
  val tSetIGlobals : t * iGlobal IVD.t -> t
  val tGetIGlobals : t -> iGlobal IVD.t
  val tSetDefs : t * def IVD.t -> t
  val tGetDefs : t -> def IVD.t
  val tSetUses : t * use DList.t IVD.t -> t
  val tGetUses : t -> use DList.t IVD.t
  val tSetIFuncs : t * iFunc IVD.t -> t
  val tGetIFuncs : t -> iFunc IVD.t

  val iFuncNew : iFunc' -> iFunc
  val iFuncNewUninitialized : unit -> iFunc
  val iFuncIsInitialized : iFunc -> bool
  val iBlockNew : iBlock' -> iBlock
  val iBlockNewUninitialized : unit -> iBlock
  val iBlockIsInitialized : iBlock -> bool
  val iInstrNew : iInstr' -> iInstr
  val iGlobalNew : iGlobal' -> iGlobal


 (* Auxilliary functions *)
  val tGetSi : t -> Mil.symbolInfo
  val nextId : t -> int
  val iFuncAddToSize : iFunc * int -> unit
  val iFuncIncSize : iFunc -> unit
  val iFuncDecSize : iFunc -> unit

  val iBlockGetSuccIBlocks : iBlock -> iBlock List.t
  val iBlockGetPredIBlocks : iBlock -> iBlock List.t

  val iBlockGetSuccNodes : iBlock -> node List.t

  val nodeGetSuccIBlocks : node -> iBlock List.t
  val nodeGetPredIBlocks : node -> iBlock List.t

  val iBlockGetIBlockInEdges  : iBlock -> (iBlock * iBlock) List.t
  val iBlockGetIBlockOutEdges : iBlock -> (iBlock * iBlock) List.t

  val iBlockGetNodeInEdges  : iBlock -> edge List.t
  val iBlockGetNodeOutEdges : iBlock -> edge List.t

  val iFuncGetNodeByLabel : iFunc * label -> node
  val iFuncGetIBlockByLabel : iFunc * label -> iBlock

  val iInstrToInstruction : iInstr -> Mil.instruction option
  val iInstrToRhs         : iInstr -> Mil.rhs option
  val iInstrToTransfer    : iInstr -> Mil.transfer option
  val iInstrToLabel       : iInstr -> (Mil.label * Mil.variable Vector.t) option
  val useToIInstr         : use -> iInstr option
  val useToIGlobal        : use -> iGlobal option
  val iGlobalToGlobal     : iGlobal -> (Mil.variable * Mil.global) option
  val itemToIInstr        : item -> iInstr option
  val itemToIGlobal       : item -> iGlobal option
  val itemToIFunc         : item -> iFunc option
  val defToIInstr         : def -> iInstr option
  val defToIGlobal        : def -> iGlobal option
  val defToIFunc          : def -> iFunc option
end

structure IMilTypes :> IMIL_TYPES = 
struct

  structure Graph = ImpPolyLabeledGraph
  structure VS = Identifier.VariableSet
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict
  structure VD = Identifier.VariableDict
  structure IVD = Identifier.ImpVariableDict
  structure ILD = Identifier.ImpLabelDict

  val fail = 
   fn (f, s) => Fail.fail ("types.sml", f, s)

  type variable = Mil.variable
  type label = Mil.label

  datatype mInstr = 
           MInstr of Mil.instruction
         | MTransfer of Mil.transfer
         | MLabel of Mil.label * Mil.variable Vector.t
         | MDead


  datatype mGlobal = 
           GGlobal of Mil.variable * Mil.global
         | GDead

  datatype iFunc' = F of {
           id          : int,
           fname       : Mil.variable,
           size        : int ref,
           effects     : Mil.effects,
           escapes     : bool,
           recursive   : bool,
           conv        : Mil.variable Mil.callConv,
           args        : Mil.variable Vector.t,
           rtyps       : Mil.typ Vector.t,
           start       : Mil.label,
           iBlocks     : iBlock ILD.t, 
           entry       : (iBlock option, unit) Graph.node,
           exit        : (iBlock option, unit) Graph.node,
           cfg         : (iBlock option, unit) Graph.t}

       and iBlock'  = B of {id    : int,
                            label : iInstr,
                            code  : iInstr DList.t,
                            trans : iInstr,
                            iFunc : iFunc,
                            node  : (iBlock option, unit) Graph.node}

       and iInstr'  = I of {id     : int,
                            mil    : mInstr, 
                            vars   : use DList.cursor IVD.t,
                            loc    : iInstr DList.cursor option,
                            iBlock : iBlock}

       and iGlobal' = G of {id    : int,
                            mil   : mGlobal,
                            vars  : use DList.cursor IVD.t}
       and use = 
           Used
         | UseInstr  of iInstr
         | UseGlobal of iGlobal

  withtype iFunc   = iFunc' option ref
       and iBlock  = iBlock' option ref
       and iInstr  = iInstr' ref
       and iGlobal = iGlobal' ref

  type node = (iBlock option, unit) Graph.node
  type edge = (iBlock option, unit) Graph.edge
  type graph = (iBlock option, unit) Graph.t


  datatype def = 
           DefUnk
         | DefExtern
         | DefInstr of iInstr
         | DefGlobal of iGlobal
         | DefFunc of iFunc
         | DefParameter of iFunc

  datatype t = 
           P of {
           includes : Mil.includeFile Vector.t,
           externs  : Mil.externGroup Vector.t,
           nextId   : int ref,
           config   : Config.t,
           stm      : Mil.symbolTableManager,
           entry    : Mil.variable,
           iGlobals : iGlobal IVD.t,
           defs     : def IVD.t,
           uses     : use DList.t IVD.t,  
           iFuncs   : iFunc IVD.t
  }

  datatype item = ItemInstr of iInstr
                | ItemGlobal of iGlobal
                | ItemFunc of iFunc




  val (iFuncSetGetId, iFuncSetGetFName, iFuncSetGetSize, iFuncSetGetEffects, iFuncSetGetEscapes, iFuncSetGetRecursive, 
       iFuncSetGetConv, iFuncSetGetArgs, iFuncSetGetRtyps, iFuncSetGetStart, iFuncSetGetIBlocks, iFuncSetGetEntry, 
       iFuncSetGetExit, iFuncSetGetCfg) = 
      let
        val r2t = 
         fn (F {id, fname, size, effects, escapes, recursive, conv, args, rtyps, start, iBlocks, entry, exit, cfg}) =>
            (id, fname, size, effects, escapes, recursive, conv, args, rtyps, start, iBlocks, entry, exit, cfg)
        val t2r = 
         fn (id, fname, size, effects, escapes, recursive, conv, args, rtyps, start, iBlocks, entry, exit, cfg) =>
             F {id = id, fname = fname, size = size, effects = effects, 
                escapes = escapes, recursive = recursive, 
                conv = conv, args = args, rtyps = rtyps, start = start, 
                iBlocks = iBlocks, entry = entry, exit = exit, cfg = cfg}
      in FunctionalUpdate.mk14 (r2t, t2r)
      end

  val iFuncGetIFunc' = 
   fn iFunc =>
      case !iFunc
       of SOME r => r
        | NONE => fail ("iFuncGetIFunc'",
                        "Uninitialized")
  val iFuncSetIFunc' =
   fn (iFunc, iFunc') => iFunc := (SOME iFunc')

  val ((iFuncSetId,        iFuncGetId),
       (iFuncSetFName,     iFuncGetFName),
       (iFuncSetSize,      iFuncGetSize),
       (iFuncSetEffects,   iFuncGetEffects),
       (iFuncSetEscapes,   iFuncGetEscapes),
       (iFuncSetRecursive, iFuncGetRecursive),
       (iFuncSetConv,      iFuncGetConv),
       (iFuncSetArgs,      iFuncGetArgs),
       (iFuncSetRtyps,     iFuncGetRtyps),
       (iFuncSetStart,     iFuncGetStart),
       (iFuncSetIBlocks,   iFuncGetIBlocks),
       (iFuncSetEntry,     iFuncGetEntry),
       (iFuncSetExit,      iFuncGetExit),
       (iFuncSetCfg,       iFuncGetCfg)) = 
      let
        val iFuncLiftSetGet = 
         fn (set, get) => (fn (r, a) => iFuncSetIFunc' (r, set (iFuncGetIFunc' r, a)),
                           get o iFuncGetIFunc')
      in
        (iFuncLiftSetGet iFuncSetGetId,
         iFuncLiftSetGet iFuncSetGetFName,
         iFuncLiftSetGet iFuncSetGetSize,
         iFuncLiftSetGet iFuncSetGetEffects,
         iFuncLiftSetGet iFuncSetGetEscapes,
         iFuncLiftSetGet iFuncSetGetRecursive,
         iFuncLiftSetGet iFuncSetGetConv,
         iFuncLiftSetGet iFuncSetGetArgs,
         iFuncLiftSetGet iFuncSetGetRtyps,
         iFuncLiftSetGet iFuncSetGetStart,
         iFuncLiftSetGet iFuncSetGetIBlocks,
         iFuncLiftSetGet iFuncSetGetEntry,
         iFuncLiftSetGet iFuncSetGetExit,
         iFuncLiftSetGet iFuncSetGetCfg)
      end

  val iBlockGetIBlock' = 
   fn iBlock =>
      case !iBlock
       of SOME r => r
        | NONE => fail ("iBlockGetIBlock'",
                        "Uninitialized")
  val iBlockSetIBlock' =
   fn (iBlock, iBlock') => iBlock := (SOME iBlock')


  val (iBlockSetGetId,
       iBlockSetGetLabel,
       iBlockSetGetCode,
       iBlockSetGetTrans,
       iBlockSetGetIFunc,
       iBlockSetGetNode) = 
      let
        val r2t = 
            fn (B {id, label, code, trans, iFunc, node}) => 
               (id, label, code, trans, iFunc, node)
        val t2r = 
         fn (id, label, code, trans, iFunc, node) => 
            B {id = id, label = label, code = code, trans = trans, iFunc = iFunc, node = node}
      in
        FunctionalUpdate.mk6 (r2t, t2r)
      end

  val ((iBlockSetId,    iBlockGetId),
       (iBlockSetLabel, iBlockGetLabel),
       (iBlockSetCode,  iBlockGetCode),
       (iBlockSetTrans, iBlockGetTrans),
       (iBlockSetIFunc, iBlockGetIFunc),
       (iBlockSetNode,  iBlockGetNode)) = 
      let
        val iBlockLiftSetGet = 
         fn (set, get) => (fn (r, a) => iBlockSetIBlock' (r, set (iBlockGetIBlock' r, a)),
                           get o iBlockGetIBlock')
      in
        (iBlockLiftSetGet iBlockSetGetId,
         iBlockLiftSetGet iBlockSetGetLabel,
         iBlockLiftSetGet iBlockSetGetCode,
         iBlockLiftSetGet iBlockSetGetTrans,
         iBlockLiftSetGet iBlockSetGetIFunc,
         iBlockLiftSetGet iBlockSetGetNode)
      end

  val iInstrGetIInstr' = 
   fn iInstr => !iInstr
  val iInstrSetIInstr' =
   fn (iInstr, iInstr') => iInstr := iInstr'

  val (iInstrSetGetId,
       iInstrSetGetMil,
       iInstrSetGetVars,
       iInstrSetGetLoc,
       iInstrSetGetIBlock) = 
      let
        val r2t = 
         fn (I {id, mil, vars, loc, iBlock}) => 
            (id, mil, vars, loc, iBlock)
        val t2r = 
         fn (id, mil, vars, loc, iBlock) => 
            I {id = id, mil = mil, vars = vars, loc = loc, iBlock = iBlock}
      in
        FunctionalUpdate.mk5 (r2t, t2r)
      end

  val ((iInstrSetId, iInstrGetId),
       (iInstrSetMil, iInstrGetMil),
       (iInstrSetVars, iInstrGetVars),
       (iInstrSetLoc, iInstrGetLoc),
       (iInstrSetIBlock, iInstrGetIBlock)) = 
      let
        val iInstrLiftSetGet = 
         fn (set, get) => (fn (r, a) => iInstrSetIInstr' (r, set (iInstrGetIInstr' r, a)),
                           get o iInstrGetIInstr')
      in
        (iInstrLiftSetGet iInstrSetGetId,
         iInstrLiftSetGet iInstrSetGetMil,
         iInstrLiftSetGet iInstrSetGetVars,
         iInstrLiftSetGet iInstrSetGetLoc,
         iInstrLiftSetGet iInstrSetGetIBlock)
      end

  val iGlobalGetIGlobal' = 
   fn iGlobal => !iGlobal
  val iGlobalSetIGlobal' =
   fn (iGlobal, iGlobal') => iGlobal := iGlobal'

  val (iGlobalSetGetId,
       iGlobalSetGetMil,
       iGlobalSetGetVars) = 
      let
        val r2t = 
         fn (G {id, mil, vars}) => 
            (id, mil, vars)
        val t2r = 
         fn (id, mil, vars) => 
            G {id = id, mil = mil, vars = vars}
      in
        FunctionalUpdate.mk3 (r2t, t2r)
      end

  val ((iGlobalSetId,   iGlobalGetId),
       (iGlobalSetMil,  iGlobalGetMil),
       (iGlobalSetVars, iGlobalGetVars)) =
      let
        val iGlobalLiftSetGet = 
         fn (set, get) => (fn (r, a) => iGlobalSetIGlobal' (r, set (iGlobalGetIGlobal' r, a)),
                           get o iGlobalGetIGlobal')
      in
        (iGlobalLiftSetGet iGlobalSetGetId,
         iGlobalLiftSetGet iGlobalSetGetMil,
         iGlobalLiftSetGet iGlobalSetGetVars)
      end


  val ((tSetIncludes, tGetIncludes),
       (tSetExterns,  tGetExterns),
       (tSetNextId,   tGetNextId),
       (tSetConfig,   tGetConfig),
       (tSetStm,      tGetStm),
       (tSetEntry,    tGetEntry),
       (tSetIGlobals, tGetIGlobals),
       (tSetDefs,     tGetDefs),
       (tSetUses,     tGetUses),
       (tSetIFuncs,   tGetIFuncs)) = 
      let
        val r2t = 
         fn (P {includes, externs, nextId, config, stm, entry, iGlobals, defs, uses, iFuncs}) => 
            (includes, externs, nextId, config, stm, entry, iGlobals, defs, uses, iFuncs)
        val t2r = 
         fn (is, evs, nextId, config, stm, entry, iGlobals, defs, uses, iFuncs) => 
            P {includes = is, externs = evs, nextId = nextId, config = config, stm = stm, entry = entry, 
               iGlobals = iGlobals, defs = defs, uses = uses, iFuncs = iFuncs}
      in FunctionalUpdate.mk10 (r2t, t2r)
      end


  val nextId = 
   fn p => 
      let
        val counter = tGetNextId p
        val id = !counter
        val () = counter := id + 1
      in id
      end

  val iFuncNew : iFunc' -> iFunc =
      fn f => ref (SOME f)
  val iFuncNewUninitialized : unit -> iFunc =
      fn () => ref NONE
  val iFuncIsInitialized =
      fn iFunc => Option.isSome (!iFunc)
  val iBlockNew : iBlock' -> iBlock  = 
   fn b => ref (SOME b)
  val iBlockNewUninitialized : unit -> iBlock = 
   fn () => ref NONE
  val iBlockIsInitialized =
      fn iBlock => Option.isSome (!iBlock)
  val iInstrNew : iInstr' -> iInstr = 
   fn i => ref i
  val iGlobalNew : iGlobal' -> iGlobal = 
   fn g => ref g

  val tGetSi : t -> Mil.symbolInfo = 
      fn t => Identifier.SymbolInfo.SiManager (tGetStm t)

  val iFuncAddToSize = 
   fn (iFunc, i) =>
      let
        val sr = iFuncGetSize iFunc
        val () = sr := !sr + i
      in ()
      end
  val iFuncIncSize  = 
   fn iFunc => iFuncAddToSize (iFunc, 1)
  val iFuncDecSize =
   fn iFunc => iFuncAddToSize (iFunc, ~1)

  val nodeEdgeToIBlockEdge =
   fn e => 
      let
        val n1 = Graph.Edge.from (e)
        val n2 = Graph.Edge.to (e)
      in
        (case (Graph.Node.getLabel (n1), Graph.Node.getLabel (n2))
          of (SOME b1, SOME b2) => SOME (b1, b2)
           | _ => NONE)
      end
      
  val nodeEdgesToIBlockEdges =
   fn (es : edge list) => 
      List.keepAllMap (es, nodeEdgeToIBlockEdge)

  val nodeGetIBlockOutEdges =
   fn (n : node) => 
      nodeEdgesToIBlockEdges (Graph.Node.outEdges (n))

  val nodeGetIBlockInEdges =
   fn (n : node) => 
      nodeEdgesToIBlockEdges (Graph.Node.inEdges (n))

  val iBlockGetNodeOutEdges =
   fn (b : iBlock) => 
      let
        val graph = iFuncGetCfg (iBlockGetIFunc b)
        val n = iBlockGetNode b
        val es = Graph.Node.outEdges (n)
      in es
      end

  val iBlockGetNodeInEdges =
   fn (b : iBlock) => 
      let
        val graph = iFuncGetCfg (iBlockGetIFunc b)
        val n = iBlockGetNode b
        val es = Graph.Node.inEdges (n)
      in es
      end

  val iBlockGetIBlockOutEdges =
   fn (b : iBlock) => 
      let
        val graph = iFuncGetCfg (iBlockGetIFunc b)
        val n = iBlockGetNode b
        val es = Graph.Node.outEdges (n)
        val es = nodeEdgesToIBlockEdges (es)
      in es
      end

  val iBlockGetIBlockInEdges =
   fn (b : iBlock) => 
      let
        val graph = iFuncGetCfg (iBlockGetIFunc b)
        val n = iBlockGetNode b
        val es = Graph.Node.inEdges (n)
        val es = nodeEdgesToIBlockEdges (es)
      in es
      end


  val nodeGetSuccNodes =
   fn (n : (iBlock option, unit) Graph.node) => Graph.Node.succs (n)

  val nodeGetSuccIBlocks =
   fn (n : (iBlock option, unit) Graph.node) => 
      let
        val succs = nodeGetSuccNodes (n)
        val help = 
         fn n => Graph.Node.getLabel (n)
        val blocks = List.keepAllMap (succs, help)
      in blocks
      end

  val iBlockGetSuccNodes =
   fn (b : iBlock) => 
      let
        val graph = iFuncGetCfg (iBlockGetIFunc b)
        val n = iBlockGetNode b
        val succs = nodeGetSuccNodes (n)
      in succs
      end

  val iBlockGetSuccIBlocks =
   fn (b : iBlock) => nodeGetSuccIBlocks (iBlockGetNode b)

  val nodeGetPredNodes =
   fn (n : (iBlock option, unit) Graph.node) => 
      Graph.Node.preds (n)

  val nodeGetPredIBlocks =
   fn (n : (iBlock option, unit) Graph.node) => 
      let
        val preds = nodeGetPredNodes (n)
        val help = fn n => Graph.Node.getLabel (n)
        val blocks = List.keepAllMap (preds, help)
      in blocks
      end

  val iBlockGetPredIBlocks =
   fn (b : iBlock) => nodeGetPredIBlocks (iBlockGetNode b)


  val iFuncGetIBlockByLabel =
   fn (iFunc, l) => 
      case ILD.lookup (iFuncGetIBlocks iFunc, l)
       of SOME b => b
        | NONE => 
          let
            val s = (Layout.toString o Identifier.layoutLabel) l
          in
            fail ("getBlockForLabel", 
                  "Unknown block label: "^s)
          end  

  val iFuncGetNodeByLabel =
   fn (iFunc, l) => iBlockGetNode (iFuncGetIBlockByLabel (iFunc, l))

  val iInstrToInstruction = 
   fn i => 
      (case iInstrGetMil i
        of MInstr i => SOME i
         | _    => NONE)

  val iInstrToRhs = 
   fn i => 
      (case iInstrGetMil i
        of MInstr (Mil.I {rhs, ...}) => SOME rhs
         | _    => NONE)

  val iInstrToTransfer = 
   fn i => 
      (case iInstrGetMil i
        of MTransfer t => SOME t
         | _   => NONE)

  val iInstrToLabel = 
   fn i => 
      (case iInstrGetMil i
        of MLabel l => SOME l
         | _    => NONE)
      
  val useToIInstr = 
   fn u =>
      (case u
        of UseInstr i => SOME i
         | _ => NONE)

  val useToIGlobal = 
   fn u => 
      (case u
        of UseGlobal g => SOME g
         | _ => NONE)

  val iGlobalToGlobal =
   fn g =>
      (case iGlobalGetMil g
        of GGlobal g => SOME g
         | _ => NONE)

  val itemToIInstr =
   fn i =>
      (case i
        of ItemInstr i => SOME i
         | _ => NONE)

  val itemToIGlobal =
   fn i =>
      (case i
        of ItemGlobal g => SOME g
         | _ => NONE)

  val itemToIFunc =
   fn i =>
      (case i
        of ItemFunc f => SOME f
         | _ => NONE)

  val defToIInstr =
   fn i =>
      (case i
        of DefInstr i => SOME i
         | _ => NONE)

  val defToIGlobal =
   fn i =>
      (case i
        of DefGlobal g => SOME g
         | _ => NONE)

  val defToIFunc =
   fn i =>
      (case i
        of DefFunc f => SOME f
         | DefParameter f => SOME f
         | _ => NONE)

end

(* This signature is included in the IMil sub-structure signatures 
 * to allow the appropriate coherence properties to be introduced after 
 * the fact via the sharing specifications in IMIL 
 *)
signature IMIL_PUBLIC_TYPES = 
sig
  type t

  eqtype iInstr
  eqtype iGlobal
  eqtype iBlock
  eqtype iFunc

  type mInstr
  type mGlobal

  type use
  type def
  type item

  type variable = Mil.variable
  type label = Mil.label
end

structure IMilPublicTypes : IMIL_PUBLIC_TYPES = IMilTypes
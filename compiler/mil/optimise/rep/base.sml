(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_BASE = 
sig

  datatype 'node iInfo = 
           IiCode of {cargs : 'node Mil.callConv, args : 'node Vector.t, returns : 'node Vector.t}
         | IiMetaData of {pok : Mil.pObjKind, fixed : 'node Vector.t, array : (int * 'node) option}
         | IiTupleDescriptor of {fixed : 'node Vector.t, array : 'node option}
         | IiThunk of {typ : 'node, fvs : 'node Vector.t}
         | IiClosure of 'node Vector.t
         | IiPSum of 'node

  val layoutIInfo : Config.t * Mil.symbolInfo * 'node iInfo * ('node -> Layout.t) -> Layout.t

  structure PObjKindLat : LATTICE where type element = Mil.pObjKind
  structure NameIntDict : DICT where type key = Mil.name * int
  datatype 'a edge = EUnify of 'a * 'a | EFlow of 'a * 'a

  val features : Config.Feature.feature list

  val noTupleUnbox    : PassData.t -> bool
  val noConstantProp  : PassData.t -> bool
  val statPhases      : PassData.t -> bool
  val useShallowTypes : PassData.t -> bool
  val splitAggressive : PassData.t -> bool
  val noSplitting     : PassData.t -> bool
  val noCFA           : PassData.t -> bool
end  (* signature MIL_REP_BASE *)

structure MilRepBase :> MIL_REP_BASE = 
struct
  structure M = Mil
  structure MU = MilUtils
  structure LU = LayoutUtils
  structure L = Layout
  structure PD = PassData

  datatype 'node iInfo = 
           IiCode of {cargs : 'node Mil.callConv, args : 'node Vector.t, returns : 'node Vector.t}
         | IiMetaData of {pok : Mil.pObjKind, fixed : 'node Vector.t, array : (int * 'node) option}
         | IiTupleDescriptor of {fixed : 'node Vector.t, array : 'node option}
         | IiThunk of {typ : 'node, fvs : 'node Vector.t}
         | IiClosure of 'node Vector.t
         | IiPSum of 'node

  val layoutIInfo =
   fn (config, si, info, node) => 
      let
        val vector = fn nv => Vector.toListMap (nv, node)
        val l = 
            (case info
              of IiCode {cargs, args, returns} => 
                 let
                   val cargs = MilLayout.layoutCallConv (fn (_, _, n) => node n) (config, si, cargs)
                   val args = LU.parenSeq (vector args)
                   val returns = L.seq [L.str " => ", LU.parenSeq (vector returns)]
                 in  L.mayAlign [cargs, args, returns]
                 end
               | IiMetaData {pok, fixed, array} => 
                 let
                   val pok = L.str (MU.PObjKind.toString pok)
                   val fixed = vector fixed
                   val array = 
                       (case array
                         of NONE => []
                          | SOME (i, n) => [LU.bracket (L.seq [Int.layout i, L.str " : ", node n])])
                   val elts = LU.angleSeq (fixed @ array)
                 in L.mayAlign [L.seq [L.str "MetaData ", pok], elts]
                 end
               | IiTupleDescriptor {fixed, array} => 
                 let
                   val fixed = vector fixed
                   val array = 
                       (case array
                         of NONE => []
                          | SOME n => [LU.bracket (node n)])
                   val elts = LU.angleSeq (fixed @ array)
                 in L.mayAlign [L.str "TupleDesc ", elts]
                 end
               | IiThunk {typ, fvs} =>
                 L.seq [L.str "Thunk", LU.paren (node typ), LU.angleSeq (vector fvs)]
               | IiClosure fvs =>
                 L.seq [L.str "Closure", LU.angleSeq (vector fvs)]
               | IiPSum n => 
                 L.seq [L.str "PSum", LU.angleBracket (node n)])
      in l
      end

  structure PObjKindLat = FlatLatticeFn (struct
                                           type element = M.pObjKind
                                           val equal = MilUtils.PObjKind.eq
                                         end)
 
  structure NameIntDict = DictF (struct
                                   type t = Mil.name * int
                                   val compare = Compare.pair (Identifier.nameCompare, Int.compare)
                                 end)

  datatype 'a edge = EUnify of 'a * 'a | EFlow of 'a * 'a

  val mkLogFeature : string * string * int -> (Config.Feature.feature * (PassData.t -> bool)) = 
   fn (tag, description, level) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk ("MilRep" ^ ":" ^ tag, description)
        val feature = 
         fn d => 
            let
              val config = PD.getConfig d
            in feature config orelse 
               (Config.logLevel (config, "MilRep") >= level)
            end
      in (featureD, feature)
      end

  val mkFeature : string * string -> (Config.Feature.feature * (PassData.t -> bool)) = 
   fn (tag, description) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk ("MilRep" ^ ":" ^ tag, description)
        val feature = 
         fn d => feature (PD.getConfig d)
      in (featureD, feature)
      end

  val (noTupleUnboxF, noTupleUnbox) =
      mkFeature ("no-unbox", "disable global unboxing")
      
  val (noConstantPropF, noConstantProp) =
      mkFeature ("no-constant-prop", "disable global constant propogation")

  val (statPhasesF, statPhases) = 
      mkLogFeature ("stat-phases", "Show stats between each phase", 2)

  val (useShallowTypesF, useShallowTypes) =
      mkFeature ("shallow-types", "Don't expand types deeply")

  val (splitAggressiveF, splitAggressive) =
      mkFeature ("split-aggressive", "Split globals aggressively")

  val (noSplittingF, noSplitting) =
      mkFeature ("no-global-splitting", "Disable global splitting")

  val (noCFAF, noCFA) =
      mkFeature ("no-cfa", "Disable global control flow analysis")

  val features = [noConstantPropF, noSplittingF, noTupleUnboxF, splitAggressiveF, statPhasesF, useShallowTypesF, noCFAF]

end (* structure MilRepBase *)
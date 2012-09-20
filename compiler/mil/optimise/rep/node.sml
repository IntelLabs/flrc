(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_NODE =
sig
  type id = int
  type node

  structure Dict : DICT where type key = node
  structure Set : SET where type element = node

  (* No class object constraint *)
  val mkBottom : id * Mil.fieldKind option 
                 * Mil.valueSize option
                 * Mil.fieldVariance option 
                 * node MilRepObject.Shape.shape option 
                 -> node
  (* Class object constraint initialized to shape *)
  val mkShaped : id * Mil.fieldKind option 
                 * Mil.valueSize option
                 * Mil.fieldVariance option 
                 * node MilRepObject.Shape.shape 
                 -> node
  val markNodeEscaping : node * string -> unit
  val markNodeUnknownDefs : node * string -> unit
  val propagate : node -> unit 

  val id : node -> id
  val classId : node -> id
  val usesKnown : node -> bool
  val defsKnown : node -> bool
  val usesUnknownReason : node -> string option
  val defsUnknownReason : node -> string option
  val object : node -> node MilRepObject.Object.object
  val shape' : node -> node MilRepObject.Shape.shape option
  val shape : node -> node MilRepObject.Shape.shape
  val fieldVariance' : node -> Mil.fieldVariance option
  val fieldVariance : node -> Mil.fieldVariance
  val fieldKind' : node -> Mil.fieldKind option
  val fieldKind : node -> Mil.fieldKind
  val alignment' : node -> Mil.valueSize option
  val alignment : node -> Mil.valueSize
  val fieldDescriptor : node -> Mil.fieldDescriptor
  val setData : node 
                * node MilRepObject.Shape.shape option 
                * Mil.fieldKind option 
                * Mil.valueSize option
                * Mil.fieldVariance option 
                -> unit
  val setShape : node * node MilRepObject.Shape.shape option -> unit
  val setAlignment : node * Mil.valueSize option -> unit
  val setFieldVariance : node * Mil.fieldVariance option -> unit
  val layout : Config.t * Mil.symbolInfo * node * ((id -> Layout.t option) option) -> Layout.t
  val layoutShort : Config.t * Mil.symbolInfo * node * ((id -> Layout.t option) option) -> Layout.t

  structure Unify :
  sig
    val node : Config.t * node * node -> unit
    val nodeWithShape : Config.t * node * node MilRepObject.Shape.shape -> unit
    val shapeWithNode : Config.t * node MilRepObject.Shape.shape * node -> unit
    val vectorPrefix : Config.t * node Vector.t * node Vector.t -> unit
  end (* structure Unify *)
end (* signature MIL_REP_NODE *)

structure MilRepNode :> MIL_REP_NODE = 
struct
  val fail = 
   fn (f, m) => Fail.fail ("node.sml", f, m)

  structure I = Identifier

  structure M = Mil
  structure MU = MilUtils
  structure EC = EquivalenceClass

  structure MRB = MilRepBase
  structure MRO = MilRepObject
  structure Object = MRO.Object
  structure Shape = MRO.Shape

  structure ID = IntDict
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure ND = Mil.ND
  structure VS = Mil.VS
  structure LS = Mil.LS
  structure IVD = I.ImpVariableDict

  structure L = Layout
  structure LU = LayoutUtils

  type id = int
  type flowset = int

  datatype backwardData = BD of {usesKnown : bool,
                                 reason : string option
                                }

  datatype forwardData = FD of {defsKnown : bool,
                                reason : string option,
                                object : node MilRepObject.Object.object,
                                id : flowset} 

       and nodeData = ND of {fk : Mil.fieldKind option,
                             shape : node Shape.shape option,
                             alignment : Mil.valueSize option,
                             variance : Mil.fieldVariance option}

       and node = N of {id    : int,
                        forward : forwardData EC.t,
                        backward : backwardData EC.t,
                        nodeData : nodeData ref}


  val ((nodeDataSetFk, nodeDataGetFk),
       (nodeDataSetAlignment, nodeDataGetAlignment),
       (nodeDataSetShape, nodeDataGetShape),
       (nodeDataSetVariance, nodeDataGetVariance)) = 
      let
        val r2t = 
         fn (ND {fk, alignment, shape, variance}) => (fk, alignment, shape, variance)
        val t2r = 
         fn (fk, alignment, shape, variance) => ND {fk = fk, alignment = alignment, shape = shape, variance = variance}
      in
        FunctionalUpdate.mk4 (r2t, t2r)
      end

  val ((forwardDataSetDefsKnown, forwardDataGetDefsKnown),
       (forwardDataSetReason, forwardDataGetReason),
       (forwardDataSetObject, forwardDataGetObject),
       (forwardDataSetId, forwardDataGetId)) = 
      let
        val r2t = 
         fn (FD {defsKnown, reason, object, id}) => (defsKnown, reason, object, id)
        val t2r = 
         fn (defsKnown, reason, object, id) => FD {defsKnown = defsKnown, reason = reason, object = object, id = id}
      in
        FunctionalUpdate.mk4 (r2t, t2r)
      end

  val ((backwardDataSetUsesKnown, backwardDataGetUsesKnown),
       (backwardDataSetReason, backwardDataGetReason)) = 
      let
        val r2t = 
         fn (BD {usesKnown, reason}) => (usesKnown, reason)
        val t2r = 
         fn (usesKnown, reason) => BD {usesKnown = usesKnown, reason = reason}
      in
        FunctionalUpdate.mk2 (r2t, t2r)
      end


  val ((nodeSetId, nodeGetId),
       (nodeSetForward, nodeGetForward),
       (nodeSetBackward, nodeGetBackward),
       (nodeSetNodeData, nodeGetNodeData)) = 
      let
        val r2t = 
         fn (N {id, forward, backward, nodeData}) => (id, forward, backward, nodeData)
        val t2r = 
         fn (id, forward, backward, nodeData) => 
            N {id = id, forward = forward, backward = backward, nodeData = nodeData}
      in
        FunctionalUpdate.mk4 (r2t, t2r)
      end

  val nodeGetDefsKnown = forwardDataGetDefsKnown o EC.get o nodeGetForward
  val nodeGetDefsUnknownReason = forwardDataGetReason o EC.get o nodeGetForward
  val nodeGetObject = forwardDataGetObject o EC.get o nodeGetForward
  val nodeGetClassId = forwardDataGetId o EC.get o nodeGetForward
  val nodeGetUsesKnown = backwardDataGetUsesKnown o EC.get o nodeGetBackward
  val nodeGetUsesUnknownReason = backwardDataGetReason o EC.get o nodeGetBackward
  val nodeGetFieldKind = nodeDataGetFk o (op !) o nodeGetNodeData
  val nodeGetShape = nodeDataGetShape o (op !) o nodeGetNodeData
  val nodeGetVariance = nodeDataGetVariance o (op !) o nodeGetNodeData
  val nodeGetAlignment = nodeDataGetAlignment o (op !) o nodeGetNodeData

  structure Ord = 
  struct
    type t = node
    val compare = fn (n1, n2) => Int.compare (nodeGetId n1, nodeGetId n2)
  end (* structure Ord *)
  structure Dict = DictF (Ord)

  structure Set = SetF (Ord)

  val setData =
   fn (n, s, fk, ag, vs) => 
      let
        val ndR = nodeGetNodeData n
        val nd = ND {fk = fk, alignment = ag, shape = s, variance = vs}
        val () = ndR := nd
      in ()
      end

  val setShape = 
   fn (n, s) => 
      let
        val ndR as ref (ND {fk, alignment, shape, variance}) = nodeGetNodeData n
        val nd = ND {fk = fk, alignment = alignment, shape = s, variance = variance}
        val () = ndR := nd
      in ()
      end

  val setAlignment = 
   fn (n, alignment) => 
      let
        val ndR as ref (ND {fk, alignment=_, shape, variance}) = nodeGetNodeData n
        val () = setData (n, shape, fk, alignment, variance)
      in ()
      end

  val setFieldVariance = 
   fn (n, variance) => 
      let
        val ndR as ref (ND {fk, alignment, shape, variance=_}) = nodeGetNodeData n
        val () = setData (n, shape, fk, alignment, variance)
      in ()
      end

  val mkBottom =
   fn (id, fk, alignment, variance, shape) => 
      N {id = id, 
         forward = EC.new  (FD {defsKnown = true, reason = NONE, object = Object.bottom (), id = id}),
         backward = EC.new (BD {usesKnown = true, reason = NONE}),
         nodeData = ref (ND {fk = fk, shape = shape, alignment = alignment, variance = variance})}

  val mkShaped =
   fn (id, fk, alignment, variance, shape) => 
      N {id = id, 
         forward = EC.new  (FD {defsKnown = true, reason = NONE, object = Object.fromShape shape, id = id}),
         backward = EC.new (BD {usesKnown = true, reason = NONE}),
         nodeData = ref (ND {fk = fk, alignment = alignment, shape = SOME shape, variance = variance})}


  val markNodeUnknownDefs = 
   fn (n, reason) => 
      let
        val forward = nodeGetForward n
        val fd = EC.get forward
        val defsKnown = forwardDataGetDefsKnown fd
        val () = 
            if defsKnown then
              let
                val fd = forwardDataSetDefsKnown (fd, false)
                val fd = forwardDataSetReason(fd, SOME reason)
                val () = EC.set (forward, fd)
              in ()
              end
            else ()
      in ()
      end

  val markNodeEscaping = 
   fn (n, reason) => 
      let
        val backward = nodeGetBackward n
        val bd = EC.get backward
        val usesKnown = backwardDataGetUsesKnown bd
        val () = 
            if usesKnown then
              let
                val bd = backwardDataSetUsesKnown (bd, false)
                val bd = backwardDataSetReason (bd, SOME reason)
                val () = EC.set (backward, bd)
              in ()
              end
            else ()
      in ()
      end

  val rec propagateUnknownDefs = 
   fn (n, reason) => 
      if nodeGetDefsKnown n then
        propagateUnknownDefs' (n, reason)
      else 
        ()

  and rec propagateUnknownDefs' = 
   fn (n, reason) => 
      let
        val () = markNodeUnknownDefs (n, reason)
        val object = nodeGetObject n
        val () = Object.foreachWithParity (object, 
                                           fn n => propagateUnknownDefs (n, reason), 
                                           fn n => propagateEscaping (n, reason))
        val () = 
            (case nodeGetVariance n
              of SOME (Mil.FvReadWrite) => propagateEscaping (n, reason)
               | _ => ())
      in ()
      end

  and rec propagateEscaping = 
   fn (n, reason) => 
      if nodeGetUsesKnown n then
        propagateEscaping' (n, reason)
      else
        ()

  and rec propagateEscaping' = 
   fn (n, reason) => 
      let
        val () = markNodeEscaping (n, reason)
        val object = nodeGetObject n
        val () = Object.foreachWithParity (object, 
                                           fn n => propagateEscaping (n, reason), 
                                           fn n => propagateUnknownDefs (n, reason))
        val () = 
            (case nodeGetVariance n
              of SOME (Mil.FvReadWrite) => propagateUnknownDefs (n, reason)
               | _ => ())

      in ()
      end

  val propagate = 
   fn n => 
      let
        val () = 
            if nodeGetDefsKnown n then
              ()
            else
              propagateUnknownDefs' (n, Utils.Option.get (nodeGetDefsUnknownReason n, ""))
        val () = 
            if nodeGetUsesKnown n then 
              ()
            else
              propagateEscaping' (n, Utils.Option.get (nodeGetUsesUnknownReason n, ""))
      in ()
      end

  val id = nodeGetId
  val classId = nodeGetClassId
  val usesKnown = nodeGetUsesKnown
  val defsKnown = nodeGetDefsKnown
  val usesUnknownReason = nodeGetUsesUnknownReason
  val defsUnknownReason = nodeGetDefsUnknownReason
  val object = nodeGetObject

  val shape' = nodeGetShape
  val shape = 
   fn n => 
      let
        val shape = 
            (case nodeGetShape n
              of SOME shape => shape
               | NONE => fail ("shape", "Not a shaped node: node = " ^ (Int.toString (id n))))
      in shape
      end

  val fieldVariance' = nodeGetVariance
  val fieldVariance = 
   fn n => 
      let
        val var = 
            (case nodeGetVariance n
              of SOME var => var
               | NONE => fail ("fieldVariance", "Not a field node"))
      in var
      end

  val fieldKind' = nodeGetFieldKind
  val fieldKind = 
   fn n => 
      let
        val fk = 
            (case nodeGetFieldKind n
              of SOME fk => fk
               | NONE => fail ("fieldKind", "No field kind for node"))
      in fk
      end

  val alignment' = nodeGetAlignment
  val alignment = 
   fn n => 
      let
        val a = 
            (case nodeGetAlignment n
              of SOME a => a
               | NONE => fail ("alignment", "No alignment for node"))
      in a
      end

  val fieldDescriptor = 
   fn n => 
      let
        val fk = fieldKind n
        val var = fieldVariance n
        val alignment = alignment n
      in M.FD {kind = fk, alignment = alignment, var = var}
      end

  val layout' = 
   fn (config, si, n, name, long) => 
      let
        val name = 
         fn b => 
         fn (id, cid) => 
            let
              val s = if b then L.seq [L.str "node id = ", Int.layout id]
                      else L.seq [L.str "node ref = ", Int.layout id, L.str "//", Int.layout cid]
              val l = 
                  (case name
                    of NONE =>  s
                     | SOME f => 
                       (case f id
                         of SOME l => L.seq [s, L.str " name = ", l]
                          | NONE => s))
            in l
            end
        val shallow = 
         fn node => LU.angleBracket (name false (id node, classId node))

        val layoutShape = fn shape => MRO.Layout.shape (config, si, shallow, shape)
        val layoutObject = fn object => L.str "object" (*MRO.Layout.object (config, si, shallow, object)*)

        val N {id, 
               forward,
               backward,
               nodeData = ref (ND {fk, alignment, shape, variance})} = n
        val FD {defsKnown, object, reason = duReason, id=classId} = EC.get forward
        val BD {usesKnown, reason = uuReason} = EC.get backward
        val fk = (case fk of NONE => L.empty
                           | SOME fk => L.str (" : " ^ (MU.FieldKind.toString fk)))
        val ag = (case alignment of NONE => L.empty
                                  | SOME vs => L.str (" - " ^ (MU.ValueSize.toString vs)))
        val var = (case variance of NONE => L.empty 
                                  | SOME v => L.str ("^"^MU.FieldVariance.toString v))

        val header = L.seq [L.str (if defsKnown then "!" else "?"),
                            L.str (if usesKnown then "!" else "^"),
                            name true (id, classId), fk, ag, var
                           ]
        val l = 
            if long then
              let
                val shape = 
                    (case shape
                      of NONE => L.str "NONE"
                       | SOME shape => layoutShape shape)
                      
                val l = L.mayAlign [header,
                                    LU.indent (L.seq [L.str "shape = ", shape]),
                                    LU.indent (L.seq [L.str "class = ", Int.layout classId]),
                                    LU.indent (layoutObject object)
                           ]
              in l 
              end
            else
              header
      in l
      end

  val layoutShort = 
   fn (config, si, n, name) => layout' (config, si, n, name, false)

  val layout = 
   fn (config, si, n, name) => layout' (config, si, n, name, true)


  structure Unify =
  struct
  
    val flowSet =
     fn (config, id1, id2) => id1

    val forward = 
     fn (config, forward1, forward2) =>
        if EC.equal (forward1, forward2) then [] else
        let
          val FD {defsKnown = defsKnown1, reason = reason1, object = object1, id = id1} = EC.get forward1
          val FD {defsKnown = defsKnown2, reason = reason2, object = object2, id = id2} = EC.get forward2
          val defsKnown = defsKnown1 andalso defsKnown2
          val reason = 
              (case (reason1, reason2)
                of (NONE, _)          => reason2
                 | (_, NONE)          => reason1
                 | (SOME r1, SOME r2) => SOME (r1 ^ "&" ^ r2))
          val (object, edges) = Object.flowsTo (config, object1, object2)
          val id = flowSet (config, id1, id2)
          val entry = FD {defsKnown = defsKnown, reason = reason, object = object, id = id}
          val _ = EC.joinWith (forward1, forward2, fn _ => entry)
        in edges
        end

    val backward =
     fn (config, backward1, backward2) => 
        if EC.equal (backward1, backward2) then () else
        let
          val BD {usesKnown = usesKnown1, reason = reason1} = EC.get backward1
          val BD {usesKnown = usesKnown2, reason = reason2} = EC.get backward2
          val reason = 
              (case (reason1, reason2)
                of (NONE, _)          => reason2
                 | (_, NONE)          => reason1
                 | (SOME r1, SOME r2) => SOME (r1 ^ "&" ^ r2))
          val usesKnown = usesKnown1 andalso usesKnown2
          val entry = BD {usesKnown = usesKnown, reason = reason}
          val _ = EC.joinWith (backward1, backward2, fn _ => entry)
        in ()
        end

    val node' = 
     fn (config, 
         N {id = id1, forward = forward1, backward = backward1, nodeData = data1},
         N {id = id2, forward = forward2, backward = backward2, nodeData = data2}) => 
        let
          val edges = forward (config, forward1, forward2)
          val () = backward (config, backward1, backward2)
        in edges
        end

   (* XXX For directed, must deal with variance. *)
    val rec node = 
     fn (config, node1, node2) =>
        let
          val es = node' (config, node1, node2)
          val () = edges (config, es)
        in ()
        end
    and rec edge = 
     fn (config, e) => 
        (case e
          of MRB.EUnify (n1, n2) => node (config, n1, n2)
           | MRB.EFlow (n1, n2) => node (config, n1, n2))
    and rec edges =
     fn (config, es) => 
        List.foreach (es, fn e => edge (config, e))

    val vectorPrefix = 
     fn (config, v1, v2) => 
        let
          val limit = Int.min (Vector.length v1, Vector.length v2)
          val rec loop = 
           fn i => 
              if i < limit then
                let
                  val () = node (config, Vector.sub (v1, i), Vector.sub (v2, i))
                in loop (i + 1)
                end
              else ()
        in loop 0
        end

    val nodeWithShape = 
     fn (config, node, shape) => 
        let
          val forward = nodeGetForward node
          val fd  = EC.get forward
          val object = forwardDataGetObject fd
          val (object, es) = Object.shapeFlowsTo (config, object, shape)
          val fd = forwardDataSetObject (fd, object)
          val () = EC.set (forward, fd)
          val () = edges (config, es)
        in ()
        end

   (* XXX This only works with the unification based version.  -leaf *)
    val shapeWithNode = 
     fn (config, shape, n) => nodeWithShape (config, n, shape) 

  end (* MilRepUnify *)


end (* structure MilRepNode *)


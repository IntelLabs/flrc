(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature DATAFLOW = 
sig
  type analysis
  type node
  type info
  type result = node -> {iInfo : info, oInfo : info}
  val analyze : analysis -> result
end (* signature DATAFLOW *)

functor DataFlowF(type analysis
                  type node
                  structure NodeDict : DICT where type key = node
                  structure Info : 
                            sig
                              type info
                              type infoRef
                              val refMk : info -> infoRef
                              val refGet : infoRef -> info
                            end
                  val initial : analysis * node -> bool
                  val initialVal : analysis -> Info.info
                  val bottom : analysis -> Info.info
                  val components : analysis -> node List.t List.t
                  val successors : analysis * node -> node List.t
                  val transfer : analysis * node -> (Info.info * Info.infoRef -> bool)
                 ) : DATAFLOW where type node = node
                                and type info = Info.info 
                                and type analysis = analysis = 
struct 
  type node = node
  type info = Info.info
  type analysis = analysis

  type result = node -> {iInfo : info, oInfo : info}

  datatype item = I of {node : node, 
                        input : Info.infoRef, 
                        transfer : Info.info * Info.infoRef -> bool,
                        active : bool ref,
                        successors : item List.t ref}

  structure ND = NodeDict

  val fail = fn (f, msg) => Fail.fail ("DataFlowF", f, msg)

  (* TODO put each component in a good order (e.g. reverse post order) *)
  val orderComponents : node List.t List.t -> node List.t List.t = 
   fn ccs => ccs

  val foldCCs = 
   fn (ccs, init, doIt) => 
      List.fold (ccs, init, fn (cc, d) => List.fold (cc, d, doIt))

  val mapCCs = 
   fn (ccs, doIt) => 
      List.map (ccs, fn cc => List.map (cc, doIt))

  val foreachCCs = 
   fn (ccs, doIt) => 
      List.foreach (ccs, fn cc => List.foreach (cc, doIt))

  val initialize : analysis -> item List.t List.t = 
   fn analysis => 
      let
        val initialVal = initialVal analysis
        val bottom = bottom analysis

        val ccs = components analysis

        val ccs = orderComponents ccs

        val ccs = 
            let
              val doIt = 
               fn node => I {node = node, 
                             input = Info.refMk (if initial (analysis, node) then initialVal else bottom),
                             transfer = transfer (analysis, node),
                             active = ref true,
                             successors = ref []}
            in mapCCs (ccs, doIt)
            end

        val getItem =
            let
              val doIt = 
               fn (item as I {node, ...}, d) => ND.insert (d, node, item)
              val d = foldCCs (ccs, ND.empty, doIt)
            in fn n => case ND.lookup (d, n)
                        of SOME i => i
                         | NONE   => fail ("initialize", "Graph is not closed")
            end

        val () = 
            let
              val doIt = 
               fn (I {node, successors = ssR, ...}) => 
                  let
                    val ss = successors (analysis, node)
                    val ssItems = List.map (ss, getItem)
                    val () = ssR := ssItems
                  in ()
                  end
            in foreachCCs (ccs, doIt)
            end
      in ccs
      end

  val propagateEdge : item * item -> unit = 
   fn (I {input = i1, transfer, ...}, I {active, input = i2, ...}) =>
      if transfer (Info.refGet i1, i2) then
        active := true
      else
        ()

  val propagate : item -> unit = 
   fn (i1 as I {successors, ...}) => List.foreach (!successors, fn i2 => propagateEdge (i1, i2))

  val step1 : item -> bool = 
   fn (item as I {active, ...}) => 
      let
        val b = !active
        val () = active := false
        val () = if b then propagate item else ()
      in b
      end

  val step : item List.t -> bool =
   fn cc => 
      let
        val doOne = 
         fn (item, b) => 
            let
              val active = step1 item
            in b orelse active
            end
      in List.fold (cc, false, doOne)
      end

  val iterate1 : item List.t -> unit =
   fn cc =>
      let
        val rec loop = 
         fn () => 
            if step cc then loop () else ()
      in loop ()
      end

  val iterate : item List.t List.t -> unit = 
   fn ccs => List.foreach (ccs, iterate1)

  val extract : item List.t List.t * info -> result = 
   fn (ccs, bottom) => 
      let
        val doOne = 
         fn (I {node, input, transfer, active, ...}, d) => 
            let
              val () = if !active then fail ("extract", "Didn't properly reach a fixed point") 
                       else ()
              val iInfo = Info.refGet input
              val output = Info.refMk bottom
              val _ = transfer (iInfo, output)
              val oInfo = Info.refGet output
            in ND.insert (d, node, {iInfo = iInfo, oInfo = oInfo})
            end
        val d = foldCCs (ccs, ND.empty, doOne)
        val result = 
         fn n => (case ND.lookup (d, n)
                   of SOME r => r
                    | NONE   => fail ("extract", "Not a node"))
      in result
      end
      
  val analyze : analysis -> result = 
   fn analysis =>
      let
        val ccs = initialize analysis
        val () = iterate ccs
        val result = extract (ccs, bottom analysis)
      in result
      end

end (* functor DataFlowF *)
(*
functor BitVectorDataFlowF(type analysis
                           type node
                           type index
                           structure NodeDict : DICT where type key = node
                           structure IndexDict : DICT where type key = index
                           structure IndexSet : SET where type element = index
                           val indices : analysis -> index List.t
                           val initial : analysis * node -> bool
                           val initialVal : analysis -> bool
                           val bottom : analysis -> bool
                           val components : analysis -> node List.t List.t
                           val successors : analysis * node -> node List.t
                           val transfer   : analysis * node -> {phis : index * (index List.t) List.t,
                                                                gen  : index List.t, 
                                                                kill : index List.t}
                 ) : DATAFLOW where type graph = graph
                                and type node = node
                                and type info = IndexSet.t 
                                and type analysis = analysis =
struct 
  structure IBS = ImpBitSet
  structure ID = IndexDict
  structure IS = IndexSet

  val fail = fn (f, msg) => Fail.fail ("BitVectorDataFlowF", f, msg)

  type analysis0 = analysis
  type analysis = {analysis0 : analysis0, 
                   upwards : bool,
                   offsets : int IndexDict.t,
                   indices : index Vector.t,
                   indexCount : int}

  type node = node
  type info = IndexSet.t
  type result = node -> {iInfo : info, oInfo : info}

  val indexToOffset : analysis * index -> int = 
   fn (analysis, index) => 
      let
        val offsets = #offsets analysis
      in case ID.lookup (offsets, index)
          of SOME i => i
           | NONE => fail ("indexToOffset", "No offset for index")
      end

  val offsetToIndex : analysis * int -> index = 
   fn (analysis, i) => Vector.sub (#indices analysis, i)

  val emptyVector : analysis -> IBS.t = 
   fn analysis => IBS.empty (#indexCount analysis)

  val newVector : analysis * bool -> IBS.t = 
   fn (analysis, b) => IBS.new (#indexCount analysis, b)

  val indexListToVector : analysis * index List.t -> IBS.t = 
   fn (analysis, elts) => 
      let
        val bv = emptyVector analysis
        val () = List.foreach (elts, fn i => IBS.insert (bv, indexToOffset (analysis, i)))
      in bv
      end

  val vectorToIndexSet : analysis * IBS.t -> IS.t = 
   fn (analysis, bv) => 
      let
        val indices = IBS.toList bv
        val indices = List.map (indices, fn i => offsetToIndex (analysis, i))
        val s = IS.fromList indices
      in s
      end

  structure DFArg = 
  struct
    type analysis = analysis
    type node = node
    structure NodeDict = NodeDict
    structure Info =
      struct
      type info = IBS.t
      type infoRef = IBS.t
      val refMk : info -> infoRef = IBS.copy
      val refGet : infoRef -> info = fn v => v
      end
    val initial = fn (analysis, node) => initial (#analysis0 analysis, node)
    val initialVal = fn analysis => newVector (analysis, initialVal (#analysis0 analysis))
    val bottom = fn analysis => newVector (analysis, bottom (#analysis0 analysis))
    val components = fn analysis => components (#analysis0 analysis)
    val successors = fn (analysis, node) => successors (#analysis0 analysis, node)
    val transfer : analysis * node -> (IBS.t * IBS.t -> bool) = 
     fn (analysis, node) =>
        let
          val analysis0 = #analysis0 analysis
          val {phis, gen, kill} = transfer (analysis0, node)
          val genV = indexListToVector (analysis0, gen)
          val killV = indexListToVector (analysis0, kill)
          (* XXX this could be optimized for the cases that these are empty *)
          val operator = 
           fn (bv1, bv2) => 
              let
                val tmpV = IBS.copy bv1
                val () = 
                    if upwards analysis then
                      List.foreach (phis, 
                val () = IBS.difference (tmpV, killV)
                val () = IBS.union (tmpV, genV)
                val bv2Old = IBS.copy bv2
                val () = 
                    if upwards analysis then
                      IBS.union (bv2, tmpV)
                    else
                      IBS.intersection (bv2, tmpV)
                val changed = IBS.equal (bv2, bv2Old) 
              in changed
              end
        in operator
        end
  end 

  structure DF = DataFlowF(DFArg)

  val analyze : analysis -> result = 
   fn analysis0 =>
      let
        val indices = Vector.fromList (indices analysis0)
        val offsets = Vector.foldi (indices, ID.empty, fn (i, idx, d) => ID.insert (d, idx, i))
        val analysis = {analysis0 = analysis0,
                        upwards = bottom analysis0,
                        offsets = offsets,
                        indices = indices,
                        indexCount = ID.size offsets}
        val result0 = DF.analyze analysis
        val result = fn node => 
                        let
                          val {iInfo, oInfo} = result0 node
                        in {iInfo = vectorToIndexSet iInfo,
                            oInfo = vectorToIndexSet oInfo}
                        end
      in result
      end

end (* BitVectorDataFlowF *)
*)

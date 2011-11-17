(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_OBJECT = 
sig
  structure Shape :
  sig
    type 'node shape

    (* Filter nodes from shape according to the predicate.  The predicate
     * is ignored for:
     *  closure code pointers
     *  sum carried values
     *  p set carried values
     *  thunk carried values
     *)
    val filter : 'node shape * ('node -> bool) -> 'node shape

    val foreachWithParity : 'node shape * ('node -> unit) * ('node -> unit) -> unit

    val fieldKind : Config.t * 'node shape -> Mil.fieldKind option

    val typOf : Config.t * 'node shape * ('node -> Mil.typ) * ('node -> Mil.valueSize) * ('node -> Mil.fieldVariance) 
                -> Mil.typ
    val flatTypOf : (Config.t * 'node shape) -> Mil.typ

    structure Build :
    sig
      val unknown : Mil.typ -> 'node shape
      val base : Mil.typ -> 'node shape
      val closure : {name : Mil.variable option, code : 'node, fvs : 'node Vector.t} -> 'node shape
      val pSet : 'node -> 'node shape
      val pSetEmpty : unit -> 'node shape
      val sum : {tag : Mil.constant, fields : 'node Vector.t} -> 'node shape
      val sum' : {tag : Mil.constant, fields : 'node Vector.t} Vector.t -> 'node shape
      val tuple : {pok : Mil.pObjKind option, fields : 'node Vector.t, array : 'node option} -> 'node shape
      val thunkValue : {code : 'node, result : 'node} -> 'node shape
      val thunk : {name : Mil.variable option, code : 'node, result : 'node, fvs : 'node Vector.t} -> 'node shape
      val code : {name : Mil.variable option, args : 'node Vector.t, ress : 'node Vector.t} -> 'node shape
      val call : {filter : Mil.VS.t option, args : 'node Vector.t, ress : 'node Vector.t} -> 'node shape
      val callClosure : 'node -> 'node shape
      val eval : {filter : Mil.VS.t option, ress : 'node Vector.t} -> 'node shape
      val evalThunk : {result : 'node, code : 'node} -> 'node shape
      val cont : {label : Mil.label option, args : 'node Vector.t} -> 'node shape
      val cut : {filter : Mil.LS.t option, args : 'node Vector.t} -> 'node shape
    end (* structure Build *)
         
    structure Dec :
    sig
      val tuple : 'node shape -> {pok : Mil.pObjKind option, fields : 'node Vector.t, array : 'node option} option
    end
  end (* structure Shape *)

  structure Object :
  sig
    type 'node object

    val typOf : Config.t * 'node object * ('node -> Mil.typ) * ('node -> Mil.valueSize) * ('node -> Mil.fieldVariance)
                -> Mil.typ
    val flowsTo : Config.t * 'node object * 'node object -> 'node object * 'node MilRepBase.edge list
    val shapeFlowsTo : Config.t * 'node object * 'node Shape.shape -> 'node object * 'node MilRepBase.edge list
    val flowsToShape : Config.t * 'node Shape.shape * 'node object -> 'node Shape.shape * 'node MilRepBase.edge list
    val fromShape : 'node Shape.shape -> 'node object
    val bottom : unit -> 'node object
    val foreachWithParity : 'node object * ('node -> unit) * ('node -> unit) -> unit
  end (* structure Object *)

  structure Layout :
  sig
    val shape : Config.t * Mil.symbolInfo * ('node -> Layout.t) * 'node Shape.shape -> Layout.t
    val object : Config.t * Mil.symbolInfo * ('node -> Layout.t) * 'node Object.object -> Layout.t
  end

end

structure MilRepObject :> MIL_REP_OBJECT = 
struct

  structure M = Mil
  structure Type = MilType.Type
  structure Typer = M
  structure MU = MilUtils
  structure Seq = MilRepSeq 
  structure I = Identifier
  structure MRB = MilRepBase
  structure ID = IntDict
  structure IID = IntIntDict
  structure NID = MRB.NameIntDict
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure ND = Mil.ND
  structure VS = Mil.VS
  structure LS = Mil.LS
  structure CD = MU.Constant.Dict
  structure POKL = MRB.PObjKindLat

  datatype edge = datatype MRB.edge 

  structure Shape = 
  struct
    (*
     * functions:  We map function names to a tuple of their free variable
     * arguments, which get instantiated at closure creation (only).  The function then maps
     * to a simple code object.  Code objects are indexed by function name, and by arity for
     * anonymous functions (which arise from unknown calls).  Closures/Thunks consist of
     * a code node, and an environment.  We do not conflate environments unless forced to
     * do so by a free variable projection.  This could be avoided by naming free variable
     * projections.  To avoid conflating environments, we index them by variable name where
     * possible.  If not possible, we include an anonymous environment, keyed by arity, and a boolean
     * indicating whether or not we have conflated.
     *)

    datatype 'node env = EEnv of 'node Vector.t ID.t
                       | EClosure of 'node Vector.t VD.t (* Key by allocation site *)

    (*
     * I think the way this works is that for a known call, we can simply match up
     * the appropriate actual/formal pair.  For an unknown call, we may discover
     * later on that there are more functions in the equivalence class, so we must
     * include a summary node.  If a summary node exists, it must be linked with 
     * everything that joins the node.
     *)
    datatype 'node code = 
             CCode of {calls : ('node Vector.t * 'node Vector.t) option,
                       filter : VS.t option,
                       named : ('node Vector.t * 'node Vector.t) VD.t} IID.t

    (* This is the same as code *)
    datatype 'node cont = 
             CCont of {cuts : 'node Vector.t option,
                       filter : LS.t option,
                       named : 'node Vector.t LD.t} ID.t
                      
    datatype 'node shape = 
             TUnknown of M.typ
           | TBase of M.typ
           | TClosure of 'node * ('node env)
           | TPSet of 'node Seq.t
           | TSum  of 'node Seq.t CD.t
           | TTuple of POKL.t * 'node Seq.t
           | TThunk of 'node * 'node * ('node env)
           | TCode of 'node code
           | TCont of 'node cont

    val dictFlowsTo = 
     fn (map2, itemFlowsTo, d1, d2) => 
        let
          val edgesR = ref []
          val help = 
           fn (key, s1, s2) => 
              Utils.Option.union 
                (s1, s2, fn (s1, s2) => 
                            let
                              val (s, edges) = itemFlowsTo (s1, s2)
                              val () = edgesR := edges @ (!edgesR)
                            in s
                            end)
          val d = map2 (d1, d2, help)
        in (d, !edgesR)
        end

    (* Assumes equal length *)
    val vectorFlowsTo = 
     fn (v1, v2) => (v1, Utils.Vector.toListMap2 (v1, v2, EFlow))

    val vectorUnify = 
     fn (v1, v2) => (v1, Utils.Vector.toListMap2 (v1, v2, EUnify))

    val envFlowsTo =
     fn (e1, e2) => 
        let
          val add = 
           fn combine => 
           fn (id, v) => 
              let
                val i = Vector.length v
              in 
                case ID.lookup (id, i)
                 of SOME v' => 
                    let
                      val (v, edges) = combine (v', v)
                      val id = ID.insert (id, i, v)
                    in (id, edges)
                    end
                  | NONE => (ID.insert (id, i, v), [])
              end
          val (e, edges) =
              (case (e1, e2) 
                of (EEnv id1, EEnv id2) => 
                   let
                     val (id, edges) = dictFlowsTo (ID.map2, vectorFlowsTo, id1, id2)
                   in (EEnv id, edges)
                   end
                 | (EClosure vd1, EClosure vd2) => 
                   let
                     val (vd, edges) = dictFlowsTo (VD.map2, vectorFlowsTo, vd1, vd2)
                   in (EClosure vd, edges)
                   end
                 | (EEnv id, EClosure vd) => 
                   let
                     val help = 
                      fn (x, v, (id, edges)) => 
                         let
                           val (id, edges') = add vectorFlowsTo (id, v)
                           val edges = edges' @ edges
                         in (id, edges)
                         end
                     val (id, edges) = VD.fold (vd, (id, []), help)
                   in (EEnv id, edges)
                   end
                 | (EClosure vd, EEnv id2) => 
                   let
                     val help = 
                      fn (x, v, (id, edges)) => 
                         let
                           val (id, edges') = add vectorUnify (id, v)
                           val edges = edges' @ edges
                         in (id, edges)
                         end
                     val (id1, edges1) = VD.fold (vd, (ID.empty, []), help)
                     val (id, edges2) = dictFlowsTo (ID.map2, vectorFlowsTo, id1, id2)
                   in (EEnv id, edges1 @ edges2)
                   end)
        in (e, edges)
        end
        

    val argsRessFlowsTo = 
     fn ((args1, ress1), (args2, ress2)) => 
        let
          val (args, edges1) = vectorFlowsTo (args2, args1)  (* contra-variant *)
          val (ress, edges2) = vectorFlowsTo (ress1, ress2)
        in ((args, ress), edges1 @ edges2)
        end

    val argsRessUnify = 
     fn ((args1, ress1), (args2, ress2)) => 
        let
          val (args, edges1) = vectorUnify (args2, args1)  (* contra-variant *)
          val (ress, edges2) = vectorUnify (ress1, ress2)
        in ((args, ress), edges1 @ edges2)
        end

    val codeFlowsTo' = (* These have the same arity *)
     fn ({calls = calls1, filter = filter1, named = named1},
         {calls = calls2, filter = filter2, named = named2}) =>
        let
          val named = VD.union (named1, named2, fn (k, a, _) => a)
          val filter = 
              Utils.Option.union 
                (filter1, filter2, fn (filter1, filter2) => VS.union (filter1, filter2))
              
          val edgesR = ref []
          val add = fn edges => edgesR := edges @ !edgesR
          val add' = fn (a, edges) => 
                        let
                          val () = add edges
                        in a
                        end

          val add'' = fn (a, edges) => add edges

          val calls = 
              Utils.Option.union
                (calls1, calls2, fn (p1, p2) => 
                                    add' (argsRessUnify (p1, p2)))
              
          val applies =
           fn v => 
              (case filter
                of SOME filter => VS.member (filter, v)
                 | NONE => true)
              
          val flow = 
           fn p1 => 
           fn (v, p2) =>
              if applies v then
                add'' (argsRessFlowsTo (p1, p2))
              else
                ()

          val () = Option.foreach (calls, fn p1 => VD.foreach (named, flow p1))
        in ({calls = calls, filter = filter, named = named}, !edgesR)
        end

    val codeFlowsTo = 
     fn (CCode iid1, CCode iid2) => 
        let
          val (iid, edges) = dictFlowsTo (IID.map2, codeFlowsTo', iid1, iid2)
        in (CCode iid, edges)
        end

    val contFlowsTo' = (* These have the same arity *)
     fn ({cuts = cuts1, filter = filter1, named = named1},
         {cuts = cuts2, filter = filter2, named = named2}) =>
        let
          val named = LD.union (named1, named2, fn (k, a, _) => a)
          val filter = 
              Utils.Option.union 
                (filter1, filter2, fn (filter1, filter2) => LS.union (filter1, filter2))
              
          val edgesR = ref []
          val add = fn edges => edgesR := edges @ !edgesR
          val add' = fn (a, edges) => 
                        let
                          val () = add edges
                        in a
                        end

          val add'' = fn (a, edges) => add edges

          val cuts = 
              Utils.Option.union
                (cuts1, cuts2, fn (p1, p2) => 
                                  add' (vectorUnify (p1, p2)))
              
          val applies =
           fn l => 
              (case filter
                of SOME filter => LS.member (filter, l)
                 | NONE => true)

          val flow = 
           fn p1 => 
           fn (l, p2) =>
              if applies l then
                add'' (vectorFlowsTo (p1, p2))
              else
                ()
          val () = Option.foreach (cuts, fn p1 => LD.foreach (named, flow p1))
        in ({cuts = cuts, filter = filter, named = named}, !edgesR)
        end


    val contFlowsTo = 
     fn (CCont iid1, CCont iid2) => 
        let
          val (iid, edges) = dictFlowsTo (ID.map2, contFlowsTo', iid1, iid2)
        in (CCont iid, edges)
        end

    val seqFlowsTo =
     fn (s1, s2) => 
        let
          val (s, edges) = Seq.union (s1, s2)
          val edges = List.map (edges, EUnify)
        in (s, edges)
        end

    val shapeFlowsTo = 
     fn (config, shape1, shape2) => 
        let
          val noflow = (shape1, [])
          val (shape, edges) = 
              (case (shape1, shape2)
                of (TUnknown t1, TUnknown t2) => (TUnknown (Type.lub (config, t1, t2)), [])
                 | (TUnknown _, _) => noflow
                 | (TBase t1, TBase t2) => (TBase (Type.lub (config, t1, t2)), [])
                 | (TBase _, _) => noflow
                 | (TClosure (n1, e1), TClosure (n2, e2)) => 
                   let
                     val (e, edges) = envFlowsTo (e1, e2)
                     val edges = EFlow (n1, n2)::edges
                   in (TClosure (n1, e), edges)
                   end
                 | (TClosure _, _) => noflow
                 | (TPSet s1, TPSet s2) => 
                   let
                     val (s, edges) = seqFlowsTo (s1, s2)
                   in (TPSet s, edges)
                   end
                 | (TPSet _, _) => noflow
                 | (TSum d1, TSum d2) => 
                   let
                     val flow = fn (a1, a2) => seqFlowsTo (a1, a2)
                     val (d, edges) = dictFlowsTo (CD.map2, flow, d1, d2)
                   in (TSum d, edges)
                   end
                 | (TSum _, _) => noflow
                 | (TTuple (pok1, s1), TTuple (pok2, s2)) => 
                   let
                     val pok = POKL.join (pok1, pok2)
                     val (s, edges) = seqFlowsTo (s1, s2)
                   in (TTuple (pok, s), edges)
                   end
                 | (TTuple _, _) => noflow
                 | (TThunk (n1, r1, e1), TThunk (n2, r2, e2)) => 
                    let
                      val (e, edges) = envFlowsTo (e1, e2)
                      val edges = EFlow (r1, r2) :: EFlow (n1, n2) :: edges
                    in (TThunk (n1, r1, e), edges)
                    end
                 | (TThunk _, _) => noflow
                 | (TCode c1, TCode c2) => 
                   let
                     val (c, edges) = codeFlowsTo (c1, c2)
                   in (TCode c, edges)
                   end
                 | (TCode _, _) => noflow
                 | (TCont s1, TCont s2) => 
                   let
                     val (s, edges) = contFlowsTo (s1, s2)
                   in (TCont s, edges)
                   end
                 | (TCont _, _) => noflow)
        in (shape, edges)
        end

    val foreachWithParity = 
     fn (shape, node, nodeMinus) => 
        let
          val seq = fn s => Seq.foreach (s, node)
          val vector = fn v => Vector.foreach (v, node)
          val vectorMinus = fn v => Vector.foreach (v, nodeMinus)
          val argRes = fn (v1, v2) => (vectorMinus v1;
                                       vector v2)
          val env = fn env => ()

          val code = 
           fn (CCode d) => 
              IID.foreach (d, fn (i, {calls, filter, named}) => 
                                 let
                                   val () = Option.foreach (calls, argRes)
                                   val () = VD.foreach (named, fn (i, p) => argRes p)
                                 in ()
                                 end)
          val cont = 
           fn (CCont d) => 
              ID.foreach (d, fn (i, {cuts, filter, named}) => 
                                let
                                  val () = Option.foreach (cuts, vectorMinus)
                                  val () = LD.foreach (named, fn (i, p) => vectorMinus p)
                                in ()
                                end)

          val () = 
              (case shape
                of TUnknown t => ()
                 | TBase t => ()
                 | TClosure (n, e) => (node n; env e)
                 | TPSet s => seq s
                 | TSum cd => CD.foreach (cd, fn (k, s) => seq s)
                 | TTuple (pok, s) => seq s
                 | TThunk (n, r, e) => (node n; node r; env e)
                 | TCode c => code c
                 | TCont c => cont c)
        in ()
        end

    val fieldKind = 
        fn (config, s) => 
           (case s
             of TUnknown t => MU.FieldKind.fromTyp' (config, t)
              | TBase t    => MU.FieldKind.fromTyp' (config, t)
              | TClosure _ => SOME M.FkRef
              | TPSet _    => SOME M.FkRef
              | TSum _     => SOME M.FkRef
              | TTuple _   => SOME M.FkRef
              | TThunk _   => SOME M.FkRef
              | TCode _    => SOME (M.FkBits (MU.FieldSize.ptrSize config))
              | TCont _    => SOME (M.FkBits (MU.FieldSize.ptrSize config)))

    structure TypOf =
    struct

      val code = 
       fn (node, variance, config, CCode d) => 
          let
            val doOne =
             fn (v, {calls, filter, named}, t0) => 
                let
                  val mk =
                   fn (args, ress) => M.TCode {cc = M.CcCode, 
                                               args = Vector.map (args, node), 
                                               ress = Vector.map (ress, node)}
                  val t0 = 
                      (case calls
                        of SOME ar => Type.lub (config, t0, mk ar)
                         | NONE => t0)
                  val t = VD.fold (named, t0, fn (v, ar, t) => Type.lub (config, t, mk ar))
                in t
                end

            val t = IID.fold (d, M.TNone, doOne)
          in t
          end

      val cont = 
       fn (node, variance, config, CCont d) => 
          let
            val doOne =
             fn (v, {cuts, filter, named}, t0) => 
                let
                  val mk =
                   fn args => M.TContinuation (Vector.map (args, node))
                  val t0 = 
                      (case cuts
                        of SOME args => Type.lub (config, t0, mk args)
                         | NONE => t0)
                  val t = LD.fold (named, t0, fn (l, args, t) => Type.lub (config, t, mk args))
                in t
                end

            val t = ID.fold (d, M.TNone, doOne)
          in t
          end

      val shape = 
       fn (config, s, node, alignment, variance) =>
          let
            val t =
                (case s
                  of TUnknown t => t
                   | TBase t => t
                   | TClosure (code, env) => 
                     (case node code
                       of M.TCode {args, ress, ...} => M.TClosure {args = args, ress = ress}
                        | _ => M.TPAny)
                   | TPSet elts => 
                     let
                       val over =
                           (case elts
                             of Seq.SeqCons (n, _) => node n
                              | Seq.Seq n => node n
                              | Seq.SeqZ => M.TNone
                              | Seq.SeqBot => M.TNone)
                     in M.TPType {kind = M.TkE, over = over}
                     end
                   | TSum cd => 
                     let
                       val tag = CD.fold (cd, M.TNone, fn (c, _, t) => 
                                                          Type.lub (config, t, MU.Constant.typOf (config, c)))
                       val armsL = CD.toListSorted cd
                       val seq = fn s => Vector.map (#1 (Seq.deconstruct s), node)
                       val arms = Vector.fromListMap (armsL, fn (c, s) => (c, seq s))
                     in M.TSum {tag = tag, arms = arms}
                     end
                   | TTuple (pokl, nodes) => 
                     if POKL.isTop pokl then M.TRef
                     else
                       let
                         val pok = 
                             (case POKL.get pokl
                               of NONE => M.PokNone
                                | SOME pok => pok)
                         val (elts, terminator) = Seq.deconstruct nodes
                         val field = 
                          fn n => (node n, alignment n, variance n)
                         val fixed = Vector.map (elts, field)
                         val array = 
                             (case terminator
                               of SOME (SOME a) => (field a)
                                | _ => (M.TNone, M.Vs8, M.FvReadWrite))
                       in M.TTuple {pok = pok, fixed = fixed, array = array}
                       end
                   | TThunk (code, res, env) => M.TThunk (node res)
                   | TCode c => code (node, variance, config, c)
                   | TCont c => cont (node, variance, config, c))
          in t
          end
    end (* structure TypOf *)

    val typOf = TypOf.shape

    val flatTypOf = 
       fn (config, s) =>
          let
            val t =
                (case s
                  of TUnknown t => MU.FlatTyp.fromTyp (config, t)
                   | TBase t    => MU.FlatTyp.fromTyp (config, t)
                   | TClosure (code, env) => M.TPAny
                   | TPSet elts => M.TPAny
                   | TSum nd    => M.TPAny
                   | TTuple (pokl, nodes) => (case POKL.get pokl 
                                               of SOME pok => if pok = M.PokNone then M.TRef else M.TPAny
                                                | NONE => M.TRef)
                   | TThunk (code, res, env) => M.TRef
                   | TCode c => M.TBits (MU.ValueSize.ptrSize config)
                   | TCont c => M.TBits (MU.ValueSize.ptrSize config))
          in t
          end

    val class =
     fn shape =>
        (case shape
          of TUnknown _ => 0
           | TBase _    => 1
           | TClosure _ => 2
           | TPSet _    => 3
           | TSum _     => 4
           | TTuple _   => 5
           | TThunk _   => 6
           | TCode _    => 7
           | TCont _    => 10)

    val filter = 
     fn (shape, dead) => 
        let
          val live = not o dead
          val seq = fn s => Seq.filter (s, dead)
          val vector = fn v => Vector.keepAll (v, live)
          val iDictMap = 
           fn (d, doOne) => ID.map (d, fn (i, e) => doOne e)
          val vDictMap = 
           fn (d, doOne) => VD.map (d, fn (i, e) => doOne e)
          val lDictMap = 
           fn (d, doOne) => LD.map (d, fn (i, e) => doOne e)
          val iiDictMap = 
           fn (d, doOne) => IID.map (d, fn (i, e) => doOne e)
          val pairMap =
           fn doOne => fn (a, b) => (doOne a, doOne b)
          val env = 
           fn e => 
              case e
               of EEnv d     => EEnv (iDictMap (d, vector))
                | EClosure d => EClosure (vDictMap (d, vector))

          val code = 
           fn (CCode d) => 
              CCode (iiDictMap (d, fn {calls, filter, named} => 
                                      {calls = Option.map (calls, pairMap vector),
                                       filter = filter,
                                       named = vDictMap (named, pairMap vector)}))
          val cont = 
           fn (CCont d) => 
              CCont (iDictMap (d, fn {cuts, filter, named} => 
                                     {cuts = Option.map (cuts, vector),
                                      filter = filter,
                                      named = lDictMap (named, vector)}))

          val shape = 
              case shape
               of TUnknown t        => shape
                | TBase t          => shape
                | TClosure (n, e)  => TClosure (n, env e)
                | TPSet s          => TPSet s
                | TSum cd          => TSum (CD.map (cd, fn (c, s) => seq s))
                | TTuple (pok, s)  => TTuple (pok, seq s)
                | TThunk (n, r, e) => TThunk (n, r, env e)
                | TCode c          => TCode (code c)
                | TCont c          => TCont (cont c)
        in shape
        end

    structure Build = 
    struct
      val unknown =
       fn t => TUnknown t

      val base = 
       fn t => TBase t

      val closure = 
       fn {name, code, fvs} => 
          let
            val env = 
                (case name
                  of SOME name => EClosure (VD.singleton (name, fvs))
                   | NONE => EEnv (ID.singleton (Vector.length fvs, fvs)))
          in TClosure (code, env)
          end

      val callClosure = 
       fn code => 
          let
            val env = EClosure VD.empty
          in TClosure (code, env)
          end


      val pSet = 
       fn element => TPSet (Seq.seq1 element)

      val pSetEmpty =
       fn () => TPSet Seq.seq0

      val sum' =
       fn v => TSum (Vector.fold (v, CD.empty, fn ({tag, fields}, cd) 
                                                  => CD.insert (cd, tag, Seq.fromVectorOpen fields)))

      val sum = 
       fn r => sum' (Vector.new1 r)

      val tuple = 
       fn {pok, fields, array} => 
          let
            val toField = fn node => node
            val fields = Vector.map (fields, toField)
            val array = Option.map (array, toField)
            val seq = 
                (case array
                  of NONE => Seq.fromVectorClosed fields
                   | SOME f => Seq.fromVector (fields, Seq.Seq f))
            val pok = 
                (case pok
                  of NONE => POKL.bot
                   | SOME pok => POKL.elt pok)
          in TTuple (pok, seq)
          end

      val thunkValue =
       fn {code, result} => 
          let
            val env = EClosure VD.empty
          in TThunk (code, result, env)
          end

      val thunk =
       fn {name, code, result, fvs} => 
          let
            val env = 
                (case name
                  of SOME name => EClosure (VD.singleton (name, fvs))
                   | NONE => EEnv (ID.singleton (Vector.length fvs, fvs)))
          in TThunk (code, result, env)
          end

      val evalThunk = 
       fn {code, result} => 
          let
            val env = EClosure VD.empty
          in TThunk (code, result, env)
          end


      val code =
       fn {name, args, ress} => 
          let
            val idx = (Vector.length args, Vector.length ress)
            val entry = 
                (case name
                  of SOME name => {calls = NONE,
                                   filter = NONE,
                                   named = VD.singleton (name, (args, ress))}
                   | NONE => {calls = SOME (args, ress),
                              filter = NONE,
                              named = VD.empty})
            val code = CCode (IID.singleton (idx, entry))
          in TCode code
          end

      val call = 
       fn {filter, args, ress} => 
          let
            val idx = (Vector.length args, Vector.length ress)
            val calls = SOME (args, ress)
            val entry = {calls = calls,
                         filter = filter,
                         named = VD.empty}
            val code = CCode (IID.singleton (idx, entry))
          in TCode code
          end

      val eval = 
       fn {filter, ress} => call {filter = filter, args = Vector.new0 (), ress = ress}

      val cont = 
       fn {label, args} => 
          let
            val idx = Vector.length args
            val entry = 
                (case label
                  of SOME label => {cuts = NONE,
                                    filter = NONE,
                                    named = LD.singleton (label, args)}
                   | NONE => {cuts = SOME args,
                              filter = NONE,
                              named = LD.empty})
            val cont = CCont (ID.singleton (idx, entry))
          in TCont cont
          end

      val cut = 
       fn {filter, args} => 
          let
            val idx = Vector.length args
            val cuts = SOME args
            val entry = {cuts = cuts,
                         filter = filter,
                         named = LD.empty}
            val cont = CCont (ID.singleton (idx, entry))
          in TCont cont
          end

    end (* structure Build *)

    structure Dec = 
    struct
      val tuple = 
       fn s => 
          (case s
            of TTuple (pok, seq) => 
               let
                 val (fixed, arrayO) = Seq.deconstruct seq
                 val array = 
                     (case arrayO
                       of NONE => NONE
                        | SOME array => array)
               in SOME {pok = POKL.get pok, fields = fixed, array = array}
               end
             | _ => NONE)

    end (* structure Dec *)
  end (* structure Shape *)

  structure Object = 
  struct
    type 'node object = 'node Shape.shape ID.t

    val fromShape = 
     fn shape => ID.singleton (Shape.class shape, shape)

    val addShape = 
     fn (object, shape) => ID.insert (object, Shape.class shape, shape)

    val lookupShape = 
     fn (object, shape) => ID.lookup (object, Shape.class shape)

    val flowsTo = 
     fn (config, u1, u2) => 
        let
          val flow = fn (s1, s2) => Shape.shapeFlowsTo (config, s1, s2)
          val (u, edges) = Shape.dictFlowsTo (ID.map2, flow, u1, u2)
        in (u, edges)
        end

    val shapeFlowsTo = 
     fn (config, object, shape) => 
        let
          val (shape, edges) = 
              (case lookupShape (object, shape)
                of SOME shape' => Shape.shapeFlowsTo (config, shape', shape)
                 | NONE => (shape, []))
          val object = addShape (object, shape)
        in (object, edges)
        end

    val flowsToShape = 
     fn (config, shape, object) => 
        let
          val flow = fn (_, s, (shape, edges)) => 
                        let 
                          val (shape, edges') = Shape.shapeFlowsTo (config, shape, s)
                        in (shape, edges' @ edges)
                        end
          val (s, edges) = ID.fold (object, (shape, []), flow)
        in (s, edges)
        end

    val bottom = 
     fn () => ID.empty

    val foreachWithParity = 
     fn (object, nodePlus, nodeMinus) => 
        ID.foreach (object, fn (i, shape) => Shape.foreachWithParity (shape, nodePlus, nodeMinus))

    val typOf = 
     fn (config, object, node, alignment, variance) => 
        let
          val shape = fn (config, shape) => Shape.typOf (config, shape, node, alignment, variance)
        in ID.fold (object, M.TNone, fn (i, s, t) => Type.lub (config, shape (config, s), t))
        end

    val flatTypOf =
     fn (config, object) => 
        ID.fold (object, M.TNone, fn (i, s, t) => Type.lub (config, Shape.flatTypOf s, t))


  end (* structure Object *)

  structure Layout = 
  struct
    structure L = Layout
    structure LU = LayoutUtils

    val typ = MilLayout.layoutTyp
    val pObj = MilLayout.layoutPObjKind
    val var = MilLayout.layoutVariable
    val label = MilLayout.layoutLabel
    val int = Int.layout

    val env = 
     fn (config, si, layoutNode, env) =>
        let
          val node = layoutNode
          val vector = fn v => Vector.layout node v
          val var = fn v => var (config, si, v)
          val dict = 
           fn (key, dict, d) => 
              let
                val entry = 
                 fn (i, e) => L.seq [key i, 
                                     L.str " => ", vector e]
                val d = dict (d, entry)
              in
                LU.parenSeq [L.str "env = ", d]
              end
              
          val l = 
              (case env
                of (Shape.EEnv env) => dict (int, ID.layout, env)
                 | (Shape.EClosure cls) => dict (var, VD.layout, cls))
        in l
        end

    val codeEntry =
     fn (config, si, layoutNode) => 
     fn ((i1, i2), {calls, filter, named}) => 
        let
          val node = layoutNode
          val vector = fn v => Vector.layout node v
          val var = fn v => var (config, si, v)
          val doPair = 
           fn (args, ress) => L.seq [vector args, L.str " => ", vector ress]
          val calls = 
              case calls
               of NONE => L.str "NONE"
                | SOME p => doPair p
          val filter = 
              case filter
               of NONE => L.empty
                | SOME s => L.seq [L.str "Filter is ", VS.layout (s, var)]
          val named = 
              let
                val entry = 
                 fn (i, p) => L.seq [var i, 
                                     L.str " => ", doPair p]
                val d = VD.layout (named, entry)
              in
                L.mayAlign [L.str "Candidates are ", LU.indent d]
              end
        in L.seq [L.str "Arity ", LU.parenSeq [int i1, int i2], L.str " => ",
                  L.mayAlign [calls, filter, named]]
        end

    val code = 
     fn (config, si, layoutNode, Shape.CCode d) => 
        L.seq [L.str "Code ", IID.layout (d, codeEntry (config, si, layoutNode))]

    val contEntry =
     fn (config, si, layoutNode) => 
     fn (i1, {cuts, filter, named}) => 
        let
          val node = layoutNode
          val vector = fn v => Vector.layout node v
          val label = fn l => label (config, si, l)
          val doOne = 
           fn args => L.seq [L.str " -/-> ", vector args]
          val cuts = 
              case cuts
               of NONE => L.str "NONE"
                | SOME p => doOne p
          val filter = 
              case filter
               of NONE => L.empty
                | SOME s => L.seq [L.str "Filter is ", LS.layout (s, label)]
          val named = 
              let
                val entry = 
                 fn (i, p) => L.seq [label i, 
                                     L.str " => ", doOne p]
                val d = LD.layout (named, entry)
              in
                LU.parenSeq [L.str "Candidates are ", d]
              end
        in L.seq [L.str "Arity ", LU.parenSeq [int i1], L.str " ",
                  L.mayAlign [cuts, filter, named]]
        end

    val cont = 
     fn (config, si, layoutNode, Shape.CCont d) => 
        L.seq [L.str "Cont ", ID.layout (d, contEntry (config, si, layoutNode))]

    val shape = 
     fn (config, si, layoutNode, shape) => 
        let
          val typ = fn t => typ (config, si, t)
          val node = layoutNode
          val env = fn e => env (config, si, layoutNode, e)
          val code = fn c => code (config, si, layoutNode, c)
          val cont = fn c => cont (config, si, layoutNode, c)
          val seq = Seq.layout node
          val pObj = fn pok => pObj (config, si, pok)

          val l = 
              (case shape
                of Shape.TUnknown t => L.seq[L.str "UNKNOWN[", typ t, L.str "]"]
                 | Shape.TBase t => L.seq[L.str "Base[", typ t, L.str "]"]
                 | Shape.TClosure (n, e) => 
                   L.seq [L.str "Clos ", LU.parenSeq [node n], L.str " where ", env e]
                 | Shape.TPSet s => L.seq [L.str "PSet", seq s]
                 | Shape.TSum cd => 
                   let
                     val help = 
                      fn (k, s) => L.seq [MilLayout.layoutConstant (config, si, k), 
                                          L.str " => ", seq s]
                   in
                     L.seq [L.str "Sum", 
                            CD.layout (cd, help)]
                   end
                 | Shape.TTuple (pok, s) => L.seq [L.str "Tuple", 
                                                   LU.parenSeq [POKL.layout pObj pok,
                                                                seq s]]
                 | Shape.TThunk (n, r, e) => L.mayAlign [L.str "Thunk ", LU.bracketSeq [node r], LU.parenSeq [node n], 
                                                         L.str " where ", env e]
                 | Shape.TCode c => code c
                 | Shape.TCont c => cont c)
        in l
        end

    val object = 
     fn (config, si, layoutNode, object) => 
        let
          val help = 
           fn (i, s) => shape (config, si, layoutNode, s)
          val l = ID.layout (object, help)
        in l
        end

  end (* structure Layout *)
end (* structure MilRepObject *)
(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_RECONSTRUCT = 
sig
  val debugs : Config.Debug.debug list
  val features : Config.Feature.feature list
  val program : PassData.t * MilRepSummary.summary * bool * Mil.t -> Mil.t
end (* signature MIL_REP_RECONSTRUCT *)

structure MilRepReconstruct :> MIL_REP_RECONSTRUCT = 
struct

  val passname = "MilRepReconstruct"

  val fail = 
   fn (f, m) => Fail.fail ("reconstruct.sml", f, m)

  val (showTypChangesD, showTypChanges) =
      Config.Debug.mk (passname ^ ":show-type-changes", "Show changes to variable types")

  val (showFieldKindChangesD, showFieldKindChanges) =
      Config.Debug.mk (passname ^ ":show-fk-changes", "Show changes to field kinds/vtables")

  val debugs = [showTypChangesD, showFieldKindChangesD]

  val features = []

  structure M = Mil
  structure MU = MilUtils
  structure MRB = MilRepBase
  structure MRN = MilRepNode
  structure MRS = MilRepSummary
  structure MRC = MilRewriterClient
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure I = Identifier
  structure IM = Identifier.Manager
  structure L = Layout
  structure LU = LayoutUtils

  datatype state  = S of {summary : MRS.summary}
  datatype env = E of {config : Config.t,
                       flat : bool,
                       st : Mil.symbolTable}

  val getConfig = 
   fn (E {config, ...}) => config

  val getFlat = 
   fn (E {flat, ...}) => flat

  val variableTyp = 
   fn (E{st, ...}, v) => MU.SymbolTable.variableTyp (st, v)

  val getSummary = 
   fn (S {summary}) => summary

  val getSi = 
   fn (E {st, ...}) => Identifier.SymbolInfo.SiTable st

  val fieldDescriptorForNode = 
   fn n => MRN.fieldDescriptor n

  val fieldKindForNode = 
   fn n => MRN.fieldKind n

  val typForNode = 
   fn (summary, flat, node) =>
      if flat then
        MRS.nodeFlatTyp (summary, node)
      else
        MRS.nodeTyp (summary, node)

  val typForVariable = 
   fn (summary, flat, v) =>
      if flat then
        MRS.variableFlatTyp (summary, v)
      else
        MRS.variableTyp (summary, v)
  

  val buildCodeTyp = 
   fn (summary, flat, v) => 
      (case MRS.iInfo' (summary, MU.Id.G v)
        of SOME (MRB.IiCode {cargs, args, returns}) => 
           let
             val node = fn n => typForNode (summary, flat, n)
             val args = Vector.map (args, node)
             val ress = Vector.map (returns, node)
             val cc = MU.CallConv.map (cargs, node)
             val cc = case cc
                       of M.CcCode => M.CcCode
                        | M.CcThunk {thunk, fvs} => 
                          M.CcThunk {thunk = M.TThunk (Vector.sub (ress, 0)),
                                     fvs = fvs}
                        | M.CcClosure {cls, fvs} => 
                          M.CcClosure {cls = M.TClosure {args = args, ress = ress},
                                       fvs = fvs}
           in SOME (M.TCode {cc = cc, args = args, ress = ress})
           end
         | _ => NONE)

  val buildMetaData = 
   fn (se as (state, env), id) => 
      let
        val summary = getSummary state
        val config = getConfig env
        val mdDesc = 
            (case MRS.iInfo (summary, id)
              of MRB.IiMetaData {pok, pinned, fixed, array} => 
                 M.MDD {pok = pok, 
                        pinned = pinned,
                        fixed = Vector.map (fixed, fn n => fieldDescriptorForNode n),
                        array = Option.map (array, fn (i, n) => (i, fieldDescriptorForNode n))}
               | _ => fail ("buildMetaData", "Id has no vtable entry"))
      in mdDesc
      end

  val buildTupleDescriptor = 
   fn (se as (state, env), id) => 
      let
        val summary = getSummary state
        val config = getConfig env
        val td = 
            (case MRS.iInfo (summary, id)
              of MRB.IiTupleDescriptor {fixed, array} => 
                 M.TD {fixed = Vector.map (fixed, fn n => fieldDescriptorForNode n),
                       array = Option.map (array, fn n => fieldDescriptorForNode n)}
               | MRB.IiMetaData {pok, pinned, fixed, array} => 
                 M.TD {fixed = Vector.map (fixed, fn n => fieldDescriptorForNode n),
                       array = Option.map (array, fn (_, n) => fieldDescriptorForNode n)}
               | _ => fail ("buildTupleDescriptor", "Id has no tuple descriptor entry"))
      in td
      end

  val buildThunkDescriptor = 
   fn (se as (state, env), id) => 
      let
        val summary = getSummary state
        val config = getConfig env
        val td = 
            (case MRS.iInfo (summary, id)
              of MRB.IiThunk {typ, fvs} => 
                 {typ = fieldKindForNode typ,
                  fvs = Vector.map (fvs, fn n => fieldKindForNode n)}
               | _ => fail ("buildThunkDescriptor", "Id has no thunk descriptor entry"))
      in td
      end

  val buildClosureDescriptor = 
   fn (se as (state, env), id) => 
      let
        val summary = getSummary state
        val config = getConfig env
        val td = 
            (case MRS.iInfo (summary, id)
              of MRB.IiClosure fvs => 
                 Vector.map (fvs, fn n => fieldKindForNode n)
               | _ => fail ("buildClosureDescriptor", "Id has no pFunction descriptor entry"))
      in td
      end

  val buildSumDescriptor = 
   fn (se as (state, env), id) => 
      let
        val summary = getSummary state
        val config = getConfig env
        val td = 
            (case MRS.iInfo (summary, id)
              of MRB.IiSum (node, nodes) => (fieldKindForNode node, Vector.map (nodes, fieldKindForNode))
               | _ => fail ("buildSumDescriptor", "Id has no Sum descriptor entry"))
      in td
      end

  val buildCodeReturnTypes = 
   fn (se as (state, env), id) =>
      let
        val summary = getSummary state
        val rets = 
            (case MRS.iInfo (summary, id)
              of MRB.IiCode {returns, ...} => 
                 Vector.map (returns, fn n => typForNode (summary, getFlat env, n))
               | _ => fail ("buildCodeReturnTypes", "Id has no code entry"))
      in rets
      end

  val checkItem = 
   fn (debug, cmp, layout) =>
   fn (se as (state, env), id, new, old) => 
      if debug (getConfig env) andalso not (cmp (new, old)) then
        let
          val config = getConfig env
          val si = getSi env
          val l = L.seq [L.str "Info for ", MU.Id.layout (si, id), L.str " has changed "]
          val l =
              if Config.debugLevel (config, passname) >= 1 then
                L.mayAlign [l, 
                            LU.indent (L.seq [L.str "Old = ", layout (config, si, old)]),
                            LU.indent (L.seq [L.str "New = ", layout (config, si, new)])]
              else
                l
          val () = LU.printLayout l
        in ()
        end
      else ()
  val checkMetaData = checkItem (showFieldKindChanges, MU.MetaDataDescriptor.eq, MilLayout.layoutMetaDataDescriptor)
  val checkTupleDescriptor = checkItem (showFieldKindChanges, MU.TupleDescriptor.eq, MilLayout.layoutTupleDescriptor)
  val checkThunkDescriptor = 
      let
        val eq = Equality.rec2 (#typ, MU.Eq.fieldKind, #fvs, Equality.vector MU.Eq.fieldKind)
        val layout = fn (config, si, {typ, fvs}) => 
                        Layout.mayAlign [L.seq [L.str "typ = ", MilLayout.layoutFieldKind (config, si, typ)],
                                         L.seq [L.str "fvs = ", MilLayout.layoutFieldKinds (config, si, fvs)]]
      in checkItem (showFieldKindChanges, eq, layout)
      end
  val checkClosureDescriptor =
      let
        val eq = Equality.vector MU.Eq.fieldKind
        val layout = fn (config, si, fvs) => 
                        L.seq [L.str "fvs = ", MilLayout.layoutFieldKinds (config, si, fvs)]
      in checkItem (showFieldKindChanges, eq, layout)
      end
  val checkSumDescriptor = 
      let
        val eq = Equality.vector MU.FieldKind.eq
        val layout = 
         fn (config, si, fks) => L.seq [L.str "fks = ", MilLayout.layoutFieldKinds (config, si, fks)]
      in checkItem (showFieldKindChanges, eq, layout)
      end
  val checkSumTagDescriptor = 
      let
        val eq = MU.FieldKind.eq
        val layout = 
         fn (config, si, fk) => L.seq [L.str "fk = ", MilLayout.layoutFieldKind (config, si, fk)]
      in checkItem (showFieldKindChanges, eq, layout)
      end
  val checkCodeReturnTypes =
      let
        val layout = 
         fn (config, si, ts) => 
            L.seq [L.str "rets = ", LU.parenSeq (Vector.toListMap (ts, fn t => MilLayout.layoutTyp (config, si, t)))]
      in checkItem (showTypChanges, Equality.vector MU.Typ.eq, layout)
      end

  val instruction = 
   fn (se as (state, env), M.I {dests, n, rhs}) => 
      let
        val id = MU.Id.I n
        val rhs = 
            (case rhs
              of M.RhsSimple _ => rhs
               | M.RhsPrim _ => rhs
               | M.RhsTuple {mdDesc = mdDescOld, inits} => 
                 let
                   val mdDesc = buildMetaData (se, id)
                   val () = checkMetaData (se, id, mdDesc, mdDescOld)
                 in M.RhsTuple {mdDesc = mdDesc, inits = inits}
                 end
               | M.RhsTupleSub (M.TF {tupDesc = tupDescOld, tup, field}) => 
                 let
                   val tupDesc = buildTupleDescriptor (se, id)
                   val () = checkTupleDescriptor (se, id, tupDesc, tupDescOld)
                   val tf = M.TF {tupDesc = tupDesc, tup = tup, field = field}
                 in M.RhsTupleSub tf
                 end
               | M.RhsTupleSet {tupField, ofVal} => 
                 let
                   val M.TF {tupDesc = tupDescOld, tup, field} = tupField
                   val tupDesc = buildTupleDescriptor (se, id)
                   val () = checkTupleDescriptor (se, id, tupDesc, tupDescOld)
                   val tupField = M.TF {tupDesc = tupDesc, tup = tup, field = field}
                 in M.RhsTupleSet {tupField = tupField, ofVal = ofVal}
                 end
               | M.RhsTupleInited {mdDesc = mdDescOld, tup} => 
                 let
                   val mdDesc = buildMetaData (se, id)
                   val () = checkMetaData (se, id, mdDesc, mdDescOld)
                 in M.RhsTupleInited {mdDesc = mdDesc, tup = tup}
                 end
               | M.RhsIdxGet _ => rhs
               | M.RhsCont l => rhs
               | M.RhsObjectGetKind _ => rhs
               | M.RhsThunkMk rOld => 
                 let
                   val r = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, rOld)
                 in M.RhsThunkMk r
                 end
               | M.RhsThunkInit {typ = typOld, thunk, fx, code, fvs} => 
                 let
                   val (fksOld, opers) = Vector.unzip fvs
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = fksOld})
                   val fvs = Vector.zip (fvs, opers)
                 in M.RhsThunkInit {typ = typ, thunk = thunk, fx = fx, code = code, fvs = fvs}
                 end
               | M.RhsThunkGetFv {typ = typOld, fvs = fvsOld, thunk, idx} => 
                 let
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = fvsOld})
                 in M.RhsThunkGetFv {typ = typ, fvs = fvs, thunk = thunk, idx = idx}
                 end
               | M.RhsThunkValue {typ = typOld, thunk, ofVal} => 
                 let
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = Vector.new0 ()})
                 in M.RhsThunkValue {typ = typ, thunk = thunk, ofVal = ofVal}
                 end
               | M.RhsThunkGetValue {typ = typOld, thunk} => 
                 let
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = Vector.new0 ()})
                 in M.RhsThunkGetValue {typ = typ, thunk = thunk}
                 end
               | M.RhsThunkSpawn {typ = typOld, thunk, fx} => 
                 let
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = Vector.new0 ()})
                 in M.RhsThunkSpawn {typ = typ, thunk = thunk, fx = fx}
                 end
               | M.RhsClosureMk {fvs = fvsOld} => 
                 let
                   val fvs = buildClosureDescriptor (se, id)
                   val () = checkClosureDescriptor (se, id, fvs, fvsOld)
                 in M.RhsClosureMk {fvs = fvs}
                 end
               | M.RhsClosureInit {cls, code, fvs} => 
                 let
                   val (fvsOld, opers) = Vector.unzip fvs
                   val fvs = buildClosureDescriptor (se, id)
                   val () = checkClosureDescriptor (se, id, fvs, fvsOld)
                   val fvs = Vector.zip (fvs, opers)
                 in M.RhsClosureInit {cls = cls, code = code, fvs = fvs}
                 end
               | M.RhsClosureGetFv {fvs = fvsOld, cls, idx} => 
                 let
                   val fvs = buildClosureDescriptor (se, id)
                   val () = checkClosureDescriptor (se, id, fvs, fvsOld)
                 in M.RhsClosureGetFv {fvs = fvs, cls = cls, idx = idx}
                 end
               | M.RhsPSetNew _ => rhs
               | M.RhsPSetGet _ => rhs
               | M.RhsPSetCond _ => rhs
               | M.RhsPSetQuery _ => rhs
               | M.RhsEnum {tag, typ} => 
                 let
                   val (typ2, _) = buildSumDescriptor (se, id)
                   val () = checkSumDescriptor (se, id, Vector.new1 typ2, Vector.new1 typ)
                 in M.RhsEnum {tag = tag, typ = typ2}
                 end
               | M.RhsSum {tag, typs = typOlds, ofVals} => 
                 let
                   val (_, typs) = buildSumDescriptor (se, id)
                   val () = checkSumDescriptor (se, id, typs, typOlds)
                 in M.RhsSum {tag = tag, typs = typs, ofVals = ofVals}
                 end
               | M.RhsSumProj {typs = typOlds, sum, tag, idx} => 
                 let
                   val (_, typs) = buildSumDescriptor (se, id)
                   val () = checkSumDescriptor (se, id, typs, typOlds)
                 in M.RhsSumProj {tag = tag, typs = typs, sum = sum, idx = idx}
                 end
               | M.RhsSumGetTag {typ = typOld, sum} => 
                 let
                   val (typ, _) = buildSumDescriptor (se, id)
                   val () = checkSumTagDescriptor (se, id, typ, typOld)
                 in M.RhsSumGetTag {typ = typ, sum = sum}
                 end)
      in M.I {dests = dests, n = n, rhs = rhs}
      end

  val transfer = 
   fn (se, l, tf) =>
      let
        val id = MU.Id.T l
        val tf = 
            (case tf
              of M.TInterProc {callee, ret, fx} => 
                 (case callee 
                   of M.IpEval {typ = typOld, eval} => 
                      let
                        val r as {typ, fvs} = buildThunkDescriptor (se, id)
                        val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = Vector.new0 ()})
                      in M.TInterProc {callee = M.IpEval {typ = typ, eval = eval}, ret = ret, fx = fx}
                      end
                    | _ => tf)
               | M.TCase {select = M.SeSum fk, on, cases, default} => 
                 let
                   val (typ, _) = buildSumDescriptor (se, id)
                   val () = checkSumTagDescriptor (se, id, typ, fk)
                 in M.TCase {select = M.SeSum typ, on = on, cases = cases, default = default}
                 end
               | _ => tf)
      in tf
      end

  val block = 
   fn (se, (l, M.B {parameters, instructions, transfer = tfer})) =>
      let
        val instructions = Vector.map (instructions, fn i => instruction (se, i))
        val transfer = transfer (se, l, tfer)
      in M.B {parameters = parameters, instructions = instructions, transfer = transfer}
      end

  val codeBody = 
   fn (se, M.CB {entry, blocks}) => M.CB {entry = entry, blocks = LD.map (blocks, fn lb => block (se, lb))}

  val global = 
   fn (se, (v, g)) =>
      let
        val id = MU.Id.G v 
        val g = 
            (case g
              of M.GCode (M.F {fx, escapes, recursive, cc, args, rtyps = rtypsOld, body}) =>
                 let
                   val rtyps = buildCodeReturnTypes (se, id)
                   val () = checkCodeReturnTypes (se, id, rtyps, rtypsOld)
                   val body = codeBody (se, body)
                   val f = M.F {fx = fx, escapes = escapes, recursive = recursive, 
                                cc = cc, args = args, rtyps = rtyps, body = body}
                 in M.GCode f
                 end
               | M.GErrorVal _ => g
               | M.GIdx _ => g
               | M.GTuple {mdDesc = mdDescOld, inits} => 
                 let
                   val mdDesc = buildMetaData (se, id)
                   val () = checkMetaData (se, id, mdDesc, mdDescOld)
                 in M.GTuple {mdDesc = mdDesc, inits = inits}
                 end
               | M.GRat _ => g
               | M.GInteger _ => g
               | M.GCString _ => g
               | M.GThunkValue {typ = typOld, ofVal} => 
                 let
                   val r as {typ, fvs} = buildThunkDescriptor (se, id)
                   val () = checkThunkDescriptor (se, id, r, {typ = typOld, fvs = Vector.new0 ()})
                 in M.GThunkValue {typ = typ, ofVal = ofVal}
                 end
               | M.GSimple _ => g
               | M.GClosure {code, fvs} => 
                 let
                   val (fvsOld, opers) = Vector.unzip fvs
                   val fvs = buildClosureDescriptor (se, id)
                   val () = checkClosureDescriptor (se, id, fvs, fvsOld)
                   val fvs = Vector.zip (fvs, opers)
                 in M.GClosure {code = code, fvs = fvs}
                 end
               | M.GSum {tag, typs = typOlds, ofVals} => 
                 let
                   val (typ, typs) = buildSumDescriptor (se, id)
                   val () = checkSumDescriptor (se, id, typs, typOlds)
                 in M.GSum {tag = tag, typs = typs, ofVals = ofVals}
                 end
               | M.GPSet _ => g)
      in g
      end

  val globals = 
      fn (se, gs) => VD.map (gs, fn vg => global (se, vg))

  val checkTypes = 
   fn (config, st, v, typ) => 
      let
        val oldTyp = MU.SymbolTable.variableTyp (st, v)
        val si = Identifier.SymbolInfo.SiTable st
        val () = 
            if MilType.Type.subtype (config, typ, oldTyp) then
              ()
            else
              let
                val l = 
                    L.seq[L.str "Changed type for variable ",
                          MilLayout.layoutVariable (config, si, v)]
                val l = 
                    if Config.debugLevel (config, passname) >= 1 then
                      L.mayAlign [l,
                                  L.str " from: ",
                                  LU.indent (MilLayout.layoutTyp (config, si, oldTyp)),
                                  L.str " to: ",
                                  LU.indent (MilLayout.layoutTyp (config, si, typ))]
                    else l
                val () = LU.printLayout l
              in ()
              end
      in ()
      end

  val updateSymbolTable = 
   fn (config, summary, flat, st) =>
      let
        (* Keep old info for any variables not in the summary *)
        val stm = IM.fromExistingAll st
        val vars = MRS.listVariables summary
        val help = 
         fn v => 
            let
              val kind = MU.SymbolTable.variableKind (st, v)
              val typ = 
                  (case buildCodeTyp (summary, flat, v)
                    of SOME t => t
                     | NONE => typForVariable (summary, flat, v))
              val () = MU.SymbolTableManager.variableSetInfo (stm, v, M.VI {typ = typ, kind = kind})
              val () = 
                  if showTypChanges config then
                    checkTypes (config, st, v, typ)
                  else
                    ()  
            in ()
            end
        val () = List.foreach (vars, help)
        val st = IM.finish stm
      in st
      end
      
  val program = 
   fn (pd, summary, flat, p) => 
      let
        val () = MRS.resetTyps summary
        val config = PassData.getConfig pd
        val M.P {includes, externs, globals = gs, symbolTable = st, entry} = p
        val st = updateSymbolTable (config, summary, flat, st)
        val env = E {config = config, flat = flat, st = st}
        val state = S {summary = summary}
        val gs = globals ((state, env), gs)
        val p = M.P {includes = includes, externs = externs, globals = gs, symbolTable = st, entry = entry}
      in p
      end

end (* structure MilRepReconstruct *)

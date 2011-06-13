(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_EXTENDED_LAYOUT =
sig

  val controls : Config.Control.control list
  val debugs   : Config.Debug.debug list

  type 'a layout = Config.t * Mil.symbolInfo * 'a -> Layout.t

  (* Layout according to command line control options *)
  val layoutCodeBody   : Mil.codeBody layout
  val layoutCode       : Mil.code layout
  val layoutGlobal     : (Mil.variable * Mil.global) layout
  val layout           : Config.t * Mil.t -> Layout.t

  (* Layout with specific options *)
  structure General : 
  sig
    datatype codeStyle = CoDominator | CoLoops | CoNone
    datatype control = C of {style : codeStyle}
                            
    type 'a layout = Config.t * Mil.symbolInfo * control * 'a -> Layout.t
                                                                 
    val layoutCodeBody   : Mil.codeBody layout
    val layoutCode       : Mil.code layout
    val layoutGlobal     : (Mil.variable * Mil.global) layout
    val layout           : Config.t * control * Mil.t -> Layout.t
                                               
  end (* structure General *)
end (* signature MIL_EXTENDED_LAYOUT *);

structure MilExtendedLayout :> MIL_EXTENDED_LAYOUT =
struct

  val modulename = "MilExtendedLayout"

  structure ML = MilLayout
  structure LD = Mil.LD
  structure LS = Mil.LS
  structure VD = Mil.VD
  structure L = Layout
  structure LU = LayoutUtils
  structure MU = MilUtils

  val fail = fn (f, msg) => Fail.fail (modulename, f, msg)

  structure General = 
  struct

    datatype codeStyle = CoDominator | CoLoops | CoNone
    datatype control = C of {style : codeStyle}
                            
    type 'a layout = Config.t * Mil.symbolInfo * control * 'a -> Layout.t

    val layoutByDominator : Mil.symbolInfo -> Config.t * Mil.codeBody -> ML.Helpers.items = 
     fn si => 
     fn (config, cb) => 
        let
          val rec layoutTree = 
           fn (Tree.T (lb, children)) => 
               let 
                 val lcs = layoutForest children
                 val ln = ML.Helpers.IBlock lb
                 val l = 
                     if Vector.length children <= 1 then
                       ln :: lcs
                     else
                       [ln, ML.Helpers.IIndent (ML.Helpers.IItems lcs)]
               in ML.Helpers.IItems l
               end
          and rec layoutForest =
           fn ts => Vector.toListMap (ts, layoutTree)

          val cfg = MilCfg.build (config, si, cb)
          val t   = MilCfg.getLabelBlockDomTree cfg
          val item = layoutTree t
          val extras = 
              let
                val Mil.CB {blocks, ...} = cb
                val blocks = Tree.foldPre (t, blocks, fn ((l, b), blocks) => LD.remove (blocks, l))
                val blocks = LD.toList blocks
                val items = List.map (blocks, ML.Helpers.IBlock)
              in items
              end
          val item = ML.Helpers.IItems (item::extras)
        in item
        end

    structure TopoSort = TopoSortF(struct
                                     structure Dict = LD
                                     structure Set  = LS
                                   end)

    val layoutByLoops : Mil.symbolInfo -> Config.t * Mil.codeBody -> ML.Helpers.items = 
     fn si =>
     fn (config, cb) => 
        let
          datatype loopEntry = LeLoop of (Mil.label * loopEntry) list | LeBlock of Mil.block
          val cfg = MilCfg.build (config, si, cb)
          val t   = MilCfg.getLabelBlockDomTree cfg
          val li = MilLoop.build (config, si, cfg, t)
          val loops = MilLoop.getLoops li
          val blocks = MilLoop.getBlocksNotInLoops li
          val entries = 
              let
                val doBlocks = 
                 fn blocks => List.map (LD.toList blocks, fn (l, b) => (l, LeBlock b))
                val rec doTree = 
                 fn (Tree.T (MilLoop.L {header, blocks}, children)) => 
                    (header, LeLoop (doBlocks blocks @ doForest children))
                and rec doForest =
                 fn ts => Vector.toListMap (ts, doTree)
              in doBlocks blocks @ doForest loops
              end
          val leq = 
              let
                val rpo = MU.CodeBody.listRPO (config, cb)
                val add = 
                 fn ((l, b), (d, i)) => (LD.insert (d, l, i), i+1)
                val (map, _) = List.fold (rpo, (LD.empty, 0), add)
                val leq = fn ((l1, _), (l2, _)) => 
                            (case (LD.lookup (map, l1), LD.lookup (map, l2))
                              of (SOME i1, SOME i2) => i1 <= i2
                               | _ => fail ("layoutByLoops", "Bad labels"))
              in leq
              end
          val sort = 
           fn entries => QuickSort.sortList (entries, leq)
          val rec doEntry =
           fn (l, entry) => 
              (case entry 
                of LeBlock b => ML.Helpers.IBlock (l, b)
                 | LeLoop ls => 
                   ML.Helpers.IItems
                   [ ML.Helpers.ILayout (L.seq [L.str "Loop ", Identifier.layoutLabel l]),
                     ML.Helpers.IIndent (doEntries ls)])
          and rec doEntries = 
           fn entries => ML.Helpers.IItems (List.map (sort entries, doEntry))
        in doEntries entries
        end

    val mkHelpers = 
     fn (control, si, block) => 
        let
          val C {style} = control
          val cb = 
              (case style
                of CoDominator => SOME (layoutByDominator si)
                 | CoLoops     => SOME (layoutByLoops si)
                 | CoNone      => NONE)
          val helpers = 
              {varBind = NONE,
               block   = block,
               edge    = NONE,
               cb      = cb}
        in helpers
        end

    val layoutBlockPreds = 
     fn cfg =>
     fn l => 
        let
          val n = MilCfg.labelGetNode (cfg, l)
          val preds = MilCfg.pred (cfg, n)
          val preds = List.keepAllMap (preds, fn n => MilCfg.nodeGetLabel (cfg, n))
          val preds = List.map (preds, Identifier.layoutLabel)
          val preds = L.seq (List.separate (preds, L.str ", "))
        in preds
        end

    val mkHelpersFromCodeBody = 
     fn (config, si, control, cb) => 
        let
          val cfg = MilCfg.build (config, si, cb)
          val block = layoutBlockPreds cfg
        in mkHelpers (control, si, SOME block)
        end

    val mkHelpersFromCode = 
     fn (config, si, control, Mil.F {body, ...}) => mkHelpersFromCodeBody (config, si, control, body)

    val mkHelpersFromGlobal =
     fn (config, si, control, (v, g)) => 
        (case g
          of Mil.GCode code => mkHelpersFromCode (config, si, control, code)
           | _              => mkHelpers (control, si, NONE))


    val layoutCodeBody   : Mil.codeBody layout = 
     fn (config, si, control, a) => 
        let
          val helpers = mkHelpersFromCodeBody (config, si, control, a)
        in ML.General.layoutCodeBody (config, si, helpers, a)
        end

    val layoutCode   : Mil.code layout = 
     fn (config, si, control, a) => 
        let
          val helpers = mkHelpersFromCode (config, si, control, a)
        in ML.General.layoutCode (config, si, helpers, a)
        end

    val layoutGlobal     : (Mil.variable * Mil.global) layout = 
     fn (config, si, control, a) => 
        let
          val helpers = mkHelpersFromGlobal (config, si, control, a)
        in ML.General.layoutGlobal (config, si, helpers, a)
        end

    val layout           : Config.t * control * Mil.t -> Layout.t       = 
     fn (config, control, a) => 
        let
          val Mil.P {globals, symbolTable, ...} = a
          val si = Identifier.SymbolInfo.SiTable symbolTable
          val doCodeBody =
           fn (dict, cb as Mil.CB {entry, blocks}) =>
              let
                val cfg = MilCfg.build (config, si, cb)
                val doBlock = fn (l, _, dict) => LD.insert (dict, l, cfg)
              in LD.fold (blocks, dict, doBlock)
              end
          val doCode = 
           fn (dict, Mil.F {body, ...}) => doCodeBody (dict, body)
          val doGlobal = 
           fn (_, global, dict) => 
              (case global
                of Mil.GCode code => doCode (dict, code)
                 | _              => dict)
          val dict = VD.fold (globals, LD.empty, doGlobal)
          val block = 
           fn l => 
              (case LD.lookup (dict, l)
                of SOME cfg => layoutBlockPreds cfg l
                 | NONE     => L.str "UNKNOWN PREDS")
          val helpers = mkHelpers (control, si, SOME block)
        in ML.General.layout (config, helpers, a)
        end

  end (* structure General *)

  val describe =
   fn () =>
      L.align [L.str (modulename ^ " control string consists of:"),
               LU.indent (L.align [L.str "D => dominator layout",
                                   L.str "L => loop nest layout",
                                   L.str "N => use MilLayout order"]),
               L.str "default is N",
               L.str "(see also MilLayout controls)"]

  val parse =
      fn str =>
         let
           val style = ref NONE
           val setStyle = 
            fn st =>
               (case !style 
                 of NONE => let val () = style := SOME st in true end
                  | _    => false)
           val getStyle = 
            fn () => 
               (case !style
                 of NONE    => General.CoNone
                  | SOME st => st)
           val doOne =
               fn c =>
                  (case c
                    of #"D" => setStyle General.CoDominator
                     | #"L" => setStyle General.CoLoops
                     | #"N" => setStyle General.CoNone
                     | _    => false)
         in
           if List.forall (String.explode str, doOne) then
             SOME (General.C {style = getStyle ()})
           else
             NONE
         end

  val dft = fn _ => General.C {style = General.CoNone}

  val (control, controlGet) = Config.Control.mk (modulename, describe, parse, dft)

  type 'a layout = Config.t * Mil.symbolInfo * 'a -> Layout.t
    
  val wrap : 'a General.layout -> 'a layout =
   fn f => fn (config, si, a) => f (config, si, controlGet config, a)

  val layoutCodeBody   : Mil.codeBody layout                = wrap General.layoutCodeBody
  val layoutCode       : Mil.code layout                    = wrap General.layoutCode
  val layoutGlobal     : (Mil.variable * Mil.global) layout = wrap General.layoutGlobal
  val layout           : Config.t * Mil.t -> Layout.t       = 
   fn (config, p) => General.layout (config, controlGet config, p)

   val controls = [control]
   val debugs = []

end (* structure MilExtendedLayout *);

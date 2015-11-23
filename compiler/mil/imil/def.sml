(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_DEF = 
sig
  include IMIL_PUBLIC_TYPES

  val add         : t * variable * def -> unit 
  val get         : t * variable -> def 
  val delete      : t * variable -> unit
  val toItem      : t * def -> item option
  val defsToItems : t * def Vector.t -> item Vector.t

  val toIInstr      : def -> iInstr option
  val toIGlobal     : def -> iGlobal option

  val toInstruction : def -> Mil.instruction option
  val toRhs         : def -> Mil.rhs option
  val toTransfer    : def -> Mil.transfer option
  val toLabel       : def -> (Mil.label * Mil.variable vector) option
  val toGlobal      : def -> (Mil.variable * Mil.global) option
  val toMilDef      : def -> MilUtils.Def.t option
  val layout        : t * def -> Layout.t

end
structure IMilDef : 
sig 
  include IMIL_DEF
end
  = 
struct
  open IMilPublicTypes

  structure IMC = IMilCommon
  structure IMT = IMilTypes
  structure IVD = IMilTypes.IVD
  structure MU = MilUtils
  structure Chat = IMC.Chat

  val fail = 
   fn (f, s) => Fail.fail ("def.sml", f, s)

  val get = 
   fn (p, v) =>
      let
        val defs = IMT.tGetDefs p
        val d = 
            case IVD.lookup (defs, v)
             of SOME d => d
              | NONE => 
                let
                  val s = (Layout.toString o IMilLayout.var) (p, v)
                  val () = Chat.warn1 (p, "Def.getDef: " ^  "Unknown variable: "^s)
                in IMT.DefUnk
                end
      in d
      end

  val add =
   fn (p, v, d) =>
      let
        val defs = IMT.tGetDefs p
        val () = IVD.insert (defs, v, d)
      in ()
      end

  val delete =
   fn (p, v) =>
      let
        val defs = IMT.tGetDefs p
        val () = IVD.insert (defs, v, IMT.DefUnk)
      in ()
      end

  val toIInstr      = IMT.defToIInstr
  val toIGlobal     = IMT.defToIGlobal
  val toIFunc       = IMT.defToIFunc

  val toInstruction = Utils.Option.compose (IMT.iInstrToInstruction, toIInstr)
  val toRhs         = Utils.Option.compose (IMT.iInstrToRhs, toIInstr)
  val toTransfer    = Utils.Option.compose (IMT.iInstrToTransfer, toIInstr)
  val toLabel       = Utils.Option.compose (IMT.iInstrToLabel, toIInstr)
  val toGlobal      = Utils.Option.compose (IMT.iGlobalToGlobal, toIGlobal)

  val toItem = 
   fn (p, def) =>
      (case def
        of IMT.DefUnk => NONE
         | IMT.DefExtern => NONE
         | IMT.DefInstr i => SOME (IMT.ItemInstr i)
         | IMT.DefGlobal g => SOME (IMT.ItemGlobal g)
         | IMT.DefFunc c => SOME (IMT.ItemFunc c)
         | IMT.DefParameter c => SOME (IMT.ItemFunc c))

  val defsToItems = 
      fn (p, defs) => Vector.keepAllMap (defs, fn d => toItem (p, d))

  val toMilDef = 
   fn def => 
      (case def
        of IMT.DefGlobal g =>
           (case IMT.iGlobalGetMil g
             of IMT.GGlobal (v, mg) => SOME (MU.Def.DefGlobal mg)
              | _ => NONE)
          | IMT.DefInstr i => 
            (case IMT.iInstrGetMil i
              of IMT.MInstr (Mil.I {dests, n, rhs})  => SOME (MU.Def.DefRhs rhs)
               | _ => NONE)
          | IMT.DefFunc c => NONE
          | IMT.DefParameter c => NONE
          | IMT.DefExtern => NONE
          | IMT.DefUnk => NONE)

  val layout = IMilLayout.def
end

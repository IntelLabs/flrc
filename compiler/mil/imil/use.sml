(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_USE = 
sig
  include IMIL_PUBLIC_TYPES

  val addUse      : t * variable * use -> use DList.cursor
  val deleteUse   : t * use DList.cursor -> unit
  val replaceUses : t * variable * Mil.operand -> unit
  val getUses     : t * variable -> use Vector.t
  val markUsed    : t * variable -> unit
  val splitUses'  : t * variable * use Vector.t -> {inits : use Vector.t, others : use Vector.t}
  val splitUses   : t * variable  -> {inits : use Vector.t, others : use Vector.t}
  val layout      : t * use -> Layout.t

  val toItem        : use -> item option
  val toIInstr      : use -> iInstr option
  val toIGlobal     : use -> iGlobal option
  val toInstruction : use -> Mil.instruction option
  val toRhs         : use -> Mil.rhs option
  val toTransfer    : use -> Mil.transfer option
  val toLabel       : use -> (Mil.label * Mil.variable vector) option
  val toGlobal      : use -> (Mil.variable * Mil.global) option


end
structure IMilUse : 
sig
  include IMIL_USE
  val emitGlobalH : (t * (Mil.variable * Mil.global) -> iGlobal) BackPatch.t
  val replaceMilGH : (t * iGlobal * mGlobal -> unit) BackPatch.t
  val replaceMilIH : (t * iInstr * mInstr -> unit) BackPatch.t
end = 
struct
  open IMilPublicTypes

  structure M = Mil
  structure MU = MilUtils
  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure IMC = IMilCommon
  structure Chat = IMC.Chat
  structure BP = BackPatch

  val fail = 
   fn (f, s) => Fail.fail ("use.sml",
                           f,
                           s)

  val getUsesDList = 
   fn (p, v) =>
      let
        val uses = IMT.tGetUses p
        val l = 
            case IVD.lookup (uses, v)
             of SOME l => l
              | NONE => 
                let
                  val l = DList.empty ()
                  val () = IVD.insert (uses, v, l)
                in l
                end
      in l
      end

  val addUse = 
   fn (p, v, u) =>
      let
        val l = getUsesDList (p, v)
        val c = DList.insert (l, u)
      in c
      end

  val deleteUse = 
   fn (p, u) => DList.remove u


  val emitGlobalH : (IMT.t * (Mil.variable * Mil.global), iGlobal) BP.func = BP.new ()
  val emitGlobal = BP.apply emitGlobalH

  val replaceMilGH : (IMT.t * iGlobal * mGlobal, unit) BP.func = BP.new ()
  val replaceMilG = BP.apply replaceMilGH

  val replaceMilIH : (IMT.t * iInstr * mInstr, unit) BP.func = BP.new ()
  val replaceMilI = BP.apply replaceMilIH

  local
    structure MRC = MilRewriterClient
                   
    datatype state = S of {v : M.variable, oper : M.operand}
    datatype env = E of {t : IMT.t}

    val rwLabel = fn (s, t, l) => MRC.Stop

    val rwOperand = 
     fn (s as S {v, oper}, t, oper') =>
        let
          val res = 
              case oper'
               of M.SVariable v' => if v = v' then 
                                      MRC.StopWith (t, oper)
                                    else 
                                      MRC.Stop
                | _ => MRC.Continue
        in res
        end

    val rwVariable = 
     fn (s as S {v, oper}, env as E {t}, v') =>
        let

          val strip = 
           fn oper =>
              case oper
               of M.SVariable v => v
                | M.SConstant c => 
                  let
                    val stm = IMT.tGetStm t
                    val typ = MU.SymbolTableManager.variableTyp (stm, v)
                    val gv = MU.SymbolTableManager.variableRelated (stm, v, "", typ, M.VkGlobal)
                    val _ = emitGlobal (t, (gv, M.GErrorVal typ))
                  in gv
                  end
          val res = 
              if v = v' then 
                MRC.StopWith (env, strip oper)
              else 
                MRC.Stop
        in res
        end

    val rwInst = fn (s, _, i)     => MRC.Continue
    val rwTransfer = fn (s, _, t) => MRC.Continue
    val rwBlock = fn (s, _, b)    => MRC.Continue
    val rwGlobal = fn (s, _, g)   => MRC.Continue

    val bind = fn (s, t, v) => (t, NONE)
    val bindLabel = fn (s, t, l) => (t, NONE)

    val config = fn (E {t}) => IMT.tGetConfig t
    structure Cfg = MilCfg

    structure R = MilRewriterF (struct
                                  type env = env
                                  type state = state
                                  val config = config
                                  val label    = rwLabel
                                  val variable = rwVariable
                                  val operand  = rwOperand
                                  val instruction = rwInst
                                  val transfer = rwTransfer
                                  val block = rwBlock
                                  val global = rwGlobal
                                  val bind = bind
                                  val bindLabel = bindLabel
                                  val indent = 2
                                  val cfgEnum = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                                end)
                  
  in
  val replaceVarInMilInstr = 
   fn(t, v, oper, mil) =>
      let
        val state = S {v = v, oper = oper}
        val env = E {t = t}
        val res = 
            case mil
             of IMT.MInstr i => 
                let
                  val (_, i) = R.instruction (state, env, i)
                in IMT.MInstr i
                end
              | IMT.MTransfer t => 
                let
                  val (_, t) = R.transfer (state, env, (NONE, t))
                in IMT.MTransfer t
                end
              | IMT.MLabel (l, vs) => 
                let
                  val () = Chat.warn0 (t, 
                                       "Labels don't use variables")
                in IMT.MLabel (l, vs)
                end
              | IMT.MDead => 
                let
                  val () = Chat.warn0
                             (t, 
                              "Dead instructions shouldn't have uses")
                in IMT.MDead
                end
                
      in res
      end



  val replaceVarInMilGlobal = 
   fn (t, v, oper, mil) =>
      let
        val res = 
            case mil
             of IMT.GGlobal g => 
                let
                  val state = S {v = v, oper = oper}
                  val env = E {t = t}
                  val g = R.global (state, env, g)
                in IMT.GGlobal g
                end
              | IMT.GDead => 
                let
                  val () = Chat.warn0 (t, 
                                       "Dead globals shouldn't have uses")
                in IMT.GDead
                end
      in res
      end
  end

  val replaceUseI = 
   fn (p, i, v, oper) =>
      let
        val mil = IMT.iInstrGetMil i

        val mil  = replaceVarInMilInstr (p, v, oper, mil)
        val () = replaceMilI (p, i, mil)
      in ()
      end

  val replaceUseG = 
   fn (p, g, v, oper) =>
      let
        val mil = IMT.iGlobalGetMil g
        val mil  = replaceVarInMilGlobal (p, v, oper, mil)
        val () = replaceMilG (p, g, mil)
      in ()
      end

  val replaceUse = 
      fn (p, u, v, oper) =>
         case DList.getVal u
          of IMT.Used => ()
           | IMT.UseInstr i  => (deleteUse (p, u); replaceUseI (p, i, v, oper))
           | IMT.UseGlobal g => (deleteUse (p, u); replaceUseG (p, g, v, oper))
                         
  val getUses = 
   fn (p, v) =>
      DList.toVectorUnordered (getUsesDList (p, v))

  val replaceUses = 
   fn (p, v, oper) =>
      let
        val uses = DList.all (getUsesDList (p, v))
        val () = List.foreach (uses, fn u => 
                                        replaceUse (p, u, v, oper))
      in ()
      end

  val markUsed = 
   fn (p, v) =>
      let
        val _ = addUse (p, v, IMT.Used)
      in ()
      end

  val splitUses' = 
   fn (p, v, uses) => 
      let
        fun split u = 
            (case u
              of IMT.UseInstr i => 
                 (case IMT.iInstrGetMil i
                   of IMT.MInstr mi => MU.Instruction.isInitOf (mi, v)
                    | _ => false)
               | _ => false)
        val {yes, no} = 
             Vector.partition (uses, split)
       in {inits = yes, others = no}
       end

  val splitUses = 
   fn (p, v) => splitUses' (p, v, getUses (p, v))

  val toItem = 
   fn u => 
      (case u
        of IMT.Used => NONE
         | IMT.UseInstr i => SOME (IMT.ItemInstr i)
         | IMT.UseGlobal g => SOME (IMT.ItemGlobal g))
  val toIInstr      = IMT.useToIInstr
  val toIGlobal     = IMT.useToIGlobal
  val toInstruction = Utils.Option.compose (IMT.iInstrToInstruction, toIInstr)
  val toRhs         = Utils.Option.compose (IMT.iInstrToRhs, toIInstr)
  val toTransfer    = Utils.Option.compose (IMT.iInstrToTransfer, toIInstr)
  val toLabel       = Utils.Option.compose (IMT.iInstrToLabel, toIInstr)
  val toGlobal      = Utils.Option.compose (IMT.iGlobalToGlobal, toIGlobal)

  val layout = IMilLayout.use
end

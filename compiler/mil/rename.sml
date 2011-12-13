(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature RENAMER = 
sig
  type t
  val program     : Config.t * t * Mil.t -> Mil.t
  val global      : Config.t * t * Mil.variable * Mil.global -> Mil.variable * Mil.global
  val code        : Config.t * t * Mil.code -> Mil.code
  val codeBody    : Config.t * t * Mil.codeBody -> Mil.codeBody
  val block       : Config.t * t * Mil.label * Mil.block -> (Mil.label * Mil.block)
  val instruction : Config.t * t * Mil.instruction -> Mil.instruction
  val transfer    : Config.t * t * Mil.transfer -> Mil.transfer
end

signature MIL_RENAME =
sig
  structure Var      : RENAMER where type t = Rename.t 
  structure Label    : RENAMER where type t = Mil.label Identifier.LabelDict.t
  structure VarLabel : RENAMER where type t = Rename.t * Mil.label Identifier.LabelDict.t
end;

structure MilRename :> MIL_RENAME =
struct

  structure LD = Identifier.LabelDict
  structure M = Mil
  structure MRC = MilRewriterClient

  structure VarLabel = 
  struct

    type t = Rename.t * Mil.label LD.t

    datatype state = S of {}
    datatype env = E of {config : Config.t, r : Rename.t, lr : Mil.label LD.t}

    fun mkStateEnv (c, (r, lr)) = (S {}, E {config = c, r = r, lr = lr})

    fun replaceLabel (lr, l) = Utils.Option.get (LD.lookup (lr, l), l)

    fun rwVariable (s, e as E {r, ...}, v) = 
        MRC.StopWith (e, Rename.use (r, v))

    fun rwLabel (s, e as E {lr, ...}, l) = 
        MRC.StopWith (e, replaceLabel (lr, l))

    fun rwOperand (s, e, oper) = MRC.Continue
    fun rwInst (s, e, i) = MRC.Continue
    fun rwTransfer (s, e, t) = MRC.Continue
    fun rwBlock (s, e, b) = MRC.Continue
    fun rwGlobal (s, e, g) = MRC.Continue
                          
    fun bind (s,e as E {r, ...}, v) = (e, Rename.use' (r, v))

    fun bindLabel (s, e as E {lr, ...}, l) = (e, LD.lookup (lr, l))

    fun cfgEnum (_, _, M.CB {entry, blocks}) =
        let
          val blks = LD.toList blocks
          val blks = Vector.fromList blks
          fun doOne x = Tree.T (x, Vector.new0 ())
          val blks = Vector.map (blks, doOne)
        in blks
        end

    structure MR = 
    MilRewriterF (struct
                    type state = state
                    type env = env
                    val config = fn (E {config, ...}) => config
                    val label = rwLabel
                    val variable = rwVariable
                    val operand = rwOperand
                    val instruction = rwInst
                    val transfer = rwTransfer
                    val block = rwBlock
                    val global = rwGlobal
                    val bind = bind
                    val bindLabel = bindLabel
                    val indent = 2
                    val cfgEnum = cfgEnum
                  end)

    fun block (c, r, l, b) = 
     let
       val (s, e) = mkStateEnv (c, r)
       val (l, b) = MR.block (s, e, (l, b))
     in (l, b)
     end

    fun global (c, r, v, g) =
        let
          val (s, e) = mkStateEnv (c, r)
          val (v, g) = MR.global (s, e, (v, g))
        in (v, g)
        end

    fun instruction (c, r, i) =
      let
        val (s, e) = mkStateEnv (c, r)
        val (_, i) = MR.instruction (s, e, i)
      in i
      end


    fun transfer (c, r, t) =
        let
          val (s, e) = mkStateEnv (c, r)
          val (_, t) = MR.transfer (s, e, (NONE, t))
        in t
        end

    fun codeBody (c, r, cb) = 
        let
          val (s, e) = mkStateEnv (c, r)
          val cb = MR.codeBody (s, e, cb)
        in cb
        end

    fun code (c, r, cd) = 
        let
          val (s, e) = mkStateEnv (c, r)
          val cd = MR.code (s, e, cd)
        in cd
        end

    fun program (c, r, p) =
        let
          val (s, e) = mkStateEnv (c, r)
          val p = MR.program (s, e, p)
        in p
        end

  end

  structure Var = 
  struct
    type t = Rename.t
    fun lifts (r : t) : VarLabel.t = (r, LD.empty)
    fun global (c, r, v, g) = VarLabel.global (c, lifts r, v, g)
    fun block (c, r, l, b) = VarLabel.block (c, lifts r, l, b)
    fun lift f (c, r, i) = f (c, lifts r, i)
    val instruction = lift VarLabel.instruction
    val transfer = lift VarLabel.transfer
    val codeBody = lift VarLabel.codeBody
    val code = lift VarLabel.code
    val program = lift VarLabel.program
  end

  structure Label = 
  struct
    type t = Mil.label LD.t
    fun lifts d = (Rename.none, d)
    fun global (c, r, v, g) = VarLabel.global (c, lifts r, v, g)
    fun block (c, r, l, b) = VarLabel.block (c, lifts r, l, b)
    fun lift f (c, r, i) = f (c, lifts r, i)
    val instruction = lift VarLabel.instruction
    val transfer = lift VarLabel.transfer
    val codeBody = lift VarLabel.codeBody
    val code = lift VarLabel.code
    val program = lift VarLabel.program
  end

end;

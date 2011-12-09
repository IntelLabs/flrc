(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_COMMON = 
sig
  val passname : string

  structure Chat : CHAT where type env = IMilTypes.t
  structure FV : MIL_FREE_VARS where type t = IMilTypes.t
  structure VLRN : RENAMER where type t = Rename.t * IMilTypes.label Identifier.LabelDict.t 
  val debugPassD : Config.Debug.debug
  val debugPass : Config.t -> bool
  val debugDo : IMilTypes.t * (unit -> unit) -> unit
end

structure IMilCommon :> IMIL_COMMON = 
struct

  val passname = "IMil"
  
  structure IMT = IMilTypes
  structure Chat = ChatF (struct 
                            type env = IMilTypes.t
                            val extract = IMilTypes.tGetConfig
                            val name = passname
                            val indent = 2
                          end)

  structure FV = 
  struct
    open MilFreeVars
    type t = IMT.t
    val lift2 = 
     fn f => fn (t, b) => f (IMT.tGetConfig t, b)
    val lift3 = 
     fn f => fn (t, l, b) => f (IMT.tGetConfig t, l, b)
    val rhs = lift2 rhs
    val instruction = lift2 instruction
    val call = lift2 call
    val eval = lift2 eval
    val transfer = lift2 transfer
    val block = lift3 block
    val blocks = lift2 blocks
    val codeBody = lift2 codeBody
    val global = lift3 global
    val program = lift2 program
  end

  structure VLRN = MilRename.VarLabel


  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the IMil module")

  fun debugDo (p, f) = 
      if Config.debug andalso debugPass (IMilTypes.tGetConfig p)
      then f ()
      else ()


end

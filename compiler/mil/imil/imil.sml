(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(************** Notes on usage ************************************************
 *
 * For notes on the general architecture of the IMil implementation and
 * how to modify it, see the comment at the top of imil-types.sml.
 *
 * Clients of the IMil should only access things exported through the
 * IMil structure directly, rather than through the structures defined
 * in the various imil-xxx.sml files.  The abstraction mechanism will
 * enforce this.  However, the signatures (interfaces) of the
 * sub-structures of the IMil are defined in the imil-xxx.sml files.  So
 * for example, one should look in the file imil-use.sml to find the signature 
 * IMIL_USE which classifies the IMil sub-structure for uses:
 *
 *  structure Use : IMIL_USE
 *
 * Each such signature has the form:
 *
 *  signature IMIL_VAR = 
 *  sig
 *    include IMIL_PUBLIC_TYPES 
 *    ....
 *  end
 *
 * The "include IMIL_PUBLIC_TYPES" line can be ignored - it simply
 * includes some types into the signature for clarity and for coherence
 * purposes.  The rest of the signature declares all of the public
 * exports of the structure.
 *
 *****************************************************************************)

signature IMIL = 
sig
  val controls : Config.Control.control list
  val debugs   : Config.Debug.debug list

  type t
  eqtype iInstr
  eqtype iBlock
  eqtype iFunc
  eqtype iGlobal

  type variable = Mil.variable
  type label = Mil.label

  datatype mGlobal = 
           GGlobal of Mil.variable * Mil.global  
         | GDead

  datatype mInstr = 
           MInstr of Mil.instruction
         | MTransfer of Mil.transfer
         | MLabel of Mil.label * Mil.variable Vector.t
         | MDead

  datatype def = 
           DefUnk
         | DefExtern
         | DefInstr of iInstr
         | DefGlobal of iGlobal
         | DefFunc of iFunc
         | DefParameter of iFunc

  datatype use = 
           Used
         | UseInstr of iInstr
         | UseGlobal of iGlobal

  datatype item = 
           ItemInstr of iInstr
         | ItemGlobal of iGlobal
         | ItemFunc of iFunc


  structure T : IMIL_T
  structure IGlobal : IMIL_GLOBAL
  structure IInstr : IMIL_INSTR
  structure IBlock : IMIL_BLOCK
  structure IFunc : IMIL_FUNC
  structure Var : IMIL_VAR
  structure Use : IMIL_USE
  structure Def : IMIL_DEF
  structure Item : IMIL_ITEM
  structure Enumerate : IMIL_ENUMERATE
  structure WorkSet : IMIL_WORKSET
  structure Layout : IMIL_LAYOUT

  sharing type T.t = IGlobal.t = IInstr.t = IBlock.t = IFunc.t = Var.t 
                   = Use.t = Def.t = Item.t = Enumerate.t = WorkSet.t 
                   = Layout.t = t
  sharing type T.iInstr = IGlobal.iInstr = IInstr.iInstr = IBlock.iInstr = IFunc.iInstr = Var.iInstr 
                        = Use.iInstr = Def.iInstr = Item.iInstr = Enumerate.iInstr = WorkSet.iInstr 
                        = Layout.iInstr = iInstr
  sharing type T.iGlobal = IGlobal.iGlobal = IInstr.iGlobal = IBlock.iGlobal = IFunc.iGlobal = Var.iGlobal 
                         = Use.iGlobal = Def.iGlobal = Item.iGlobal = Enumerate.iGlobal = WorkSet.iGlobal 
                         = Layout.iGlobal = iGlobal
  sharing type T.iBlock = IGlobal.iBlock = IInstr.iBlock = IBlock.iBlock = IFunc.iBlock = Var.iBlock 
                        = Use.iBlock = Def.iBlock = Item.iBlock = Enumerate.iBlock = WorkSet.iBlock 
                        = Layout.iBlock = iBlock
  sharing type T.iFunc = IGlobal.iFunc = IInstr.iFunc = IBlock.iFunc = IFunc.iFunc = Var.iFunc 
                       = Use.iFunc = Def.iFunc = Item.iFunc = Enumerate.iFunc = WorkSet.iFunc 
                       = Layout.iFunc = iFunc
  sharing type T.mInstr = IGlobal.mInstr = IInstr.mInstr = IBlock.mInstr = IFunc.mInstr = Var.mInstr 
                        = Use.mInstr = Def.mInstr = Item.mInstr = Enumerate.mInstr = WorkSet.mInstr 
                        = Layout.mInstr = mInstr
  sharing type T.mGlobal = IGlobal.mGlobal = IInstr.mGlobal = IBlock.mGlobal = IFunc.mGlobal = Var.mGlobal 
                         = Use.mGlobal = Def.mGlobal = Item.mGlobal = Enumerate.mGlobal = WorkSet.mGlobal 
                         = Layout.mGlobal = mGlobal
  sharing type T.use = IGlobal.use = IInstr.use = IBlock.use = IFunc.use = Var.use 
                     = Use.use = Def.use = Item.use = Enumerate.use = WorkSet.use 
                     = Layout.use = use
  sharing type T.def = IGlobal.def = IInstr.def = IBlock.def = IFunc.def = Var.def 
                     = Use.def = Def.def = Item.def = Enumerate.def = WorkSet.def 
                     = Layout.def = def
  sharing type T.item = IGlobal.item = IInstr.item = IBlock.item = IFunc.item = Var.item 
                      = Use.item = Def.item = Item.item = Enumerate.item = WorkSet.item 
                      = Layout.item = item

end

structure IMil :> IMIL = 
struct
  open IMilTypes

  val fail = 
   fn (f, s) => Fail.fail ("imil.sml", f, s) 

  structure M = Mil
  structure IMC = IMilCommon
  structure FV = IMC.FV
  structure Enumerate = IMilEnumerate
  structure IM = Identifier.Manager

  structure Use = IMilUse
  structure Def = IMilDef
  structure Var = IMilVar

  structure IInstr = IMilInstr
  structure IGlobal = IMilGlobal
  structure IBlock = IMilBlock
  structure IFunc = IMilFunc
  structure T = IMilT

  structure Item = IMilItem
  structure WorkSet = IMilWorkSet
  structure Layout = IMilLayout
 
  val debugs = [IMC.debugPassD]
  val controls = []
end

(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Enumeration routines.
 * blocks enumerates live blocks (those that have not been deleted)
 * instructions enumerates all non-deleted instructions 
 * labels enumerates all non-deleted block label instructions 
 * transfers enumerates all non-deleted block transfer instructions 
 * operations enumerates all non-deleted intra-block instructions
 *)

signature IMIL_ENUMERATE = 
sig
  include IMIL_PUBLIC_TYPES

  structure IBlock :
            sig
              val instructions : t * iBlock -> iInstr list
              val labels       : t * iBlock -> iInstr list
              val transfers    : t * iBlock -> iInstr list
              val operations   : t * iBlock -> iInstr list
            end

  structure IFunc :
            sig
              val blocks       : t * iFunc -> iBlock list
              val instructions : t * iFunc -> iInstr list
              val labels       : t * iFunc -> iInstr list
              val transfers    : t * iFunc -> iInstr list
              val operations   : t * iFunc -> iInstr list
            end

  structure T :
            sig
              val funcs        : t -> iFunc list
              val globals      : t -> iGlobal list
              val blocks       : t -> iBlock list
              val instructions : t -> iInstr list
              val labels       : t -> iInstr list
              val transfers    : t -> iInstr list
              val operations   : t -> iInstr list
            end
end

structure IMilEnumerate : IMIL_ENUMERATE = 
struct
  open IMilPublicTypes
  structure IMT = IMilTypes
  structure ILD = IMT.ILD
  structure IVD = IMT.IVD

  structure IInstr = 
  struct
    val dead = 
     fn i => 
        (case IMT.iInstrGetMil i
          of IMT.MDead => true
           | _ => false)
  end

  structure IBlock = 
  struct
    val dead = 
     fn b => IInstr.dead (IMT.iBlockGetLabel b)

    val instructions' : IMT.t * IMT.iBlock -> IMT.iInstr list = 
     fn (p, b) =>
        let
          val label = IMT.iBlockGetLabel b
          val trans = IMT.iBlockGetTrans b
          val code = IMT.iBlockGetCode b
          val code = DList.toListUnordered code
          val code = label::trans::code
        in code
        end

    val labels' : IMT.t * IMT.iBlock -> IMT.iInstr list = 
     fn (p, b) =>
        let
          val label = IMT.iBlockGetLabel b
        in [label]
        end

    val transfers' : IMT.t * IMT.iBlock -> IMT.iInstr list = 
     fn (p, b)=>
        let
          val trans = IMT.iBlockGetTrans b
        in [trans]
        end

    val operations' : IMT.t * IMT.iBlock -> IMT.iInstr list = 
     fn (p, b) =>
        let
          val code = IMT.iBlockGetCode b
          val code = DList.toListUnordered code
        in code
        end

    val instructions_ = 
     fn (instructions) => 
        (fn (p : IMT.t, b : IMT.iBlock) => 
            if dead b then 
              []
            else
              List.keepAll (instructions (p, b), 
                            not o IInstr.dead))

    val instructions = instructions_ instructions'
    val labels       = instructions_ labels'
    val transfers    = instructions_ transfers'
    val operations   = instructions_ operations'

  end

  structure IFunc = 
  struct

    val blocks =
     fn (p : IMT.t, iFunc : IMT.iFunc) => 
        ILD.fold (IMT.iFuncGetIBlocks iFunc, [], 
               fn (l, b, bs) => if IBlock.dead b then 
                                  bs
                                else
                                  b :: bs)

    val instructions_ = 
     fn (instructions) =>
        (fn (p : IMT.t, iFunc : IMT.iFunc) =>
            ILD.fold (IMT.iFuncGetIBlocks iFunc, [], 
                   fn (l, b, is) => (instructions (p, b)) @ is))

    val instructions  = instructions_ IBlock.instructions
    val labels        = instructions_ IBlock.labels
    val transfers     = instructions_ IBlock.transfers
    val operations    = instructions_ IBlock.operations

  end

  structure IGlobal = 
  struct
    val dead = 
     fn g => 
        (case IMT.iGlobalGetMil g
          of IMT.GDead => true
           | _ => false)
  end

  structure T = 
  struct
    val funcs = 
     fn (p : IMT.t) => IVD.fold (IMT.tGetIFuncs p, [], fn (_, c, cs) => c::cs)

    val globals = 
     fn p => 
        IVD.fold (IMT.tGetIGlobals p, [], 
               fn (v, g, gs) => 
                  if IGlobal.dead g then 
                    gs
                  else
                    g :: gs)

    val blocks = 
     fn (p : IMT.t) => 
        IVD.fold (IMT.tGetIFuncs p, [], 
               fn (v, iFunc, bs) => (IFunc.blocks (p, iFunc)) @ bs)

    val instructions_ = 
     fn instructions => 
        (fn (p : IMT.t) => 
            IVD.fold (IMT.tGetIFuncs p, [], 
                   fn (v, iFunc, is) => (instructions (p, iFunc)) @ is))
        
    val instructions  = instructions_ IFunc.instructions
    val labels        = instructions_ IFunc.labels
    val transfers     = instructions_ IFunc.transfers
    val operations    = instructions_ IFunc.operations

  end
end

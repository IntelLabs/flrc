(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_WORKSET = 
sig
  include IMIL_PUBLIC_TYPES

  type ws
  val new : unit -> ws
  val addAll : ws * t -> unit
  val addAllInstrs : ws * t -> unit
  val addUses : ws * (use Vector.t) -> unit
  val addItem  : ws * item -> unit
  val addItems : ws * (item Vector.t) -> unit
  val addInstr : ws * iInstr -> unit
  val addGlobal : ws * iGlobal -> unit
  val addCode   : ws * iFunc -> unit
  val hasWork : ws -> bool
  val chooseWork : ws -> item option
  val clear : ws -> unit
end

structure IMilWorkSet : 
sig 
  include IMIL_WORKSET 
end
  = 
struct
  open IMilPublicTypes

  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure IID = ImpIntDict
  structure Enumerate = IMilEnumerate
  structure Item = IMilItem

  datatype ws = WS of {items : IMT.item IID.t}

  val new =
   fn () => WS {items  = IID.empty ()}

  val addItem =
   fn (WS {items}, i) => 
      let
        val id = Item.itemGetId i
        val () = IID.insert (items, id, i)
      in ()
      end

  val addInstr =
   fn (ws, i)  => addItem (ws, IMT.ItemInstr i)
  val addGlobal =
   fn (ws, g) => addItem (ws, IMT.ItemGlobal g)
  val addCode =
   fn (ws, c)   => addItem (ws, IMT.ItemFunc c)

  val addAllInstrs =
   fn (ws, p) =>
      let
        val addInstr =
         fn i => addInstr (ws, i)
        val instrs = Enumerate.T.instructions p
        val () = List.foreach (instrs, addInstr)
      in ()
      end
      
  val addAll =
   fn (ws, p) => 
      let
        val addGlobal =
         fn (_, g) => addGlobal (ws, g)
        val addInstr =
         fn i => addInstr (ws, i)
        val addCode =
         fn (_, c) => addCode (ws, c)
        val () = 
            IVD.foreach (IMT.tGetIGlobals p,
                         addGlobal)
        val instrs = Enumerate.T.instructions p
        val () = List.foreach (instrs, addInstr)
        val cfgs = IVD.foreach (IMT.tGetIFuncs p,
                                addCode)
      in ()
      end

  val addUse =
   fn (ws, use) => 
      case use
       of IMT.Used => ()
        | IMT.UseInstr i  => addInstr (ws, i)
        | IMT.UseGlobal g => addGlobal (ws, g)



  val addUses =
   fn (ws, uses) => Vector.foreach (uses, fn u => addUse (ws, u))
  val addItems =
   fn (ws, items) => Vector.foreach (items, fn i => addItem (ws, i))

  val hasWork =
   fn (WS {items}) => not (IID.isEmpty items)

  val chooseWork =
   fn (WS {items}) => 
      (case IID.choose items
        of SOME (_, w) => SOME w
         | NONE => NONE)

  val clear = fn (WS {items}) => IID.clear items

end

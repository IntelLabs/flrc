(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


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

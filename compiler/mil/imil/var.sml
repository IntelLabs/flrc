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


signature IMIL_VAR = 
sig
  include IMIL_PUBLIC_TYPES

  val new       : t * string * Mil.typ * Mil.variableKind -> variable
  val clone     : t * variable -> variable
  val related   : t * variable * string * Mil.typ * Mil.variableKind -> variable
  val setInfo   : t * variable * Mil.typ * Mil.variableKind -> unit
  val getInfo   : t * variable  -> Mil.typ * Mil.variableKind
  val kind      : t * variable -> Mil.variableKind
  val typ       : t * variable -> Mil.typ
  val fieldKind : t * variable -> Mil.fieldKind
  val print     : t * variable -> unit
  val layout    : t * variable -> Layout.t

  val labelFresh : t -> label
end 

structure IMilVar : 
sig
  include IMIL_VAR
end
= 
struct
  open IMilPublicTypes

  structure M = Mil
  structure IM = MilUtils.SymbolTableManager
  structure IMT = IMilTypes
  structure Def = IMilDef

  val fail = 
   fn (f, s) => Fail.fail ("var.sml", f, s)

  val new = 
   fn (p, hint, t, g) =>
      let
        val v = IM.variableFresh (IMT.tGetStm p, hint, t, g)
        val () = Def.add (p, v, IMT.DefUnk)
      in v
      end

  val clone = 
   fn (p, v) =>
      let
        val v = IM.variableClone (IMT.tGetStm p, v)
        val () = Def.add (p, v, IMT.DefUnk)
      in v
      end

  val related = 
      fn (p, b, hint, t, k) =>
      let
        val v = IM.variableRelated (IMT.tGetStm p, b, hint, t, k)
        val () = Def.add (p, v, IMT.DefUnk)
      in v
      end

  val setInfo = 
   fn (p, v, t, k) => 
      let
        val v = IM.variableSetInfo (IMT.tGetStm p, v, M.VI {typ = t, kind = k})
      in ()
      end

  val getInfo = 
   fn (p, b) => 
      let 
        val M.VI {typ, kind} = IM.variableInfo (IMT.tGetStm p, b)
      in (typ, kind)
      end

  val kind = 
   fn (p, v) => IM.variableKind (IMT.tGetStm p, v)

  val typ = 
   fn (p, v) => 
      IM.variableTyp (IMT.tGetStm p, v)

  val fieldKind = 
      fn (p, v) => MilUtils.FieldKind.fromTyp (IMT.tGetConfig p, typ (p, v))

  val layout = IMilLayout.var

  val print = LayoutUtils.printLayout o layout

  val labelFresh = 
   fn p =>
      Identifier.Manager.labelFresh (IMT.tGetStm p)


end

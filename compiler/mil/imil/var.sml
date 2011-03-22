(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

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
   fn (f, s) => Fail.fail ("imil-var.sml", f, s)

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

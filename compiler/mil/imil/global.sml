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


signature IMIL_GLOBAL = 
sig
  include IMIL_PUBLIC_TYPES
  val build         : t * (Mil.variable * Mil.global) -> iGlobal
  val delete        : t * iGlobal -> unit
  val getMil        : t * iGlobal -> mGlobal
  val replaceMil    : t * iGlobal * mGlobal -> unit
  val replaceGlobal : t * iGlobal * (Mil.variable * Mil.global) -> unit

  (* These two give an error if dead*)
  val getVar    : t * iGlobal -> Mil.variable
  val getObject : t * iGlobal -> Mil.global

  val getUsedBy : t * iGlobal -> item Vector.t
  val getUses   : t * iGlobal -> use Vector.t
  val freeVars  : t * iGlobal -> Identifier.VariableSet.t
  val freeVars' : t * iGlobal -> Mil.variable list

  val toGlobal : iGlobal -> (Mil.variable * Mil.global) option

  val layout    : t * iGlobal -> Layout.t
  val layoutMil : t * mGlobal -> Layout.t
end

structure IMilGlobal : sig
  include IMIL_GLOBAL 
  val unBuild : iGlobal -> (Mil.variable * Mil.global) option
end
  =
struct
  open IMilPublicTypes
  val fail = 
   fn (f, s) => Fail.fail ("global.sml", f, s)

  structure L = Layout
  structure LU = LayoutUtils
  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure VS = IMT.VS
  structure FV = IMilCommon.FV
  structure Def = IMilDef
  structure Var = IMilVar
  structure Use = IMilUse
  structure IML = IMilLayout

  type t = IMT.t
  type iGlobal = IMT.iGlobal
  type mGlobal = IMT.mGlobal
  type item = IMilTypes.item
  type use = IMilTypes.use

  val getId = 
   fn (p, g) => IMT.iGlobalGetId g

  val getMil =
   fn (p, g) => IMT.iGlobalGetMil g

  val getVars =
   fn (p, g) => IMT.iGlobalGetVars g
            
  val unBuild = 
   fn g => 
      (case IMT.iGlobalGetMil g
        of IMT.GDead => NONE
         | IMT.GGlobal a => SOME a)

  val getVar = 
   fn (p, g) => 
      (case unBuild g
        of NONE => fail ("getVar", "Global is dead")
         | SOME (v, _) => v)

  val getObject = 
   fn (p, g) => 
      (case unBuild g
        of NONE => fail ("getObject", "Global is dead")
         | SOME (_, g) => g)

  val freeVarsInMilGlobal =
   fn (p, m) => 
      let
        val s = 
            case m
             of IMT.GGlobal (v, g)   => FV.global (p, v, g)
              | IMT.GDead => VS.empty
      in s
      end

  val buildMilVars =
   fn (p, g, m) => 
      let
        val vs = freeVarsInMilGlobal (p, m)
        val vs = VS.toList vs
        val vars = List.map (vs, (fn v => (v, Use.addUse (p, v, IMT.UseGlobal g))))
        val vars = IVD.fromList vars
      in vars
      end
      
  val addDefs =
   fn (p, g, m) => 
      let
        val () = case m
                  of IMT.GGlobal (v, _) => Def.add (p, v, IMT.DefGlobal g)
                   | IMT.GDead => ()
      in ()
      end


  val deleteDefs =
   fn (p, m) => 
      let
        val defsD = IMT.tGetDefs p
        val removeDef = fn v => IVD.remove (defsD, v)
        val () = case m
                  of IMT.GGlobal (v, _) => removeDef v
                   | IMT.GDead => ()
      in ()
      end

  val replaceMil =
   fn (p, g, m) => 
      let
        val () = 
            IVD.foreach (IMT.iGlobalGetVars g, fn (v, u) => Use.deleteUse (p, u))
        val () = deleteDefs (p, IMT.iGlobalGetMil g)
        val () = addDefs (p, g, m)
        val vars = buildMilVars (p, g, m)
        val () = IMT.iGlobalSetVars (g, vars)
        val () = IMT.iGlobalSetMil (g, m)
      in ()
      end
  val () = BackPatch.fill (Use.replaceMilGH, replaceMil)
      
  val replaceGlobal = 
   fn (p, g, a) => replaceMil (p, g, IMT.GGlobal a)

  val build =
   fn (p, (v, m)) => 
      let
        val id = IMT.nextId p
        val g' = IMT.G {id   = id,
                        vars = IVD.empty(),
                        mil  = IMT.GDead}
        val g = IMT.iGlobalNew g'
        val mg = IMT.GGlobal (v, m)
        val vars = buildMilVars (p, g, mg)
        val () = IVD.insert (IMT.tGetIGlobals p, v, g)
        val () = addDefs (p, g, mg)
        val () = IMT.iGlobalSetVars (g, vars)
        val () = IMT.iGlobalSetMil (g, mg)
      in g
      end
  val () = BackPatch.fill (Use.emitGlobalH, build)

  val getUses =
   fn (p, g) => 
      let
        val uses = 
            case getMil (p, g)
             of IMT.GDead => Vector.new0 ()
              | IMT.GGlobal (v, g) => Use.getUses (p, v)
      in uses
      end

  val freeVars' =
      fn (p, g) => 
      let
        val vars = getVars (p, g)
        val vars = IVD.domain vars
      in vars
      end

  val freeVars = VS.fromList o freeVars'

                 
  val getUsedBy =
   fn (p, g) => 
      let
        val dls = freeVars' (p, g)
        val defs = Vector.fromListMap (dls, 
                                    fn v => Def.get (p, v))
        val items = Def.defsToItems (p, defs)
      in items
      end


  val delete =
   fn (p, g) => 
      (case getMil (p, g) 
        of IMT.GDead => ()
         | IMT.GGlobal (v, _) => 
           let
             val () = replaceMil (p, g, IMT.GDead)
             val () = 
                 IVD.remove(IMT.tGetIGlobals p, v)
           in ()
           end)

  val toGlobal = 
   fn g => 
      (case IMT.iGlobalGetMil g
        of IMT.GDead => NONE
         | IMT.GGlobal a => SOME a)

  val layoutMil = IML.mGlobal

  val layout =
   fn (p, g) => 
      let
        val IMT.G {id, mil, vars} = IMT.iGlobalGetIGlobal' g
        val l = IML.mGlobal (p, mil)
        val vars = IVD.layout (vars, fn (v, _) => IML.var (p, v))
        val uses = Vector.layout 
                     (fn u => IML.use (p, u)) 
                     (getUses (p, g))
        val res = 
            L.mayAlign [l,
                        LU.indent (
                        L.mayAlign [L.seq[L.str " <- ", vars],
                                    L.seq[L.str " -> ", uses]])]

      in res
      end

  val () = BackPatch.fill (IML.iGlobalH, layout)

end

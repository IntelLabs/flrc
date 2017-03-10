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

(*
 * Translation from ANormLazy.t to AbsCore.t.
 *)
 (* FIXME: TypeRef.new_ *)
signature ANORM_LAZY_TO_ABS_CORE =
sig
  type t
  val doModule : ANormLazy.t -> t
end

functor ANormLazyToAbsCoreF (structure AbsCore : ABS_CORE) :> ANORM_LAZY_TO_ABS_CORE 
  where type t = AbsCore.t =
struct
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure GP = GHCPrimType
  structure AL = ANormLazy
  structure AC = AbsCore
  structure Dom = AC.Dom
  structure ACL = AbsCoreLayoutF (struct structure AbsCore = AC 
                                         type ty = ANormLazy.ty
                                         val layoutTy = ANormLazyLayout.layoutTy
                                  end)
  structure VD = Identifier.VariableDict
  structure IM = Identifier.Manager

  type t = AC.t

  val passname = "ANormLazyToAbsCore"

  val fail = fn (f, msg) => Fail.fail (passname, f, msg)

  val resultTy = 
    fn ty =>
      (case TypeRep.repToBase ty
        of AL.Arr (t1, t2, _) => t2
         | _ => TypeRep.newRep_ AL.Data)

  val rec doExp =
   fn (im, e, ty) =>
      (case e
        of AL.Var v => AC.Var v
         | AL.PrimApp (f, vs) => 
           let
             val vs = GHCPrimOp.keepStrictArgs (f, vs)
           in
             if List.isEmpty vs then AC.Const Dom.top else AC.GLB vs
           end
         | AL.ConApp ((c, _), vs) => 
           (case TypeRep.repToBase ty
             of AL.Sum [(_, _)] => AC.Con (c, vs) (* preserve only sum type with single constructors *)
              | _ => AC.Const Dom.top)
         | AL.Multi vs => AC.Multi vs  
         | AL.ExtApp (p, cc, f, ty, vs) => if List.isEmpty vs then AC.Const Dom.top else AC.GLB vs
         | AL.App (e, v) => AC.App (doExp (im, e, TypeRep.newRep_ (AL.Arr (IM.variableInfo (im, v), ty, NONE))), v)
         | AL.Lam ((v, vty, strictness), e) => AC.Lam (v, doExp (im, e, resultTy ty)) (* ignore existing strictness for the moment *)
         | AL.Let (vdefg, e) => AC.Let (doVDefg (im, vdefg), doExp (im, e, ty))
         | AL.Case (e, (v, vty), ty, alts) =>
          let
            val e = doExp (im, e, vty)
            fun doAlts () = 
              case alts 
                of [AL.Acon (con, vs, e1)] => 
                  AC.Let (AC.Nonrec (AC.Vdef (AC.VbMulti (List.map (vs, #1), false), e)), 
                          doExp (im, e1, ty))
                | _ => 
                  let
                    val vs = List.map (alts, fn _ => IM.variableFresh (im, "alt", vty))
                    val getAltE = fn AL.Acon (_, _, e) => e
                                   | AL.Alit (_, _, e) => e
                                   | AL.Adefault e => e
                    val es = List.zip (List.map (alts, getAltE), vs)
                  in
                    case vs
                      of [] => AC.Const Dom.bottom
                       | _  => List.foldr (es, AC.LUB vs, fn ((e1, v), e) => 
                                 AC.Let (AC.Nonrec (AC.Vdef (AC.VbSingle v, doExp (im, e1, ty))), e))
                  end
          in
            AC.Cond (e, doAlts ())
          end
         | AL.Lit (l, ty) => AC.Const Dom.top
         | AL.Cast (e, t1, t2) => doExp (im, e, t1)
      )

  and rec doVDef = 
   fn (im, vd) => 
      (case vd
        of (AL.Vdef (AL.VbSingle (v, vty, strict), e)) => AC.Vdef (AC.VbSingle v, doExp (im, e, vty))
         | (AL.Vdef (AL.VbMulti (vtys, effectful), e)) => 
          let
            val (vs, ts) = List.unzip vtys
            val ty = TypeRep.newRep_ (AL.Prim (GHCPrimType.Tuple ts)) 
          in
            AC.Vdef (AC.VbMulti (vs, effectful), doExp (im, e, ty))
          end)

  and rec doVDefg =
   fn (im, vdg) => 
      (case vdg
        of AL.Rec vdefs => AC.Rec (List.map(vdefs, fn def => doVDef (im, def)))
         | AL.Nonrec vdef => AC.Nonrec (doVDef (im, vdef)))

  fun doModule (AL.Module (main, vdefgs), im, tm) =
      let 
        val im = IM.fromExistingAll im
        val vdefgs = List.map (vdefgs, fn vdefg => doVDefg (im, vdefg))
      in 
        (AC.Module (main, vdefgs), IM.finish im)
      end

end

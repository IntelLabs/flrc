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

signature ANORM_STRICT_UTILS = 
sig
  structure VDef : 
  sig
    val binder        : ANormStrict.vDef -> ANormStrict.var * ANormStrict.ty
    val binders       : ANormStrict.vDef List.t -> (ANormStrict.var * ANormStrict.ty) List.t
    val variableDefd  : ANormStrict.vDef -> ANormStrict.var 
    val variablesDefd : ANormStrict.vDef List.t -> ANormStrict.var List.t
  end (* structure VDef *)

  structure VDefg : 
  sig
    val binder        : ANormStrict.vDefg -> (ANormStrict.var * ANormStrict.ty) List.t
    val binders       : ANormStrict.vDefg List.t -> (ANormStrict.var * ANormStrict.ty) List.t
    val variableDefd  : ANormStrict.vDefg -> ANormStrict.var List.t
    val variablesDefd : ANormStrict.vDefg List.t -> ANormStrict.var List.t
  end (* structure VDef *)
end (* signature ANORM_STRICT_UTILS *)

structure ANormStrictUtils :> ANORM_STRICT_UTILS = 
struct
  structure AS = ANormStrict
  structure VDef = 
  struct
    val binder       : ANormStrict.vDef -> ANormStrict.var * ANormStrict.ty = 
     fn vd => 
        (case vd
          of AS.Vfun {name, ty, ...} => (name, ty)
           | AS.Vthk {name, ty, ...} => (name, ty))
          
    val binders      : ANormStrict.vDef List.t -> (ANormStrict.var * ANormStrict.ty) List.t = 
     fn vds => List.map (vds, binder)

    val variableDefd  : ANormStrict.vDef -> ANormStrict.var = #1 o binder

    val variablesDefd : ANormStrict.vDef List.t -> ANormStrict.var List.t = 
     fn vds => List.map (vds, variableDefd)

  end (* structure VDef *)

  structure VDefg = 
  struct
    val binder       : ANormStrict.vDefg -> (ANormStrict.var * ANormStrict.ty) List.t = 
     fn vdg => 
        (case vdg
          of AS.Vdef (vts, e) => vts
           | AS.Nonrec vd     => VDef.binders [vd]
           | AS.Rec vds       => VDef.binders vds)

    val binders      : ANormStrict.vDefg List.t -> (ANormStrict.var * ANormStrict.ty) List.t = 
     fn vds => List.concatMap (vds, binder)

    val variableDefd  : ANormStrict.vDefg -> ANormStrict.var List.t = 
     fn vd => List.map (binder vd, #1)

    val variablesDefd : ANormStrict.vDefg List.t -> ANormStrict.var List.t = 
     fn vds => List.concatMap (vds, variableDefd)

  end (* structure VDef *)
end (* structure ANormStrictUtils *)

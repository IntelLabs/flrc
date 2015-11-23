(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_CODE_COPY =
sig
  type 'a t = Config.t * Mil.symbolTableManager * 'a -> 'a * (Mil.variable Mil.VD.t * Mil.label Mil.LD.t)
  val program     : Mil.t t
  val global      : (Mil.variable * Mil.global) t
  val code        : Mil.code t
  val codeBody    : Mil.codeBody t
  val block       : (Mil.label * Mil.block) t
  val instruction : Mil.instruction t
  val transfer    : Mil.transfer t
end;

structure MilCodeCopy :> MIL_CODE_COPY =
struct

  type 'a t = Config.t * Mil.symbolTableManager * 'a -> 'a * (Mil.variable Mil.VD.t * Mil.label Mil.LD.t)

  structure LD = Mil.LD
  structure VD = Mil.VD
  structure LS = Mil.LS
  structure VS = Mil.VS
  structure M = Mil
  structure MRC = MilRewriterClient
  structure MBVL = MilBoundVarsLabels
  structure MSTM = MilUtils.SymbolTableManager
  structure MR = MilRename.VarLabel

  fun mkRename (c, stm, vs, ls) = 
      let
        val dupVar = 
         fn (v, vd) => VD.insert (vd, v, MSTM.variableClone (stm, v))
        val vd = VS.fold (vs, VD.empty, dupVar)
        val dupLabel = 
         fn (l, ld) => LD.insert (ld, l, MSTM.labelFresh (stm))
        val ld = LS.fold (ls, LD.empty, dupLabel)
        val rename = (vd, ld)
      in rename
      end

  fun block (c, stm, (l, b)) = 
      let
        val (vs, ls) = MBVL.block (c, l, b)
        val r = mkRename (c, stm, vs, ls)
        val (l, b) = MR.block (c, r, l, b)
      in ((l, b), r)
      end

  fun global (c, stm, (v, g)) =
      let
        val (vs, ls) = MBVL.global (c, v, g)
        val r = mkRename (c, stm, vs, ls)
        val (v, g) = MR.global (c, r, v, g)
      in ((v, g), r)
      end

  fun instruction (c, stm, i) =
      let
        val (vs, ls) = MBVL.instruction (c, i)
        val r = mkRename (c, stm, vs, ls)
        val i = MR.instruction (c, r, i)
      in (i, r)
      end

  fun transfer (c, stm, t) =
      let
        val (vs, ls) = MBVL.transfer (c, t)
        val r = mkRename (c, stm, vs, ls)
        val t = MR.transfer (c, r, t)
      in (t, r)
      end
      
  fun codeBody (c, stm, cb) = 
      let
        val (vs, ls) = MBVL.codeBody (c, cb)
        val r = mkRename (c, stm, vs, ls)
        val cb = MR.codeBody (c, r, cb)
      in (cb, r)
      end

  fun code (c, stm, cd) = 
      let
        val (vs, ls) = MBVL.code (c, cd)
        val r = mkRename (c, stm, vs, ls)
        val cd = MR.code (c, r, cd)
      in (cd, r)
      end
      
  fun program (c, stm, p) =
      let
        val (vs, ls) = MBVL.program (c, p)
        val r = mkRename (c, stm, vs, ls)
        val p = MR.program (c, r, p)
      in (p, r)
      end

end;

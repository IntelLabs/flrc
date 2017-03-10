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


signature IMIL_LAYOUT = 
sig
  include IMIL_PUBLIC_TYPES
  val var   : t * variable -> Layout.t
  val label : t * label -> Layout.t
  val use   : t * use -> Layout.t
  val def   : t * def -> Layout.t
  val t     : t -> Layout.t
  val iFunc  : t * iFunc -> Layout.t
  val iBlock  : t * iBlock -> Layout.t
  val iInstr  : t * iInstr -> Layout.t
  val iGlobal  : t * iGlobal -> Layout.t
  val mInstr  : t * mInstr -> Layout.t
  val mGlobal : t * mGlobal -> Layout.t
  val item : t * item -> Layout.t
end

structure IMilLayout :
sig
  include IMIL_LAYOUT

  val iFuncH : (t * iFunc -> Layout.t) BackPatch.t
  val iBlockH : (t * iBlock -> Layout.t) BackPatch.t
  val iInstrH : (t * iInstr -> Layout.t) BackPatch.t
  val iGlobalH : (t * iGlobal -> Layout.t) BackPatch.t
end
= 
struct
  open IMilPublicTypes

  structure M = Mil
  structure L = Layout
  structure ID = Identifier
  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure BP = BackPatch

  val fail = 
   fn (f, s) => Fail.fail ("layout.sml",
                           f,
                           s)                 

  val var  = 
   fn (p, v) => MilLayout.layoutVariable (IMT.tGetConfig p, IMT.tGetSi p, v)

  val label = 
   fn (p, l) => ID.layoutLabel l

  val layoutIInstrName = 
   fn (p, i) => 
      let
        val id = Int.layout (IMT.iInstrGetId i)
        val l = 
            (case IMT.iInstrGetMil i
              of IMT.MInstr (M.I {dests, n, rhs}) => 
                 (case Vector.toListMap (dests, fn v => var (p, v))
                   of [] => L.str "Instr(!)"
                    | l  => L.seq [L.str "Instr", LayoutUtils.parenSeq l])
               | IMT.MTransfer _ => L.str "Trans"
               | IMT.MLabel _ => L.str "Label"
               | IMT.MDead =>  L.str "Dead")
        val l = L.seq [l, L.str " ", id]
      in l
      end

  val layoutIGlobalName = 
   fn (p, g) =>
      (case IMT.iGlobalGetMil g
        of IMT.GGlobal (v, g) => var (p, v)
         | IMT.GDead => L.str "GDead!")
      
  val use = 
   fn (p, u) =>
      (case u
        of IMT.Used => L.str "Used"
         | IMT.UseInstr i => layoutIInstrName (p, i)
         | IMT.UseGlobal g => layoutIGlobalName (p, g))
      
  val mInstr = 
   fn (p, mi) =>
      let
        val si = IMT.tGetSi p
        val l = 
            case mi
             of IMT.MInstr mi => 
                MilLayout.layoutInstruction (IMT.tGetConfig p, si, mi)
              | IMT.MTransfer t => 
                MilLayout.layoutTransfer (IMT.tGetConfig p, si, t)
              | IMT.MLabel (l, ops) => 
                L.seq[Identifier.layoutLabel l,
                      Vector.layout (fn v => var (p, v)) ops]
              | IMT.MDead => L.str "Dead"
      in l
      end

  val mGlobal = 
   fn (p, g) =>
      case g
       of IMT.GGlobal mg => 
          MilLayout.layoutGlobal (IMT.tGetConfig p, IMT.tGetSi p, mg)
        | IMT.GDead => L.str "Dead"

  val iFuncH : (IMT.t * IMT.iFunc, Layout.t) BP.func = BP.new ()
  val iFunc = BP.apply iFuncH
  val iBlockH : (IMT.t * IMT.iBlock, Layout.t) BP.func = BP.new ()
  val iBlock = BP.apply iBlockH
  val iInstrH : (IMT.t * IMT.iInstr, Layout.t) BP.func = BP.new ()
  val iInstr = BP.apply iInstrH
  val iGlobalH : (IMT.t * IMT.iGlobal, Layout.t) BP.func = BP.new ()
  val iGlobal = BP.apply iGlobalH

  val item = 
   fn (p, i) =>
      case i
       of IMT.ItemInstr i  => iInstr (p, i)
        | IMT.ItemGlobal g => iGlobal (p, g)
        | IMT.ItemFunc c   => iFunc (p, c)

  val def = 
   fn (p, d) =>
      let
        val var = fn v => var (p, v)
        val d = 
            case d
             of IMT.DefUnk => L.str "Unk"
              | IMT.DefExtern => L.str "Extern"
              | IMT.DefInstr i => 
                let
                  val vars = IVD.layout (IMT.iInstrGetVars i, 
                                         var o #1)
                in vars
                end
              | IMT.DefGlobal g => 
                let
                  val vars = IVD.layout (IMT.iGlobalGetVars g, 
                                         var o #1)
                in vars
                end
              | IMT.DefFunc iFunc => 
                L.seq [L.str "Func ", 
                       var (IMT.iFuncGetFName iFunc),
                       L.str ", size = ",
                       Int.layout (!(IMT.iFuncGetSize iFunc))]
              | IMT.DefParameter func =>
                L.seq [L.str "Parameter ", var (IMT.iFuncGetFName func)]
                
      in d
      end

  val t = 
   fn p =>
      let
        val IMT.P {uses, defs, ...} = p
        val var = fn v => var (p, v)
        fun layoutVD (v, d) = 
            L.seq[var v, L.str " <- ", def (p, d)]
        val layoutUse = fn u => use (p, u)
        fun layoutUseList (v, l) = 
            L.seq[var v, L.str " -> ",
                  DList.layout (l, layoutUse)]
        val l = 
            L.align [L.str "USES",
                     IVD.layout (uses, layoutUseList),
                     L.str "DEFS",
                     IVD.layout (defs, layoutVD)
                    ]
      in l
      end

  end

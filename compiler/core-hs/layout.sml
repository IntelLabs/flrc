(* COPYRIGHT_NOTICE_1 *)
signature CORE_HS_LAYOUT =
sig
  type env = Config.t

  val controls             : Config.Control.control list
  val escape               : string -> string
  val qNameToString        : string CoreHs.qualified -> string
  val qNameToStringDecoded : string CoreHs.qualified -> string

  val layoutQName          : env * string CoreHs.qualified -> Layout.t
  val layoutPName          : env * CoreHs.pName            -> Layout.t
  val layoutAnMName        : env * CoreHs.anMName          -> Layout.t
  val layoutMName          : env * CoreHs.mName            -> Layout.t
  val layoutCC             : env * CoreHs.callconv         -> Layout.t
  val layoutKind           : env * CoreHs.kind             -> Layout.t
  val layoutTy             : env * CoreHs.ty               -> Layout.t
  val layoutVBind          : env * CoreHs.vBind            -> Layout.t
  val layoutTBind          : env * CoreHs.tBind            -> Layout.t
  val layoutBind           : env * CoreHs.bind             -> Layout.t
  val layoutCoreLit        : env * CoreHs.coreLit          -> Layout.t
  val layoutLit            : env * CoreHs.lit              -> Layout.t
  val layoutCoercionKind   : env * CoreHs.coercionKind     -> Layout.t
  val layoutKindOrCoercion : env * CoreHs.kindOrCoercion   -> Layout.t
  val layoutAlt            : env * CoreHs.alt              -> Layout.t
  val layoutExp            : env * CoreHs.exp              -> Layout.t
  val layoutVDef           : env * CoreHs.vDef             -> Layout.t
  val layoutVDefg          : env * CoreHs.vDefg            -> Layout.t
  val layoutCDef           : env * CoreHs.cDef             -> Layout.t
  val layoutTDef           : env * CoreHs.tDef             -> Layout.t
  val layoutModule         : env * CoreHs.module           -> Layout.t
end

structure CoreHsLayout :> CORE_HS_LAYOUT =
struct
  open CoreHs
  structure CU = CoreHsUtils
  structure U = Utils
  structure L = Layout
  structure LU = LayoutUtils
  
  type env = Config.t
  
  val modulename = "CoreHsLayout"

  val describe = fn () =>
      L.align [L.str (modulename ^ " control string consists of:"),
               LU.indent (L.align [L.str "d => show decoded names" ]),
               L.str "default is nothing"]
      
  val parse = fn str =>
      if String.contains (str, #"d") then SOME true else NONE

  val dft = fn _ => false    

  val (control, useDecodedNames) = Config.Control.mk (modulename, describe, parse, dft)
  val controls = [control]

  fun escape s =
      let
        fun enc c =
          if c < 32 orelse c >= 127 orelse c = 34 orelse c = 39 orelse c = 72
            then
              let val hex = "0123456789abcdef"
                  val h1 = String.sub (hex, c div 16)
                  val h2 = String.sub (hex, c mod 16)
              in implode [#"\\", #"x", h1, h2]
              end
            else String.fromChar (Char.chr c)
      in
        String.concat (map (enc o ord) (explode s))
      end

  fun semiMap (l, f)
    = (case l
        of [] => []
         | [x] => [f x]
         | l => List.map (l, fn v => L.seq [f v, L.str ";"]))

  val separate = L.sequence ("", "", " ") 

  fun layoutNameAux (d, n) = L.str (if d then CU.zDecodeString n else n)
  fun layoutName (env, n) = layoutNameAux (useDecodedNames env, n)

  fun layoutPNameAux (d, P n) = layoutNameAux (d, n)
  fun layoutPName (env, p) = layoutPNameAux (useDecodedNames env, p)

  fun hierModuleSep d = if d then "." else CU.zEncodeString "."

  fun layoutAnMNameAux (d, M (pname, parents, name)) =
      let
        val hms = hierModuleSep d
        val parentLayout =
              case parents
                of [] => L.empty
                 | _  => L.seq (map (fn x => L.str (x ^ hierModuleSep d)) parents)
      in
        L.seq [ layoutPNameAux (d, pname)
              , L.str ":"
              , parentLayout
              , layoutNameAux (d, name) ]
      end
  fun layoutAnMName (env, n) = layoutAnMNameAux (useDecodedNames env, n)

  fun layoutMNameAux (d, NONE)     = L.empty
    | layoutMNameAux (d, (SOME m)) = L.seq [layoutAnMNameAux (d, m), L.str "."]
  fun layoutMName (env, m) = layoutMNameAux (useDecodedNames env, m)

  fun layoutQNameAux (d, (m, v)) = L.seq [layoutMNameAux (d, m), layoutNameAux (d, v)]
  fun layoutQName (env, n) = layoutQNameAux (useDecodedNames env, n)
  fun qNameToString name = Layout.toString (layoutQNameAux (false, name))
  fun qNameToStringDecoded name = Layout.toString (layoutQNameAux (true, name))

  fun layoutCC (env, cc) 
    = case cc
        of Prim    => L.str "prim"
         | CCall   => L.str "ccall"
         | StdCall => L.str "stdcall" 
         | Dynamic => L.str "dynamic" 
         | Label   => L.str "label" 

  fun layoutKind (env, k)
    = (case k
        of (Karrow (k1, k2)) => L.paren (L.mayAlign [ L.seq [layoutAKind (env, k1), L.str " ->"]
                                                    , layoutKind (env, k2)])
         | (Keq (t1, t2)) => layoutEqKind (env, t1, t2)
         | k => layoutAKind (env, k))

  and layoutAKind (env, k)
    = (case k
        of Klifted => L.str "*"
         | Kunlifted => L.str "#"
         | Kopen => L.str "?"
         | k => L.paren (layoutKind (env, k)))

  and layoutEqKind (env, t1, t2) 
    = L.paren (L.seq [ L.paren (layoutTy (env, t1))
                     , L.str " :=: "
                     , L.paren (layoutTy (env, t2))])

  and layoutATy (env, ty)
    = (case ty
        of (Tvar n) => L.str n
         | (Tcon c) => layoutQName (env, c)
         | t => L.paren (layoutTy (env, t)))

  and layoutATy1 (env, (ty, strict))
    = if strict then L.seq [ L.str "!", layoutATy (env, ty) ] else layoutATy (env, ty)

  and layoutAppTy (env, t, ts) 
    = (case t
        of Tapp (t1, t2) => layoutAppTy (env, t1, t2::ts)
         | t => separate (List.map (t::ts, fn t => layoutATy (env, t))))

  and layoutBTy (env, t)
    = (case t
        of Tapp (t1, t2) =>
          (case t1
            of Tapp (Tcon tc, t3) => if tc = CU.tcArrow
                                       then L.paren (L.mayAlign [ L.seq [layoutBTy (env, t3), L.str " ->"]
                                                                , layoutTy (env, t2)])
                                       else layoutAppTy (env, t1, [t2])
             | _ => layoutAppTy (env, t1, [t2]))
         | t => layoutATy (env, t))

  and layoutForall (env, tbs, t)
    = (case t 
        of Tforall (tb, t) => layoutForall (env, tbs @ [tb], t)
         | t => L.seq [separate (List.map (tbs, fn t => layoutTBind (env, t)))
                      , L.str " . ", layoutTy (env, t)])

  and layoutTy (env, t)
    = (case t
        of Tforall (tb, t) => 
           L.seq [L.str "%forall ", layoutForall (env, [tb], t)]
         | TransCoercion (t1, t2) =>
           L.seq [L.str "%trans ", separate [layoutATy (env, t1), layoutATy (env, t2)]]
         | SymCoercion t =>
           L.seq [L.str "%sym ", layoutATy (env, t)]
         | UnsafeCoercion (t1, t2) =>
           L.seq [L.str "%unsafe ", separate [layoutATy (env, t1), layoutATy (env, t2)]]
         | LeftCoercion t =>
           L.seq [L.str "%left ", layoutATy (env, t)]
         | RightCoercion t =>
           L.seq [L.str "%right ", layoutATy (env, t)]
         | InstCoercion (t1, t2) =>
           L.seq [L.str "%inst ", separate [layoutATy (env, t1), layoutATy (env, t2)]]
         | NthCoercion (i, t1) => 
           L.seq [L.str "%nth ", separate [Int.layout i, layoutATy (env, t1)]]
         | t => layoutBTy (env, t))

  and layoutTBind (env, (t, k))
    = (case k 
        of Klifted => L.str t
         | _ => L.paren (L.seq [L.str t, L.str " :: ", layoutKind (env, k)]))

  fun layoutAtTBind (env, (t, k)) = L.seq [L.str "@ ", layoutTBind (env, (t, k))]

  fun layoutVBind (env, (x, t)) = L.paren (L.seq[L.str x, L.str " :: ", layoutTy (env, t)])

  fun layoutBind (env, b)
    = (case b
        of Tb tb => L.seq [L.str "@ ", layoutTBind (env, tb)]
         | Vb vb => layoutVBind (env, vb))

  fun layoutCoreLit (env, l)
    = (case l
        of Lint v => IntInf.layout v
         | Lrational v =>
          let 
            val (n, d) = Rat.toInts v
          in
            L.seq [ IntInf.layout n , L.str "%" , IntInf.layout d]
          end
         | Lchar c => L.str ("'" ^ escape (String.fromChar (Char.chr c)) ^ "'")
         | Lstring s => L.str ("\"" ^ escape s ^ "\""))

  fun layoutLit (env, Literal (l, t)) =
      L.paren (L.seq[layoutCoreLit (env, l), L.str " :: ", layoutTy (env, t)])

  fun layoutCoercionKind (env, DefinedCoercion (tbs, t1, t2)) =
      L.sequence ("<C", ">", " ") 
        (List.map (tbs, fn b => layoutTBind (env, b)) @ [L.paren (layoutKind (env, Keq (t1, t2)))])

  fun layoutKindOrCoercion (env, k) 
    = (case k
        of Kind k => L.sequence ("<K", ">", " ") [layoutKind (env, k)]
         | Coercion ck =>  layoutCoercionKind (env, ck))

  fun layoutAlt (env, alt)
    = (case alt
        of Acon (c, tbs, vbs, e) =>
          L.mayAlign [ L.seq [ layoutQName (env, c), L.str " "
                             , separate (List.map (tbs, fn t => layoutAtTBind (env, t)))
                             , separate (List.map (vbs, fn v => layoutVBind (env, v)))
                             , L.str " ->"]
                     , LU.indent (layoutExp (env, e))]
         | Alit (l, e) =>
          L.mayAlign [ L.seq [layoutLit (env, l), L.str " -> "]
                     , LU.indent (layoutExp (env, e))]
         | Adefault e =>
          L.mayAlign [ L.str "%_ -> ", LU.indent (layoutExp (env, e))])

  and layoutAppExp (env, e, s) 
    = (case e
        of App (e1, e2) => layoutAppExp (env, e1, U.Inl e2 :: s)
         | Appt (e, t) => layoutAppExp (env, e, U.Inr t :: s)
         | e =>
          let fun pa (U.Inl ex) = layoutAExp (env, ex)
                | pa (U.Inr t) = L.seq [L.str "@ ", layoutATy (env, t)]
          in L.mayAlign (layoutAExp (env, e) :: List.map (s, pa))
          end)

  and layoutFExp (env, e)
    = (case e
        of App (e1, e2) => layoutAppExp (env, e1, [U.Inl e2])
         | Appt (e, t) => layoutAppExp (env, e, [U.Inr t])
         | e => layoutAExp (env, e))

  and layoutAExp (env, e)
    = (case e
        of Var x => layoutQName (env, x)
         | Dcon x => layoutQName (env, x)
         | Lit l => layoutLit (env, l)
         | e => L.paren (layoutExp (env, e)))

  and layoutLamExp (env, bs, e)
    = (case e
        of Lam (b, e) => layoutLamExp (env, bs @ [b], e)
         | e => L.mayAlign [ L.seq [separate (List.map (bs, fn b => layoutBind (env, b))),  L.str " ->" ]
                           , LU.indent (layoutExp (env, e))])

  and layoutExp (env, e)
    = (case e
        of Lam (b, e) =>
           L.seq [L.str "\\ ", layoutLamExp (env, [b], e)]
         | Let (vd, e) =>
           L.mayAlign [ L.seq [L.str "%let ", layoutVDefg (env, vd)]
                      , L.seq [L.str "%in ", layoutExp (env, e)]]
         | Case (e, vb, t, alts) =>
           L.mayAlign [ L.seq [L.str "%case ", separate [layoutATy (env, t), layoutAExp (env, e)]]
                      , L.seq [L.str "%of ", layoutVBind (env, vb)]
                      , LU.indent ( L.sequence ("{", "}", ";") (List.map (alts, fn a => layoutAlt (env, a))))]
         | Cast (e, t) =>
           L.mayAlign [ L.seq [L.str "%cast ", L.paren (layoutExp (env, e))]
                      , layoutATy (env, t)]
         | Note (s, e) =>
           L.mayAlign [ L.str ("%note \"" ^ escape s ^ "\"")
                      , layoutExp (env, e)]
         | External (p, cc, n, t) =>
           L.mayAlign [ L.seq [L.str "%external ", layoutCC (env, cc), L.str (" \"" ^ escape n ^ "\"")]
                      , layoutATy (env, t)]
         | e => layoutFExp (env, e))

  and layoutVDef (env, Vdef (qv, t, e)) =
      L.mayAlign [ L.seq [ layoutQName (env, qv), L.str " :: ", layoutTy (env, t), L.str " =" ]
                 , LU.indent (layoutExp (env, e))]

  and layoutVDefg (env, vdefs)
    = (case vdefs
        of Rec vdefs =>
          L.mayAlign [ L.str "%rec {"
                     , LU.indent (L.align (semiMap (vdefs, fn d => layoutVDef (env, d))))
                     , L.str "}"]
         | Nonrec vdef => layoutVDef (env, vdef))

  fun layoutCDef (env, Constr (qdcon, tbinds, tys)) =
      L.mayAlign [ L.seq [layoutQName (env, qdcon), L.str " "]
                 , separate (List.map (tbinds, fn b => layoutAtTBind (env, b)))
                 , separate (List.map (tys, fn t => layoutATy1 (env, t)))]

  fun layoutTDef (env, td)
    = (case td
        of Data (qtcon, tbinds, cdefs) =>
          L.mayAlign [ L.seq [L.str "%data ", layoutQName (env, qtcon), L.str " ", 
                              separate (List.map (tbinds, fn b => layoutTBind (env, b))), L.str " = "]
                     , LU.indent (L.seq [ L.str "{"
                                        , L.align (semiMap (cdefs, fn d => layoutCDef (env, d)))
                                        , L.str "}"])]
         | Newtype (qtcon, coercion, tbinds, tyopt) =>
          L.mayAlign [ L.seq [ L.str "%newtype " 
                             , separate (layoutQName (env, qtcon) :: layoutQName (env, coercion) :: 
                                         List.map (tbinds, fn b => layoutTBind (env, b)))]
                     , LU.indent (L.seq [L.str " = ", layoutTy (env, tyopt)]) ])


  fun layoutModule (env, Module (mname, tdefs, vdefgs)) =
      L.align [ L.seq [L.str "%module ", layoutAnMName (env, mname)]
              , LU.indent (L.align (semiMap (tdefs, fn d => layoutTDef (env, d))))
              , LU.indent (L.align (semiMap (vdefgs, fn d => layoutVDefg (env, d))))
              , L.str "\n"]

end


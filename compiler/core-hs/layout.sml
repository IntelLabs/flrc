signature CORE_HS_LAYOUT =
sig
  val escape               : string -> string
  val qNameToString        : string CoreHs.qualified -> string

  val layoutQName          : string CoreHs.qualified -> Layout.t
  val layoutPName          : CoreHs.pName            -> Layout.t
  val layoutAnMName        : CoreHs.anMName          -> Layout.t
  val layoutMName          : CoreHs.mName            -> Layout.t
  val layoutKind           : CoreHs.kind             -> Layout.t
  val layoutTy             : CoreHs.ty               -> Layout.t
  val layoutVBind          : CoreHs.vBind            -> Layout.t
  val layoutTBind          : CoreHs.tBind            -> Layout.t
  val layoutBind           : CoreHs.bind             -> Layout.t
  val layoutCoreLit        : CoreHs.coreLit          -> Layout.t
  val layoutLit            : CoreHs.lit              -> Layout.t
  val layoutCoercionKind   : CoreHs.coercionKind     -> Layout.t
  val layoutKindOrCoercion : CoreHs.kindOrCoercion   -> Layout.t
  val layoutAlt            : CoreHs.alt              -> Layout.t
  val layoutExp            : CoreHs.exp              -> Layout.t
  val layoutVDef           : CoreHs.vDef             -> Layout.t
  val layoutVDefg          : CoreHs.vDefg            -> Layout.t
  val layoutCDef           : CoreHs.cDef             -> Layout.t
  val layoutTDef           : CoreHs.tDef             -> Layout.t
  val layoutModule         : CoreHs.module           -> Layout.t
end

structure CoreHsLayout :> CORE_HS_LAYOUT =
struct
  open CoreHs
  structure CU = CoreHsUtils
  structure U = Utils
  structure L = Layout

  val tabSize = 2

  fun escape s =
      let
        fun enc c =
          if c < 32 orelse c > 127 orelse c = 34 orelse c = 39 orelse c = 72
            then
              let val hex = "0123456789abcdef"
                  val h1 = String.sub (hex, c div 16)
                  val h2 = String.sub (hex, c mod 16)
              in implode [#"\\", #"x", h1, h2, #"\\", #"&"]
              end
            else String.fromChar (Char.chr c)
      in
        String.concat (map (enc o ord) (explode s))
      end

  fun semiMap f []  = []
    | semiMap f [x] = [f x]
    | semiMap f l   = map (fn v => L.seq [f v, L.str ";"]) l

  val separate = L.sequence ("", "", " ") 

  fun layoutPName (P n) = L.str n

  val hierModuleSep = CU.zEncodeString "."

  fun layoutAnMName (M (pname, parents, name)) =
      let val parentLayout =
              case parents
                of [] => L.empty
                 | _  => L.seq (map (fn x => L.str (x ^ hierModuleSep)) parents)
      in
        L.seq [ layoutPName pname
              , L.str ":"
              , parentLayout
              , L.str name ]
      end

  fun layoutMName NONE = L.empty
    | layoutMName (SOME m) = L.seq [layoutAnMName m, L.str "."]

  fun layoutQName (m, v) = L.seq [layoutMName m, L.str v]
  fun qNameToString name = Layout.toString (layoutQName name)

  fun layoutKind (Karrow (k1, k2)) = L.paren (L.mayAlign [ L.seq [layoutAKind k1, L.str " ->"]
                                                         , layoutKind k2])
    | layoutKind (Keq (t1, t2)) = layoutEqKind (t1, t2)
    | layoutKind k = layoutAKind k

  and layoutAKind Klifted = L.str "*"
    | layoutAKind Kunlifted = L.str "#"
    | layoutAKind Kopen = L.str "?"
    | layoutAKind k = L.paren (layoutKind k)

  and layoutEqKind (t1, t2) = L.paren (L.seq [ L.paren (layoutTy t1)
                                             , L.str " :=: "
                                             , L.paren (layoutTy t2)])

  and layoutATy (Tvar n) = L.str n
    | layoutATy (Tcon c) = layoutQName c
    | layoutATy t = L.paren (layoutTy t)

  and layoutAppTy (Tapp (t1, t2)) ts = layoutAppTy t1 (t2::ts)
    | layoutAppTy t ts = separate (map layoutATy (t::ts))

  and layoutBTy (Tapp (t1, t2)) =
      (case t1
        of Tapp (Tcon tc, t3) => if tc = CU.tcArrow
                                   then L.paren (L.mayAlign [ L.seq [layoutBTy t3, L.str " ->"]
                                                            , layoutTy t2])
                                   else layoutAppTy t1 [t2]
         | otherwise => layoutAppTy t1 [t2])
    | layoutBTy t = layoutATy t

  and layoutForall tbs (Tforall (tb, t)) = layoutForall (tbs @ [tb]) t
    | layoutForall tbs t =
       L.seq [separate (map layoutTBind tbs), L.str " . ", layoutTy t]

  and layoutTy (Tforall (tb, t)) =
      L.seq [L.str "%forall ", layoutForall [tb] t]
    | layoutTy (TransCoercion (t1, t2)) =
      L.seq [L.str "%trans ", separate [layoutATy t1, layoutATy t2]]
    | layoutTy (SymCoercion t) =
      L.seq [L.str "%sym ", layoutATy t]
    | layoutTy (UnsafeCoercion (t1, t2)) =
      L.seq [L.str "%unsafe ", separate [layoutATy t1, layoutATy t2]]
    | layoutTy (LeftCoercion t) =
      L.seq [L.str "%left ", layoutATy t]
    | layoutTy (RightCoercion t) =
      L.seq [L.str "%right ", layoutATy t]
    | layoutTy (InstCoercion (t1, t2)) =
      L.seq [L.str "%inst ", separate [layoutATy t1, layoutATy t2]]
    | layoutTy t = layoutBTy t

  and layoutTBind (t, Klifted) = L.str t
    | layoutTBind (t, k) = L.paren (L.seq [L.str t, L.str " :: ", layoutKind k])

  fun layoutAtTBind (t, k) = L.seq [L.str "@ ", layoutTBind (t, k)]

  fun layoutVBind (x, t) = L.paren (L.seq[L.str x, L.str " :: ", layoutTy t])

  fun layoutBind (Tb tb) = L.seq [L.str "@ ", layoutTBind tb]
    | layoutBind (Vb vb) = layoutVBind vb

  fun layoutCoreLit (Lint v) = IntInf.layout v
    | layoutCoreLit (Lrational v) =
      let val (n, d) = Rat.toInts v
      in L.seq [ IntInf.layout n , L.str "%" , IntInf.layout d]
      end
    | layoutCoreLit (Lchar c) =
      L.str ("'" ^ escape (String.fromChar (Char.chr c)) ^ "'")
    | layoutCoreLit (Lstring s) =
      L.str ("\"" ^ escape s ^ "\"")

  fun layoutLit (Literal (l, t)) =
      L.paren (L.seq[layoutCoreLit l, L.str " :: ", layoutTy t])

  fun layoutCoercionKind (DefinedCoercion (tbs, t1, t2)) =
      L.sequence ("<C", ">", " ") (map layoutTBind tbs @ [L.paren (layoutKind (Keq (t1, t2)))])

  fun layoutKindOrCoercion (Kind k) =
      L.sequence ("<K", ">", " ") [layoutKind k]
    | layoutKindOrCoercion (Coercion ck) = layoutCoercionKind ck

  fun layoutAlt (Acon (c, tbs, vbs, e)) =
      L.mayAlign [ L.seq [ layoutQName c, L.str " ", separate (map layoutAtTBind tbs)
                         , separate (map layoutVBind vbs), L.str " ->"]
                 , L.indent (layoutExp e, tabSize)]
    | layoutAlt (Alit (l, e)) =
      L.mayAlign [ L.seq [layoutLit l, L.str " -> "]
                 , L.indent (layoutExp e, tabSize)]
    | layoutAlt (Adefault e) =
      L.mayAlign [ L.str "%_ -> ", L.indent (layoutExp e, tabSize)]

  and layoutAppExp (App (e1, e2)) s = layoutAppExp e1 (U.Inl e2 :: s)
    | layoutAppExp (Appt (e, t)) s = layoutAppExp e (U.Inr t :: s)
    | layoutAppExp e s =
      let fun pa (U.Inl ex) = layoutAExp ex
            | pa (U.Inr t) = L.seq [L.str "@ ", layoutATy t]
      in L.mayAlign (layoutAExp e :: map pa s)
      end

  and layoutFExp (App (e1, e2)) = layoutAppExp e1 [U.Inl e2]
    | layoutFExp (Appt (e, t)) = layoutAppExp e [U.Inr t]
    | layoutFExp e = layoutAExp e

  and layoutAExp (Var x) = layoutQName x
    | layoutAExp (Dcon x) = layoutQName x
    | layoutAExp (Lit l) = layoutLit l
    | layoutAExp e = L.paren (layoutExp e)

  and layoutLamExp bs (Lam (b, e)) = layoutLamExp (bs @ [b]) e
    | layoutLamExp bs e =
      L.mayAlign [ L.seq [separate (map layoutBind bs),  L.str " ->" ]
                 , L.indent (layoutExp e, tabSize)]

  and layoutExp (Lam (b, e)) =
      L.seq [L.str "\\ ", layoutLamExp [b] e]
    | layoutExp (Let (vd, e)) =
      L.mayAlign [ L.seq [L.str "%let ", layoutVDefg vd]
                 , L.seq [L.str "%in ", layoutExp e]]
    | layoutExp (Case (e, vb, t, alts)) =
      L.mayAlign [ L.seq [L.str "%case ", separate [layoutATy t, layoutAExp e]]
                 , L.seq [L.str "%of ", layoutVBind vb]
                 , L.indent (L.sequence ("{", "}", ";") (map layoutAlt alts), tabSize)]
    | layoutExp (Cast (e, t)) =
      L.mayAlign [ L.seq [L.str "%cast ", L.paren (layoutExp e)]
                 , layoutATy t]
    | layoutExp (Note (s, e)) =
      L.mayAlign [ L.str ("%note \"" ^ escape s ^ "\"")
                 , layoutExp e]
    | layoutExp (External (n, t)) =
      L.mayAlign [ L.str ("%external ccall \"" ^ escape n ^ "\"")
                 , layoutATy t]
    | layoutExp e = layoutFExp e

  and layoutVDef (Vdef (qv, t, e)) =
      L.mayAlign [ L.seq [ layoutQName qv, L.str " :: ", layoutTy t, L.str " =" ]
                 , L.indent (layoutExp e, tabSize)]

  and layoutVDefg (Rec vdefs) =
      L.mayAlign [L.str "%rec",
                  L.seq [ L.str "{"
                        , L.indent (L.align (semiMap layoutVDef vdefs), tabSize)]
                        , L.str "}"]
    | layoutVDefg (Nonrec vdef) = layoutVDef vdef

  fun layoutCDef (Constr (qdcon, tbinds, tys)) =
      L.mayAlign [ L.seq [layoutQName qdcon , L.str " "]
                 , separate (map layoutAtTBind tbinds)
                 , separate (map layoutATy tys)]

  fun layoutTDef (Data (qtcon, tbinds, cdefs)) =
      L.mayAlign [ L.seq [L.str "%data ", layoutQName qtcon, L.str " ", 
                          separate (map layoutTBind tbinds), L.str " = "]
                 , L.indent (L.seq [ L.str "{"
                                   , L.align (semiMap layoutCDef cdefs)
                                   , L.str "}"], tabSize)]
    | layoutTDef (Newtype (qtcon, coercion, tbinds, tyopt)) =
      L.mayAlign [ L.seq [ L.str "%newtype ", layoutQName qtcon, L.str " ", layoutQName coercion
                         , separate (map layoutTBind tbinds)]
                 , L.indent (L.seq [L.str " = ", layoutTy tyopt], tabSize) ]


  fun layoutModule (Module (mname, tdefs, vdefgs)) =
      L.align [ L.seq [L.str "%module ", layoutAnMName mname]
              , L.indent (L.align (semiMap layoutTDef tdefs), tabSize)
              , L.indent (L.align (semiMap layoutVDefg vdefgs), tabSize)
              , L.str "\n"]

end


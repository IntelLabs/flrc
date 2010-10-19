(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature RENAME = sig

    type t
    type variable = Identifier.variable

    (* No renamings *)
    val none     : t

    (* Add a specified renaming *)
    val renameTo : t * variable * variable -> t

    (* If the variable is renamed, return the new variable,
     * otherwise return the old. *)
    val use      : t * variable -> variable

    (* If the variable is renamed, return the new variable,
     * otherwise return NONE. *)
    val use'     : t * variable -> variable option

    (* Return if variable is renamed *)
    val renamed  : t * variable -> bool

    (* Compose a specified renaming before *)
    val renameBefore : t * variable * variable -> t

    (* Compose a specified renaming after *)
    val renameAfter  : variable * variable * t -> t

    (* Outputs a list of pairs of the renamings *)
    val toList : t -> (variable * variable) list

    (* Reverse a renaming *)
    val invert : t -> t

    (* Compse two renamins e.g. compose (g, f) = Lookup(g, Lookup(f, x)) *)
    val compose      : t * t -> t

    val layout       : t -> Layout.t    
end


structure Rename :> RENAME =
struct

    structure VD = Identifier.VariableDict

    type variable = Identifier.variable   

    type t = variable VD.t

    val none = VD.empty

    fun renamed (map, x) = VD.contains(map, x)

    fun renameTo (map, x, y) =
        if (x = y) then map 
        else if renamed(map,x)  then
            Fail.fail ("Rename", 
                       "renameTo", 
                       "saw repeated variable " ^ Identifier.variableString' x)
        else
            let
                val map = VD.insert(map,x,y)
            in 
                map
            end

    fun use' (map, x) = VD.lookup(map, x)

    fun use (r, x) = 
        case use'(r,x)
         of SOME y => y
          | NONE => x
   

    fun renameBefore (map, x, y) = 
        if (x = y) then map
        else 
            let
                val y = use(map, y)
                val map = renameTo(map, x, y)
            in 
                map
            end

    fun renameAfter (x, y, map) = 
        if (x = y) then map
        else 
          let
            val map = VD.map(map, fn(x', y') => if y' = x then y else y')
          in 
            if renamed(map, x) then
              map
            else
              renameTo(map, x, y)
          end

    fun toList (renaming) = VD.toList renaming

    fun invert (renaming) = 
        let
          fun swap (a, b) = (b, a)
         in
          VD.fromList (map swap (VD.toList renaming))
         end

    fun compose (map2, map1) =
        let
          fun go ([], map') = map'
            | go ((x, y)::rs, map') = 
              case (use' (map2, y))
               of SOME z => go (rs, renameTo(map', x, z))
                | NONE => go (rs, map')
        in go (VD.toList map1, none)
        end

    fun layout map = 
        let
           fun layoutP(v1,v2) = Layout.seq[
                                Identifier.layoutVariable' v1,
                                Layout.str"->",
                                Identifier.layoutVariable' v2]
        in
            VD.layout (map,layoutP)
        end

end;

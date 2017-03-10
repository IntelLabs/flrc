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

signature ANORM_STRICT_CLOSURE_CONVERT = 
sig
  val pass : (ANormStrict.t, ANormStrict.t * (Identifier.VariableSet.t Identifier.VariableDict.t)) Pass.t
end

structure ANormStrictClosureConvert :> ANORM_STRICT_CLOSURE_CONVERT = 
struct
  structure AS = ANormStrict
  structure ASL = ANormStrictLayout
  structure ASU = ANormStrictUtils
  structure ASC = ANormStrictClone
  structure CHU = CoreHsUtils
  structure I = Identifier
  structure VD = I.VariableDict
  structure DG = DepGraph
  structure IM = Identifier.Manager
  structure PD = PassData
  structure L = Layout
  structure LU = LayoutUtils
  structure VS = I.VariableSet

  val passname = "ANormStrictClosureConvert"

  structure Click = 
  struct
    val stats = []
  end (* structure Click *)

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  val fail = 
   fn (f, m) => Fail.fail ("anorm-strict-closure-convert.sml", f, m)

  val mkDebug = 
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val (debugPassD, debugPass) =
      mkDebug ("debug", "Debug according to debug level")

  val mkLevelDebug = 
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val (showAnalysisD, showAnalysis) =
      mkLevelDebug ("show-analysis", "print the analysis", 0)

  val debugs = [debugPassD, showAnalysisD]

  val mkFeature = 
   fn (tag, description) => PD.mkFeature (passname^":"^tag, description)

  val mkLogFeature = 
   fn (tag, description, level) => PD.mkLogFeature (passname, passname^":"^tag, description, level)

  val features = []

  val run : (PD.t * int * string) -> ('a -> 'b) -> 'a -> 'b = 
   fn (pd, level, name) => fn f => fn args => 
      let
        val () = Chat.log (pd, level, "Doing "^name)
        val s = Time.now ()
        val p = f args
        val e = Time.toString (Time.- (Time.now (), s))
        val () = Chat.log (pd, level, "Done with "^name^" in "^e^"s")
      in p
      end


  structure Analyze =
  struct
   (* function ids *)
   type fid = AS.var

   (* instruction ids *)
   type iid = AS.var

   type fvInfo = {frees : VS.t, extras : VS.t, escapes : bool, recursive : bool}
        
   structure DG = DepGraph

   type globalStatus = AS.var DG.status

   val GsLocal  = DG.DgTop
   val GsGlobal = DG.DgBot

   type callStatus = AS.var DG.status

   val CsStd    = DG.DgTop
   val CsFlat   = DG.DgBot

   (*  For every known call site, we keep track of:
    *
    *   callee: The callee (a named function)
    *
    *   caller: The caller (any function)
    *
    *   kind: A dataflow node tracking whether or not this call is
    *    suitable for flat calling.
    *
    *   available: The available set at the call site (see Context.t below)
    *)
   datatype callInfo = CI of {callee    : AS.var,
                              caller    : AS.var option,
                              kind      : callStatus ref,
                              available : VS.t}
  
   (* For every function, we keep track of:
    *
    *  calls: A list of the direct calls (by name) to this function
    *
    *  kind: A dataflow node tracking whether or not this function
    *   is suitable for flat calling.  It is the join of all of its
    *   direct calls, and is Top if there are any unknown calls (escapes)
    *
    *  frees: The set of syntactically free variables of the function.
    *
    *  available: A subset of the variables in frees which is available
    *    at all call sites.
    *
    *  escapes: True if there could be any unknown calls, false otherwise
    *
    *  recursive: True if the function escapes, or if it is part of a cycle
    *)
   datatype functionInfo = FI of {calls     : callInfo list ref,
                                  kind      : callStatus ref,
                                  frees     : VS.t ref,
                                  available : VS.t ref,
                                  escapes   : bool ref,
                                  recursive : bool ref}

   structure State = 
   struct
     (* The global state is initialized with entries for every object
      * of interest.  There is only ever one global state object, and it
      * is never updated.  The status of the objects is changed by 
      * mutating the embedded references.  
      *
      *  functions: Function information for every function 
      *
      *  variables: For every variable, keep a dataflow node indicating
      *   whether it can be globalized or not.  A variable can be globalized
      *   if it is appropriate, and either all of its free variables are 
      *   globals or it is a flat called closure.
      *
      *  calls: For every call instruction, map the result variable to the
      *   the target of the call, if known.
      *
      *)
     datatype t = S of {functions     : functionInfo VD.t,
                        variables     : globalStatus ref VD.t,
                        calls         : AS.var VD.t}

     val mk :  {functions     : functionInfo VD.t ref,
                variables     : globalStatus ref VD.t ref} -> t = 
      fn {functions, variables} =>
         S {functions     = !functions, 
            variables     = !variables,
            calls         = VD.empty}

     val ((_, getFunctions), 
          (_, getVariables), 
          (_, getCalls)) =
         let
           val r2t = fn (S {functions, variables, calls}) => 
                        (functions, variables, calls)
           val t2r = fn (functions, variables, calls) => 
                        S {functions = functions, 
                           variables = variables, 
                           calls     = calls}
         in 
           FunctionalUpdate.mk3 (r2t, t2r)
         end

     val layout : t * Config.t * AS.symbolTable -> Layout.t = 
      fn (t, config, symbolTable) => 
         let
           val var = fn v => ASL.var (config, I.SymbolInfo.SiTable symbolTable, v)
           val map = 
            fn (ld, lr) => fn (d, r) => L.seq [ld d, L.str " => ", lr r]
           val option =
            fn f => 
            fn po => case po
                      of SOME p => f p
                       | NONE   => L.str "_"
           val globalStatus = 
            fn gi => DG.layout (gi, L.str "Local", L.str "Global", option var)
           val callStatus = 
            fn ci => DG.layout (ci, L.str "Std", L.str "Flat", option var)
           val callInfo = 
            fn (CI {callee, caller, kind, available}) => 
               L.mayAlign [L.seq[var callee, L.str "->", option var caller, 
                                 L.str " called as ", callStatus kind], 
                           LU.indent (L.seq [L.str " with ", VS.layout (available, var)])]
           val funInfo = 
            fn (f, FI {calls, frees, available, kind, escapes, recursive}) => 
               let
                 val name = var f
                 val escapes = if !escapes then L.str "^" else L.str ""
                 val recursive = if !recursive then L.str "*" else L.str ""
                 val kind = callStatus kind
                 val frees = VS.layout (!frees, var)
                 val available = VS.layout (!available, var)
                 val calls = L.mayAlign (List.map (!calls, callInfo))
               in L.align [L.seq [name, L.str "=", name, escapes, recursive],
                           LU.indent (L.seq [L.str "Called as: ", kind]),
                           LU.indent (L.seq [L.str "Has frees: ", frees]),
                           LU.indent (L.seq [L.str "Has available: ", available]),
                           LU.indent (L.seq [L.str "Has calls: ", calls])]
               end
           val functions = VD.layout (getFunctions t, funInfo)
           val variables = VD.layout (getVariables t, map (var, globalStatus))
           val calls = VD.layout (getCalls t, map (var, var))
         in 
           L.align [L.str "Functions", LU.indent functions,
                    L.str "Variables", LU.indent variables,
                    L.str "Calls", LU.indent calls]
         end
   end (* structure State *)

   structure  Env = 
   struct

     datatype t = E of {pd : PassData.t,
                        symbolTable : AS.symbolTable}

     val mk : PassData.t * AS.symbolTable -> t = fn (pd, symbolTable) => E {pd = pd, symbolTable = symbolTable}

     val ((_, getPd), (_, getSymbolTable)) =
         FunctionalUpdate.mk2 (fn (E {pd, symbolTable}) => (pd, symbolTable),
                               fn (pd, symbolTable) => E {pd = pd, symbolTable = symbolTable})


     val getConfig = PD.getConfig o getPd
   end (* structure Env *)

   structure Scope = 
   struct
     (* A scope is a partition of the bound variables in scope at a given point
      * in the program, along with a set of free variables associated with
      * each partition.  
      *
      * The available sets partition the bound variables in scope.  The union
      * of the available sets is the set of variables in scope.  Each available/free
      * set pair tracks the free variables of an object: the available set for the
      * object contains the set of variables which came into scope locally within
      * the object (and hence are not free with respect to the object) and the
      * free set contains the free variables of the object encountered so far.
      * When a variable comes into scope, it is added to the top available set.  
      * When a variable is used, it is added to every free set above the available 
      * set in which it came into scope.
      *)
     datatype t = S of {available : VS.t,
                        frees     : VS.t ref,
                        scopes    : {available : VS.t, frees : VS.t ref} list}

     val ((setAvailable, getAvailable), 
          (setFrees, getFrees), 
          (setScopes, getScopes)) =
         FunctionalUpdate.mk3
           (fn (S {available, frees, scopes}) => (available, frees, scopes),
            fn (available, frees, scopes) => 
               S {available = available, frees = frees, scopes = scopes})

     val new : unit -> t = 
      fn () => S {available = VS.empty, frees = ref VS.empty, scopes = []}

     val push : t * VS.t * VS.t ref -> t =
      fn (t, available, frees) => 
         let
           val t = setScopes (t, {available = getAvailable t, frees = getFrees t} :: getScopes t)
           val t = setAvailable (t, available)
           val t = setFrees (t, frees)
         in t
         end

     val top : t -> {available : VS.t, frees : VS.t ref} = 
      fn t => {available = getAvailable t, frees = getFrees t}

     val pop : t -> t option = 
      fn t => case getScopes t
               of []                         => NONE
                | {available, frees}::scopes => SOME (S {available = available, frees = frees, scopes = scopes})

     val pushNew : t -> t = 
      fn t => push (t, VS.empty, ref VS.empty)

     val addToAvailable : t * AS.var -> t = 
      fn (t, v) => setAvailable (t, VS.insert (getAvailable t, v))

   end

   structure Context = 
   struct
     (* The analysis is done relative to a context which tracks the relevant
      * information about the current program context.
      *
      *  active: the set of recursively defined variables currently being defined.
      *   This is the union of the variables bound by all of the Recs in 
      *   which we are currently nested.  If a variable in the active set is 
      *   used, then that variable is recursive.
      *
      *  fScopes: The function scope stack.  Each entry in this scope stack
      *   corresponds to a function in which we are currently nested.  A
      *   used variable is added to every free set in which we are nested
      *   up to the scope in which it was introduced (the first available
      *   set in which it appears).
      *
      *  gScopes: The global object stack. Each entry in this scope stack 
      *   corresponds to a potentially global object in which we are 
      *   currently nested.  A used variable is added to every free set in 
      *   which we are nested up to the scope in which it was introduced 
      *   (the first available set in which it appears).
      *
      *  current: The function/instruction nesting stack in which we currently
      *   are located, starting at the most recent function.  
      *   If current = (b, [i1, i2]) the inner most function in which we are
      *   nested is b, i1 is nested within b, and i2 is nested within i1.
      *
      *)
     datatype t = C of {active  : VS.t,
                        fScopes : Scope.t,
                        gScopes : Scope.t,
                        current : fid option}

     val ((setActive, getActive),
          (setFScopes, getFScopes),
          (setGScopes, getGScopes),
          (setCurrent, getCurrent)) =
         FunctionalUpdate.mk4
           (fn (C {active, fScopes, gScopes, current}) => (active, fScopes, gScopes, current),
            fn (active, fScopes, gScopes, current) => 
               C {active = active, fScopes = fScopes, gScopes = gScopes, current = current})
     
     val new =
      fn bid => C {active = VS.empty, fScopes = Scope.new (), gScopes = Scope.new (), current = bid}
                
     val enterFunction : t * fid * VS.t ref -> t = 
      fn (t, fid, frees) => 
         let
           val t = setFScopes (t, Scope.push (getFScopes t, VS.empty, frees))
           val t = setCurrent (t, SOME fid)
         in t
         end

     val enterGlobal : t -> t = 
      fn t => setGScopes (t, Scope.pushNew (getGScopes t))

     val addToAvailable : t * AS.var -> t = 
      fn (t, v) => 
         let
           val t = setFScopes (t, Scope.addToAvailable (getFScopes t, v))
           val t = setGScopes (t, Scope.addToAvailable (getGScopes t, v))
         in t
         end

     val addToActive : t * AS.var -> t = 
      fn (t, v) => setActive (t, VS.insert (getActive t, v))

   end (* structure Context *)

   val variableToString : State.t * Env.t * AS.var -> string = 
    fn (state, env, v) => Identifier.variableString (Env.getSymbolTable env, v)

   val iidToString : State.t * Env.t * iid -> string = variableToString
                                                       
   val lookupFunctionInfo : State.t * Env.t * fid -> functionInfo option =
    fn (state, env, fid) => VD.lookup (State.getFunctions state, fid)

   val getFunctionInfo : State.t * Env.t * fid -> functionInfo = 
    fn (state, env, fid) =>
       (case lookupFunctionInfo (state, env, fid)
         of SOME info => info
          | NONE      => fail ("getFunction", "Not a function fid: " ^ variableToString (state, env, fid)))

   val isFid : State.t * Env.t * fid -> bool = 
    fn (state, env, fid) => isSome (lookupFunctionInfo (state, env, fid))

   val getFunctionFrees : State.t * Env.t * fid -> VS.t ref = 
    fn (state, env, fid) => 
       let
         val FI {frees, ...} = getFunctionInfo (state, env, fid)
       in frees
       end

   val getVariableInfo : State.t * Env.t * AS.var -> globalStatus ref = 
    fn (state, env, v) =>
       let
         val variables = State.getVariables state
         val info = 
             case VD.lookup (variables, v)
              of SOME info => info
               | NONE      => fail ("getVariableInfo", "Unknown variable: " ^ variableToString (state, env, v))
       in info
       end

   val addAvailable : Context.t * AS.var -> Context.t =
       fn (context, v) => Context.addToAvailable (context, v)

   val addAvailablesV : Context.t * AS.var Vector.t -> Context.t = 
    fn (context, vars) => Vector.fold (vars, context, fn (v, context) => addAvailable (context, v))

   val addAvailables : Context.t * AS.var List.t -> Context.t = 
    fn (context, vars) => List.fold (vars, context, fn (v, context) => addAvailable (context, v))

   val addAvailablesFst : Context.t * (AS.var * 'a) list -> Context.t = 
    fn (context, vars) => List.fold (vars, context, fn ((v, _), context) => addAvailable (context, v))

   val markVariableGlobal : State.t * Env.t * AS.var -> unit = 
    fn (state, env, v) => getVariableInfo (state, env, v) := GsGlobal (SOME v)

   val markVariableLocal : State.t * Env.t * AS.var -> unit = 
    fn (state, env, v) => getVariableInfo (state, env, v) := GsLocal (SOME v)

   val markVariablesLocalV : State.t * Env.t * (AS.var Vector.t) -> unit = 
    fn (state, env, v) => Vector.foreach (v, fn v => markVariableLocal (state, env, v))

   val markVariablesLocal : State.t * Env.t * (AS.var List.t) -> unit = 
    fn (state, env, vs) => List.foreach (vs, fn v => markVariableLocal (state, env, v))

   val markVariableDependsOn : State.t * Env.t * AS.var * AS.var -> unit = 
    fn (state, env, v1, v2) => 
       let
         val r1 = getVariableInfo (state, env, v1)
         val r2 = getVariableInfo (state, env, v2)
       in DG.either (r1, r2)
       end

   val markVariableDependsOnAll : State.t * Env.t * AS.var * VS.t -> unit = 
    fn (state, env, v1, vs) => VS.foreach (vs, fn v2 => markVariableDependsOn (state, env, v1, v2))

   val functionEscapes : State.t * Env.t * AS.var -> unit = 
    fn (state, env, fid) => 
       let
         val FI {escapes, recursive, kind, ...} = getFunctionInfo (state, env, fid)
         val () = escapes := true
         val () = recursive := true
         val () = kind := CsStd (DG.info kind)
       in ()
       end

   val variableEscapes : State.t * Env.t * AS.var -> unit = 
    fn (state, env, v) => 
       if isFid (state, env, v) then
         functionEscapes (state, env, v)
       else
         ()

   val variableIsRecursive : State.t * Env.t * AS.var -> unit = 
    fn (state, env, v) => 
       (case lookupFunctionInfo (state, env, v)
         of SOME (FI {recursive, ...}) => recursive := true
          | NONE                       => ())

   (*  A variable which is used in a context is added to every free set up to the 
    *  scope in which it was introduced.  If the variable is active, it is marked
    *  as recursive.  
    *)
   val variableAppearsInContext : State.t * Env.t * Context.t * AS.var -> unit = 
    fn (state, env, context, v) => 
       let
         val rec loop = 
          fn scopes => 
             let
               val {available, frees} = Scope.top scopes
               val () = 
                   if VS.member (available, v) then () else 
                   let
                     val () = frees := VS.insert (!frees, v) 
                   in case Scope.pop scopes
                       of NONE        => ()
                        | SOME scopes => loop scopes
                   end
             in ()
             end

         val () = loop (Context.getFScopes context)
         val () = loop (Context.getGScopes context)
         val () = if VS.member (Context.getActive context, v) then
                    variableIsRecursive (state, env, v)
                  else 
                    ()
       in ()
       end

   (* Function variables used in a call position are not marked as escaping. *)
   val variableUseNonEscaping : State.t * Env.t * Context.t * AS.var -> unit =
    fn (state, env, context, v) => variableAppearsInContext (state, env, context, v)

   (* General variable uses are marked escaping. *)
   val variableUse : State.t * Env.t * Context.t * AS.var -> unit =
    fn (state, env, context, v) => 
       let
         val () = variableAppearsInContext (state, env, context, v) 
         val () = variableEscapes (state, env, v)
       in ()
       end

   val variablesUse : State.t * Env.t * Context.t * AS.var list -> unit =
    fn (state, env, context, vs) => List.foreach (vs, fn v => variableUse (state, env, context, v))

   val variablesUseV : State.t * Env.t * Context.t * AS.var Vector.t -> unit =
    fn (state, env, context, vs) => Vector.foreach (vs, fn v => variableUse (state, env, context, v))

   val isGlobalCandidate : State.t * Env.t * AS.exp -> bool = 
    fn (state,env, exp) =>
       (case exp 
         of AS.Return _        => true
          | AS.ConApp _        => true
          | AS.Lit _           => true
          | AS.Cast AS.NullRef => true
          | AS.Cast (AS.Bottom _) => true
          | _                  => false)

   val doCall : State.t * Env.t * Context.t * AS.var * AS.var List.t -> unit = 
    fn (state, env, context, f, vs) => 
       let
         val () = variableUseNonEscaping (state, env, context, f) 
         val () = variablesUse (state, env, context, vs)
         val () = 
             case lookupFunctionInfo (state, env, f)
              of SOME (FI {calls, kind, ...}) => 
                 let
                   val callKind = ref (CsFlat (SOME f))
                   val () = DG.either (kind, callKind)
                   val bc = Context.getCurrent context
                   val available = Scope.getAvailable (Context.getFScopes context)
                   val ci = CI {callee = f, caller = bc, kind = callKind, available = available}
                 in calls := ci :: !calls
                 end
               | NONE                   => ()
       in ()
       end

   val doCast : State.t * Env.t * Context.t * AS.cast -> unit =
    fn (state, env, context, cast) => 
       let 
         val () =
             case cast
              of AS.FromAddr v => variableUse (state, env, context, v)
               | AS.ToAddr v   => variableUse (state, env, context, v)
               | AS.NullRef    => ()
               | AS.Bottom v   => variableUse (state, env, context, v)
       in ()
       end

   val rec doCase : State.t * Env.t * Context.t * AS.var * AS.alt list -> unit = 
    fn (state, env, context, v, alts) => 
       let
         val () = variableUse (state, env, context, v)
         val doAlt =
          fn alt => 
             (case alt
               of AS.Acon (c, vbs, e) => 
                  let
                    val vs = List.map (vbs, #1)
                    val () = markVariablesLocal (state, env, vs)
                    val context = addAvailables (context, vs) 
                  in doExp (state, env, context, e)
                  end
                | AS.Alit (l, t, e) => doExp (state, env, context, e)
                | AS.Adefault e => doExp (state, env, context, e))
         val () = List.foreach (alts, doAlt)
       in ()
       end
   and rec doExp : State.t * Env.t * Context.t * AS.exp -> unit =
    fn (state, env, context, e) =>
       let
         val () = 
             case e
              of AS.Return vs                 => variablesUse (state, env, context, vs)
               | AS.PrimApp (s, vs)           => variablesUse (state, env, context, vs)
               | AS.ExtApp (p, cc, s, t, vs)  => variablesUse (state, env, context, vs)
               | AS.ConApp (c, vs)            => variablesUse (state, env, context, vs)
               | AS.App (f, vs, _)            => doCall (state, env, context, f, vs)
               | AS.Let (vdg, e)              => doExp (state, env, doVDefG (state, env, context, vdg), e)
               | AS.Case (v, alts)            => doCase (state, env, context, v, alts)
               | AS.Lit _                     => ()
               | AS.Cast cast                 => doCast (state, env, context, cast)
               | AS.Eval v                    => variableUse (state, env, context, v)
       in ()
       end
   and rec doVDef : State.t * Env.t * Context.t * AS.vDef * bool -> unit = 
    fn (state, env, context, vd, r) => 
       let
         val doIt = 
          fn (v, args, g, body) => 
             let
               val context = if g then Context.enterGlobal context else context
               val () =
                   let
                     val context = Context.enterFunction (context, v, getFunctionFrees (state, env, v))
                     val context = if r then addAvailable (context, v) else context
                     val argvs = List.map (args, #1)
                     val () = markVariablesLocal (state, env, argvs)
                     val context = addAvailables (context, argvs)
                     val () = doExp (state, env, context, body)
                   in ()
                   end
               val () = 
                   if g then 
                     let
                       val {frees, ...} = Scope.top (Context.getGScopes context)
                       val () = markVariableDependsOnAll (state, env, v, !frees)
                     in ()
                     end
                   else
                     markVariableLocal (state, env, v)
             in ()
             end
         val () = 
             doIt (case vd 
                    of AS.Vfun {name, args, body, ...} => (name, args, true, body)
                     | AS.Vthk {name, body, ...}       => 
                       let
                         val g = case body 
                                  of AS.Return v => true
                                   | _           => false
                       in (name, [], g, body)
                       end)
       in ()
       end

   and rec doRecVDefs : State.t * Env.t * Context.t * AS.vDef List.t -> Context.t =
    fn (state, env, context, vds) => 
       let
         val vs = ASU.VDef.variablesDefd vds
         val context = addAvailables (context, vs)
         val () = 
             let
               val context = List.fold (vs, context, fn (v, context) => Context.addToActive (context, v))
               val () = List.foreach (vds, fn vd => doVDef (state, env, context, vd, true))
             in ()
             end
       in context
       end

   and rec doNonRecVDef : State.t * Env.t * Context.t * AS.vDef -> Context.t =
    fn (state, env, context, vd) => 
       let
         val () = doVDef (state, env, context, vd, false)
         val context = addAvailable (context, ASU.VDef.variableDefd vd)
       in context
       end

   and rec doVDefG : State.t * Env.t * Context.t * AS.vDefg -> Context.t = 
    fn (state, env, context, vdg) => 
       (case vdg
         of AS.Rec vDefs => doRecVDefs (state, env, context, vDefs)
          | AS.Nonrec vDef => doNonRecVDef (state, env, context, vDef)
          | AS.Vdef (bs, exp) => 
            let
              val () = 
                  let
                    val g = isGlobalCandidate (state, env, exp) andalso (List.length bs = 1)
                    val context = 
                        if g then 
                          Context.enterGlobal context
                        else
                          context
                    val () = doExp (state, env, context, exp)
                    val () = 
                        if g then
                          let
                            val (v,_) = hd bs
                            val {frees, ...} = Scope.top (Context.getGScopes context)
                            val () = markVariableDependsOnAll (state, env, v, !frees)
                          in ()
                          end
                        else
                          List.foreach (bs, fn (v, _) => markVariableLocal (state, env, v))
                  in ()
                  end
              val context = addAvailablesFst (context, bs)
            in context
            end)

   and rec doVDefGs : State.t * Env.t * Context.t * AS.vDefg List.t -> Context.t = 
    fn (state, env, context, vdgs) =>
       List.fold (vdgs, context, fn (vdg, context) => doVDefG (state, env, context, vdg))

   val doModule : State.t * Env.t * AS.module -> unit = 
    fn (state, env, AS.Module (main, vdgs)) => 
       let
         val context = Context.new NONE
         val context = doVDefGs (state, env, context, vdgs)
         val () = variableUse (state, env, context, main)
       in ()
       end

   val doProgram : State.t * Env.t * AS.t -> unit = 
    fn (state, env, (m, im, tm)) => doModule (state, env, m)

   (*  Before running the analysis, entries are allocated for all of the objects
    *  of interest.  Dataflow nodes are introduced with a reasonable default 
    *  initial value.
    *)         
   structure Init = 
   struct
     type state = 
          {functions     : functionInfo VD.t ref,
           variables     : globalStatus ref VD.t ref}
          
     type env = Config.t

     val initVDefInfo : state * AS.vDef -> unit = 
      fn (state, vd) => 
         let
           val v = ASU.VDef.variableDefd vd
           val (v, kind) = 
               case vd
                of AS.Vthk {name, ...} => (name, CsStd (SOME name))
                 | AS.Vfun {name, ...} => (name, CsFlat (SOME name))
           val {functions as ref fi, ...} = state
           val info = FI {calls     = ref [],
                          frees     = ref VS.empty,
                          available = ref VS.empty, 
                          kind      = ref kind,
                          escapes   = ref false,
                          recursive = ref false}
           val () = functions := VD.insert (fi, v, info)
         in ()
         end


     val initVariableInfo : state * AS.var -> unit =
      fn ({variables as ref vi, ...}, v) => 
         variables := VD.insert (vi, v, ref (DG.DgBot (SOME v)))

     val variableBind' : (state * env * AS.var -> env) =
      fn (state, env, v) => let val () = initVariableInfo (state, v) in env end

     val analyzeVDef' : (state * env * AS.vDef -> unit) = 
      fn (state, env, vd) => initVDefInfo (state, vd)

     val config : env -> Config.t = fn env => env
     val variableBind       : (state * env * ANormStrict.var -> env) option = SOME variableBind'
     val variableUse        : (state * env * ANormStrict.var -> unit) option = NONE
     val analyzeTy          : (state * env * ANormStrict.ty -> unit) option  = NONE
     val analyzeExp         : (state * env * ANormStrict.exp -> unit) option = NONE
     val analyzeAlt         : (state * env * ANormStrict.alt -> unit) option = NONE
     val analyzeVDef        : (state * env * ANormStrict.vDef -> unit) option = SOME analyzeVDef'
     val analyzeVDefg       : (state * env * ANormStrict.vDefg -> env) option = NONE
   end (* structure Prep *)

   structure Prep = ANormStrictAnalyzeF (Init)

   (* Run the dataflow analyses and use the results to decide the cut 
    * information, the globals, and the flat calls.
    *)
   structure Finalize = 
   struct
     (* A variable is global if all of its free variables are global, or 
      * if it is a function which is flat called.
      *
      * A call can be a flat call if all of its free variables are global,
      * or local to the enclosing function, or already in the closure of 
      * the enclosing function.
      *
      * A function is flat called if all of its calls can be flat calls.
      *
      * Note that these definitions are mutually dependent.  To compute 
      * a solution to these contraints, the analysis above makes each
      * potential global a dataflow node which is the disjunction of
      * the global status of each of its free variables: if any of its
      * free variables must be local (Top) it must be local as well.
      * It also makes the function kind nodes the disjunction of the call
      * kind nodes: if any call is standard (Top) then the function must
      * be standard.
      * 
      * In the analysis below, we add two more sets of contraints:
      * For each call, we compute a residual set of variables 
      * consisting of those which are neither local nor already
      * in the enclosing functions closure.  The call kind node
      * is the disjunction of these, indicating that if any
      * variable in the residual is local (Top) then the call
      * must be a standard call.
      * For each function, we add a conjuction constraint to 
      * function variables indicating that a function is only local
      * if it has local free variables *and* it is standard called:
      * that is, we make its final global status the conjunction of its
      * preliminary global status and its kind.
      *
      * With this structure in place, we simply decide each node and
      * use the result.
      *  
      *)
     val finalizeGlobalsAndFunctions : State.t * Env.t -> unit = 
      fn (state, env) => 
         let
           val variables = State.getVariables state
           val functions = State.getFunctions state

           val () = 
               let
                 val addResidual =
                  fn (CI {caller, available, kind, ...}, calleeFrees, calleeAvailable) => 
                     let
                       val callerFrees = 
                           case caller 
                            of SOME f => 
                               let
                                 val FI {frees as ref callerFrees, ...} = getFunctionInfo (state, env, f)
                               in callerFrees
                               end
                               | NONE => VS.empty
                       val inScopeAtCall = VS.union (callerFrees, available)
                       val residual = VS.difference (calleeFrees, inScopeAtCall)
                       val () = calleeAvailable := VS.intersection (inScopeAtCall, !calleeAvailable)
                       val () = VS.foreach (residual, fn v => DG.either (kind, getVariableInfo (state, env, v)))
                     in ()
                     end

                 val addResiduals = 
                  fn (bid, FI {calls, frees as ref calleeFrees, available, escapes, ...}) => 
                     let
                       val () = if !escapes then 
                                  available := VS.empty
                                else
                                  available := calleeFrees
                     in 
                       List.foreach (!calls, fn call => addResidual (call, calleeFrees, available))
                     end
                     
               in VD.foreach (functions, addResiduals)
               end

           val () = 
               let
                 val add = 
                  fn (v, status) => 
                     case lookupFunctionInfo (state, env, v)
                      of SOME (FI {kind, ...}) => DG.both (status, kind)
                       | NONE                  => ()
               in VD.foreach (variables, add)
               end

           val () = VD.foreach (variables, fn (_, s) => DG.decide s)
           val () = VD.foreach (functions, fn (_, FI {kind, ...}) => DG.decide kind)
         in ()
         end


     val finalize : State.t * Env.t -> unit = finalizeGlobalsAndFunctions
   end (* structure Finalize *)

   (* Use the computed information to extract out
    * the global and free variable information.
    *)
   structure Extract = 
   struct
     val globals : State.t * Env.t -> VS.t =
      fn (state, env) =>
         let
           val add = 
            fn (v, status, globals) => 
               case !status 
                of DG.DgBot _ => VS.insert (globals, v)
                 | DG.DgTop _ => globals
                 | _          => fail ("Extract.globals", "Undecided global")
           val globals = VD.fold (State.getVariables state, VS.empty, add)
         in globals
         end

     val functions : State.t * Env.t * VS.t -> fvInfo VD.t =
      fn (state, env, globals) => 
         let
           val toInfo = 
            fn (f, info) => 
               let
                 val FI {calls, frees, available, kind, escapes, recursive} = info
                 val frees = VS.difference (VS.difference (!frees, globals), !available)
                 val extras = VS.difference (!available, globals)
                 val info = {frees = frees, extras = extras, escapes = !escapes, recursive = !recursive}
               in info
               end
           val info = VD.map (State.getFunctions state, toInfo)
         in info
         end

     val extract : State.t * Env.t -> (VS.t * fvInfo VD.t) = 
      fn (state, env) =>
         let
           val globals = globals (state, env)
           val info = functions (state, env, globals)
         in (globals, info)
         end
   end (* structure Extract *)

   val init : Config.t * AS.t -> State.t = 
    fn (config, p) => 
       let
         val state = 
             {functions     = ref VD.empty,
              variables     = ref VD.empty}
         val env = config
         val () = Prep.program (state, env, p)
         val state = State.mk state
       in state
       end

   val show : State.t * Env.t -> unit = 
    fn (state, env) =>
       if showAnalysis (Env.getPd env) then
         LU.printLayout (State.layout (state, Env.getConfig env, Env.getSymbolTable env))
       else
         ()

   val program : PD.t * AS.t -> (VS.t * fvInfo VD.t) =
    fn (pd, p as (m, symbolTable, tm)) =>
       let
         val config = PD.getConfig pd
         val env = Env.mk (pd, symbolTable)
         val state = run (pd, 2, "Initialization") init (config, p)
         val () = show (state, env)
         val () = run (pd, 2, "Closure Analysis") doProgram (state, env, p)
         val () = show (state, env)
         val () = run (pd, 2, "Finalization") Finalize.finalize (state, env)
         val () = show (state, env)
         val interface = run (pd, 2, "Result extraction") Extract.extract (state, env)
       in interface
       end

   val debugs = [showAnalysisD]

  end; (* structure Analyze *)

  structure Rewrite = 
  struct

    type fvInfo = Analyze.fvInfo

    structure State = 
     struct
     
     datatype t = S of {stm : AS.symbolTableManager,
                        funAliases : VS.t VD.t ref}
                       
     val mk : AS.symbolTableManager -> t = 
      fn (stm) => S {stm = stm, funAliases = ref VD.empty}

     val ((_, getStm),
          (_, getFunAliases)) =
         FunctionalUpdate.mk2 (fn (S {stm, funAliases}) => (stm, funAliases),
                               fn (stm, funAliases) => S {stm = stm, funAliases = funAliases})
                              
     end (* structure State *)
    structure  Env = 
     struct
     
       datatype t = E of {pd : PD.t,
                          fvInfo : fvInfo VD.t,
                          fns    : AS.var VD.t,
                          rename : Rename.t}
                         
       val mk : PD.t * fvInfo VD.t -> t = 
        fn (pd, fvInfo) => E {pd = pd, 
                              fvInfo = fvInfo,
                              fns    = VD.empty,
                              rename = Rename.none}
                                    
       val ((_, getPd), (_, getFvInfo), (setFns, getFns), (setRename, getRename)) =
           FunctionalUpdate.mk4 (fn (E {pd, fvInfo, fns, rename}) => (pd, fvInfo, fns, rename),
                                 fn (pd, fvInfo, fns, rename) => E {pd = pd, 
                                                                    fvInfo = fvInfo,
                                                                    fns    = fns,
                                                                    rename = rename})
           
       val getConfig = PD.getConfig o getPd
     end (* structure Env *)
     

    type state = State.t
    type env = Env.t 

    (* addFunAlias (state, env, v1, v2) means v2 is an alias for v1 *)
    val addFunAlias : State.t * Env.t * AS.var * AS.var -> unit = 
     fn (state, env, v1, v2) => 
        let
          val fa = State.getFunAliases state
          val s = case VD.lookup (!fa, v1)
                   of SOME s => s
                    | NONE => VS.empty
          val s = VS.insert (s, v2)
          val () = fa := VD.insert (!fa, v1, s)
        in ()
        end

    val addFunAliases : State.t * Env.t * AS.var List.t * AS.var List.t -> unit = 
     fn (state, env, vs1, vs2) => List.foreach2 (vs1, vs2, fn (v1, v2) => addFunAlias (state, env, v1, v2))

    val variableTy : State.t * Env.t * AS.var -> AS.ty = 
     fn (state, env, v) => 
        let 
          val (t, _) = IM.variableInfo (State.getStm state, v) 
        in t 
        end

    val varsToBinds : State.t * Env.t * AS.var List.t -> (AS.var * AS.ty) List.t = 
     fn (state, env, vs) => 
        let
          val stm = State.getStm state
          val doOne = 
           fn v => (v, variableTy (state, env, v)) 
        in List.map (vs, doOne)
        end

    val variableToString : State.t * Env.t * AS.var -> string = 
     fn (state, env, v) => IM.variableString (State.getStm state, v)

    val getFunctionFvs' : state * env * AS.var -> fvInfo option = 
     fn (state, env, f) => VD.lookup (Env.getFvInfo env, f)

    val getFunctionFvs : state * env * AS.var -> fvInfo = 
     fn (state, env, f) => 
        case getFunctionFvs' (state, env, f)
         of SOME r => r
          | NONE   => fail ("getFunctionFvs", "No info for variable "^variableToString (state, env, f))

    val getFunctionExtras : state * env * AS.var -> AS.var List.t = 
     fn (state, env, f) => 
        case getFunctionFvs' (state, env, f)
         of SOME {extras, ...} => VS.toList extras
          | NONE               => []

    val doVar : state * env * AS.var -> AS.var = 
        fn (state, env, v) => Rename.use (Env.getRename env, v)

    val doVars : state * env * AS.var List.t -> AS.var List.t = 
        fn (state, env, vs) => List.map (vs, fn v => doVar (state, env, v))

    val chooseNewVariables : state * env * AS.var List.t -> AS.var List.t * env = 
     fn (state, env, vs) => 
        let
          val chooseOne = 
           fn (v, env) => 
              let
                val v' = IM.variableClone (State.getStm state, v)
                val vl = L.str (variableToString (state, env, v))
                val vl' = L.str (variableToString (state, env, v'))
                val env = Env.setRename (env, Rename.renameAfter (v, v', Env.getRename env))
              in (v', env)
              end
        in Utils.List.mapFoldl (vs, env, chooseOne)
        end

    val derivedLocalVar : state * env * AS.var * string * AS.ty -> AS.var =
     fn (state, env, v, s, t) => IM.variableRelated (State.getStm state, v, s, (t, AS.VkLocal))

    val getFnVar : state * env * AS.var -> AS.var option = 
     fn (state, env, v) => VD.lookup (Env.getFns env, v)

    val setFnVar : state * env * AS.var * AS.var -> env = 
     fn (state, env, v, vf) => Env.setFns (env, VD.insert (Env.getFns env, v, vf))

    val rec doExp : state * env * AS.exp -> AS.exp =
     fn (state, env, e) => 
        let
          val return = fn e => e
          val r =
              case e
               of AS.Return vs => 
                  let
                    val vs = doVars (state, env, vs)
                  in return (AS.Return vs)
                  end
                | AS.PrimApp (s, vs) => 
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.PrimApp (s, vs)
                  in return e
                  end
                | AS.ExtApp (pname, cc, s, t, vs) => 
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ExtApp (pname, cc, s, t, vs)
                  in return e
                  end
                | AS.ConApp (c, vs) => 
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ConApp (c, vs)
                  in return e
                  end
                | AS.App (f, vs, effect) => 
                  let
                    val extras = getFunctionExtras (state, env, f)
                    val f = doVar (state, env, f)
                    val vs = doVars (state, env, vs)
                    val extras = doVars (state, env, extras)
                  in AS.App (f, vs @ extras, effect)
                  end
                | AS.Let (defG, e) => 
                  let
                    val defG = doVDefg (state, env, defG)
                    val e = doExp (state, env, e)
                  in return (AS.Let (defG, e))
                  end
                | AS.Case (v, alts) => 
                  let
                    val v = doVar (state, env, v)
                    val doAlt = 
                     fn alt => 
                        (case alt 
                          of AS.Acon (c, vbs, e) => AS.Acon (c, vbs, doExp (state, env, e))
                           | AS.Alit (l, t, e)   => AS.Alit (l, t, doExp (state, env, e))
                           | AS.Adefault e       => AS.Adefault (doExp (state, env, e)))
                    val alts = List.map (alts, doAlt)
                    val e = AS.Case (v, alts)
                  in return e
                  end
                | AS.Lit (l, t) => 
                  let
                    val e = AS.Lit (l, t)
                  in return e
                  end
                | AS.Cast cast => 
                  let
                    fun doV v = doVar (state, env, v)
                    val cast = case cast 
                                 of AS.FromAddr v => AS.FromAddr (doV v)
                                  | AS.ToAddr v => AS.ToAddr (doV v)
                                  | _ => cast

                  in return (AS.Cast cast)
                  end
                | AS.Eval v => 
                  let
                    val v = doVar (state, env, v)
                  in AS.Eval v
                  end
        in r
        end

    and rec doVDef0 : state * env * AS.vDef -> unit = 
     fn (state, env, vd) => 
        let
          val () = 
              case vd
               of AS.Vfun {name, ty, ...} => 
                  let
                    val {frees, extras, ...} = getFunctionFvs (state, env, name)
                    val extraTs = List.map (VS.toList extras, fn v => variableTy (state, env, v))
                    val ty = 
                        case TypeRep.repToBase ty
                         of AS.Arr (ts, rts, effect) => AS.Arr (ts @ extraTs, rts, effect)
                          | _                => fail ("doVDef0", "Vfun typ is not an arrow typ")
                    val () = IM.variableSetInfo (State.getStm state, name, (TypeRep.newRep_ ty, AS.VkLocal))
                  in ()
                  end
                | AS.Vthk _ => ()
        in ()
        end


    and rec doVDef : state * env * AS.vDef -> AS.vDef = 
     fn (state, env, vd) => 
        let
          val r = 
              case vd
               of AS.Vfun {name, ty, escapes = escapes0, recursive = recursive0, fvs, args, body} => 
                  let
                    val {frees, extras, escapes, recursive} = getFunctionFvs (state, env, name)
                    val fvs = doVars (state, env, VS.toList frees)
                    val extras0 = VS.toList extras
                    val extras = doVars (state, env, extras0)
                    val (extras, env) = chooseNewVariables (state, env, extras)
                    val () = addFunAliases (state, env, extras0, extras)
                    val body = doExp (state, env, body)
                    val args = args @ varsToBinds (state, env, extras)
                    val ty = variableTy (state, env, name)
                    val escapes = escapes0 andalso escapes
                    val recursive = recursive0 andalso recursive
                  in AS.Vfun {name = name, ty = ty, escapes = escapes, recursive = recursive, 
                              fvs = fvs, args = args, body = body}
                  end
                | AS.Vthk {name, ty, escapes = escapes0, recursive = recursive0, fvs, body} => 
                  let
                    val {frees, extras, escapes, recursive} = getFunctionFvs (state, env, name)
                    val fvs = VS.toList (VS.union (frees, extras))
                    val fvs = doVars (state, env, fvs)
                    val body = doExp (state, env, body)
                    val escapes = escapes0 andalso escapes
                    val recursive = recursive0 andalso recursive
                  in AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive, fvs = fvs, body = body}
                  end
        in r
        end

    and rec doVDefg : state * env * AS.vDefg -> AS.vDefg = 
     fn (state, env, vdg) => 
        let
          val r = 
              case vdg
               of AS.Rec vDefs => 
                  let
                    val () = List.foreach (vDefs, fn vd => doVDef0 (state, env, vd))
                    val vDefs = List.map (vDefs,  fn vd => doVDef (state, env, vd))
                  in AS.Rec vDefs
                  end
                | AS.Nonrec vDef => 
                  let
                    val () = doVDef0 (state, env, vDef)
                    val vDef = doVDef (state, env, vDef)
                  in AS.Nonrec vDef
                  end
                | AS.Vdef (vbs, e) => AS.Vdef (vbs, doExp (state, env, e))
        in r
        end

    val markGlobals : VS.t * AS.symbolTableManager -> unit = 
     fn (globals, stm) => 
        let
          val doIt = 
           fn v => 
              let
                val (t, _) = IM.variableInfo (stm, v)
                val () = IM.variableSetInfo (stm, v, (t, AS.VkGlobal))
              in ()
              end
          val () = VS.foreach (globals, doIt)
        in ()
        end

    val program : PD.t * VS.t * fvInfo VD.t * AS.t -> AS.t * (VS.t VD.t) =
     fn (pd, globals, fvInfo, (m, st, tm)) => 
        let
          val stm = IM.fromExistingAll st
          val state = State.mk stm
          val env = Env.mk (pd, fvInfo)
          val AS.Module (v, vdgs) = m
          val doOne = 
           fn vdg => doVDefg (state, env, vdg)
          val vdgs = List.map (vdgs, doOne)
          val v = doVar (state, env, v)
          val m = AS.Module (v, vdgs)
          val () = markGlobals (globals, stm)
          val st = IM.finish stm
          val p = (m, st, tm)
          val fa = !(State.getFunAliases state)
        in (p, fa)
        end

  end (* structure Rewrite *)

  val program : AS.t * PD.t -> AS.t * (VS.t VD.t) = 
   fn (p as (m, im, tm), pd) =>
      let
        val (globals, info) = run (pd, 1, "Analysis") Analyze.program (pd, p)
        val (p, fa) = run (pd, 1, "Rewriting") Rewrite.program (pd, globals, info, p)
      in (p, fa)
      end

  val stater = ANormStrictStats.layout (ANormStrictStats.O {id = SOME passname})

  val description = {name        = passname,
                     description = "ANormStrict closure converter",
                     inIr        = { printer = Utils.Function.flipIn ASL.layout,
                                     stater  = stater },
                     outIr       = { printer = fn ((p, fa), config) => ASL.layout (config, p),
                                     stater  = fn ((p, fa), config) => stater (p, config) },
                     mustBeAfter = [],
                     stats       = Click.stats}

  val associates = {controls = [], debugs = debugs, features = features, subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, program)

end

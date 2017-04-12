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


signature MIL_VECTORIZE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end (* signature MIL_VECTORIZE *)

structure MilVectorize :> MIL_VECTORIZE = 
struct
 
 structure I = Identifier
 structure IA = IntArb
 structure ID = IntDict
 structure L = Layout
 structure LD = Identifier.LabelDict
 structure LU = LayoutUtils      
 structure M = Mil
 structure ML = MilLoop
 structure MR = MilRename
 structure MU = MilUtils
 structure MP = Mil.Prims
 structure PU = MU.Prims.Utils
 structure SymTableM = Identifier.Manager
 structure U = Utils
 structure VS = Identifier.VariableSet
 structure VD = Identifier.VariableDict

 val passname = "MilVectorize" 

 val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil vectorize pass")

 val (whyFailD, whyFail) = Config.Debug.mk (passname ^ ":why-fail", "output reasons for failure to vectorize")

 val (allowReductionsF, allowReductions) = 
     Config.Feature.mk (passname^":float-reductions", "Vectorize floating point reductions")

 val allowReductions = fn config => allowReductions config orelse Config.sloppyFp config

 structure MDA = MilDependenceAnalysisF (struct
                                           type env = Config.t
                                           val getConfig = fn x => x
                                           val passname = passname
                                           val indent = 2
                                         end)

 (* env is the type of the environment passed through the 
    vectorizer *)
 datatype env = E of {config : Config.t,
                      vectorConfig : PU.VectorConfig.t,
                      program : Mil.t,
                      fmil : FMil.t,
                      code : Mil.code,
                      cfg : MilCfg.t,
                      domInfo : MilCfg.LabelDominance.t,
                      loopInfo : MilLoop.t}

 val ((setConfig, getConfig),
      (setVectorConfig, getVectorConfig),
      (setProgram, getProgram),
      (setFmil, getFmil),
      (setCode, getCode),
      (setCfg, getCfg),
      (setDomInfo, getDomInfo),
      (setLoopInfo, getLoopInfo)) = 
     let
       val r2t = 
        fn E {config, vectorConfig, program, fmil, code, cfg, domInfo, loopInfo} =>
           (config, vectorConfig, program, fmil, code, cfg, domInfo, loopInfo)
       val t2r = 
        fn (config, vectorConfig, program, fmil, code, cfg, domInfo, loopInfo) =>
           E {config = config, vectorConfig = vectorConfig, program = program, fmil = fmil, 
              code = code, cfg = cfg, domInfo = domInfo, loopInfo = loopInfo} 
      in FunctionalUpdate.mk8 (r2t, t2r)
      end

 datatype info = LI of {dependencyInfo : (MDA.varinfo VD.t),
                        loopHeader : M.label,
                        loopParams : (M.variable vector),
                        vectorLength : int option}

 val ((setDependencyInfo, getDependencyInfo),
      (setLoopHeader, getLoopHeader),
      (setLoopParams, getLoopParams),
      (setVectorLength, getVectorLength)) = 
     let
       val r2t = 
        fn LI {dependencyInfo, loopHeader, loopParams, vectorLength} =>
           (dependencyInfo, loopHeader, loopParams, vectorLength)
       val t2r = 
        fn (dependencyInfo, loopHeader, loopParams, vectorLength) =>
           LI {dependencyInfo = dependencyInfo, loopHeader = loopHeader, 
               loopParams = loopParams, vectorLength = vectorLength} 
      in FunctionalUpdate.mk4 (r2t, t2r)
      end

 val updateVectorLength : (info * int) -> info =
   fn (info, vectorLength) => setVectorLength (info, SOME vectorLength)

 val readVectorLength : info -> int = 
  fn info =>
     case getVectorLength info
      of SOME i => i
       | NONE   => Fail.fail ("Vectorize", "readVectorLength", "vectorLength not set")

 datatype state = S of {symTableM : MU.SymbolTableManager.t, 
                        globals : (M.variable * M.global) List.t ref}

 val ((_, stateGetSymTableM), 
      (_, stateGetGlobals)) = 
     let
       val r2t = 
        fn (S {symTableM, globals}) => (symTableM, globals)
       val t2r = 
        fn (symTableM, globals) => S {symTableM = symTableM, globals = globals}
     in FunctionalUpdate.mk2 (r2t, t2r)
     end

 val stateGetSi = fn state => I.SymbolInfo.SiManager (stateGetSymTableM state)
 (* Type-level "new" tag *)
 datatype 'a new = New of 'a
 fun tagAsNew x = New x

 (* Bind short forms of useful combinators *)
 val <- = Try.<-
 val <@ = Try.<@
 val try = Try.try 
 val distribute = U.Option.distribute
 val distributeV = U.Option.distributeV
 val bool = isSome
 val fail = Try.fail
 val failure = NONE
 val success = SOME
 val require = Try.require

 (* fail with reason *) 
 val <?- : 'a option -> (env * string list) -> 'a =
  fn x => fn (env, str) =>
   case x
    of NONE => (if (Config.debug andalso (whyFail (getConfig env))) then
                  let val () = print "Vectorization failed because: " 
                      val () = print ((concat str) ^ "\n")
                  in fail()
                  end
                else
                  fail ())
     | SOME x' => x'

 (* fail with reason *) 
 val <?@ : ('a -> 'b option) -> 'a -> (env * string list) -> 'b =
  fn f => fn x => <?- (f x)

 (* require with reason *)
 val requireR : bool -> (env * string list) -> unit =
    fn x => fn (env, str) =>
     if x then
       ()
     else
        if (Config.debug andalso (whyFail (getConfig env))) then
          let val () = print "Vectorization failed because: "
              val () = print ((concat str) ^ "\n")
          in fail()
          end
        else
          fail()

 (* instruction tags - Simple vectorization or affine vectorization *)
 datatype tag = TagSimple | TagAffine | TagReduction

 (* Various type abbreviations *)
 type tags = tag ID.t

 type instructions = Mil.instruction list
 type instruction = Mil.instruction

(* -------------------------------------------------------------------------- *)
(*    Helpers                                                                 *)
(* -------------------------------------------------------------------------- *)

 val mkVDescriptorFS : (env * info * Mil.fieldSize) -> Mil.Prims.vectorDescriptor option =
   fn (env, info, fieldSize) => try (fn () => 
    let
      val E {config, ...} = env
      val vectorLength = readVectorLength info
      val vectorSize = <@ PU.VectorSize.fromBits ((MU.FieldSize.numBits fieldSize) * vectorLength)
      (* XXX hack -leaf *)
      val () = Try.require (vectorSize <> MP.Vs64)
    in
      (* PU.VectorSize.platformSize config *)
      Mil.Prims.Vd {vectorSize = vectorSize, elementSize = fieldSize}
    end)

 (* /mkVDescriptor/ builds a vector descriptor for the current config and given mil typ *)
 val mkVDescriptor : (env * info * Mil.typ) -> Mil.Prims.vectorDescriptor option =
   fn (env, info, typ) => try (fn () => 
    let
      val E {config, ...} = env
      val vectorLength = readVectorLength info
      val fieldSize = <@ MU.Typ.fieldSize (config, typ)
      val vectorSize = <@ PU.VectorSize.fromBits ((MU.FieldSize.numBits fieldSize) * vectorLength)
      (* XXX hack -leaf *)
      val () = Try.require (vectorSize <> MP.Vs64)
    in
      (* PU.VectorSize.platformSize config *)
      Mil.Prims.Vd {vectorSize = vectorSize,
                    elementSize = fieldSize}
    end)

 (* /mkVectorType/ given a mil type, create the vector version of this type *)
 val mkVectorType : (env * info * Mil.typ) -> Mil.typ option =
   fn (env, info, typ) => try (fn () => 
      let
        val E {config, ...} = env
        val vectorLength = readVectorLength info
        val fieldSize = <@ MU.Typ.fieldSize (config, typ)
        val vectorSize = <@ PU.VectorSize.fromBits ((MU.FieldSize.numBits fieldSize) * vectorLength)
      in
        Mil.TViVector {vectorSize = vectorSize,
                       elementTyp = typ}
      end)

 (* /mkVectorType/ given a mil type, create the vector version of this type *)
 val mkVectorMaskType : (env * info * Mil.typ) -> Mil.typ option =
   fn (env, info, typ) => try (fn () => 
      let
        val E {config, ...} = env
        val vectorLength = readVectorLength info
        val fieldSize = <@ MU.Typ.fieldSize (config, typ)
        val vectorSize = <@ PU.VectorSize.fromBits ((MU.FieldSize.numBits fieldSize) * vectorLength)
      in
        Mil.TViMask (Mil.Prims.Vd {vectorSize = vectorSize,
                       elementSize = fieldSize})
      end)

  (* /variableType/ gets the type of a variable *)
  val variableType : (state * env * M.variable) -> Mil.typ =
    fn (state, env, var) =>
      let 
        val E {config, ...} = env
        val symTableM = stateGetSymTableM state
        val si = I.SymbolInfo.SiManager symTableM
      in 
        MilType.Typer.variable (config, si, var)
      end

  (* /operandType/ gets the type of an operand *)
  val operandType : (state * env * M.operand) -> Mil.typ =
    fn (state, env, operand) =>
      let 
        val E {config, ...} = env
        val symTableM = stateGetSymTableM state
        val si = I.SymbolInfo.SiManager symTableM
      in 
        MilType.Typer.operand (config, si, operand)
      end
 
 fun xor x y = (x andalso (not y)) orelse ((not x) andalso y)

 (* /decBaseInductionVar/ - deconstruct an induction variable, looking for a base induction variable value *)
 val decBaseInductionVar = 
  fn x =>
     case x 
      of (ML.BIV iv) => SOME iv
       | _           => NONE

 (*  pointwiseConcat : 'a list list * 'a list list -> 'a list list *)
 val rec pointwiseConcat : 'a list list * 'a list list -> 'a list list =
  fn (xs, ys) => 
     (case (xs, ys) 
       of (xs   , []   ) => xs
        | ([]   , ys   ) => ys
        | (x::xs, y::ys) => (x @ y) :: (pointwiseConcat (xs, ys)))
                                  
 (* unionR - Performs a union between two label dictionaries where the
             collision behaviour is that the right operand's value is chosen
             the right operand must have its type tagged as a "new" dictionary,
             thus "new" values are preferred over "old" *)             
 val unionR : (('a LD.t) * ('a LD.t) new) -> ('a LD.t) =
  fn (oldLD, New newLD) =>
     let val rightPrefer = fn (key, b1, b2) => b2
     in  LD.union (oldLD, newLD, rightPrefer)
     end

 val notPrim : Mil.Prims.compareOp -> Mil.Prims.compareOp =
  fn prim =>
     case prim
      of Mil.Prims.CEq => Mil.Prims.CNe
       | Mil.Prims.CNe => Mil.Prims.CEq
       | Mil.Prims.CLt => Mil.Prims.CLe (* but with a swap of args *) 
       | Mil.Prims.CLe => Mil.Prims.CLt (* ditto *)

 (* ************************************************************************* *)
 (*    Various predicates                                                     *)
 (* ************************************************************************* *)

 (* /bidirection/ is the type of a (partial) function that maps
                  'a to 'b paired with a homomorphism that maps 'b to 'a, i.e.
                  preserves the original 'a structure. partiality is encoded using
                  the option type *)
 type ('a, 'b) bidirectional = 'a -> ('b * ('b -> 'a)) option
                                     
 (* /hasTwoTargets/ - Takes a switch and if the switch has two targets
                      returns a pair of targets, along with a 
                       bidirectional update to the parameter switch *)
val hasTwoTargets : (({select  : M.selector,
                       on      : M.operand, 
                       cases   : (M.constant * M.target) Vector.t, 
                       default : M.target option}, M.target * M.target) bidirectional) =
  fn {select, on, cases, default} =>
     case (Vector.length cases, default)
      of (1, SOME d) =>
         let 
           val (a, t) = Vector.sub (cases, 0)
           val targets = (d, t)
           val injection = (fn (d', t') =>
                               {select = select, 
                                on = on,
                                cases = Vector.new1 (a, t'),
                                default = SOME d'})
         in SOME (targets, injection)
         end
       | (2, NONE) => 
         let 
           val (a1, t1) = Vector.sub (cases, 0)
           val (a2, t2) = Vector.sub (cases, 1)
           val targets = (t1, t2)
           val injection = (fn (t1', t2') =>
                               {select = select, 
                                on = on,
                                cases = Vector.new2 ((a1, t1'),
                                                     (a2, t2')),
                                default = NONE})
         in SOME (targets, injection)
         end
       | _ => NONE

 (* /identifyInPair - Given a test on the type 'a, identifyInPair builds a 
                      bidirectional map from pairs of 'a to pairs of 'a, where
                      the element of the pair that passed the test is first.
                      Exactly one element must pass the test. If they both pass
                      or both fail then the result is NONE. *)
 val identifyInPair : ('a -> bool) -> (('a * 'a), ('a * 'a)) bidirectional = 
   fn test =>
    fn (x, y) =>
       (case (test x, test y)
         of (true, false) => SOME ((x, y), fn (x, y) => (x, y))
          | (false, true) => SOME ((y, x), fn (x, y) => (y, x))
          | _             => NONE)

 (* /renameVariables/ renames all variables in a block to some fresh, related name *)
 val renameVariables : (state * env * info * M.block) -> (M.block * Rename.t) = 
  fn (state, env, info, block) =>
     let
       val symTableM = stateGetSymTableM state
       val E {config, ...} = env
       val LI {loopHeader, ...} = info

       val mkRenaming = 
        fn (var, subst) => Rename.renameTo(subst, var, SymTableM.variableClone (symTableM, var))

        val boundVars = MilBoundVars.block (config, loopHeader, block)
        val renamer = List.fold (VS.toList boundVars, Rename.none, mkRenaming)
        val (_, block') = MilRename.Var.block (config, renamer, loopHeader, block)
     in
       (block', renamer)
     end

 (* <t> approach subject to change, to allow vectorization of non-inner loops *)
 (* /isInvariantVar/ - Request whether a variable is a loop invariant of the current 
                     loop, headed by the block labelled "header" (passed with the env) *)
 val isInvariantVar : (state * env * info * Mil.variable) -> bool = 
   fn (state, env, info, var) =>
     let 
       val E {loopInfo, ...} = env
       val LI {loopHeader, ...} = info

       val binderInfo = ML.getBinderLocations loopInfo 
     in                                          
       case VD.lookup (binderInfo, var)
                                 (* variable is invariant for an inner loop 
                                  if the header of the loop
                                  that contains the binding of var, is not equal
                                  to the current loop header *)
         of SOME binderHeader => not (loopHeader=binderHeader)
          | NONE => true (* if we have no binder location, then the
                            variable is not locally defined so must be invariant *)
     end

 (* /isInductionVariable/ ask if a variable is an induction variable (from the mil loop 
                           analysis, if so the induction variable info is returned *)
 val isInductionVariable : state * env * info * M.variable -> MilLoop.inductionVariable option =
    fn (state, env, info, var) =>
       let
         val E {loopInfo, ... } = env
         val LI {loopHeader, ...} = info
                                    
         val IVs = ML.getInductionVariables (loopInfo, loopHeader) 
                                            
        (* if the variable matches return the variable info *)
         val inductionVarInfo = List.peek (IVs,
                                        fn iv => 
                                           let val canonIV = ML.canonizeInductionVariable iv
                                           in I.variableEqual (#variable canonIV, var)
                                           end)
      in
        inductionVarInfo
      end

  (* /isUnitStrideIV/ given a variable return true if the variable is a unit stride
                     induction variable, else return false *)
  val getStridedIV : state * env * info * M.variable -> int option =
    fn (state, env, info, var) =>
       case isInductionVariable (state, env, info, var)
        of SOME ivInfo => 
           let
             val step = #step (ML.canonizeInductionVariable ivInfo)
             val isUnitStride = Rat.toInt step
           in isUnitStride
           end
         | NONE        => NONE

 (* -------------------------------------------------------------------------- *)
 (*  Depth 2 : May vectorize stage, classifies loops as to whether they can,   *)
 (*            in theory, be vectorized.                                       *)
 (* -------------------------------------------------------------------------- *)

 (* /mayVectorize/ - Determine whether a loop can be feasibly vectorized 
                    (does not make use of any architecture specific info *)
 val mayVectorize : (state * env * info * ML.loop) -> unit option = 
  fn (state, env, info, loop) =>
     try (fn () =>
     let 
       val E {config, program, fmil, code, cfg, domInfo, loopInfo, ...} = env
       val symTableM = stateGetSymTableM state
       val LI {dependencyInfo, ...} = info
       val si = I.SymbolInfo.SiManager symTableM 
       val MilLoop.L {header, blocks} = loop

       (* **** Check just single block *)
       val () = requireR (LD.size blocks = 1) (env, ["More than one block in loop"])
       val (loopHeader, block) = hd (LD.toList blocks)
       val M.B {parameters, instructions, transfer} = block

       (* **** Sanity check on loop structure *)
       val () = require (loopHeader = header)

       (* **** Check we have only two targets in transfer *)
       val switch = <?- (MU.Transfer.Dec.tCase transfer) (env, ["Non case transfer at the end of block "])
       val () = <@ MU.Selector.Dec.seConstant (#select switch)
       val () = requireR (bool (hasTwoTargets switch)) (env, ["More than two targets in loop exit"])

       (* **** Check that the test for exit condition is based on an induction variable *)
       val {select, on, cases, default} = switch
       val testVar = <?- (MU.Simple.Dec.sVariable on) (env, ["Test is a constant"])
        (* find definition of the test *)
       val testVarDef = FMil.getVariable (fmil, testVar)
       val (_, rhs) = case testVarDef 
                       of FMil.VdInstr x => x 
                        | _              => Try.fail ()

        (* deconstruct the test *)
       val {args, prim, ...} = <@ MU.Rhs.Dec.rhsPrim rhs
       val () = require (Vector.length args = 2)
       val (arg1, arg2) = (Vector.sub(args, 0), Vector.sub(args, 1))
       (* check that the test if for < or <= *)
       val prim' = <@ PU.T.Dec.prim prim
       val {typ, operator} = <@ PU.Prim.Dec.pNumCompare prim'
       val () = requireR (PU.Eq.compareOp(operator, Mil.Prims.CLt) orelse
                          PU.Eq.compareOp(operator, Mil.Prims.CLe)) (env, ["Loop test should be <, or <="])

        (* find if one of the arguments is an induction var *)
       val argIsInductionVar : M.operand -> bool =
          fn arg => 
             bool (try (fn () =>
                           let
                             val var = <@ MU.Simple.Dec.sVariable arg
                             val iv = <@ isInductionVariable (state, env, info, var)
                           in
                             iv
                           end))
 
       val identifyIVArg = identifyInPair argIsInductionVar
       (* the following will return NONE if neither of the params are IVs, or
          if they both are (local this loop) *)                                                                          
       val ((argIV, argB), _) = <@ identifyIVArg (arg1, arg2)

       (* **** Perform checks on each instruction *)
       val checkInstruction : instruction -> bool = fn instr =>
           bool (try (fn () =>
             let
               val M.I {dests, n, rhs} = instr
               (* no more than one lhs *)
               val () = requireR (Vector.length dests <= 1) (env, ["Instruction ", Int.toString n,
                                                             " bound to more than one variable"])

               (* allow only certain RHS *)
               val () = requireR (case rhs 
                                  of M.RhsSimple _ => true
                                   | M.RhsPrim _ => true
                                   | M.RhsTupleSet _ => true
                                   | M.RhsTupleSub _ => true
                                   | M.RhsTuple _ => true
                                   | _ => false) (env, ["Instruction ", Int.toString n,
                                                  "has non vectorizable rhs"])

               (* check effects *)
               val allowedEffects = Effect.union (Effect.Control, Effect.InitReadS)
               val allowedEffects = Effect.union (allowedEffects, Effect.InitWriteS)
               val effects = MU.Instruction.fx (config, instr)
               val () = require (Effect.subset (effects, allowedEffects))
             in true
             end))        
       val () = require (Vector.forall (instructions, checkInstruction))

       (* **** Check dependencies *)

       (* check each dependency cycle *)
       val rec checkCycle =
        fn xs => 
           case xs
            of []      => ()
             | (x::xs) =>
               let
                 (* debugging, print dependency cycle *)
                 val () = if (Config.debug andalso (whyFail config)) then
                            LU.printLayout (
                            LU.sequence ("cycle [", "]", ", ")
                                       (List.map (x, fn var => MilLayout.layoutVariable (config, si, var))))
                          else
                            ()
                              
                              (* check if a dependency cycle contains a variable with tuple type *)
                 val varHasTupleType = fn var => case (variableType (state, env, var))
                                                  of (M.TTuple _) => true
                                                   | _            => false
                 val containsTuple = List.fold (x, false, fn (var, res) => (varHasTupleType var) orelse res) 
                 val () = requireR (not (containsTuple)) (env, ["dependency cycle involving a tuple or array"])
                                   
                                   (*
                                    (* check that dependencies are only on induction variables *)
                 val isInductionVariable' =
                  fn (var, res) => (bool (isInductionVariable (state, env, var)) andalso res)
                 val onlyIVdeps = List.fold (x, true, isInductionVariable')
                                  
                 val () = requireR (onlyIVdeps) (env, ["dependency cycle involving non induction variable"]) *)
               in checkCycle xs
               end
       val cycles = MDA.getCycles dependencyInfo
       val () = checkCycle cycles
     in ()
     end)

 (* -------------------------------------------------------------------------- *)
 (*  Depth 3.0 : Tagging stage - process instructions and tag with vectorize   *)
 (*              demands                                                       *)
 (* -------------------------------------------------------------------------- *)
 
 type tagEnv = {affineVars : VS.t,
                tags : tag ID.t,  (* instruction id -> tag *)
                minSize : int,
                maxSize : int}

    (*------------------------------------------------------------------------ *)
    (*     Calculate the min and max size of types in a computations           *)
    (* ----------------------------------------------------------------------- *)

 (* /calcMinMax/ - Given an int and pair of min and max values compute the new
               min/max pair *)
 val calcMinMax : (int option * (int * int)) -> (int * int) =
   fn (x, (xmin, xmax)) => 
      case x
        of NONE => (xmin, xmax)
         | SOME x' => (Int.min (x', xmin), Int.max (x', xmax))

 (* /varBitSize/ - Get the bit size of the type of a list of variables *)
 val varBitSize : (state * env * Mil.variable vector) -> int option =
   fn (state, env, dests) => 
      if Vector.length dests = 1 then
        let
          val E {config, ...} = env
          val symTableM = stateGetSymTableM state
          val var = Vector.sub (dests, 0)
          val Mil.VI {typ, kind} = SymTableM.variableInfo (symTableM, var)
        in MU.Typ.numBits (config, typ)
        end
      else
        NONE

    (* ----------------------------------------------------------------------- *)
    (*     Calculating whether expr as linear funtions, and tagging            *)
    (* ----------------------------------------------------------------------- *)

 (* /isAffieVar/ - Requst whether a variable is a derived induction variable
                   (ie an affine computation on a base induction variable *)
 val isAffineVar : (state * env * VS.t * Mil.operand) -> Mil.variable option =
   fn (state, env, affineVars, operand) =>
     case operand
       of Mil.SConstant _ => NONE
        | Mil.SVariable var => 
          if VS.member (affineVars, var) then
            SOME var
          else 
            NONE

 (* /isInvariant/ - Request whether an operand is a loop invariant = either
                    a constant or a loop-invariant variables *)
 val isInvariant : (state * env * info * Mil.operand) -> bool = 
   fn (state, env, info, operand) =>
      case operand
        of Mil.SConstant _   => true
         | Mil.SVariable var => isInvariantVar (state, env, info, var)
                            
 (* /isNumericTyp/ - Is the given type a numeric type *)
 val isNumericTyp : M.typ -> bool =
  fn typ =>  isSome (MU.Typ.Dec.tNumeric typ)

 (* /isPreciseNumericTyp/ - Is the given type a precise (non-float/double) numeric type *)
 val isPreciseNumericTyp : MP.numericTyp -> bool =
  fn nt =>  case nt
              of MP.NtFloat _ => false
               | _            => true

 (* /rhsIsAffine/ - Ascertain whether a right hand side expression is an
                    affine expression  *)
 val rhsIsAffine : (state * env * info * Mil.rhs * VS.t) -> Mil.variable option = 
  fn (state, env, info, rhs, affineVars) => 
   try (fn () =>
    let
      val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)
      val prim' = <- (PU.T.Dec.prim prim)
      val {typ, operator} = <- (PU.Prim.Dec.pNumArith prim')
 
      (* check we have an integer type- todo: could be relaxed to float if sloppyFp mode is on *)
      val integerTyp = require (isPreciseNumericTyp typ)
                            
     (* Only allow +, -, * in affine bases computations *)
      val () = require ((operator = Mil.Prims.APlus) orelse 
                        (operator = Mil.Prims.ATimes) orelse
                        (operator = Mil.Prims.AMinus))
      val () = require (Vector.length args = 2)
             
      val x = Vector.sub (args, 0) 
      val y = Vector.sub (args, 1)
      val xIsAffineVar = isAffineVar (state, env, affineVars, x)
      val yIsAffineVar = isAffineVar (state, env, affineVars, y)
      val xIsInvariant = isInvariant (state, env, info, x)
      val yIsInvariant = isInvariant (state, env, info, y)

     (* note that - is not commutative, we only - in the form i' = i - a, thus 
        put a check in here *)
      val implies = fn (x, y) => ((not x) orelse y)
      val () = 
          case operator
           of Mil.Prims.AMinus => require ((bool xIsAffineVar) andalso yIsInvariant)
            | _                => ()
    in
      case ((xIsAffineVar, yIsInvariant), (yIsAffineVar, xIsInvariant))
       of ((SOME var, true), _               ) => var
        | (_               , (SOME var, true)) => var
        | _                                    => Try.fail ()
    end)

 (* /instrIsReduction/ - Predicate testing if the instruction is a reduction *)
 val instrIsReduction : (state * env * info * Mil.variable vector * Mil.instruction)
                      -> M.variable option =
   fn (state, env, info, params, instr) =>
     try (fn () =>
      let
        val Mil.I {dests, n, rhs} = instr
        val E {config, loopInfo, ...} = env
        val LI {loopHeader, dependencyInfo, ...} = info
        val inductionVars = ML.getInductionVariables (loopInfo, loopHeader)
        val justBaseVars = fn x => case x 
                                  of ML.BIV {variable, ...}
                                       => SOME variable
                                   | _ => NONE 
        val baseIVs = List.keepAllMap (inductionVars, justBaseVars)  

        (* check just one desintation *)
        val () = require (Vector.length dests = 1)
        val bindVar = Vector.sub(dests, 0)

        (* check we have a prim *)
        val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)

        (* check if prim is associativity at this typ *)
        val () = require (PU.Properties.Associativity.t (config, prim) orelse allowReductions config)

        (* check if one parameter is a formal loop parameter *)
        val () = require (Vector.length args = 2)
        val (arg1, arg2) = (Vector.sub(args, 0), Vector.sub(args, 1))

        val varIsNotBIV = 
         fn var => (not (List.exists (baseIVs, fn v => I.variableEqual(var, v))))
        (* Returns true if the operand is a parameter variable, else false *)
        val argIsParamVar : M.operand -> bool =
          fn arg => bool (try (fn () =>
                     let
                       val var = <- (MU.Simple.Dec.sVariable arg)
                       val () = require (Vector.contains (params, var, I.variableEqual))
                       val () = require (varIsNotBIV var)
                     in
                       var
                     end))
             
        (* select the argument which is a parameter variable *)
        val identifyParamArg = identifyInPair argIsParamVar
        val ((argParam, _), _) = <- (identifyParamArg (arg1, arg2))

        (* arg param is a variable *)
        val paramVar = <@ MU.Simple.Dec.sVariable argParam

        (* check that the reduction variable isn't a base induction variable *)
        val () = require (varIsNotBIV bindVar)

        (* find if the destination variable is in cycle, with the loop
           parameter that appears in the rhs of this instruction *)
        (* check each dependency cycle *)
        val rec checkCycle =
         fn xs => 
            (case xs 
              of []      => false
               | (x::xs) =>
                 let
                   val containsDest  = List.exists(x, fn var => I.variableEqual(var, bindVar))
                   val containsParam = List.exists(x, fn var => I.variableEqual(var, paramVar))
                 in (containsDest andalso containsParam) orelse checkCycle xs
                 end)

        val cycles = MDA.getCycles dependencyInfo
        val () = require (checkCycle cycles)
      in
        paramVar
      end)
                          
 (* /tagInstructions/ - Tag each instruction with whether it should be 
                        vectorized, or if it is affine *)
 val tagInstructions : (state * env * info * Mil.instruction vector * Mil.variable vector * tagEnv) -> tagEnv = 
   fn (state, env, info, instrs, parameters, tagEnv) =>
       let
         (* /tagInstruction/ *)
         val tagInstruction = 
          fn (instr, tagEnv) =>
             let 
               val {affineVars, tags, minSize, maxSize} = tagEnv
               val Mil.I {dests, n, rhs} = instr
               val bitSize = varBitSize (state, env, dests)
               val tag = 
                   case (rhsIsAffine (state, env, info, rhs, affineVars))
                    of SOME var => TagAffine
                     | NONE => 
                       (case (instrIsReduction (state, env, info, parameters, instr))
                         of SOME var => TagReduction
                          | NONE     => TagSimple)
               val affineVars = 
                   case tag
                    of TagAffine    => VS.insertList (affineVars, Vector.toList dests)
                     | TagReduction => affineVars
                     | TagSimple    => affineVars

               val (minSize, maxSize) =
                   case tag
                    of TagAffine    => (minSize, maxSize)
                     | TagReduction => calcMinMax (bitSize, (minSize, maxSize))
                     | TagSimple    => calcMinMax (bitSize, (minSize, maxSize))
             in
               {affineVars = affineVars,
                tags = ID.insert(tags, n, tag),
                minSize = minSize,
                maxSize = maxSize}
             end
       in
         Vector.fold(instrs, tagEnv, tagInstruction)
       end

 (* /tagBlocks/ tags a label dict of blocks returns a label dict of tagged
     instruction lists *)
 val tagBlocks : (state * env * info * (Mil.block LD.t)) -> tagEnv = 
   fn (state, env, info, blocks) =>
    let
      val E {loopInfo, ...} = env

      (* /justBaseVars/ is used by tagBlocks to filter induction vars to get just
                           the base induction vars *)
      val justBaseVars = fn x => case x 
                                  of ML.BIV {variable, ...}
                                       => SOME variable
                                   | _ => NONE        

      val tagBlock = 
       fn (label, block, tagEnv) =>
          let
            val Mil.B {parameters, instructions, transfer} = block
            val {affineVars, tags, minSize, maxSize} = tagEnv
            (* add base induction variables for the block to affine info *)
            val inductionVars = ML.getInductionVariables (loopInfo, label)
            val baseIVs = List.keepAllMap (inductionVars, justBaseVars)
            val affineVars = VS.insertList (affineVars, baseIVs)
            val tagEnv = {affineVars = affineVars, tags = tags,
                          minSize = minSize, maxSize = maxSize}
            val x = tagInstructions (state, env, info, instructions, parameters, tagEnv)
          in x
          end

      val tagEnv = {affineVars = VS.empty,
                    tags = ID.empty,
                    minSize = MU.ValueSize.numBits MU.ValueSize.maxSize, 
                    maxSize = 0}

      val tagEnv = LD.fold (blocks, tagEnv, tagBlock)
    in          
      tagEnv
    end

 val printTaggedBlocks : (state * env * (Mil.block LD.t) * tags) -> unit =
   fn (state, env, blocks, tags) =>
     let
       val E {config, ...} = env
       val symTableM = stateGetSymTableM state
       val si = I.SymbolInfo.SiManager symTableM 
  
       val printInstruction = 
        fn instr =>
           let 
             val M.I {dests, n, rhs} = instr
             val instrL = MilLayout.layoutInstruction (config, si, instr)
             val tagL =
                 case (ID.lookup(tags, n))
                   of SOME (TagSimple) => L.seq [L.str ", ", L.str "VecSimple"]
                    | SOME (TagAffine) => L.seq [L.str ", ", L.str "VecAffine"]
                    | SOME (TagReduction) => L.seq [L.str ", ", L.str "VecReduction"]
                    | NONE => L.seq [L.str " none"]
         in
           LU.printLayout (L.seq [instrL, tagL])
         end
           
       val printBlock = fn (label, block) =>
           let val () = print ((I.labelString (label))^ ":\n")
               val M.B {parameters, instructions, transfer} = block
               val _ = Vector.map (instructions, printInstruction)
           in
               ()
           end
       val _ = LD.map (blocks, printBlock)       
     in                               
       ()
     end

 val printSizeInfo : (state * env * int * int) -> unit =
   fn (state, env, minSize, maxSize) =>
     let
       val layout = L.seq [L.str "(minSize, maxSize) = (", L.str (Int.toString minSize),
                           L.str ", ", L.str (Int.toString maxSize), L.str ")"]
     in
       LU.printLayout layout
     end
 
 (* -------------------------------------------------------------------------- *)
 (*  Depth 3.1 : Actual vectorization stage. Here the vectorization demands    *)
 (*              generated at depth 3.0 are satisfied                          *)
 (* -------------------------------------------------------------------------- *)

 (* reduction finals is a Var -> (Var * M.instruction) map
    which maps a parameter reduction variable, to a triple of
    the reduction's updated reduction variable, a vector instruction computing the reduction,
    and the original operator
   e.g. L1(i, x):
         ....
         x' = x (op) a

      leads to the mapping:  x -> ((xv (opv) av), op) *)

  type reductionInfo = {reduxNextVar : Mil.variable,
                        operator : Mil.Prims.arithOp,
                        operandVar : Mil.variable,
                        typ : Mil.Prims.numericTyp}

  type vectorRenamings = {vectorDefs : Rename.t,
                          affineBases : Rename.t,
                          reductionDefs : Rename.t,
                          reductionInfos : reductionInfo VD.t}

  (* /mkRelatedVectorVar/ Create a new variable, with vectorized type,
                          that is related to another, with some tag *)
  val mkRelatedVectorVar : (state * env * info * M.variable * string) -> M.variable option =
   fn (state, env, info, var, tag) =>
      try (fn () => 
              let 
                val symTableM = stateGetSymTableM state
                val M.VI {typ, kind} = SymTableM.variableInfo (symTableM, var)
                val typ' = <@ mkVectorType (env, info, typ)
                val info' = M.VI {typ = typ', kind = kind}
                val var' = SymTableM.variableRelated (symTableM, var, tag, info')
              in
                var'
              end)

  val mkRelatedVectorMaskVar : (state * env * info * M.variable * string) -> M.variable option =
   fn (state, env, info, var, tag) =>
      try (fn () => 
              let 
                val symTableM = stateGetSymTableM state
                val M.VI {typ, kind} = SymTableM.variableInfo (symTableM, var)
                val typ' = <@ mkVectorMaskType (env, info, typ)
                val info' = M.VI {typ = typ', kind = kind}
                val var' = SymTableM.variableRelated (symTableM, var, tag, info')
              in
                var'
              end)

  (* /mkRelatedVar/ Create a new variable that is related to another + tag
                    note: with the type preserved *)
  val mkRelatedVar : (state * M.variable * string) -> M.variable =
     fn (state, var, tag) =>
       let 
         val symTableM = stateGetSymTableM state
          val info = SymTableM.variableInfo (symTableM, var)
          val var' = SymTableM.variableRelated (symTableM, var, tag, info)
       in
          var'
       end

  (* /mkFreshVar/ Create a fresh variable + tag *)
  val mkFreshVar : (state * string * M.variableKind * Mil.typ) -> M.variable =
     fn (state, tag, kind, typ) =>
      let 
        val symTableM = stateGetSymTableM state
        val info = M.VI {typ = typ, kind = kind}
        val var = SymTableM.variableFresh (symTableM, tag, info)
      in
        var
      end

  (* /mkFreshLocalVar/ Create a fresh local variable + tag *)
  val mkFreshLocalVar : (state * string * Mil.typ) -> M.variable = 
     fn (state, tag, typ) => mkFreshVar (state, tag, M.VkLocal, typ)

  (* /mkFreshGlobalVar/ Create a fresh global variable + tag *)
  val mkFreshGlobalVar : (state * string * Mil.typ) -> M.variable = 
     fn (state, tag, typ) => mkFreshVar (state, tag, M.VkGlobal, typ)

  val addGlobal : (state * M.variable * M.global) -> unit = 
   fn (state, v, g) => 
      let
        val globals = stateGetGlobals state
        val () = globals := (v, g) :: !globals
      in ()
      end

  (* /ratToConstant/ takes a mlton rat, an numeric type, and produces 
      a mil operand representing the rat at the given type *)
  val ratToConstant : state * Rat.t * Mil.Prims.numericTyp -> Mil.operand option =
    fn (state, n, nt) =>
       try (fn () => 
               (case nt
                 of MP.NtRat => 
                    let
                      val v = mkFreshGlobalVar (state, "rat", M.TNumeric nt)
                      val g = M.GRat n
                      val () = addGlobal (state, v, g)
                    in M.SVariable v
                    end
                  | MP.NtInteger MP.IpArbitrary => 
                    let
                      val v = mkFreshGlobalVar (state, "int", M.TNumeric nt)
                      val g = M.GInteger (<@ Rat.toIntInf n)
                      val () = addGlobal (state, v, g)
                    in M.SVariable v
                    end
                  | MP.NtInteger (MP.IpFixed arb) => 
                    M.SConstant (Mil.CIntegral (IA.fromIntInf (arb, <@ Rat.toIntInf n)))
                  | MP.NtFloat _ => Try.fail ()))
               
  val constantToRat : Mil.constant -> Rat.t option =
   Try.lift (fn c => 
                case c
                 of M.CRat r      => Rat.fromIntInf r
                  | M.CInteger i  => Rat.fromIntInf i
                  | M.CIntegral i => Rat.fromIntInf (IntArb.toIntInf i)
                  | _             => Try.fail ())

  (* /liftSimple/ - takes a simple and makes a constant vector of the simple *)
  val liftSimple : (state * env * info * M.simple * M.variable option) ->
                   (M.variable * M.instruction) option = 
   fn (state, env, info, simple, relatedVar) =>
    try (fn () =>
      let
        val typ = operandType (state, env, simple)
        (* <todo> check we are only lifting numeric typed things
          can remove when the backend names refs, and if using C </todo> *)
        val _ = <@ MU.Typ.Dec.tNumeric typ

        val descriptor = <@ mkVDescriptor (env, info, typ)
        val typV = <@ mkVectorType (env, info, typ)
                
        val var = 
            case relatedVar
             of SOME rVar => 
                (case mkRelatedVectorVar (state, env, info, rVar, "vlift")
                  of SOME v => v
                   | NONE   => mkFreshLocalVar (state, "vlift", typV))
              | NONE      => mkFreshLocalVar (state, "vlift", typV)

        val rhs = M.RhsPrim {prim = Mil.Prims.Vector
                                      (Mil.Prims.ViData {descriptor = descriptor,
                                                         operator = Mil.Prims.DBroadcast}),
                             createThunks = false,
                             typs = Vector.new1 typ,
                             args = Vector.new1 simple}

        val instr = M.I {dests = Vector.new1 var,
                         n = 0,
                         rhs = rhs}
      in
        (var, instr)
      end)

  (* /vectorizeSimple'/ - Given a simple, and a possible variable, generate the instruction
                          that binds its vectorization. An optional variable provides 
                          a variable for the new binder to be related to *)
  val vectorizeSimple' : (state * env * info * M.simple * M.variable option * vectorRenamings) ->
                         ((M.variable * instruction list) * vectorRenamings) option =
    fn (state, env, info, simple, bindVar, vectorRenamings) =>
      try (fn () =>
        let
          val {vectorDefs, affineBases, reductionDefs, reductionInfos} = vectorRenamings
          val symTableM = stateGetSymTableM state

          val typ = operandType (state, env, simple)
          val typV = <@ mkVectorType (env, info, typ)

          val (bindVarV, instrV) =
             case simple
               of M.SVariable var =>
                 (case (Rename.use' (vectorDefs, var))
                   (* vectorized result already exists *)
                   of SOME varV =>
                      let
                        (* build the instruction with a related var if given, or fresh var *)
                        val bindVarV = 
                            case bindVar 
                             of SOME bindVar' => <@ mkRelatedVectorVar (state, env, info, bindVar', "vres")
                              | NONE          => mkFreshLocalVar (state, "vres", typV)
                        val instrV = M.I {dests = Vector.new1 bindVarV, 
                                          n = 0,
                                          rhs = M.RhsSimple (M.SVariable varV)}
                      in (bindVarV, instrV)
                      end
                        (* vectorized does not exist - if invariant then lift *)
                    | NONE      => 
                      if (isInvariantVar (state, env, info, var)) then
                        <@ liftSimple (state, env, info, simple, bindVar)
                      else
                        Try.fail ())
                (* just a constant, so lift *)
                | M.SConstant _    => <@ liftSimple (state, env, info, simple, bindVar)

          (* If vectorizing from a definition, update the vectorized versions map *)
          val vectorDefs = 
              case bindVar 
                of (SOME bindVar') => Rename.renameTo (vectorDefs, bindVar', bindVarV)
                 | NONE            => vectorDefs
          val vectorRenamings = {vectorDefs = vectorDefs, 
                                 affineBases = affineBases,
                                 reductionDefs = reductionDefs,
                                 reductionInfos = reductionInfos}
        in
          ((bindVarV, [instrV]), vectorRenamings)
        end)

  (* /vectorizeSimple/ *)
  val vectorizeSimple : (state * env * info * instruction * vectorRenamings) ->
                        (instruction list * vectorRenamings) option =
   fn (state, env, info, instr, defs) =>
    try (fn () =>
      let
       (* Destruct instruction *)
       val M.I {dests, n, rhs} = instr
       val () = require ((Vector.length dests) = 1)

       val bindVar = SOME (Vector.sub (dests, 0))
       val simple = <- (MU.Rhs.Dec.rhsSimple rhs)
 
       val ((varV, instrsV), defs') = <@ vectorizeSimple' (state, env, info, simple, bindVar, defs)
      in
       (instrsV, defs')
      end)  

  (* /vectorizeSimpleAffine/ *)
  val vectorizeSimpleAffine : (state * env * info * instruction * vectorRenamings) ->
                              (instruction list * vectorRenamings) option =
   fn (state, env, info, instr, vectorRenamings) =>
     try (fn () => 
        let
          val symTableM = stateGetSymTableM state
          (* Desctruct instruction *)
          val M.I {dests, n, rhs} = instr
          val () = require ((Vector.length dests) = 1)
          val bindVar = Vector.sub (dests, 0)
          val simple = <- (MU.Rhs.Dec.rhsSimple rhs)

          val {vectorDefs, affineBases, reductionDefs, reductionInfos} = vectorRenamings
        in
          case simple
            of M.SVariable var' =>
               let
                 val affineBasisV = <@ Rename.use' (affineBases, var')
                 val varA = <@ mkRelatedVectorVar (state, env, info, bindVar, "basis")
                 val instrA = Mil.I {dests = Vector.new1 varA,
                                     n = 0,
                                     rhs = M.RhsSimple (M.SVariable affineBasisV)}
                 val affineBases' = Rename.renameTo (affineBases, bindVar, varA)

                 val vectorDef = <@ Rename.use' (vectorDefs, var')
                 val varV = <@ mkRelatedVectorVar (state, env, info, bindVar, "vres")
                 val instrV = Mil.I {dests = Vector.new1 varV,
                                      n = 0,
                                      rhs = M.RhsSimple (M.SVariable vectorDef)}

                 val vectorDefs = Rename.renameTo (vectorDefs, bindVar, varV)
               in
                 ([instrA, instrV], {vectorDefs = vectorDefs,
                                     affineBases = affineBases',
                                     reductionDefs = reductionDefs,
                                     reductionInfos = reductionInfos})
               end
           (* this should never get tagged as affine, but if it was then a
              lift would be most appropriate *)
            | M.SConstant c =>
              <- (vectorizeSimple (state, env, info, instr, vectorRenamings))
        end)

  (* /canVectorizePrim/ - Predicate that tests is a prim can be vectorized-
     This may be developed further to adapt to a particular architectures
     available instructions *)
  val canVectorizePrim : (state * env * Mil.Prims.prim) -> bool = 
    fn (state, env, prim) =>
      case prim
        (* todo: 11/5/10 vectorizing comparisons is tricky and needs more thought about how
                 to deal with vectors of bools (currently u32s) *)
        of M.Prims.PNumArith _ => true
         | M.Prims.PFloatOp _ => true
         | M.Prims.PNumCompare _ => true
         | M.Prims.PNumConvert _ => true
         | M.Prims.PNumCast _ => true
         | M.Prims.PBitwise _ => true
         | M.Prims.PBoolean _ => true
         | M.Prims.PName _ => false
         | M.Prims.PCString _ => false
         | M.Prims.PPtrEq => true
         | M.Prims.PCondMov => true

  (* /vectorizePrim/ - Performs simple vectorization of a prim instruction *)
  val vectorizePrim : (state * env * info * M.instruction * vectorRenamings)
                      -> (Mil.instruction list * vectorRenamings) option = 
     fn (state, env, info, instr, defs) =>
       try (fn () =>
        let
          val symTableM = stateGetSymTableM state
          val si = I.SymbolInfo.SiManager symTableM

          (* Deconstruct instruction *)
          val M.I {dests, n, rhs} = instr
          val () = require ((Vector.length dests) = 1)
          val bindVar = Vector.sub (dests, 0) 

          (* Deconstruct the prim *)
          val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)
          val primOp = <- (PU.T.Dec.prim prim)


          (* Test if the prim can be vectorized *)
          val () = require (canVectorizePrim (state, env, primOp))

          (* Produce instructions for the vector version of each argument *)
          val vectorizeArg = fn (arg, defs) =>   
                                case (vectorizeSimple' (state, env, info, arg, NONE, defs))
                                 of SOME (arg', defs') => (SOME arg', defs')
                                  | NONE               => (NONE, defs)

          val argsL = Vector.toList args
          val (vVarsAndInstrs, defs) = Utils.List.mapFoldl (argsL, defs, vectorizeArg)
          val vVarsAndInstrs = <@ distribute vVarsAndInstrs
          val (vArgVars, vArgInstrs) = List.unzip vVarsAndInstrs
          val vArgInstrs = (List.concat vArgInstrs)

          (* Create the list of arguments to the vectorized instruction *)
          val vArgs = List.map (vArgVars, M.SVariable)

          (* Build the vector primitive *)
          val typ = variableType (state, env, bindVar)
          val vDescriptor = <@ mkVDescriptor (env, info, typ)
          val vPrimOp = 
              case primOp
               of Mil.Prims.PNumArith _ =>
                  Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                         masked = false,
                                         operator = primOp}
                | Mil.Prims.PFloatOp _ =>
                  Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                         masked = false,
                                         operator = primOp}
                | Mil.Prims.PNumCompare {typ, operator} =>
                  Mil.Prims.ViCompare {descriptor = vDescriptor,
                                       typ = typ,
                                       operator = operator}
                | Mil.Prims.PNumConvert {to, from} =>
                  let
                    val toDescriptor = <@ mkVDescriptor (env, info, Mil.TNumeric to)
                    val fromDescriptor = <@ mkVDescriptor (env, info, Mil.TNumeric from)
                  in
                    Mil.Prims.ViConvert {to = {descriptor = toDescriptor,
                                               typ = to},
                                         from = {descriptor = fromDescriptor,
                                                 typ = from}}
                  end
                | Mil.Prims.PNumCast {to, from} =>
                  let
                    val toDescriptor = <@ mkVDescriptor (env, info, Mil.TNumeric to)
                    val fromDescriptor = <@ mkVDescriptor (env, info, Mil.TNumeric from)
                  in
                    Mil.Prims.ViCast {to = {descriptor = toDescriptor,
                                            typ = to},
                                      from = {descriptor = fromDescriptor,
                                              typ = from}}
                  end
                | Mil.Prims.PBitwise _ =>
                  Mil.Prims.ViPointwise {descriptor = vDescriptor, 
                                         masked = false,
                                         operator = primOp}
                | Mil.Prims.PBoolean _ =>
                  Mil.Prims.ViPointwise {descriptor = vDescriptor, 
                                         masked = false,
                                         operator = primOp}
                | Mil.Prims.PCondMov =>
                  Mil.Prims.ViData {descriptor = vDescriptor, 
                                    operator = Mil.Prims.DBlend }
                | _ => Try.fail ()
                           
          (* Build the new vectorized instruction *)
          (* val bindVarV = <@ mkRelatedVectorVar (state, env, info, bindVar, "vres") *)
          val bindVarV = 
              case primOp
               of Mil.Prims.PNumCompare _ => <@ mkRelatedVectorMaskVar (state, env, info, bindVar, "vres")
                | _ => <@ mkRelatedVectorVar (state, env, info, bindVar, "vres")
          val typs = case vPrimOp 
                       of Mil.Prims.ViData _ => Vector.new1 typ
                        | _ => Vector.new0 ()
          val vRhs = M.RhsPrim {prim = Mil.Prims.Vector vPrimOp,
                                createThunks = false,
                                typs = typs,
                                args = Vector.fromList vArgs}
          val vInstr = Mil.I {dests = Vector.new1 bindVarV,
                              n = 0,
                              rhs = vRhs}

          (* Update the mapping of bindings to their vectorized binding *)
          val {vectorDefs, affineBases, reductionDefs, reductionInfos} = defs
          val vectorDefs = Rename.renameTo (vectorDefs, bindVar, bindVarV)
          val defs = {vectorDefs = vectorDefs, 
                      affineBases = affineBases,
                      reductionDefs = reductionDefs,
                      reductionInfos = reductionInfos}                   
        in
          (vArgInstrs @ [vInstr], defs)
        end)

  (* /identifyAffineVar/ - identifies two operand that is an affine variable *)
  val identifyAffineVar : Rename.t -> ((M.operand * M.operand), (M.operand * M.operand)) bidirectional =
    fn affineBases => identifyInPair (fn operand =>
                                         case operand
                                          of M.SVariable var => Rename.renamed(affineBases, var)
                                           | M.SConstant _    => false)

  (* /vectorizePrimAffine/ - Performs vectorization of an affine prim instruction *)
  val vectorizePrimAffine : (state * env * info * M.instruction * vectorRenamings)
                            -> (Mil.instruction list * vectorRenamings) option = 
   fn (state, env, info, instr, defs) =>
     try (fn () =>
       let
         val {vectorDefs, affineBases, reductionDefs, reductionInfos} = defs
         (* Deconstruct instruction *)
         val M.I {dests, n, rhs} = instr
         val () = require ((Vector.length dests) = 1)
         val bindVar = Vector.sub (dests, 0)
         (* Get the type of the affine basis *)
         val typ = variableType (state, env, bindVar)

         (* Deconstruct the prim *)
         val {prim, createThunks, typs, args} = <@ MU.Rhs.Dec.rhsPrim rhs
         val primOp = <@ PU.T.Dec.prim prim

         (* sanity check, exactly 2 arguments *)
         val () = require (Vector.length args = 2)
         val (arg1, arg2) = (Vector.sub (args, 0), Vector.sub (args, 1))
                                          
         val ((affVarArg, otherArg), reorderArgs) = <@ (identifyAffineVar affineBases) (arg1, arg2)
         val affVar = <@ MU.Simple.Dec.sVariable affVarArg

         (* Lift the original binding to a constant vector *)
         val (varL, instrL) = <@ liftSimple (state, env, info, Mil.SVariable bindVar, NONE)

         (* Lookup the affine basis for the affine variable used *)
         val affineBasis = <@ Rename.use' (affineBases, affVar)

         val (varA, instrsA, defs) =
             case primOp
              of (Mil.Prims.PNumArith {operator = Mil.Prims.ATimes, ...}) =>
                 let
                   (* produce a vector version of the other arg *)
                   val ((otherArgV, otherArgInstrs), defs) = 
                       <@ vectorizeSimple' (state, env, info, otherArg, NONE, defs)
                   (* put the new arguments in the right order *)
                   val (arg1', arg2') = reorderArgs (M.SVariable affineBasis, M.SVariable otherArgV)
                   val vDescriptor = <@ mkVDescriptor (env, info, typ)                            
                   val primA = Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                                      masked = false,
                                                      operator = primOp}
                   val rhsA = Mil.RhsPrim {prim = Mil.Prims.Vector primA,
                                           createThunks = false,
                                           (* vector typ of each argument will be
                                              the same as the vector type of the result *)
                                           typs = Vector.new0 (),
                                           args = Vector.new2 (arg1', arg2')}
                   val varA = <@ mkRelatedVectorVar (state, env, info, bindVar, "basis")
                   val instrA = Mil.I {dests = Vector.new1 varA,
                                       n = 0,
                                       rhs = rhsA}
                 in
                   (varA, otherArgInstrs @ [instrA], defs)
                 end
                   (* + case *)
               | (Mil.Prims.PNumArith {operator = Mil.Prims.APlus, ...}) =>
                 let
                   val varA = <@ mkRelatedVectorVar (state, env, info, bindVar, "basis")
                   val instrA = Mil.I {dests = Vector.new1 varA,
                                       n = 0,
                                       rhs = M.RhsSimple (M.SVariable affineBasis)}
                 in
                   (varA, [instrA], defs)
                 end
                   (* - case, same as + case *)
               | (Mil.Prims.PNumArith {operator = Mil.Prims.AMinus, ...}) =>
                 let
                   val varA = <@ mkRelatedVectorVar (state, env, info, bindVar, "basis")
                   val instrA = Mil.I {dests = Vector.new1 varA,
                                       n = 0,
                                       rhs = M.RhsSimple (M.SVariable affineBasis)}
                 in
                   (varA, [instrA], defs)
                 end
               | _ => Try.fail ()

         (* Construct vectorized version *)
         val vDescriptor = <@ mkVDescriptor (env, info, typ)
         val {typ = primNumericTyp, ...} = <@ PU.Prim.Dec.pNumArith primOp
         val primOp = Mil.Prims.PNumArith {typ = primNumericTyp,
                                           operator = Mil.Prims.APlus}
         val primOpV = Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                              masked = false,
                                              operator = primOp}
         val rhsV = Mil.RhsPrim {
                     prim = Mil.Prims.Vector primOpV,
                     createThunks = false,
                     typs = Vector.new0 (),
                     args = Vector.new2 (Mil.SVariable varA, 
                                         Mil.SVariable varL)}
         val varV = <@ mkRelatedVectorVar (state, env, info, bindVar, "vres")
         val instrV = Mil.I {dests = Vector.new1 varV,
                             n = 0,
                             rhs = rhsV}     

         (* Update the mapping of affine bases *)
         val affineBases = Rename.renameTo (affineBases, bindVar, varA)
         val vectorDefs = Rename.renameTo (vectorDefs, bindVar, varV)
         val defs = {vectorDefs = vectorDefs,
                     affineBases = affineBases,
                     reductionDefs = reductionDefs,
                     reductionInfos = reductionInfos}
         val instrs = instrsA @ [instrL, instrV]
               
       in
         (instrs, defs)
       end)

  (* /vectorizeFieldIdentifier/ Compute a new field for tuple subscripting and setting.
                                If the index is to the fixed part of a tuple, or the 
                                index is to a variable part as is constant or loop invariant,
                                then return NONE, i.e. no new field identifier for vectorized version
                                                      
                                If the index is to the variable part and is a unit stride affine
                                variable then construct an FiViVariable field. If the index is
                                non-affine construct a M.FiViIndexed field, indexing by a vector *)

  val vectorizeFieldIdentifier
      : (state * env * info * M.tupleDescriptor * M.variable * M.fieldIdentifier * vectorRenamings) ->
        (M.fieldIdentifier option) option =
    fn (state, env, info, tupDesc, arrayVar, field, vRenamings) =>
       try (fn() =>
         let
           val {vectorDefs, affineBases, reductionDefs, reductionInfos} = vRenamings
           (* compute the element type of the array *)
           val typ = variableType (state, env, arrayVar)
           val M.TD {array, ...} = tupDesc
           val M.FD {kind, ...} = <- array
           val vDescriptor = <@ mkVDescriptorFS (env, info, MU.FieldKind.fieldSize (getConfig env, kind))
         in
           case field 
            of M.FiFixed i => SOME (M.FiVectorFixed {descriptor = vDescriptor,
                                                     mask = NONE,
                                                     index = i})
             | M.FiVariable (M.SConstant c) =>  Try.fail ()
              (* ^^^ todo: Check if arrayVar has a vector version
               if so generate a FiVectorVariable where c is lifted *)
             | M.FiVariable (M.SVariable v) => 
               let
                 (* base depends upon whether our array variable has a vector
                  * version (i.e. we have already done some vectorization and now have
                  * a vector of arrays) *)
                 (* Calculate base, an element type of tuple = TbVector, else TbScalar *)
                   val base = case (Rename.use' (vectorDefs, arrayVar))
                               of SOME arrayVarV => Mil.TbVector
                                | NONE           => Mil.TbScalar
                 in
                   if isInvariantVar (state, env, info, v) then
                     SOME (M.FiVectorVariable {descriptor = vDescriptor,
                                               base = base,
                                               mask = NONE,
                                               index = M.SVariable v,
                                               kind = M.VikStrided 0})
                         (* strided iv *)
                   else case getStridedIV (state, env, info, v)
                         of SOME i => SOME (M.FiVectorVariable {descriptor = vDescriptor,
                                                                base = base,
                                                                mask = NONE,
                                                                index = M.SVariable v,
                                                                kind = M.VikStrided i})
                          | NONE => 
                          (* non-unit stride (including -1) or invariant var, lookup the vector version *)
                          (* and index by the vector *)
                          let 
                            val varV = <- (Rename.use' (vectorDefs, v))               
                          in SOME (M.FiVectorVariable {descriptor = vDescriptor,
                                                       base = base,
                                                       mask = NONE,
                                                       index = M.SVariable varV,
                                                       kind = M.VikVector})
                          end
               end
             | _ => Try.fail ()(* don't allow other field types so hard fail here *)
         end)
       
  (* /vectorizeArrayLoad/ *)
  val vectorizeArrayLoad : (state * env * info * M.instruction * vectorRenamings)
                           -> (instructions * vectorRenamings) option =
    fn (state, env, info, instr, vRenamings) =>
      try (fn () =>
       let
         val symTableM = stateGetSymTableM state
         val {vectorDefs, affineBases, reductionDefs, reductionInfos} = vRenamings

         (* Destruct instruction *)
         val M.I {dests, n, rhs} = instr
         val () = require (Vector.length dests = 1)
         val dest = Vector.sub(dests, 0)
         val M.TF {tupDesc, tup, field} = <@ MU.Rhs.Dec.rhsTupleSub rhs

         (* get the type of the read and vector element type *)
         val M.VI {typ, kind} = SymTableM.variableInfo (symTableM, tup)

         val field' = <@ vectorizeFieldIdentifier (state, env, info, tupDesc, tup, field, vRenamings)

         (* build the new instruction *)
         val (varV, instrV) = 
             case field'
              of NONE =>
                 (* if no new field, then is constant/invariant index
                  * thus just lift the original load *)
                   <@ liftSimple (state, env, info, M.SVariable dest, SOME dest)
               | SOME field' =>
                 (* if new field, then create new load instruction *)
                 let
                   (* sanity check that we should have a vector variable field *)
                   val fieldInfo = <@ MU.FieldIdentifier.Dec.fiVectorVariable field'
                   (* check to see if we are indexing from a vector
                    * of arrays - (if so the base is set to TbVector) *)
                   val tupVar' = case (#base fieldInfo)
                                  of Mil.TbVector => <@ Rename.use' (vectorDefs, tup)
                                   | Mil.TbScalar => tup
                   val varV = <@ mkRelatedVectorVar (state, env, info, dest, "vrez")
                   val rhsV = M.RhsTupleSub (M.TF {tupDesc = tupDesc,
                                                   tup = tupVar', 
                                                   field = field'})
                   val instrV = M.I {dests = Vector.new1 varV,
                                     n = 0,
                                     rhs = rhsV}
                 in (varV, instrV)
                 end
                             
         val vectorDefs = Rename.renameTo (vectorDefs, dest, varV)
         val vRenamings = {vectorDefs = vectorDefs,
                           affineBases = affineBases,
                           reductionDefs = reductionDefs,
                           reductionInfos = reductionInfos} 
       in
         ([instrV], vRenamings)
       end)

  (* /vectorizeArrayStore/ *)
  val vectorizeArrayStore : (state * env * info * M.instruction * vectorRenamings)
                            -> (instructions * vectorRenamings) option = 
    fn (state, env, info, instr, vRenamings) =>
      try (fn () =>
       let                   
         val E {config, loopInfo, ...} = env
         val LI {loopHeader, ...} = info
         val symTableM = stateGetSymTableM state
         val si = I.SymbolInfo.SiManager symTableM  

         (* Destruct instruction *)
         val M.I {dests, n, rhs} = instr
         val {tupField, ofVal} = <@ MU.Rhs.Dec.rhsTupleSet rhs
         val M.TF {tupDesc, tup, field} = tupField
          
         val field' = <@ vectorizeFieldIdentifier (state, env, info, tupDesc, tup, field, vRenamings)

         val liftConstantOrInvariant : (M.fieldIdentifier * M.simple * vectorRenamings) 
                                       -> (instruction list * vectorRenamings) option = 
          fn (field', simple, vRenamings) =>
             try (fn () =>                  
                     let 
                       val ((varV, valInstrs), vRenamings') = 
                           <@ vectorizeSimple' (state, env, info, simple, NONE, vRenamings)
                       val rhs' = M.RhsTupleSet {tupField = M.TF {tupDesc = tupDesc,
                                                                  tup = tup,
                                                                  field = field'},
                                                 ofVal = M.SVariable varV}
                       val instr' = M.I {dests = dests,
                                         n = n,
                                         rhs = rhs'}
                     in
                       (valInstrs @ [instr'], vRenamings')
                     end)
       in
         case field' 
          of NONE => Try.fail ()
            (* if no new field, then is constant/invariant index
             * thus- which would cause mutability which we want
             * to reject. *)
            (* if we have a new field *)
           | SOME field' =>
             (case ofVal
               of M.SConstant c => <@ liftConstantOrInvariant (field', ofVal, vRenamings)
                | M.SVariable v => 
                  if isInvariantVar (state, env, info, v) then
                    <@ liftConstantOrInvariant (field', ofVal, vRenamings)
                  else
                    let
                      (* lookup vector version of rhs *)
                      val varV = <@ Rename.use' (#vectorDefs vRenamings, v)
                      (* build new store instruction for vector rhs *)
                      val tf' = M.TF {tupDesc = tupDesc, tup = tup, field = field'}
                      val rhs' = M.RhsTupleSet {tupField = tf',
                                                ofVal = M.SVariable varV}
                      val instr' = M.I {dests = dests,
                                        n = n,
                                        rhs = rhs'}
                    in
                      ([instr'], vRenamings)
                    end)
       end)

  (* /vectorizeReduction/ - Vectorize a reduction instruction *)
  val vectorizeReduction : (state * env * info * M.instruction * vectorRenamings)
                            -> (instructions * vectorRenamings) option = 
    fn (state, env, info, instruction, defs) =>
      try (fn () =>
       let
        val {reductionDefs, ...} = defs
        val LI {loopParams, ...} = info
        (* Deconstruct instruction *)
        val M.I {dests, n, rhs} = instruction
        val () = require ((Vector.length dests) = 1)
        val bindVar = Vector.sub (dests, 0)

        (* get the operator *)
        val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)
        val primOp = <@ PU.T.Dec.prim prim
        val {typ = numericTyp, operator = operator} = <@ PU.Prim.Dec.pNumArith primOp

        (* get the param that is being reduced over *)
        val paramVar = <@ instrIsReduction (state, env, info, loopParams, instruction)
        (*val () = print "found var again\n"*)

        (* Produce the vector version of the arguments *)
        val () = require (Vector.length args = 2)
        val (arg1, arg2) = (Vector.sub(args, 0), Vector.sub(args, 1))
        (*val () = print "two args\n"*)

        (* Returns true if the operand is the parameter variable, else false *)
        val argIsParamVar : M.operand -> bool =
         fn arg => 
            (case arg
              of M.SVariable var => I.variableEqual(paramVar, var)
               | M.SConstant _   => false)
             
        val identifyParamArg = identifyInPair argIsParamVar
        (* select the argument which is a parameter variable *)
        val ((argParam, argX), reorderArgs) = <@ identifyParamArg (arg1, arg2)
        (*val () = print "identified args\n"*)

        (* see if we have already renamed it to a vector version *) 
        val paramVarV = case Rename.use'(reductionDefs, paramVar)
                         of SOME paramVarV => paramVarV
                          | NONE           => <@ mkRelatedVectorVar (state, env, info, paramVar, "reduxv")
        (*val () = print "made vector\n"*)


        val argParamV = M.SVariable paramVarV
        (* vectorize the value being reduced in *)
        val ((argXVVar, argXVInstrs), defs) = <@ vectorizeSimple' (state, env, info, argX, NONE, defs)
        val argXV = M.SVariable argXVVar

        (*val () = print "vectorized the other param\n"*)

                                                   
        (* build vector reduction instruction *)
        val vArgs = Vector.new2 (reorderArgs (argParamV, argXV))
        val typ = variableType (state, env, bindVar)
        val vDescriptor = <@ mkVDescriptor (env, info, typ)
        val primOpV = Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                             masked = false,
                                             operator = primOp}
        val bindVarV = <@ mkRelatedVectorVar (state, env, info, bindVar, "reduxv")
        val rhsV = M.RhsPrim {prim = Mil.Prims.Vector primOpV,
                              createThunks = false,
                              typs = Vector.new0(),
                              args = vArgs}
        val instrV = Mil.I {dests = Vector.new1 bindVarV,
                            n = 0,
                            rhs = rhsV}

        (*val () = print "built vedctor redux the m\n"*)

        (* update definition maps and reduction info map *)
        val {vectorDefs, affineBases, reductionDefs, reductionInfos} = defs
        val reductionDefs = Rename.renameTo(reductionDefs, bindVar, bindVarV)
        val reductionDefs = (case Rename.use'(reductionDefs, paramVar)
                              of SOME paramVarV => reductionDefs
                               | NONE => Rename.renameTo(reductionDefs, paramVar, paramVarV))
         
        (*val () = print "did environment stuff\n"*)

        val reductionInfo = {reduxNextVar = bindVar,
                             operator = operator,
                             operandVar = argXVVar,
                             typ = numericTyp}

        val reductionInfos = VD.insert(reductionInfos, paramVar, reductionInfo)
        val defs = {vectorDefs = vectorDefs,
                    affineBases = affineBases,
                    reductionDefs = reductionDefs,
                    reductionInfos = reductionInfos}
       in
         (argXVInstrs @ [instrV], defs)
       end)

  (* /vectorizeInstruction/ *)
  val vectorizeInstruction : (state * env * info * tags * M.instruction * Rename.t * vectorRenamings) ->
                             (Mil.instruction list * vectorRenamings) option =
   fn (state, env, info, tags, instr, lastValues, vRenamings) =>
      try (fn () => 
       let
         val Mil.I {dests, n, rhs} = instr
         val failDebug = Config.debug andalso (whyFail (getConfig env))
         val () = if failDebug then print ("[V] "^(Int.toString n)^": vectorizing...") else ()
         val tag = <@ ID.lookup (tags, n)
         (* select the function to vectorize the rhs *)
         val vectorFun = 
             case rhs 
              of (M.RhsSimple _) =>
                 (case tag 
                   of TagSimple    => vectorizeSimple
                    | TagAffine    => vectorizeSimpleAffine
                    | TagReduction => Try.fail () (* shouldn't tag a simple as reduction *))
               | (M.RhsPrim _) =>
                 (case tag
                   of TagSimple    => vectorizePrim 
                    | TagAffine    => vectorizePrimAffine
                    | TagReduction => vectorizeReduction)
                 
               | (M.RhsTupleSub _) => vectorizeArrayLoad
               | (M.RhsTupleSet _) => vectorizeArrayStore
               | _                 => Try.fail ()
       in <@ vectorFun (state, env, info, instr, vRenamings)
       end)

   (* ----------------------------------------------------------------------- *)
   (*   Last value calculations                                               *)
   (* ----------------------------------------------------------------------- *)

 (* /lastValueSimple'/ - Build the last value for a mil simple, companion to
                          lastValueSimple below which builds the whole last 
                          value instruction *)
  val lastValueSimple' : (state * env * info * M.simple * Rename.t) ->
                          M.simple option =
    fn (state, env, info, simple, lastValues) =>
     try (fn () =>
            case simple
               of M.SVariable var => <-
                   (case (Rename.use' (lastValues, var)) 
                      of SOME varL => SOME (M.SVariable varL)
                       | NONE =>
                           (* If invariant, variable is always last value *)
                           if isInvariantVar (state, env, info, var) then
                                  SOME (M.SVariable var)
                           else
                                  NONE)
               |  M.SConstant c => M.SConstant c)

  (* /lastValueSimple/ create new instructions for the last value calculation of a
                       simple instruction *)
  val lastValueSimple :  (state * env * info * M.instruction * Rename.t)
                         -> (instructions * Rename.t) option =
   fn (state, env, info, instr, lastValues) =>
     try (fn () =>
       let val M.I {dests, n, rhs} = instr
           val () = require ((Vector.length dests) = 1)
           val bindVar = Vector.sub (dests, 0)
           val simple = <- (MU.Rhs.Dec.rhsSimple rhs)

           val varL = mkRelatedVar (state, bindVar, "last")
           val simpleL = <- (lastValueSimple' (state, env, info, simple, lastValues))
           val rhsL = M.RhsSimple simpleL
           val instrL = M.I {dests = Vector.new1 varL, n = 0, rhs = rhsL}
           val lastValues' = Rename.renameTo(lastValues, bindVar, varL)
       in
         ([instrL], lastValues')
       end)

  (* /lastValuePrim/ create new instructions for the last value calculation of a prim
                      instruction *)
  val lastValuePrim : (state * env * info * M.instruction * Rename.t)
                      -> (instructions * Rename.t) option = 
    fn (state, env, info, instr, lastValues) =>
      try (fn () =>
        let
          val M.I {dests, n, rhs} = instr
          val () = require ((Vector.length dests) = 1)
          val bindVar = Vector.sub (dests, 0)
          val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)
          val primOp = <- (PU.T.Dec.prim prim)

          val varL = mkRelatedVar (state, bindVar, "last")

          val lastValueArgs = fn arg => <@ lastValueSimple' (state, env, info, arg, lastValues)
          val args' = Vector.map (args, lastValueArgs)

          val rhsL = M.RhsPrim {prim = prim,
                                createThunks = createThunks,
                                typs = typs,
                                args = args'}
          val instrL = M.I {dests = Vector.new1 varL, n = 0, rhs = rhsL}
          val lastValues' = Rename.renameTo(lastValues, bindVar, varL)
        in
          ([instrL], lastValues')
        end)         


  (* /lastArrayLoad/ *)
  val lastArrayLoad : (state * env * info * instruction * Rename.t) ->
                      (instruction list * Rename.t) option = 
    fn (state, env, info, instr, lastValues) =>
      try (fn () =>
       let
         (* Destruct instruction *)
         val M.I {dests, n, rhs} = instr
         val M.TF {tupDesc, tup, field} = <- (MU.Rhs.Dec.rhsTupleSub rhs) 
         val () = require ((Vector.length dests) = 1)
         val bindVar = Vector.sub (dests, 0)
         
         val varL = mkRelatedVar (state, bindVar, "last")
         val field' = 
             case field
              of M.FiFixed i => M.FiFixed i
               | M.FiVariable operand =>
                 let
                   val operand' = 
                       <@ lastValueSimple' (state, env, info, operand, lastValues)
                 in M.FiVariable operand'
                 end
               | _ => Try.fail ()
         val rhsL = M.RhsTupleSub (M.TF {tupDesc = tupDesc,
                                         tup = tup,
                                         field = field'})
         val instrL = M.I {dests = Vector.new1 varL, n = n, rhs = rhsL}
         val lastValues' = Rename.renameTo(lastValues, bindVar, varL)                      
       in
         ([instrL], lastValues')
       end)

  (* /lastValueInstruction/ for an instruction computes the last value at the end of 
                            a vector iteration *)
  val lastValueInstruction : (state * env * info * tags * M.instruction * Rename.t) ->
                             (Mil.instruction list * Rename.t) option = 
    fn (state, env, info, tags, instr, lastValues) =>
      try (fn () =>
       let
         val Mil.I {dests, n, rhs} = instr
         val tag = <- (ID.lookup(tags, n))

         val failDebug = Config.debug andalso (whyFail (getConfig env))
         val () = if failDebug then print ("[V] "^(Int.toString n)^": last value...") else ()
       in
         case tag
           of TagReduction => ([], lastValues)
            | _ => 
              case rhs
               of (M.RhsSimple _)   => <@ lastValueSimple (state, env, info, instr, lastValues)
                | (M.RhsPrim _)     => <@ lastValuePrim (state, env, info, instr, lastValues)
                | (M.RhsTupleSub _) => <@ lastArrayLoad (state, env, info, instr, lastValues)
                (* don't repeat array sets for last values *)
                | (M.RhsTupleSet _) => ([], lastValues)
                (* keep other instructions as is (revise this?) *)
                | _ =>  ([instr], lastValues)
       end)

   (* ----------------------------------------------------------------------- *)
   (*   END OF Last value calculations                                        *)
   (* ----------------------------------------------------------------------- *)
              

 (* /transformInstructions *)
 val transformInstructions : (state * env * info * tags *
                              instructions * (vectorRenamings * Rename.t))
                              -> (instructions list * instructions list * instructions list
                                  * (vectorRenamings * Rename.t)) option =
  fn (state, env, info, tags, instrs, (genDefs, lastValues)) =>
   try (fn () =>
     let
      val E {config, ...} = env
      val failDebug = Config.debug andalso (whyFail config)

      (* Used to map over the instructions to generate vector instructions *)
      val vectorize : instruction * vectorRenamings -> (instructions option * vectorRenamings) =
         fn (instr, genDefs) =>
           case vectorizeInstruction (state, env, info, tags, instr, lastValues, genDefs)
                 of SOME (instrs, genDefs') => let val () = if failDebug then print "ok\n" else ()
                                               in (SOME instrs, genDefs')
                                               end
                  | NONE                    => let val () = if failDebug then print "fail\n" else ()
                                               in (NONE, genDefs)
                                               end

      (* Used to map over the instructions to generate last value instructions *)
      val lasts : instruction * Rename.t -> (instructions option * Rename.t) =
          fn (instr, lastVals) =>
           case (lastValueInstruction (state, env, info, tags, instr, lastVals))
                of SOME (instrs, lastVals') => let val () = if failDebug then print "ok\n" else ()
                                               in (SOME instrs, lastVals')
                                               end 
                 | NONE                     => let val () = if failDebug then print "fail\n" else ()
                                               in (NONE, lastVals)
                                               end
      
      (* Process the scalars striping out tuple sets *)
      val scalars : instruction -> (instructions option) = 
          fn instr =>
             let val M.I {dests, n, rhs} = instr
                 val tag = <- (ID.lookup(tags, n))
             in case tag
                    (* anything tagged as a reduction is removed *)
                 of TagReduction => SOME []
                    (* otherwise *)
                  | _            => (case rhs
                                      of M.RhsTupleSet _ => SOME []
                                       | _               => SOME [instr])
             end

      val (instrsV, genDefs') = U.List.mapFoldl (instrs, genDefs, vectorize)
      val instrsV' = <- (distribute instrsV)

      val (instrsL, lastValues') = U.List.mapFoldl (instrs, lastValues, lasts)
      val instrsL' = <- (distribute instrsL)

      val instrsS = List.map (instrs, scalars)
      val instrsS' = <- (distribute instrsS)
     
     in
      (instrsS', instrsV', instrsL', (genDefs', lastValues'))
     end)


 (* /signedType/ returns SOME true if a type is numeric and signed
                         SOME false if a type is numeric and unsigned 
                         NONE if a type is not numeric *)
 val signedType : Mil.typ -> bool option = 
   fn typ =>
     let
        val primSignedTyp : Mil.Prims.numericTyp -> bool option =
          fn pTyp =>
             case pTyp
               of M.Prims.NtRat => SOME true
                | M.Prims.NtInteger (M.Prims.IpArbitrary) => SOME true
                | M.Prims.NtInteger (M.Prims.IpFixed arb) =>
                          (case arb 
                            of IA.T (_, IA.Signed) => SOME true
                             | IA.T (_, IA.Unsigned) => SOME false)
                | M.Prims.NtFloat _ => SOME true
     in
       case typ
         of Mil.TNumeric numTyp => primSignedTyp numTyp
          | _                   => NONE
     end

 (* /addOrSubtractRat/ Takes a rat and a type and returns an option of a rat and a bool
                        The bool signals whether addition should be used for combining the rat
                        and further computations (with a value of true), or whether subtraction
                        should be used, represented by false.

                             SOME (rat, true) if the type is signed and the rat is +ve
                             SOME (-rat, false) if the type is unsigned and the rat is -ve
                                                (thus, the rat returned rat is +ve)
                             SOME (rat, true) if the type is unsigned and the rat is +ve *)
 val addOrSubtractRat : (Rat.t * Mil.typ) -> (Rat.t * bool) option = 
    fn (rat, typ) =>
        let val signed = <- (signedType typ)
        in  if signed then
               (* use plus, the type is signed, so can use + with -ve rat *)
               SOME (rat, true)
            else
              if (Rat.<(rat, Rat.zero)) then
                (* unsigned type with -ve rat, use subtract with +ve rat *)
                SOME (Rat.-(Rat.zero, rat), false)
              else
                (* unsigned type with +ve rat, use plus with +ve rat *)
                SOME (rat, true)
        end       
    
 (* /vectorizeBIV/ - creates a vector version of a base induction variable *)
 val vectorizeBIV : state * env * info * ML.inductionVariable * vectorRenamings ->
                    (instructions * vectorRenamings) option = 
  fn (state, env, info, iv, vRenamings) =>           
    try (fn () =>
       let
         val symTableM = stateGetSymTableM state
          val E {config, ...} = env
          val LI {vectorLength, ...} = info
          val vectorLength = <- vectorLength
          val si = I.SymbolInfo.SiManager symTableM       

          val {vectorDefs, affineBases, reductionDefs, reductionInfos} = vRenamings
          val {variable, init, step} = <- (decBaseInductionVar iv)

          (* Induction variable type must be an integral *)
          val ivType = variableType (state, env, variable)
          val ivNumType = <- (MU.Typ.Dec.tNumeric ivType)

          (* Lift induction variable *)
          val (varL, instrL) = <@ liftSimple (state, env, info, M.SVariable variable, SOME variable)
  
          (* test if we have +ve/-ve rat and whether the type allows negatives *)
          val (step', useAddition) = <- (addOrSubtractRat (step, ivType))

          (* Build step bases *)
          val computeIndex = 
           fn n =>
              let 
                val i = Rat.*(step', Rat.fromInt n) 
                val oper = <- (ratToConstant (state, i, ivNumType))
              in
                oper
              end
          val elts = Vector.tabulate (vectorLength, computeIndex)

          (* Create the vector operation to construct the basis of the induction variable *)
          val vDescriptor = <@ mkVDescriptor (env, info, ivType)
          val primB = Mil.Prims.ViData {descriptor = vDescriptor,
                                        operator = Mil.Prims.DVector}
          val rhsB = Mil.RhsPrim {prim = Mil.Prims.Vector primB,
                                  createThunks = false,
                                  typs = Vector.new1 (ivType),
                                  args = elts}

          (* Create the instruction that binds the basis *)
          val varB = <@ mkRelatedVectorVar (state, env, info, variable, "basis")
          val instrB = M.I {dests = Vector.new1 varB,
                            n = 0,
                            rhs = rhsB}

          (* chose addition or subtraction *)
          val operator = if useAddition then
                           Mil.Prims.APlus 
                         else
                           Mil.Prims.ATimes

          (* Create induction variable vector instruction *)
          val primOp = Mil.Prims.PNumArith {typ = ivNumType,
                                            operator = operator}
          val primIV = Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                              masked = false,
                                              operator = primOp}
          val rhsIV = M.RhsPrim {
                           prim = Mil.Prims.Vector primIV,
                           createThunks = false,
                           typs = Vector.new0 (),
                           args = Vector.new2 (M.SVariable varL,
                                               M.SVariable varB)}
          val varIV = <@ mkRelatedVectorVar (state, env, info, variable, "vres")
          val instrIV = M.I {dests = Vector.new1 varIV,
                             n = 0,
                             rhs = rhsIV}
          (* Associate the induction variable to the vector version *)
          val affineBases' = Rename.renameTo (affineBases, variable, varB)
          val vectorDefs' = Rename.renameTo (vectorDefs, variable, varIV)
          val vRenamings' = {vectorDefs = vectorDefs',
                             affineBases = affineBases',
                             reductionDefs = reductionDefs,
                             reductionInfos = reductionInfos}
       in
          ([instrL, instrB, instrIV], vRenamings')
       end)

 (* /lastValuBIV/ - create the last value of a base induction variable at the end
                     of a vectorized iteration *)
 val lastValueBIV : state * env * info * ML.inductionVariable * Rename.t ->
                    (instructions * Rename.t) option = 
  fn (state, env, info, iv, lastValues) =>
    try (fn () =>
        let
          val symTableM = stateGetSymTableM state
          val si = I.SymbolInfo.SiManager symTableM         
          val E {config, ...} = env
          val LI {vectorLength, ...} = info
          val vectorLength = <- vectorLength
                             
          val {variable, init, step} = <@ decBaseInductionVar iv
          (* Induction variable type must be an integral *)
          val ivType = variableType (state, env, variable)
          val ivNumType = <- (MU.Typ.Dec.tNumeric ivType)
                                              
          val info = SymTableM.variableInfo (symTableM, variable)
          val varL = SymTableM.variableRelated (symTableM, variable, "last", info)
          
          (* test if we have +ve/-ve rat and whether the type allows negatives *)
          val (step', useAddition) = <@ addOrSubtractRat (step, ivType)

          (* scale the BIV step by the (vector length - 1) *)
          (* for an IV with step S, the last value is = i + S*(vecLen - 1) *)          
          val step' = Rat.*(step', Rat.fromInt (vectorLength - 1))
          val iterN = <@ ratToConstant (state, step', ivNumType)

          val operator = if useAddition then
                           Mil.Prims.APlus
                         else
                           Mil.Prims.AMinus

          val primOp = Mil.Prims.PNumArith {typ = ivNumType,
                                            operator = operator}
          val rhs = M.RhsPrim {prim = Mil.Prims.Prim primOp,
                               createThunks = false,
                               typs = Vector.new0 (),
                               args = Vector.new2 (M.SVariable variable,
                                                   iterN)}
          val instrL = M.I {dests = Vector.new1 varL,
                            n = 0,
                            rhs = rhs}
          val lastValues' = Rename.renameTo (lastValues, variable, varL)
        in
          ([instrL], lastValues')
        end)                      

 (* /transformBIV/ :*)
 val transformBIV : (state * env * info * ML.inductionVariable * (vectorRenamings * Rename.t))
                    -> ((instructions * instructions) * (vectorRenamings * Rename.t)) option =
  fn (state, env, info, iv, (vRenamings, lastValues)) =>
    try (fn () =>
     let
       val (instrsV, vRenamings') = <@ vectorizeBIV (state, env, info, iv, vRenamings)
       val (instrsL, lastValues') = <@ lastValueBIV (state, env, info, iv, lastValues)
     in
       ((instrsV, instrsL), (vRenamings', lastValues'))
     end)

 (* /transformBIVs/ - Takes the base induction variables and
      creates vectorized and last value computations, returning a pair
      of each set of instructions (vector and last instructions respectively)
      as well as dictionary mapping 
      BIVars to their vector versions and last value versions respectively -

      Vectorized and last value calculations for base induction variables are inserted
      before any other instructions from the loop *)
 val transformBIVs : (state * env * info) ->
                     (instructions list * instructions list * (vectorRenamings * Rename.t)) option = 
  fn (state, env, info) =>
    try (fn () =>
      let
       val E {loopInfo, ...} = env
       val LI {loopHeader, ...} = info
       val inductionVars = ML.getInductionVariables (loopInfo, loopHeader)
       val justBIVs = fn x => case x of (ML.BIV iv) => SOME (ML.BIV iv) | _ => NONE
       val baseIVs = List.keepAllMap (inductionVars, justBIVs)

       val transformBIV = 
        fn (iv, renamings) =>
          case transformBIV (state, env, info, iv, renamings)
             of SOME (instrs, renamings') => (SOME instrs, renamings')
              | NONE => (NONE, renamings)

       val renamings = ({vectorDefs = Rename.none,
                         affineBases = Rename.none,
                         reductionDefs = Rename.none,
                         reductionInfos = VD.empty}, Rename.none)
       val (instrs, renamings') = U.List.mapFoldl (baseIVs, renamings, transformBIV)
        
       val instrs' = <- (distribute instrs)
       val (vectors, lasts) = List.unzip instrs'
      in
        (vectors, lasts, renamings')
      end)         

 (* -------------------------------------------------------------------------- *)
 (*  Depth 1 : Pass over the loop information built by MilLoop, creating a     *)
 (*            new loop forest from the old                                    *)
 (*            vectorizeForest, vectorizeLoopTree, vectorizeLoop act as        *)
 (*            mutually recursive functions, traversing a loop forest,         *)
 (*            returning a new forest a list of new blocks not in the loops    *)
 (* -------------------------------------------------------------------------- *)

    (* <todo> Provide a way to choose the vector length
               based on the maximum vector length or minimum vector length.
                max vector length = vector register size/min size 
                of types in the computation (may require some emulation
                but can result in higher throughput on some parts of a
                computation)
              vs.
                min vector length = vector register size/max size of 
                types in the computation (requires no emulation, but
                may not fully utilise packing at some types) </todo> *)

 val calcVectorLen : (state * env * int * int) -> int option =
   fn (state, env, minSize, maxSize) =>
     let 
       val E {config, vectorConfig, ...} = env
       val sizes = List.map (PU.VectorConfig.allEnabledSizes vectorConfig, PU.VectorSize.numBits)
       val widths = List.map (sizes, fn sz => sz div maxSize)
       val widthO = List.fold (widths, NONE, fn (width, prev) => 
                                                case prev
                                                 of SOME max => if width > max then SOME width else  prev
                                                  | NONE     => SOME width)
     in widthO
     end

 (* /identifyTargets/ - Orders two targets. If one points to the head and 
                        the other elsewhere then they are ordered (head, other) *)
 val identifyTargets : Mil.label ->
                      ((M.target * M.target), (M.target * M.target)) bidirectional =
   fn header =>  identifyInPair (fn target => 
                                    let val M.T {block, ...} = target
                                    in I.labelEqual(block, header)
                                    end)


 (* /identifyTestParams/ Takes a transfer and returns the pair of operands in the test condition 
                         for the loop, where the first operand is the induction variable involved and the second
                         operand is the bound, along with a function that puts the two operands back in the correct
                         order 
                         e.g. if the test for the loop is i < N then
                              (i, N) is returned along with a function: fn (x, y) => (x, y)
                              if the test for the loop is N < i then
                              (i, N) is retuend along with a function: fn (x, y) => (y, x) *)
 val identifyTestParams : (state * env * info * M.transfer) -> 
                          ((M.operand * M.operand) * ((M.operand * M.operand) ->(M.operand * M.operand))) option =
  fn (state, env, info, transfer) =>
   try (fn () =>
    let
      val E {config, fmil, loopInfo, ...} = env
      val LI {loopHeader, ...} = info
      (* deconstruct transfer *)
      val switch = <- (MU.Transfer.Dec.tCase transfer)
      val {select, on, cases, default} = switch
       val () = <@ MU.Selector.Dec.seConstant select
      val testVar = <- (MU.Simple.Dec.sVariable on)
      
      (* find definition of the test *)
      val testVarDef = FMil.getVariable (fmil, testVar)
      val (_, rhs) = <- (case testVarDef of FMil.VdInstr x => SOME x 
                                          | _              => NONE)     

      (* deconstruct the test *)
      val {args, ...} = <- (MU.Rhs.Dec.rhsPrim rhs)

      val () = require (Vector.length args = 2)
      val (arg1, arg2) = (Vector.sub(args, 0), Vector.sub(args, 1))

      (* Returns true if the operand is an induction variable, else false *)
      val argIsInductionVar : M.operand -> bool =
       fn arg => 
          bool (try (fn () =>
                        let
                          val var = <@ MU.Simple.Dec.sVariable arg
                          val iv = <@ isInductionVariable (state, env, info, var)
                        in
                          iv
                        end))
 
      val identifyIVArg = identifyInPair argIsInductionVar
      (* select the argument which is a local induction variable *)
      val ((argIV, argB), reorderArgs) = <- (identifyIVArg (arg1, arg2))
    in
      ((argIV, argB), reorderArgs)
    end)

 (* /buildNewTest
     Given a transfer and the last values calculations,
                    compute a new transfer and instructions to compute the test for
                    the vectorized loop.
                    Behaviour:
                      i' = i + s
                      test = i' < b
                      case test of ...                                  
                    The last value of i' is looked up, and s * (vl - 1) 
                    is added in a new instruction, where vl is the vector length
                       i'' = i'_last + s * (vl - 1)
                    This is the value i will have at the end of the next vector
                    iteration. We then build a new test for the transfer:
                      test' = i'' <- b
                      case test' of ... *)                       
                   
 val buildNewTest : (state * env * info * M.transfer * Rename.t) ->
                    (instructions * M.transfer) option =
  fn (state, env, info, transfer, lastValues) =>
   try (fn () =>
           
    let
      val E {config, fmil, loopInfo, ...} = env
      val LI {loopHeader, vectorLength, ...} = info
      val vectorLength = <- vectorLength
      val symTableM = stateGetSymTableM state
      val si = I.SymbolInfo.SiManager symTableM
      
      (* deconstruct transfer *)
      val switch = <- (MU.Transfer.Dec.tCase transfer)
      val {select, on, cases, default} = switch
       val () = <@ MU.Selector.Dec.seConstant select
      val testVar = <- (MU.Simple.Dec.sVariable on)

      (* find definition of the test *)
      val testVarDef = FMil.getVariable (fmil, testVar)
      val (_, rhs) = <- (case testVarDef of FMil.VdInstr x => SOME x 
                                          | _              => NONE)

      (* deconstruct the test, get the num type from the convert*)
      val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)
      val prim' = <- (PU.T.Dec.prim prim)
      val {typ, operator} = <- (PU.Prim.Dec.pNumCompare prim') 
      (* check we have an precise type for the loop counter *)
      val () = require (isPreciseNumericTyp typ)

      (* deconstruct the test, get the num type from the convert*)
      val {prim, createThunks, typs, args} = <- (MU.Rhs.Dec.rhsPrim rhs)

      (* identify the operands in the transfer's test and 
         get the variable and step information for the induction variable *)
      val ((argIV, argB), reorderArgs) = <@ identifyTestParams (state, env, info, transfer)
      val argIVvar = <- (MU.Simple.Dec.sVariable argIV)
      val argIVtype = variableType (state, env, argIVvar)
      (* sanity check- must match the prim type of the compare *)
      val () = require (MU.Typ.eq(argIVtype, M.TNumeric typ))
      val argIVinfo = <@ isInductionVariable (state, env, info, argIVvar)
      val {step, ...} = ML.canonizeInductionVariable argIVinfo

      (* last value of the next value for the IV 
          i_next_last = i_last + S 
          i_last = i + S*(l-1) *)
      val argIVVarLast = <- (Rename.use' (lastValues, argIVvar))
      
      (* test if we have +ve/-ve rat and whether the type allows negatives *)
      val (step', useAddition) = <- (addOrSubtractRat (step, argIVtype))

      (* need to calculate: i_next_last + S*(l-1) *) 
      val iterEnd = <@ ratToConstant (state, Rat.*(step', Rat.fromInt (vectorLength - 1)), typ)

      (* build the end of next iteration value *)
      val operator = if useAddition then
                       Mil.Prims.APlus
                     else
                       Mil.Prims.AMinus

      val varNN = mkFreshLocalVar (state, "next_next_last", argIVtype)           
      val primNN = Mil.Prims.PNumArith {typ = typ,
                                        operator = operator}
      val argsNN = Vector.new2 (M.SVariable argIVVarLast, iterEnd)
      val rhsPrimNN = Mil.RhsPrim {prim = Mil.Prims.Prim primNN,
                                   createThunks = false,
                                   typs = Vector.new0 (),
                                   args = argsNN}
      val instrNN = M.I {dests = Vector.new1 varNN, 
                         n = 0,
                         rhs = rhsPrimNN}

      (* build the new test *)
      val newTestVar = mkRelatedVar (state, testVar, "new_test")
      val res = 
          let
            (* Peephole optimization - if we have a loop with a constant bound
             * and we know that it exists after 1 iteration, then eliminate the
             * back edge
             *)
            val exit = 
                Try.try 
                  (fn () => 
                      let
                        val MilLoop.TC {init, flip1, comparison, bound, flip2, ...} = 
                            <@ MilLoop.getTripCount (loopInfo, loopHeader)
                        val (m, i, c) = init
                        val i = <@ MU.Simple.Dec.sConstant i
                        val b = <@ MU.Simple.Dec.sConstant bound
                        val i = <@ constantToRat i
                        val b = <@ constantToRat b
                        val t0 = Rat.+ (Rat.*(m, i), c)
                        val prim2 = if flip1 then comparison else notPrim (comparison)
                        val (c1, c2) = if flip1 then (b, t0) else (t0, b)
                        val (c1, c2) = if flip2 then (c2, c1) else (c1, c2)

                        val exits = case prim2
                                     of Mil.Prims.CEq => Rat.equals(c1, c2)
                                      | Mil.Prims.CNe => not (Rat.equals (c1, c2))
                                      | Mil.Prims.CLt => Rat.< (c1, c2)
                                      | Mil.Prims.CLe => Rat.<= (c1, c2)
                        val () = Try.require exits
                        val (targets, rebuildSwitch) = <- (hasTwoTargets switch)
                        val ((headTarget, exitTarget), reorderTargets) = <- (identifyTargets loopHeader targets)
                        val T = MU.Bool.T (getConfig env)
                        val t = M.TCase {select = select, 
                                         on = M.SConstant T,
                                         cases = Vector.new1 (T, exitTarget),
                                         default = SOME headTarget}
                      in t
                      end)
          in case exit
              of SOME t => ([], t)
               | NONE   => 
                 let
                   val newTestArgs = reorderArgs (M.SVariable varNN, argB)
                   val newTestRhs = M.RhsPrim {prim = prim,
                                               createThunks = createThunks,
                                               typs = typs,
                                               args = Vector.new2 newTestArgs}
                   val newTestInstr = M.I {dests = Vector.new1 newTestVar,
                                           n = 0,
                                           rhs = newTestRhs}
                                          
                                          (* build the new transfer *)
                   val newTransfer = M.TCase {select = select, 
                                              on = M.SVariable newTestVar,
                                              cases = cases,
                                              default = default}
                 in ([instrNN, newTestInstr], newTransfer)
                 end
          end

    in res
    end)

 (* /operatorIdentity/ - For an operation at a certain type, returns the identity, or in the case
                         of an idempotent operation (with a possibly unknown identity) an optional
                         element can be supplied *)
 val operatorIdentity : (env * M.Prims.arithOp * M.Prims.numericTyp * M.variable option) -> M.simple option = 
   fn (env, operator, typ, idempotent) =>
    try (fn () =>
       let 
         val E {config, ...} = env
         (* sanity check that the operation is allowed as a reduction *)
         val () = require (PU.Properties.Associativity.arithOp (config, typ, operator)
                           orelse allowReductions config)
         val allowRedux = allowReductions config 
         val identity = <- 
             (case operator 
               of Mil.Prims.APlus => (case typ
                                       of M.Prims.NtInteger (M.Prims.IpFixed arbTyp) =>
                                          (SOME o M.SConstant o M.CIntegral o IA.fromInt) (arbTyp, 0)
                                        | M.Prims.NtFloat (M.Prims.FpSingle) =>
                                          if allowRedux then
                                            (SOME o M.SConstant o M.CFloat) 0.0
                                          else
                                            NONE
                                        | M.Prims.NtFloat (M.Prims.FpDouble) =>
                                          if allowRedux then
                                            (SOME o M.SConstant o M.CDouble) 0.0
                                          else
                                            NONE
                                        | _ => NONE)
                | Mil.Prims.ATimes => (case typ
                                        of M.Prims.NtInteger (M.Prims.IpFixed arbTyp) =>
                                           (SOME o M.SConstant o M.CIntegral o IA.fromInt) (arbTyp, 1)
                                         | M.Prims.NtFloat (M.Prims.FpSingle) =>
                                           if allowRedux then
                                             (SOME o M.SConstant o M.CFloat) 1.0
                                           else
                                             NONE
                                         | M.Prims.NtFloat (M.Prims.FpDouble) =>
                                           if allowRedux then
                                             (SOME o M.SConstant o M.CDouble) 1.0
                                           else
                                             NONE
                                         | _ => NONE)
                | Mil.Prims.AMax => try (fn () =>
                                       let val idemp = <- idempotent
                                       in M.SVariable idemp
                                       end)
                | Mil.Prims.AMin => try (fn () =>
                                       let val idemp = <- idempotent
                                       in M.SVariable idemp
                                       end)
                | _ => NONE)
       in
         identity
       end)                                                                      
                                                                            

 (* /buildReduxEnd/ builds the final instruction(s) which will perform the final reduction of a vectorized reduction *)
 val buildReduxEnd : (state * env * info * Rename.t * M.variable * reductionInfo * Rename.t)
                     -> (M.instruction list * Rename.t) option = 
  fn (state, env, info, reductionDefs, reduxVar, reductionInfo, reduxRenamings) =>
     try (fn () =>
       let
         val LI {vectorLength, ...} = info
         val vectorLength = <- vectorLength
         val {reduxNextVar, operator, operandVar, typ} = reductionInfo
         val mTyp = Mil.TNumeric typ (* mil type of the reduction *)
         val vDescriptor = <@ mkVDescriptor (env, info, mTyp) (* vector descriptor of vector reduction *)

         (* compute the final reduction for the "previous" value - end result of the reduction
            on entering the last iteration of the loop *)
         val reduxVarV = <- (Rename.use'(reductionDefs, reduxVar))
         val reduxNextVarV = <- (Rename.use'(reductionDefs, reduxNextVar))

         (* generate sub instructions for each element of the reduction's operand - apart from the last *)
         val createSubInstruction : int -> (M.instruction * M.variable) =
           fn n =>
              let
                val prim = M.Prims.ViData {descriptor = vDescriptor,
                                           operator = M.Prims.DSub n}
                val rhs = M.RhsPrim {prim = M.Prims.Vector prim,
                                     createThunks = false,
                                     typs = Vector.new1 mTyp,
                                     args = Vector.new1 (M.SVariable operandVar)}
                val bindVar = mkFreshLocalVar (state, "redux_sub", mTyp)
                val instr = M.I {dests = Vector.new1 bindVar,
                                 n = 0,
                                 rhs = rhs}
              in
                (instr, bindVar)
              end              
         val (subInstrs, subVars) = List.unzip (List.tabulate(vectorLength - 1, createSubInstruction))

         (* generate the identity/idempotent for the instruction *)
         val initOperand = <- (operatorIdentity (env, operator, typ, SOME (List.nth(subVars, 0))))
         
         (* build up the last (vl - 1) results in a vector constant with the identity at the end *)
         val prevRConstVector = Vector.fromList (List.map(subVars, M.SVariable) @ [initOperand])

         val prevRPrim = Mil.Prims.ViData {descriptor = vDescriptor,
                                           operator = Mil.Prims.DVector}
         val prevRRhs = Mil.RhsPrim {prim = Mil.Prims.Vector prevRPrim,
                                     createThunks = false,
                                     typs = Vector.new1 mTyp,
                                     args = prevRConstVector}
         val prevRVar = mkRelatedVar (state, operandVar, "prevR")
         val prevRInstr = M.I {dests = Vector.new1 prevRVar,
                               n = 0,
                               rhs = prevRRhs}

         (* apply the reduction of the last (vl - 1) results with the partially reduced values
            at the start of this last iteration *)

         val primOp = M.Prims.PNumArith {typ = typ,
                                         operator = operator}
         val prevPrimV = Mil.Prims.ViPointwise {descriptor = vDescriptor,
                                                masked = false,
                                                operator = primOp}
         val prevRhsV = M.RhsPrim {prim = Mil.Prims.Vector prevPrimV,
                                   createThunks = false,
                                   typs = Vector.new0(),
                                   (* <todo> no attempt here to reproduce original association order
                                             - at the moment this is fine as we only allow fully
                                               associative/commutative operations - but later
                                               may want to propogate associativity info from the tagging </todo> *)
                                   args = Vector.new2 (M.SVariable prevRVar,
                                                       M.SVariable reduxVarV)}
         val prevVar = mkRelatedVar (state, operandVar, "lastV")
         val prevInstr = Mil.I {dests = Vector.new1 prevVar,
                                n = 0,
                                rhs = prevRhsV}

         (* reduces this value to get the second to last reduction result *)
         val reduxPrim = Mil.Prims.ViReduction {descriptor = vDescriptor,
                                                associativity = Mil.Prims.AAny,
                                                operator = primOp}
         val prevLastRhs = Mil.RhsPrim {prim = Mil.Prims.Vector reduxPrim,
                                        createThunks = false,
                                        typs = Vector.new0 (),
                                        args = Vector.new2 (initOperand, M.SVariable prevVar)}
         val prevLastVar = mkRelatedVar (state, reduxVar, "redux_last")
         val prevLastInstr = M.I {dests = Vector.new1 prevLastVar,
                                  n = 0,
                                  rhs = prevLastRhs}                                   

         (* compute the final reduction - the end result of the reduction *)
         val lastRhs = Mil.RhsPrim {prim = Mil.Prims.Vector reduxPrim,
                                       createThunks = false,
                                       typs = Vector.new0 (),
                                       args = Vector.new2 (initOperand, M.SVariable reduxNextVarV)}
         val lastVar = mkRelatedVar (state, reduxNextVar, "redux_last")
         val lastInstr = M.I {dests = Vector.new1 lastVar,
                              n = 0,
                              rhs = lastRhs}

         (* add renamings for the previous and final results *)
         val reduxRenamings' = Rename.renameTo(reduxRenamings, reduxVar, prevLastVar)
         val reduxRenamings' = Rename.renameTo(reduxRenamings', reduxNextVar, lastVar)                              
       in
         (subInstrs @ [prevRInstr, prevInstr, prevLastInstr, lastInstr], reduxRenamings')
       end)

 (* /buildCleanup/ constructs the cleanup block that follows a vectorized loop *)
 val buildCleanup : (state * env * info * M.transfer * Rename.t * Rename.t *
                     (instructions) * Rename.t * reductionInfo VD.t) ->
                    M.block option =
  fn (state, env, info, serialTransfer, sLoopRenamings, lastValues,
      lastValInstructions, reductionDefs, reductionInfos) =>
   try (fn () =>
           
    let
      val E {config, fmil, loopInfo, ...} = env
      val LI {loopHeader, vectorLength, ...} = info
      val vectorLength = <- vectorLength
      val symTableM = stateGetSymTableM state
      val si = I.SymbolInfo.SiManager symTableM

      (* compute the inverse map from the new serial variables to the old *)
      val originalVars = (Rename.invert sLoopRenamings)
      
      (* deconstruct transfer *)
      val switch = <@ MU.Transfer.Dec.tCase serialTransfer
      val {select, on, cases, default} = switch
       val () = <@ MU.Selector.Dec.seConstant select
      val testVar = <@ MU.Simple.Dec.sVariable on
      val originalTestVar = <@ Rename.use' (originalVars, testVar)
      
      (* find definition of the test *)
      val testVarDef = FMil.getVariable (fmil, originalTestVar)
      val (_, rhs) = <- (case testVarDef of FMil.VdInstr x => SOME x 
                                          | _              => NONE)   

      (* buil new test, replacing the given one *)
      val newTestVar = mkRelatedVar (state, originalTestVar, "cleanup")
      val newTestInstr0 = M.I {dests = Vector.new1 newTestVar,
                               n = 0,
                               rhs = rhs}
      (* in the test, rewrite variables to their last value variables i.e.
          test = i' < B
      =>  test = i'_last < B 
         thus, getting the last iteration value *)
      val newTestInstr = MilRename.Var.instruction (config, lastValues, newTestInstr0)
      
      (* build the new transfer *)
      val transfer' = M.TCase {select = select,
                               on = M.SVariable newTestVar,
                               cases = cases,
                               default = default}

      (* rename all the last value calculations *)
      val renameLastVariables : M.instruction * Rename.t -> M.instruction * Rename.t =
        fn (instruction, subst) =>
           let
             val M.I {dests, n, rhs} = instruction
             val dest = Vector.sub(dests, 0)
             val dest' = mkRelatedVar (state, dest, "cleanup")
             val subst' = Rename.renameTo(subst, dest, dest')
             val instruction' = MilRename.Var.instruction (config, subst', instruction)
           in
             (instruction', subst')
           end                             
      val (instructions', lastRenamings) = U.List.mapFoldl (lastValInstructions, Rename.none, renameLastVariables)

      (* compute the final reductions *)
      val buildReduxEnd' = fn ((var, reductionInfo), renaming) =>
                              case (buildReduxEnd (state, env, info, reductionDefs, var, reductionInfo, renaming))
                                of SOME (instrs, renaming') => (SOME instrs, renaming')
                                 | NONE                     => (NONE, renaming)

      val (reduxInstrs, endRenamings) = U.List.mapFoldl(VD.toList(reductionInfos), Rename.none, buildReduxEnd')
      val reduxInstrs = <- (distribute (reduxInstrs))
      val reduxInstructions = List.concat reduxInstrs

      (* create a renaming that goes from:
                   to the original variables,
                   to the last value variables,
                   to the new last value variables of this cleanup block *)
      val toCleanUpLastValues = Rename.compose (lastRenamings,
                                  Rename.compose (lastValues, originalVars))
      val toEndRenamings = Rename.compose (endRenamings, originalVars)
      (* rename the transfer *)
      val transfer'' = MilRename.Var.transfer (config, toCleanUpLastValues, transfer')
      val transfer'' = MilRename.Var.transfer (config, toEndRenamings, transfer'')
      
      val instructions = reduxInstructions @ instructions' @ [newTestInstr]
      val cleanUpBlock = M.B {parameters = Vector.new0 (),
                              instructions = Vector.fromList instructions,
                              transfer = transfer''}
    in
      cleanUpBlock
    end)                 

 (* /renameAndAddTail/  *)
 val renameAndAddTail : (state * env * info * M.block) ->
                        ((M.label * M.block) * (M.label * M.block) * Rename.t) option =
   fn (state, env, info, block) =>
    try (fn () =>
     let
       val symTableM = stateGetSymTableM state
       val LI {loopHeader, ...} = info

       val newHeaderLabel = SymTableM.labelFresh symTableM
       val tailLabel = SymTableM.labelFresh symTableM

       (* rename the variables in a block *)
       val (block', renamer) = renameVariables (state, env, info, block)
       val (oldNames, newNames) = List.unzip (Rename.toList (renamer))
 
       val M.B {parameters, instructions, transfer} = block'
       val switch = <- (MU.Transfer.Dec.tCase transfer)
       val () = <@ MU.Selector.Dec.seConstant (#select switch)
       val (targets, rebuildSwitch) = <- (hasTwoTargets switch)
       val ((headTarget, exitTarget), reorderTargets) = <- (identifyTargets loopHeader targets)
       val M.T {block = exitTargetLabel, arguments = exitArguments} = exitTarget
       val M.T {block = headTargetLabel, arguments = headArguments} = headTarget
       
       (* create new tail block *)
         (* make new vars to bind the previous exit arugments *)
       val mkArgVar = fn arg => mkFreshLocalVar (state, "", operandType (state, env, arg))
       val exitArgVars = Vector.map (exitArguments, mkArgVar)
       val exitArgs' = Vector.map (exitArgVars, fn var => M.SVariable var)

       val tailParams = Vector.concat [exitArgVars, Vector.fromList oldNames]
       val tailTransfer = M.TGoto (M.T {block = exitTargetLabel, arguments = exitArgs'})
       val tailBlock = M.B {parameters = tailParams,
                            instructions = Vector.new0 (),
                            transfer = tailTransfer}
       
 
       (* change targets of original block *)
       (* create new arguments *)
       val variableOutArgs = Vector.map(Vector.fromList newNames, fn x => M.SVariable x)
       val newExitArgs = Vector.concat [exitArguments, variableOutArgs]
       val exitTarget' = M.T {block = tailLabel,
                              arguments = newExitArgs}
       val headTarget' = M.T {block = newHeaderLabel,
                              arguments = headArguments}

       val switch' = (rebuildSwitch o reorderTargets) (headTarget', exitTarget')
       val block'' = M.B {parameters = parameters,
                          instructions = instructions,
                          transfer = M.TCase switch'}
     in
       ((newHeaderLabel, block''), (tailLabel, tailBlock), renamer)
     end)

 (* /rewriteTransfer/ - Given a (two-target) transfer, a label to the new loop header
                        and a label to the cleanup block, and a mapping from variables
                        to their last value calculations-
                        compute a new transfer where any loop parameters are renamed to their
                        last value variables, the previous loop header target is relabelled
                        to the new loop header, and the previous exit label is renamed to the
                        cleanup label *)
 val rewriteTransfer : (state * env * info * M.transfer * (M.label * M.label) * Rename.t * Rename.t) ->
                       M.transfer option =
  fn (state, env, info, transfer, (headerLabel, cleanupLabel), reductionDefs, lastValues) =>
    try (fn () =>
      let
        val E {config, ...} = env
        val LI {loopHeader, ...} = info

        (* rename variables to their *last* version *)
        val transfer' = MilRename.Var.transfer (config, lastValues, transfer)
        (* rename any reduction variables to point to their vector version *)
        val transfer' = MilRename.Var.transfer (config, reductionDefs, transfer')

        (* deconstruct transfer *)    
        val switch = <- (MU.Transfer.Dec.tCase transfer')
       val () = <@ MU.Selector.Dec.seConstant (#select switch)
        val ((target1, target2), rebuildSwitch) = <- (hasTwoTargets switch)
        val ((loopTarget, exitTarget), reorderTargets) = <- (identifyTargets loopHeader (target1, target2))
                         
        (* make new exit target pointing to cleanup block *)
        val M.T {block = _, arguments = loopArguments} = loopTarget
        val loopTarget' = M.T {block = headerLabel, arguments = loopArguments}
        val exitTarget' = M.T {block = cleanupLabel, arguments = Vector.new0 ()}
       
        val switch' = (rebuildSwitch o reorderTargets) (loopTarget', exitTarget')
        val transfer' = M.TCase switch'
      in
        transfer'
      end
    )

 (* /buildReduxInt/ - create instructions from a variable which is the initial value of a reduction,
                      lifting this initial value for vector reduction *)
 val buildReduxInit : (state * env * info * M.variable * Rename.t * Rename.t * reductionInfo VD.t) ->
                      (M.instruction list * Rename.t) option =
  fn (state, env, info, paramVar, paramRenamer, reductionDefs, reductionInfos) =>
  try (fn () => 
    let 
      val LI {vectorLength, ...} = info
      val vectorLength = <- vectorLength
    in 
      if Rename.renamed(reductionDefs, paramVar) then
        let
          val {operator, typ, ...} = <@ VD.lookup(reductionInfos, paramVar)
          val vDescriptor = <@ mkVDescriptor (env, info, Mil.TNumeric typ)
          val initConst = 
              case operator
                 (* for + and *, make a vector of identities with the initial parameter in the
                  * first position in the vector *)
               of M.Prims.APlus =>
                  let 
                    val identityConst = <@ operatorIdentity (env, operator, typ, NONE)
                    val identities = List.tabulate (vectorLength, (fn _ => identityConst))
                    val init = (Mil.SVariable paramVar)::(tl identities)
                  in
                    Vector.fromList init
                  end
                | M.Prims.ATimes =>
                  let 
                    val identityConst = <@ operatorIdentity (env, operator, typ, NONE)
                    val identities = List.tabulate (vectorLength, (fn _ => identityConst))
                    val init = (Mil.SVariable paramVar)::(tl identities)
                  in
                    Vector.fromList init
                  end
                    (* for max and min, lift the parameter to the whole vector - idempotency *)
                | M.Prims.AMax =>
                  Vector.tabulate (vectorLength, (fn _ => M.SVariable paramVar))
                | M.Prims.AMin =>
                  Vector.tabulate (vectorLength, (fn _ => M.SVariable paramVar))
                | _ => Try.fail ()
          val primR = Mil.Prims.ViData {descriptor = vDescriptor,
                                        operator = Mil.Prims.DVector}
          val rhsR = Mil.RhsPrim {prim = M.Prims.Vector primR,
                                  createThunks = false,
                                  typs = Vector.new1 (M.TNumeric typ),
                                  args = initConst}
          val varR = <@ mkRelatedVectorVar (state, env, info, paramVar, "redux_init")
          val instrR = M.I {dests = Vector.new1 varR,
                            n = 0,
                            rhs = rhsR}
          val paramRenamer' = Rename.renameTo(paramRenamer, paramVar, varR)
        in
          ([instrR], paramRenamer')
        end
     else
       ([], paramRenamer)
    end)

 (* /tripCountToInitBoundCheck/ from a trip count compute the initial bounds check
        to decide whether the iteration space is large enough for at least one 
        pass of the vectorized loop - test = i0 + (vl - 1)*s < n
        where i0 is the initial loop-controlling parameter value, vl is the vector
        length, s is the step, and n is the loop bound *)
 val tripCountToInitBoundCheck : (state * env * info * MilLoop.tripCount * M.variable) ->
                                    (M.variable * M.instruction list) option = 
   fn (state, env, info, tripCount, i0var) =>
    try (fn () =>
      let
       val E {config, ...} = env
       val symTableM = stateGetSymTableM state
       val LI {vectorLength, ...} = info
       val vectorLength = <- vectorLength

       val MilLoop.TC {block, cond, flip1, comparison, flip2, init, step, bound} = tripCount
       val typ = operandType (state, env, bound)
       (* only allow precise numbers currently *)
       val numTyp = <- (MU.Typ.Dec.tNumeric typ)
       val () = require (isPreciseNumericTyp numTyp)

       (* create instruction to compute the value of the loop variable at the end of
          the first vector iteration *)
         (* C = (vl - 1) * step *)
       val (step', useAddition) = <- (addOrSubtractRat (step, typ))
       val step' = Rat.*(Rat.fromInt (vectorLength - 1), step')
       val C = <@ ratToConstant (state, step', numTyp)

       (* construct the prim *)
       val operator = if useAddition then
                        Mil.Prims.APlus
                      else
                        Mil.Prims.AMinus

       val primOp = Mil.Prims.PNumArith {typ = numTyp,
                                         operator = operator}
       val rhs = M.RhsPrim {prim = Mil.Prims.Prim primOp,
                            createThunks = false,
                            typs = Vector.new0 (),
                            args = Vector.new2 (M.SVariable i0var,
                                                C)}
       val varIterN = mkFreshLocalVar (state, "", typ)
       val simpleIterN = M.SVariable varIterN
       val instrIterN = M.I {dests = Vector.new1 varIterN,
                             n = 0,
                             rhs = rhs}
       
       (* build the test *)
       fun swap (a, b) = (b, a)
       (* trip count describes the *exit* conditions, but we reverse this to be 
          entry conditions, thus the behaviour of flip1 is inverted - flip1 controls
          the comparison for exit conditions in the trip count - see description in loop.sml *) 
       val testCompare = if flip1 then comparison else notPrim (comparison)
       val args = if flip2 then (bound, simpleIterN) else (simpleIterN, bound)
       val args = if flip1 then args else swap args

       val testVar = mkFreshLocalVar (state, "", MU.Bool.t config)
       val testPrim = Mil.Prims.PNumCompare {typ = numTyp,
                                             operator = testCompare}
       val testRhs = M.RhsPrim {prim = Mil.Prims.Prim testPrim,
                                createThunks = false,
                                typs = Vector.new0 (),
                                args = Vector.new2 args}
       val testInstr = M.I {dests = Vector.new1 testVar, 
                            n = 0,
                            rhs = testRhs}
      in
       (testVar, [instrIterN, testInstr])
      end)

 (* /buildEntry/ - Takes a list of param, a loop induction variable used in the test,
                        and three labels: 1). to label the new entry header, 2) and 3) which are the
                        vector and serial loop headers *)
 val buildEntry : (state * env * info * (M.variable vector) * M.transfer * M.label * M.label * M.label
                        * Rename.t * reductionInfo VD.t)
                       -> M.block option =
  fn (state, env, info, 
      params, serialTransfer, entryHeader, vectorHeader, serialHeader, reductionDefs, reductionInfos) =>
    try (fn () =>
     let
       val E {config, loopInfo, ...} = env
       val LI {loopHeader, vectorLength, ...} = info
       val failDebug = Config.debug andalso (whyFail config)
       val symTableM = stateGetSymTableM state
       val si = I.SymbolInfo.SiManager symTableM 

        val () = if failDebug then print "[V] Building initial reduction values...\n" else ()
       (* build the initial vector values for reduction variables *)
       val buildReduxInit' = fn (v, r) =>
                              case buildReduxInit (state, env, info, v, r, reductionDefs, reductionInfos)
                                of SOME (instr, r') => (SOME instr, r')
                                 | NONE => (NONE, r)
       val (initInstrs, paramInitRenamer) = Vector.mapAndFold (params, Rename.none, buildReduxInit')
       val initInstrs = <- (distribute (Vector.toList(initInstrs)))
       val initInstrs = List.concat initInstrs

       (* rename the parameters *)
       val paramRename = fn (param, renamer) =>
                           let
                             val info = SymTableM.variableInfo (symTableM, param)
                             val M.VI {typ, kind} = info
                             val param' = mkFreshLocalVar (state, "", typ)
                             val renamer' = Rename.renameTo (renamer, param, param')
                           in
                             (param', renamer')                           
                           end
       val (params', paramRenamer) = Vector.mapAndFold (params, Rename.none, paramRename)

       (* apply renaming to the generated initial vector instructions *)
       val initInstrs = List.map(initInstrs, fn i => MilRename.Var.instruction (config, paramRenamer, i))

       val () = if failDebug then print "[V] Identifying the test...\n" else ()
       (* get the base induction variable and its information for the induction variable that
          is tested in the condition for exiting/re-entering the loop *)
       val ((loopIVop, _), _) = <@ identifyTestParams (state, env, info, serialTransfer)
       val loopIV = <- (MU.Simple.Dec.sVariable loopIVop)
       val loopIVinfo = <@ isInductionVariable (state, env, info, loopIV)
       val baseIVvar = case loopIVinfo
                         of ML.BIV {variable, ...} => variable
                          | ML.DIV {base = {variable, ...}, ...} => variable

      (* the loopIV variable should be one of the parameters *)
       val () = require (Vector.contains (params, baseIVvar, I.variableEqual))
       val loopIVparam = <- (Rename.use' (paramRenamer, baseIVvar))
       val loopIVtyp = variableType (state, env, loopIVparam)
       (* get the int-arb typ and check the loop IV is a precise type *)
       val loopIVNumTyp = <- (MU.Typ.Dec.tNumeric loopIVtyp)
       val () = require (isPreciseNumericTyp loopIVNumTyp)

       val () = if failDebug then print "[V] Find entry test...\n" else ()
       (* generate an iteration space check, from loop's tripcount,
          to decide whether to go into the vector loop or serial loop *)
       val tripCount = <- (MilLoop.getTripCount (loopInfo, loopHeader))
       val (testVar, boundsCheck) = <@ tripCountToInitBoundCheck (state, env, info, tripCount, loopIVparam)

       (* build vector target *)
       val arguments = Vector.map(params', fn p => Mil.SVariable p)                      
       (* replace reduction parameters with their initial vector versions *)
       val paramInitRenamer' = Rename.compose(paramInitRenamer, Rename.invert paramRenamer)
       val useInits = fn arg => case arg 
                                 of M.SVariable var => M.SVariable (Rename.use(paramInitRenamer', var))
                                  | arg             => arg
       val argumentsV = Vector.map(arguments, useInits)
       val targetVector = M.T {block = vectorHeader,
                               arguments = argumentsV}
       val caseVector = (MU.Bool.T config, targetVector)

       (* build serial target *)
       val targetSerial = M.T {block = serialHeader,
                               arguments = arguments}
       val caseSerial = (MU.Bool.F config, targetSerial)

       (* build the transfer and block*)
       val transfer = M.TCase {select = M.SeConstant, 
                               on = M.SVariable testVar,
                               cases = Vector.new2 (caseVector, caseSerial),
                               default = NONE}

       val block = M.B {parameters = params',
                        instructions = Vector.fromList (initInstrs @ boundsCheck),
                        transfer = transfer}
       val () = if failDebug then print "[V] Finished entry...\n" else ()
     in
       block
     end)
                   
 (* /vectorizeLoop/ *)               
 val rec vectorizeLoop : (state * env * ML.loop) ->
                         ((ML.loop Vector.t) * (Mil.block LD.t new)) option =
  fn (state, env, loop) =>
    try (fn () =>
         
      let
        val E {config, program, fmil, code, cfg, domInfo, loopInfo, ...} = env
        val failDebug = Config.debug andalso (whyFail config)
        val symTableM = stateGetSymTableM state

        val MilLoop.L {header, blocks} = loop
        val () = if debugPass config then print ("[V] loop: " ^ I.labelString header ^ "\n") else ()

        (* XXX HACK I've pulled this out of mayVectorize to avoid doing the dependence
         * analysis on loops we obviously don't care about because the dataflow analysis
         * in the dependence analysis seems to use an exponential algorithm, and also may
         * have termination issues. FIXME -leaf *)
        (* **** Check just single block *)
        val () = 
            if LD.size blocks = 1 then ()
            else
              if whyFail (getConfig env) then
                let val () = print "Vectorization failed because: "
                    val () = print ((concat ["More than one block in loop"]) ^ "\n")
                in fail()
                end
              else
                fail()

        (* get dependency info *)
        val varInfo = <- (MDA.loop' (config, program, code, header, cfg, domInfo,
                                    MilLoop.allNodes loopInfo,
                                    MilLoop.inductionVars loopInfo,
                                    MilLoop.allTripCounts loopInfo, loopInfo))
        val block = <- (LD.lookup (blocks, header))
        val (M.B {parameters, instructions, transfer}) = block
        val info = LI {dependencyInfo = varInfo,
                       loopHeader = header,
                       loopParams = parameters,
                       vectorLength = NONE}

        (* Pass to see if the loop fits the vectorization pattern *)
        val () = <?@ mayVectorize (state, env, info, loop) (env, ["May vectorize failed"])

        (* Create new labels *)
        val entryLabel = SymTableM.labelFresh symTableM 
        val vectorLoopLabel = SymTableM.labelFresh symTableM
        val cleanupLabel = SymTableM.labelFresh symTableM

        (* Tag instructions with their vectorization approach *)
        (* Stage 3.0 *)
        val () = if failDebug then print "[V] Tagging blocks...\n" else ()
        val {tags, minSize, maxSize, ...} = tagBlocks (state, env, info, blocks) 

        (* make a choice about the vector length. 
         * Bigger vector length (vectorBitSize/minSize) = more packing, possible emulation
         * Smaller vector length (vectorBitSize/maxSize) = less packing, no emulation   *)
        val vectorLength = <?- (calcVectorLen (state, env, minSize, maxSize)) (env, ["Failed to choose len"])
        (* check vectorLength is at least 2, so that the vectorization is worth doing, but also
         * some other computations rely on this (see buildCleanUp and partial reduction) *) 
        val () = requireR (vectorLength >= 2) (env, ["[V] Vectorization skipped as vector length < 2"])
        val info = updateVectorLength (info, vectorLength)

        val () = if debugPass config then printTaggedBlocks (state, env, blocks, tags) else ()
        val () = if debugPass config then printSizeInfo (state, env, minSize, maxSize) else ()

        (* Produce vector and last value calculations for base induction variables *)
        val () = if failDebug then print "[V] Vectorizing base induction variables...\n" else ()
        val (instrs0V, instrs0L, (vRenamings, lastValues)) = <@ transformBIVs (state, env, info)

        (* Vectorize the instructions *)
        (* Stage 3.1 *)
        val () = if failDebug then print "[V] Vectorizing instructions...\n" else ()
        val instructionsL = Vector.toList instructions
        val (scalars, vectors, lasts, (vRenamings', lastValues')) = 
            <@ transformInstructions (state, env, info, tags, instructionsL, (vRenamings, lastValues))

        (* zip the instructions together *)
        val instrs0 = pointwiseConcat (instrs0V, instrs0L)
        val instructions' = pointwiseConcat (scalars, (pointwiseConcat (vectors, lasts)))
        val instructions' = (List.concat instrs0) @ (List.concat instructions') 

        (* Adjust the test for the vectorized block *)
        val () = if failDebug then print "[V] Changing the test of vectorized loop...\n" else ()
        val (testInstrs, transferV) = <@ buildNewTest (state, env, info, transfer, lastValues')
        val instructions' = instructions' @ testInstrs   

        (* Replace the loop arguments that are vars with their last value var, and reduction
         * variables to their reduction vector variable and 
         * point the transfer to the loopHead to the top of the block and exit to the cleanup block *)
        val () = if failDebug then print "[V] Rewriting the transfer of the vector loop...\n" else ()
        val {reductionDefs, reductionInfos, ...} = vRenamings'
        val transferV' = <@ rewriteTransfer (state, env, info, transferV,
                                              (vectorLoopLabel, cleanupLabel), reductionDefs, lastValues')

        (* Create tail block for the serial loop that binds the final values of variables
         * defined in the loop to the original varibles *)
        val () = if failDebug then print "[V] Creating tail block for the serial loop...\n" else ()
        val ((header', block'), (tlLabel, tlBlock), serialRenamings) = <@ renameAndAddTail (state, env, info, block)
        val M.B {parameters = _, instructions = _, transfer = serialTransfer} = block' 
        (* Build the serial block *)
        val blocks' = (LD.insert (LD.empty, header', block'))
        val serialLoop = MilLoop.L {header = header', blocks = blocks'}      

        (* Create cleanup block *)
        val () = if failDebug then print "[V] Create cleanup blocks...\n" else ()
        val lasts' = (List.concat instrs0L) @ (List.concat lasts)
        val cleanupBlock = <@ buildCleanup (state, env, info, serialTransfer, serialRenamings,
                                             lastValues', lasts', reductionDefs, reductionInfos)

        (* Build the vectorized block *)
        val parameters' = Vector.map (parameters, fn pVar => Rename.use (reductionDefs, pVar))
        val blockV = Mil.B {parameters = parameters',
                            instructions = Vector.fromList instructions',
                            transfer = transferV'}
        (* Rename all variables in the vectorized block - to rename the original scalars *)
        val () = if failDebug then print "[V] Create cleanup blocks...\n" else ()
        val (blockV', vectorRenamings) = renameVariables (state, env, info, blockV)

        (* run the renamer generated for renaming the vectorized block over the cleanup block *)
        val (_, cleanupBlock') = MilRename.Var.block (getConfig env, vectorRenamings,
                                                       cleanupLabel, cleanupBlock)

        (* Build the vectorized loop *)        
        val blocksV = LD.fromList [(vectorLoopLabel, blockV')]
        val vectorLoop = MilLoop.L {header = vectorLoopLabel, blocks = blocksV}

        (* Build the entry block *)
        val () = if failDebug then print "[V] Create the prelude block...\n" else ()
        val entryBlock = <@ buildEntry (state, env, info, parameters, 
                                        transfer, header, vectorLoopLabel,
                                        header', reductionDefs, reductionInfos)

        (* create managing blocks to tie original loop and new loop *)
        val managingBlocks = LD.fromList [(header, entryBlock),
                                          (cleanupLabel, cleanupBlock'),
                                          (tlLabel, tlBlock)]
        val () = if failDebug then print "[V] Vectorization complete.\n" else ()
     in
        (Vector.new2 (vectorLoop, serialLoop), tagAsNew managingBlocks)
     end)

 (* /vectorizeLoopTree/ *)
 and rec vectorizeLoopTree : (state * env * ML.loopTree) -> (ML.loopForest * Mil.block LD.t new) =
  fn (state, env, loopTree) => 
      let val (Tree.T (node, children)) = loopTree 
      in
        if (Vector.isEmpty children) then
         (* leaf, inner loop. apply vectorization *)

         case (vectorizeLoop (state, env, node))
           of SOME (loops, blocks) =>
              let val singletonTree = fn x => Tree.T (x, Vector.new0 ())
              in  (Vector.map (loops, singletonTree), blocks)
              end
            | NONE => 
              let
                val failDebug = Config.debug andalso (whyFail (getConfig env))
                val () = if failDebug then print "[V] Vectorization failed.\n" else ()
              in
                (Vector.new1 loopTree, tagAsNew LD.empty)
              end
        else
          (*  XXX I think these blocks should be merged into the current node -leaf *)
          (* recurse on outer loops *)
          let val (children', blocks') = vectorizeForest (state, env, children)
          in (Vector.new1 (Tree.T (node, children')), blocks')
          end
      end

 (* /vectorizeForest/ - takes a loop forest (a vector of loop trees)
                       vectorizing the inner loops to return
                       a loop forest and a label dictionary of blocks not in loops *)
 and rec vectorizeForest : (state * env * ML.loopForest) -> (ML.loopForest * (Mil.block LD.t new)) =
    fn (state, env, forest) =>
       let 
         (* parameter to loopForest fold- vectorizes a loop and
          builds new forest and dictionary of blocks *)
         val vectorize : (ML.loopTree * Mil.block LD.t) -> (ML.loopForest * Mil.block LD.t) =
           fn (loopTree, blocks) =>
             let 
               val (loopForest', blocks') = vectorizeLoopTree (state, env, loopTree)
               val blocks'' = unionR (blocks, blocks')
             in (loopForest', blocks'')
             end

         val (forest', blocks') = Vector.mapAndFold (forest, LD.empty, vectorize)
       in (Vector.concatV forest', tagAsNew blocks')
       end
                                                         
 (* /doVectorize/ Takes a MilLoop, generates a (possibly) vectorized code body *)
 val doVectorize : (state * env) -> Mil.codeBody = 
  fn (state, env) =>
   let      
     val flattenLoop : MilLoop.loop * Mil.block LD.t -> Mil.block LD.t =
       fn (node, blocks) =>
           let
              val MilLoop.L {blocks = blocks', ...} = node
           in unionR (blocks, tagAsNew blocks')
           end          
                                   
     val flattenForest : ML.loopForest -> Mil.block LD.t =
       fn forest =>
          Vector.fold (forest, LD.empty,
                       (fn (loopTree, dict) =>
                           Tree.foldPre (loopTree, dict, flattenLoop)))

     (* --------------- *)

      val E {fmil, loopInfo, ...} = env

      val loops = MilLoop.getLoops loopInfo 
      val blocks = MilLoop.getBlocksNotInLoops loopInfo

      (* Perform vectorization of the forest *)
      val (loopForest', blocks') = vectorizeForest (state, env, loops)

      (* Flatten the forest into blocks *)
      val blocks'' = tagAsNew (flattenForest loopForest')
      (* Union, with right prefence on collisions:
                     original blocks not in loops,
                     new blocks not in loops,
                     new loop blocks *)
      val finalBlocks = unionR (unionR (blocks, blocks'), blocks'')                        

      val codeBody = Mil.CB {entry = MilLoop.getEntry loopInfo, 
                             blocks = finalBlocks}
   in
     codeBody
   end

 (* -------------------------------------------------------------------------- *)
 (*  Depth 0 : Pass through a program's globals, and process each global,      *)
 (*            generating new code bodies for GCodes                           *)
 (* -------------------------------------------------------------------------- *)

 (* /changeBody/ replaces the body field of a code record *)
 val changeBody : (Mil.code * Mil.codeBody) -> Mil.code =
  fn (code, codeBody) =>
     let
       val (Mil.F {fx, escapes, recursive, cc, args, rtyps, body}) = code
     in
       Mil.F {fx = fx,
              escapes = escapes,
              recursive = recursive,
              cc = cc,
              args = args,
              rtyps = rtyps,
              body = codeBody}
      end

 (* /global/ Vectorize a global *)
 val global : (Config.t * Mil.t * FMil.t * state *
               (Mil.variable * Mil.global)) -> Mil.global =
  fn (config, program, fmil, state, (var, global)) => 
       case global 
         of Mil.GCode (code as Mil.F {body, ...}) => 
           let
             val si = stateGetSi state
             val cfgInfo = MilCfg.build (config, si, body)
             val labelBlockDomTree = MilCfg.getLabelBlockDomTree cfgInfo
             val ldomtree = Tree.map (labelBlockDomTree, fn (l, b) => l)
             val ldominfo = MilCfg.LabelDominance.new ldomtree

             val loopInfo = ML.build (config, si, cfgInfo, labelBlockDomTree)
             val loopInfo = ML.addPreheaders (loopInfo, stateGetSymTableM state) 
             val loopInfo = ML.genAllNodes loopInfo
             val loopInfo = ML.genExits loopInfo
             val loopInfo = ML.genInductionVariables (loopInfo, fmil, cfgInfo)
             val loopInfo = ML.genTripCounts (loopInfo, fmil, cfgInfo, labelBlockDomTree)
             val loopInfo = ML.genBinderLocations loopInfo

             val env = E {config = config,
                          vectorConfig = PU.VectorConfig.build config,
                          program = program,
                          fmil = fmil,
                          code = code,
                          cfg = cfgInfo,
                          domInfo = ldominfo,
                          loopInfo = loopInfo}

           in
               let
                 val body' = doVectorize (state, env)
               in
                 Mil.GCode (changeBody (code, body'))
               end
           end

          | other => other

 (* /program/ - Vectorizes a program *)
 val program : (Mil.t * PassData.t) -> Mil.t =
  fn (program, passData) =>
    let
      val config = PassData.getConfig passData
      (* Set up the initial environment *)
      val (program', _) = MilNumberInstructions.program (config, program)
      val (M.P {includes, externs, globals, symbolTable, entry}) = program'
      val fmil = FMil.program' (config, program')
      val symTableM = SymTableM.fromExistingAll symbolTable
      val egs = ref []
      val state = S {symTableM = symTableM, globals = egs}
      (* Process each global *)
      val globals = VD.map (globals, (fn x => global (config, program, fmil, state, x)))

      (* Finalize the symbol table *)
      val symbolTable' = SymTableM.finish(symTableM)

      val globals = VD.insertAll (globals, !egs)
     in
       M.P {includes = includes,
            externs = externs,
            globals = globals,
            symbolTable = symbolTable',
            entry = entry}
     end
               
 val description = {name        = passname,
                    description = "Vectorizer",
                    inIr        = BothMil.irHelpers,
                    outIr       = BothMil.irHelpers,
                    mustBeAfter = [],
                    stats       = []}

 val associates = {controls  = [],
                   debugs = ([debugPassD, whyFailD] @ MDA.debugs),
                   features  = [allowReductionsF],
                   subPasses = []}
 val pass =
     Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end (* structure MilVectorize *)
 
